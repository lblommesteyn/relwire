(* Parser for .rw source files. Hand-written: no parser generator needed.

   Names must be declared before use. Macros are token-level: [use m(a, b)]
   splices m's body with each parameter replaced by one argument token. *)
open Relwire

type tok = Id of string | Int of int | Str of string | Sym of string | Eof
type ptok = { t : tok; line : int }

exception Parse_error of int * string

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'

let lex src =
  let n = String.length src in
  let toks = ref [] and line = ref 1 and i = ref 0 in
  let push t = toks := { t; line = !line } :: !toks in
  while !i < n do
    let c = src.[!i] in
    if c = '\n' then (incr line; incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '#' then (while !i < n && src.[!i] <> '\n' do incr i done)
    else if is_alpha c then begin
      let j = ref !i in
      while !j < n && (is_alpha src.[!j] || is_digit src.[!j]) do incr j done;
      push (Id (String.sub src !i (!j - !i)));
      i := !j
    end
    else if is_digit c then begin
      let j = ref !i in
      while !j < n && (is_digit src.[!j] || src.[!j] = '_') do incr j done;
      push (Int (int_of_string (String.sub src !i (!j - !i))));
      i := !j
    end
    else if c = '"' then begin
      let j = ref (!i + 1) in
      while !j < n && src.[!j] <> '"' do incr j done;
      if !j >= n then raise (Parse_error (!line, "unterminated string"));
      push (Str (String.sub src (!i + 1) (!j - !i - 1)));
      i := !j + 1
    end
    else if !i + 1 < n && List.mem (String.sub src !i 2) [ "->"; ">="; "==" ] then begin
      push (Sym (String.sub src !i 2));
      i := !i + 2
    end
    else if String.contains "[](){},@=+-*/:" c then (push (Sym (String.make 1 c)); incr i)
    else raise (Parse_error (!line, Printf.sprintf "unexpected character %C" c))
  done;
  push Eof;
  List.rev !toks

type st = {
  mutable toks : ptok list;
  consts : (string, int) Hashtbl.t;
  macros : (string, string list * ptok list) Hashtbl.t;
  mutable wires : wire_spec list;
  mutable decls : decl list;
  mutable body : stmt list option;
  mutable constraints : constr list;
}

let cur st = match st.toks with x :: _ -> x | [] -> { t = Eof; line = 0 }
let peek st = (cur st).t
let err st msg = raise (Parse_error ((cur st).line, msg))
let next st = match st.toks with x :: r -> st.toks <- r; x.t | [] -> Eof

let show = function
  | Id s -> s | Int n -> string_of_int n | Str s -> "\"" ^ s ^ "\""
  | Sym s -> s | Eof -> "end of file"

let expect st s =
  match peek st with
  | Sym x when x = s -> ignore (next st)
  | t -> err st (Printf.sprintf "expected '%s', found '%s'" s (show t))

let ident st =
  match peek st with
  | Id x -> ignore (next st); x
  | t -> err st (Printf.sprintf "expected a name, found '%s'" (show t))

let kw st k =
  match peek st with
  | Id x when x = k -> ignore (next st)
  | t -> err st (Printf.sprintf "expected '%s', found '%s'" k (show t))

let rec expr st =
  let v = ref (term st) in
  let rec loop () =
    match peek st with
    | Sym "+" -> ignore (next st); v := !v + term st; loop ()
    | Sym "-" -> ignore (next st); v := !v - term st; loop ()
    | _ -> ()
  in
  loop (); !v

and term st =
  let v = ref (atom st) in
  let rec loop () =
    match peek st with
    | Sym "*" -> ignore (next st); v := !v * atom st; loop ()
    | Sym "/" -> ignore (next st); v := !v / atom st; loop ()
    | _ -> ()
  in
  loop (); !v

and atom st =
  match peek st with
  | Int n -> ignore (next st); n
  | Id "inf" -> ignore (next st); 1_000_000
  | Id x -> (
      match Hashtbl.find_opt st.consts x with
      | Some v -> ignore (next st); v
      | None -> err st ("unknown constant " ^ x))
  | Sym "(" -> ignore (next st); let v = expr st in expect st ")"; v
  | t -> err st (Printf.sprintf "expected a number, found '%s'" (show t))

let level st =
  match peek st with
  | Int 0 -> ignore (next st); L0
  | Int 1 -> ignore (next st); L1
  | t -> err st (Printf.sprintf "expected 0 or 1, found '%s'" (show t))

let declared_field st name =
  List.exists
    (function Field { name = n; _ } | Lit { name = n; _ } | Derived { name = n; _ } -> n = name
            | _ -> false)
    st.decls

let declared_time st name =
  List.exists (function Time { name = n; _ } -> n = name | _ -> false) st.decls

let wire_ref st =
  let w = ident st in
  if not (List.exists (fun s -> s.name = w) st.wires) then err st ("undeclared wire " ^ w);
  w

let bit_ref st scope =
  let f = ident st in
  if not (declared_field st f) then err st ("undeclared field " ^ f);
  expect st "[";
  let idx =
    match peek st with
    | Id x when List.mem x scope -> ignore (next st); Loop x
    | _ -> Const (expr st)
  in
  expect st "]";
  (f, idx)

(* A time variable bound by an edge must be declared; After marks are not
   variables, so their names are free. *)
let time_ref st =
  expect st "@";
  let t = ident st in
  if not (declared_time st t) then err st ("undeclared time " ^ t);
  t

let window st =
  kw st "in"; expect st "[";
  let a = expr st in
  expect st ",";
  let b = expr st in
  expect st "]"; (a, b)

let rec block st scope =
  expect st "{";
  let rec go acc =
    match peek st with
    | Sym "}" -> ignore (next st); List.rev acc
    | _ -> go (List.rev_append (stmt st scope) acc)
  in
  go []

and stmt st scope =
  match peek st with
  | Id "edge" ->
      ignore (next st);
      let wire = wire_ref st in
      expect st "->";
      let level = level st in
      let time = time_ref st in
      let min_after, max_after = window st in
      [ Edge { wire; level; time; min_after; max_after } ]
  | Id ("put" | "put_not" | "sample" as k) ->
      ignore (next st);
      let wire = wire_ref st in
      let field, idx = bit_ref st scope in
      [ (match k with
         | "put" -> Put { wire; field; idx }
         | "put_not" -> Put_not { wire; field; idx }
         | _ -> Sample { wire; field; idx }) ]
  | Id "after" ->
      ignore (next st);
      let time = ident st in
      [ After { time; ticks = expr st } ]
  | Id "toggle" ->
      ignore (next st);
      let wire = wire_ref st in
      let field, idx = bit_ref st scope in
      expect st "@";
      let time = ident st in
      kw st "at";
      let nominal = expr st in
      let min_after, max_after = window st in
      [ Toggle { wire; field; idx; time; nominal; min_after; max_after } ]
  | Id "if_run" ->
      ignore (next st);
      let wire = wire_ref st in
      let n = expr st in
      let set =
        match peek st with
        | Id "set" ->
            ignore (next st);
            let f = ident st in
            if not (declared_field st f) then err st ("undeclared field " ^ f);
            Some f
        | _ -> None
      in
      [ If_run { wire; n; set; body = block st scope } ]
  | Id "if" ->
      ignore (next st);
      let field, idx = bit_ref st scope in
      expect st "==";
      let level = level st in
      let then_ = block st scope in
      let else_ =
        match peek st with
        | Id "else" -> ignore (next st); block st scope
        | _ -> []
      in
      [ If_bit { field; idx; level; then_; else_ } ]
  | Id "repeat" ->
      ignore (next st);
      let var = ident st in
      let count = expr st in
      [ Repeat { var; count; body = block st (var :: scope) } ]
  | Id "use" ->
      ignore (next st);
      let m = ident st in
      let params, body =
        match Hashtbl.find_opt st.macros m with
        | Some x -> x
        | None -> err st ("unknown macro " ^ m)
      in
      expect st "(";
      let rec args acc =
        let a = cur st in
        ignore (next st);
        match peek st with
        | Sym "," -> ignore (next st); args (a :: acc)
        | Sym ")" -> ignore (next st); List.rev (a :: acc)
        | t -> err st (Printf.sprintf "expected ',' or ')', found '%s'" (show t))
      in
      let args = if peek st = Sym ")" then (ignore (next st); []) else args [] in
      if List.length args <> List.length params then
        err st (Printf.sprintf "macro %s takes %d arguments" m (List.length params));
      let subst = List.combine params args in
      let line = (cur st).line in
      let expanded =
        List.map
          (fun p ->
            match p.t with
            | Id x when List.mem_assoc x subst -> { (List.assoc x subst) with line }
            | _ -> { p with line })
          body
      in
      (* Re-parse the spliced tokens as a block in the current scope. *)
      st.toks <- { t = Sym "{"; line } :: expanded @ ({ t = Sym "}"; line } :: st.toks);
      block st scope
  | t -> err st (Printf.sprintf "expected a statement, found '%s'" (show t))

let endpoint st =
  match peek st with
  | Id "change" -> ignore (next st); Change (wire_ref st)
  | Id x ->
      ignore (next st);
      if not (declared_time st x) then err st ("undeclared time " ^ x);
      At x
  | t -> err st (Printf.sprintf "expected an event, found '%s'" (show t))

let top st =
  match peek st with
  | Id "const" ->
      ignore (next st);
      let x = ident st in
      expect st "=";
      Hashtbl.replace st.consts x (expr st)
  | Id "wire" ->
      ignore (next st);
      let name = ident st in
      let resolution =
        match ident st with
        | "push_pull" -> PushPull
        | "dominant_low" -> DominantLow
        | "dominant_high" -> DominantHigh
        | r -> err st ("unknown resolution " ^ r)
      in
      let bias =
        match ident st with
        | "pull_up" -> PullUp
        | "pull_down" -> PullDown
        | "floating" -> Floating
        | b -> err st ("unknown bias " ^ b)
      in
      st.wires <- st.wires @ [ { name; resolution; bias } ]
  | Id "field" ->
      ignore (next st);
      let name = ident st in
      expect st "[";
      let width = expr st in
      expect st "]";
      kw st "from";
      st.decls <- st.decls @ [ Field { name; width; owner = ident st } ]
  | Id "lit" ->
      ignore (next st);
      let name = ident st in
      expect st "=";
      let value = level st in
      kw st "from";
      st.decls <- st.decls @ [ Lit { name; value; owner = ident st } ]
  | Id "derived" ->
      ignore (next st);
      let name = ident st in
      kw st "from";
      st.decls <- st.decls @ [ Derived { name; owner = ident st } ]
  | Id "time" ->
      ignore (next st);
      let rec names acc =
        let n = ident st in
        if peek st = Sym "," then (ignore (next st); names (n :: acc)) else List.rev (n :: acc)
      in
      let ns = names [] in
      kw st "from";
      let owner = ident st in
      st.decls <- st.decls @ List.map (fun name -> Time { name; owner }) ns
  | Id "macro" ->
      ignore (next st);
      let m = ident st in
      expect st "(";
      let rec params acc =
        match peek st with
        | Sym ")" -> ignore (next st); List.rev acc
        | Sym "," -> ignore (next st); params acc
        | _ -> params (ident st :: acc)
      in
      let ps = params [] in
      expect st "{";
      let rec grab depth acc =
        match st.toks with
        | { t = Sym "}"; _ } :: r when depth = 0 -> st.toks <- r; List.rev acc
        | ({ t = Sym "}"; _ } as x) :: r -> st.toks <- r; grab (depth - 1) (x :: acc)
        | ({ t = Sym "{"; _ } as x) :: r -> st.toks <- r; grab (depth + 1) (x :: acc)
        | { t = Eof; _ } :: _ | [] -> err st "unterminated macro"
        | x :: r -> st.toks <- r; grab depth (x :: acc)
      in
      Hashtbl.replace st.macros m (ps, grab 0 [])
  | Id "protocol" ->
      ignore (next st);
      if st.body <> None then err st "second protocol block";
      st.body <- Some (block st [])
  | Id "constraint" ->
      ignore (next st);
      let cname = match next st with Str s -> s | _ -> err st "expected a quoted name" in
      expect st ":";
      let from_ = endpoint st in
      expect st "->";
      let to_ = endpoint st in
      expect st ">=";
      st.constraints <- st.constraints @ [ { cname; from_; to_; min_ticks = expr st } ]
  | t -> err st (Printf.sprintf "expected a declaration, found '%s'" (show t))

let program_of_string src =
  let st = { toks = lex src; consts = Hashtbl.create 8; macros = Hashtbl.create 8;
             wires = []; decls = []; body = None; constraints = [] } in
  while peek st <> Eof do top st done;
  match st.body with
  | None -> raise (Parse_error (0, "no protocol block"))
  | Some body -> { wires = st.wires; decls = st.decls; body; constraints = st.constraints }

let program_of_file path =
  let ic = open_in_bin path in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  program_of_string src

let roles (prog : Relwire.program) =
  List.sort_uniq compare
    (List.map
       (function
         | Field { owner; _ } | Time { owner; _ } | Lit { owner; _ } | Derived { owner; _ } -> owner)
       prog.decls)
