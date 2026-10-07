(* Random well-formed RelWire programs, for checking the projection property:
   every role's specialization, run together, must produce one consistent
   execution of the source program.

   Shape: an open-drain clocked bus (SCL, SDA, both dominant-low with
   pull-ups). Role "a" owns the clock; fields are owned by random roles
   among a, b, c. The body is a random sequence of segments: plain bit
   cells, a hardware-style loop over a field, a literal check, or a branch
   on a bit that every role has already sampled. Windows are wide enough
   that single issue has no reaction hazards. *)
open Relwire

let roles = [ "a"; "b"; "c" ]
let forever = 1_000_000
let t_low = 24
let t_high = 16

let wires =
  [ { name = "SCL"; resolution = DominantLow; bias = PullUp };
    { name = "SDA"; resolution = DominantLow; bias = PullUp } ]

let cell field idx =
  [ Put { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L1; time = "rise"; min_after = t_low; max_after = forever };
    Sample { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L0; time = "fall"; min_after = t_high; max_after = forever } ]

type spec = {
  prog : program;
  fields : (string * int * role) list;  (* name, width, owner *)
}

(* Total field bits stay within the core's 64-bit data memory, and the
   program within 128 compiled words. *)
let generate st =
  let nfields = 1 + Random.State.int st 5 in
  let fields =
    List.init nfields (fun i ->
        ( Printf.sprintf "f%d" i,
          1 + Random.State.int st 6,
          List.nth roles (Random.State.int st 3) ))
  in
  let lit_owner = List.nth roles (Random.State.int st 3) in
  let sampled = ref [] in
  let pick () = List.nth fields (Random.State.int st nfields) in
  let rec segment depth =
    match Random.State.int st (if depth > 0 then 3 else 4) with
    | 0 ->
        (* a few individual cells *)
        let f, w, _ = pick () in
        let n = 1 + Random.State.int st (min w 3) in
        let start = Random.State.int st (w - n + 1) in
        List.concat
          (List.init n (fun k ->
               sampled := (f, start + k) :: !sampled;
               cell f (Const (start + k))))
    | 1 ->
        (* a loop over a whole field (an outer repeat becomes a hardware LOOP) *)
        let f, w, _ = pick () in
        if depth = 0 then begin
          List.iter (fun k -> sampled := (f, k) :: !sampled) (List.init w Fun.id);
          [ Repeat { var = "i"; count = w; body = cell f (Loop "i") } ]
        end
        else
          List.concat
            (List.init w (fun k ->
                 sampled := (f, k) :: !sampled;
                 cell f (Const k)))
    | 2 ->
        (* a literal: its owner drives it, everyone else checks it *)
        cell "lit" (Const 0)
    | _ -> (
        (* branch on a bit every role has already sampled *)
        match !sampled with
        | [] -> segment depth
        | s ->
            let f, b = List.nth s (Random.State.int st (List.length s)) in
            let saved = !sampled in
            let then_ = segment (depth + 1) in
            sampled := saved;
            let else_ = if Random.State.bool st then segment (depth + 1) else [] in
            sampled := saved;
            [ If_bit { field = f; idx = Const b; level = (if Random.State.bool st then L1 else L0);
                       then_; else_ } ])
  in
  let nseg = 1 + Random.State.int st 5 in
  let body = List.concat (List.init nseg (fun _ -> segment 0)) in
  let body =
    Edge { wire = "SCL"; level = L0; time = "start"; min_after = 10; max_after = forever } :: body
  in
  let prog =
    { wires;
      decls =
        List.map (fun (name, width, owner) -> Field { name; width; owner }) fields
        @ [ Lit { name = "lit"; value = L0; owner = lit_owner };
            Time { name = "start"; owner = "a" };
            Time { name = "rise"; owner = "a" };
            Time { name = "fall"; owner = "a" } ];
      body;
      constraints = [] }
  in
  { prog; fields }

let random_inputs st spec role =
  List.filter_map
    (fun (name, width, owner) ->
      if owner = role then
        Some (name, List.init width (fun _ -> if Random.State.bool st then L1 else L0))
      else None)
    spec.fields
