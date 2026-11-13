(* Compiler for the RPM core (hw/rpm_core.v, ISA v1) and file glue for the
   differential test harness (hw/tb.v). *)
open Relwire

let inf = 0xFFFF
let dw = 64 (* data-memory bits per core *)

(* Data-memory layout: fields, literals and derived bits in declaration order. *)
let layout prog =
  let next = ref 0 in
  List.filter_map
    (function
      | Field { name; width; _ } | Crc { name; width; _ } ->
          let b = !next in next := b + width; Some (name, b)
      | Lit { name; _ } | Derived { name; _ } -> let b = !next in incr next; Some (name, b)
      | Time _ -> None)
    prog.decls

let wire_index prog w =
  let rec go i = function
    | [] -> failwith ("unknown wire " ^ w)
    | s :: r -> if s.name = w then i else go (i + 1) r
  in
  go 0 prog.wires

(* ---------- ISA v1 compiler ----------

   Compiles the source AST (not the unrolled machine) for one role: the
   outermost repeat becomes a zero-overhead LOOP whose loop variable indexes
   data addresses; deeper repeats are unrolled. Timing values go to an
   8-entry constant table. [specialize] still runs first as the lint. *)

type compiled = { words : int array; consts : int array }

(* Role index of each owner, in the program's sorted role order. *)
let role_ids prog =
  let roles =
    List.sort_uniq compare
      (List.map
         (function
           | Field { owner; _ } | Time { owner; _ } | Lit { owner; _ } | Derived { owner; _ }
           | Crc { owner; _ } -> owner)
         prog.decls)
  in
  if List.length roles > 4 then failwith "more than 4 roles";
  List.mapi (fun i r -> (r, i)) roles

(* The per-core register that replaces static specialization. *)
let role_mask prog = function
  | Observe_all -> 0
  | Run_as rs ->
      (* a role that owns nothing (a pure receiver) contributes no bit *)
      List.fold_left
        (fun m r -> match List.assoc_opt r (role_ids prog) with
           | Some i -> m lor (1 lsl i) | None -> m)
        0 rs

(* One binary for every role: owned instructions carry their owner's role
   id in [25:24] and bit 19 set; a core drives iff its mask has that role. *)
let compile prog =
  if List.length prog.wires > 4 then
    failwith (Printf.sprintf "%d wires: the chip has 4" (List.length prog.wires));
  List.iter (fun (r, _) -> ignore (specialize prog (Run_as [ r ]))) (role_ids prog);
  let rid = role_ids prog in
  let own_of role = Some (List.assoc role rid) in
  let lay = layout prog in
  let base f = List.assoc f lay in
  let is_lit f =
    List.exists (function Lit { name; _ } | Derived { name; _ } -> name = f | _ -> false) prog.decls
  in
  (* Checksum: PUT with bit 6 drives the register's top bit; SAMPLE with bit 6
     folds the sampled level in, and with bit 5 compares against the top bit
     instead of data memory (with the literal bit, so an observer checks). The
     polynomial, left-aligned to 16 bits, lives in constant-table entry 7. A
     checksum sent complemented sets the level bit of both; a nonzero start
     value is a SETCRC as the program's first instruction. *)
  let crc = crc_of prog in
  if List.length (List.filter (function Crc _ -> true | _ -> false) prog.decls) > 1 then
    failwith "more than one crc";
  (match crc with
   | Some (_, w, poly) when w < 1 || w > 16 || poly lsr w <> 0 -> failwith "a crc is 1 to 16 bits and its polynomial must fit in them"
   | _ -> ());
  let is_crc f = match crc with Some (n, _, _) -> n = f | None -> false in
  let crc_init, crc_inv = crc_cfg prog in
  (match crc with
   | Some (_, w, _) when crc_init lsr w <> 0 -> failwith "a crc's start value must fit in its width"
   | _ -> ());
  let consts = ref [] in
  (* 0xFFFF in the table means unbounded, so a finite value must stay below
     it: a longer one is refused, not shortened (the certificate is of the
     source values) *)
  let clamp v =
    if v = Relwire.inf then inf
    else if v >= inf then
      failwith
        (Printf.sprintf "a timing value of %d ticks does not fit the 16-bit table (at most %d): use a longer tick"
           v (inf - 1))
    else v
  in
  let find_seq vs =
    let arr = Array.of_list !consts in
    let n = Array.length arr and m = List.length vs in
    let rec at i =
      if i + m > n then None
      else if List.for_all2 (fun j v -> arr.(i + j) = v) (List.init m Fun.id) vs then Some i
      else at (i + 1)
    in
    at 0
  in
  let const_seq vs =
    let vs = List.map clamp vs in
    match find_seq vs with
    | Some i -> i
    | None ->
        let i = List.length !consts in
        consts := !consts @ vs;
        if List.length !consts > 8 then failwith "more than 8 distinct timing constants";
        i
  in
  let k v = const_seq [ v ] in
  let w = wire_index prog in
  let hdr op owner wire lvl =
    let o = match owner with Some r -> (r lsl 24) lor (1 lsl 19) | None -> 0 | _ -> 0 in
    o lor (op lsl 20) lor (wire lsl 17) lor (Bool.to_int lvl lsl 16)
  in
  (* env: unrolled loop variables; lv: the name bound to the hardware loop's
     index, if any; hw: inside the hardware loop (so deeper repeats unroll).
     An unrolled repeat that reuses lv's name shadows it: [one] drops lv for
     its body but stays inside the hardware loop. *)
  let resolve env lv field idx =
    match idx with
    | Const n -> (base field + n, false)
    | Loop v when Some v = lv -> (base field, true)
    | Loop v -> (base field + List.assoc v env, false)
  in
  let mem lit ix ad = (Bool.to_int lit lsl 15) lor (Bool.to_int ix lsl 14) lor (ad lsl 7) in
  let rec go ?(hw = false) env lv stmts = List.concat_map (one ~hw env lv) stmts
  and one ~hw env lv = function
    | Edge { wire; level; time; min_after; max_after } ->
        [ hdr 1 (own_of (owner_of prog time)) (w wire) (level = L1)
          lor (k min_after lsl 13) lor (k max_after lsl 10) ]
    | Put { wire; field; idx } | Put_not { wire; field; idx } as st ->
        let ad, ix = resolve env lv field idx in
        let inv = match st with Put_not _ -> true | _ -> false in
        let inv = if is_crc field then inv <> crc_inv else inv in
        [ hdr 2 (own_of (owner_of prog field)) (w wire) inv lor mem false ix ad
          lor (if is_crc field then 1 lsl 6 else 0) ]
    | Sample { wire; field; idx } ->
        let ad, ix = resolve env lv field idx in
        if is_crc field then
          [ hdr 3 (own_of (owner_of prog field)) (w wire) crc_inv lor mem true ix ad
            lor (1 lsl 6) lor (1 lsl 5) ]
        else [ hdr 3 (own_of (owner_of prog field)) (w wire) false lor mem (is_lit field) ix ad ]
    | Sample_into { wire; field; idx; _ } when is_crc field ->
        (* folding the checksum into itself is just sampling it *)
        one ~hw env lv (Sample { wire; field; idx })
    | Sample_into { wire; field; idx; _ } ->
        let ad, ix = resolve env lv field idx in
        [ hdr 3 (own_of (owner_of prog field)) (w wire) false lor mem (is_lit field) ix ad
          lor (1 lsl 6) ]
    | After { ticks; _ } -> [ hdr 4 None 0 false lor (k ticks lsl 13) ]
    | Toggle { wire; field; idx; nominal; min_after; max_after; _ } ->
        let ad, ix = resolve env lv field idx in
        let kk = const_seq [ nominal; min_after; max_after ] in
        if kk > 5 then failwith "toggle constant triple does not fit";
        [ hdr 5 (own_of (owner_of prog field)) (w wire) false lor mem false ix ad lor kk ]
    | Repeat { count = 0; _ } -> []  (* as the model: no iterations, no LOOP *)
    | Repeat { var; count; body } when not hw ->
        let b = go ~hw:true env (Some var) body in
        if count > 255 || List.length b > 255 then failwith "loop too large";
        if b = [] then [] else (hdr 9 None 0 false lor (count lsl 8) lor List.length b) :: b
    | Repeat { var; count; body } ->
        let lv = if lv = Some var then None else lv in
        List.concat (List.init count (fun i -> go ~hw ((var, i) :: env) lv body))
    | If_run { wire; n; set; body } ->
        let b = go ~hw env lv body in
        if List.length b > 31 || n > 7 then failwith "if_run body too large";
        let set_bits = match set with
          | Some f -> (1 lsl 12) lor (base f lsl 5) | None -> 0 in
        (hdr 6 None (w wire) false lor (n lsl 13) lor set_bits lor List.length b) :: b
    | If_bit { field; idx; level; then_; else_ } ->
        let ad, ix = resolve env lv field idx in
        let t = go ~hw env lv then_ and e = go ~hw env lv else_ in
        if e = [] then (hdr 7 None 0 (level = L1) lor mem false ix ad lor List.length t) :: t
        else
          ((hdr 7 None 0 (level = L1) lor mem false ix ad lor (List.length t + 1)) :: t)
          @ ((hdr 8 None 0 false lor List.length e) :: e)
  in
  (match List.rev lay with
   | (f, b) :: _ ->
       let width = match List.find_opt (function Field { name; _ } | Crc { name; _ } -> name = f | _ -> false) prog.decls with
         | Some (Field { width; _ }) | Some (Crc { width; _ }) -> width | _ -> 1 in
       if b + width > dw then failwith "data memory overflow"
   | [] -> ());
  let seed =
    match crc with
    | Some (_, w, _) when crc_init <> 0 -> [ hdr 10 None 0 false lor (crc_init lsl (16 - w)) ]
    | _ -> []
  in
  let words = Array.of_list (seed @ go [] None prog.body @ [ 0 ]) in
  if Array.length words > 128 then failwith "program larger than 128 instructions";
  let c = Array.make 8 0 in
  List.iteri (fun i v -> c.(i) <- v) !consts;
  (match crc with
   | Some (_, w, poly) ->
       if List.length !consts > 7 then failwith "a crc leaves room for 7 timing constants";
       c.(7) <- poly lsl (16 - w)
   | None -> ());
  { words; consts = c }

let data_bits prog (a : agent) =
  let lay = layout prog in
  let bits = Array.make dw None in
  List.iter
    (fun (name, base) -> Array.iteri (fun i v -> bits.(base + i) <- v) (Hashtbl.find a.env name))
    lay;
  bits

let hex_data bits =
  String.init (dw / 4) (fun k ->
      let nib = (dw / 4) - 1 - k in
      let v = ref 0 in
      for j = 0 to 3 do
        if bits.(nib * 4 + j) = Some L1 then v := !v lor (1 lsl j)
      done;
      "0123456789abcdef".[!v])

let outcome_code = function
  | Match -> 1 | Peer_asserted -> 2 | Arbitration_lost -> 3 | Deadline_missed -> 4
  | Early_edge -> 5 | Bad_wire | Collision -> 6 | Mismatch -> 7

let events prog (a : agent) =
  let lay = layout prog in
  List.rev_map
    (fun e ->
      let ad = match List.assoc_opt e.var lay with Some b -> b + e.bit | None -> 0 in
      Printf.sprintf "%d %d %d" e.tick (outcome_code e.outcome) ad)
    a.events

let write_file path lines =
  let oc = open_out_bin path in
  List.iter (fun l -> output_string oc l; output_char oc '\n') lines;
  close_out oc

(* Write cfg/prog/data for up to 4 cores; [agents] pairs each core's role
   assignment with a freshly made (unrun) agent holding its inputs. *)
(* Writes the loader byte stream (load.hex) that programs tt_um_relwire, and
   cfg.hex (wire biases, which are board-level and applied by the harness).
   Returns the compiled program and the stream length. *)
let write_case dir prog (cores : (assignment * agent) list) =
  write_file (Filename.concat dir "cfg.hex")
    (List.map
       (fun s ->
         let b = match s.bias with PullDown -> 0 | PullUp -> 1 | Floating -> 2 in
         Printf.sprintf "%02x" (b lsl 4))
       prog.wires
    @ List.init (4 - List.length prog.wires) (fun _ -> "20"));
  let c = compile prog in
  let bytes = ref [] in
  let emit l = bytes := !bytes @ l in
  Array.iteri
    (fun a wd -> emit [ 1; a; (wd lsr 24) land 0xff; (wd lsr 16) land 0xff; (wd lsr 8) land 0xff; wd land 0xff ])
    c.words;
  Array.iteri (fun i v -> emit [ 2; i; v lsr 8; v land 0xff ]) c.consts;
  List.iteri
    (fun i (asg, a) ->
      let bits = data_bits prog a in
      for byte = 0 to (dw / 8) - 1 do
        let v = ref 0 in
        for j = 0 to 7 do
          if bits.((byte * 8) + j) = Some L1 then v := !v lor (1 lsl j)
        done;
        emit [ 3; i; byte; !v ]
      done;
      emit [ 4; i; role_mask prog asg ])
    cores;
  List.iteri
    (fun i s ->
      emit [ 5; i; (match s.resolution with PushPull -> 0 | DominantLow -> 1 | DominantHigh -> 2) ])
    prog.wires;
  emit [ 6 ];
  write_file (Filename.concat dir "load.hex") (List.map (Printf.sprintf "%02x") !bytes);
  (c, List.length !bytes)

let trace_lines prog (tr : trace) =
  List.map
    (fun (t, vs) ->
      Printf.sprintf "%d %s" t
        (String.concat ""
           (List.map
              (fun s -> match List.assoc_opt s.name vs with
                 | Some L1 -> "1" | Some L0 -> "0" | None -> "x")
              prog.wires)))
    tr

(* A compiled program as a Python module, for the demo-board host library
   (host/relwire_tt.py): program words, timing constants, wire modes, the
   data-memory layout of every field, literal values and role ids. *)
let image_py ?(source = "") prog =
  let c = compile prog in
  let b = Buffer.create 4096 in
  let pr fmt = Printf.bprintf b fmt in
  pr "# Generated by relwirec%s. Load with host/relwire_tt.py.
"
    (if source = "" then "" else " from " ^ source);
  pr "WORDS = [%s]
" (String.concat ", " (Array.to_list (Array.map (Printf.sprintf "0x%07x") c.words)));
  pr "CONSTS = [%s]
" (String.concat ", " (Array.to_list (Array.map string_of_int c.consts)));
  pr "WIRES = [%s]  # (name, 0 push-pull / 1 dominant-low / 2 dominant-high)
"
    (String.concat ", "
       (List.map
          (fun s ->
            Printf.sprintf "(\"%s\", %d)" s.name
              (match s.resolution with PushPull -> 0 | DominantLow -> 1 | DominantHigh -> 2))
          prog.wires));
  let width name =
    List.find_map
      (function (Field { name = n; width; _ } | Crc { name = n; width; _ }) when n = name -> Some width
              | (Lit { name = n; _ } | Derived { name = n; _ }) when n = name -> Some 1
              | _ -> None)
      prog.decls
    |> Option.get
  in
  pr "LAYOUT = {%s}  # field: (first data-memory bit, width)
"
    (String.concat ", "
       (List.map (fun (n, base) -> Printf.sprintf "\"%s\": (%d, %d)" n base (width n)) (layout prog)));
  pr "LITS = {%s}
"
    (String.concat ", "
       (List.filter_map
          (function Lit { name; value; _ } -> Some (Printf.sprintf "\"%s\": %d" name (if value = L1 then 1 else 0))
                  | _ -> None)
          prog.decls));
  pr "ROLES = {%s}
"
    (String.concat ", " (List.map (fun (r, i) -> Printf.sprintf "\"%s\": %d" r i) (role_ids prog)));
  pr "TICK_CYCLES = 11
";
  Buffer.contents b
