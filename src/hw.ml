(* Encoder for the RPM core (hw/rpm_core.v, ISA v0) and file glue for the
   differential test harness (hw/tb.v). *)
open Relwire

let inf = 0xFFFF

(* Data-memory layout: fields, literals and derived bits in declaration order. *)
let layout prog =
  let next = ref 0 in
  List.filter_map
    (function
      | Field { name; width; _ } -> let b = !next in next := b + width; Some (name, b)
      | Lit { name; _ } | Derived { name; _ } -> let b = !next in incr next; Some (name, b)
      | Time _ -> None)
    prog.decls

let wire_index prog w =
  let rec go i = function
    | [] -> failwith ("unknown wire " ^ w)
    | s :: r -> if s.name = w then i else go (i + 1) r
  in
  go 0 prog.wires

let imm n = if n >= inf then inf else n

let encode prog assignment =
  let lay = layout prog in
  let addr f bit = List.assoc f lay + bit in
  let is_lit f =
    List.exists (function Lit { name; _ } | Derived { name; _ } -> name = f | _ -> false) prog.decls
  in
  let word ~op ?(sup = false) ?(wire = 0) ?(lvl = false) ?(lit = false) ?(ad = 0)
      ?(a = 0) ?(b = 0) ?(c = 0) () =
    let open Int64 in
    let f v sh = shift_left (of_int v) sh in
    List.fold_left logor 0L
      [ f op 60; f (Bool.to_int sup) 59; f wire 57; f (Bool.to_int lvl) 56;
        f (Bool.to_int lit) 55; f ad 48; f (imm a) 32; f (imm b) 16; f (imm c) 0 ]
  in
  let sup o = o = Supplied in
  let words =
    Array.map
      (function
        | IEdge { wire; level; own; min_after; max_after; _ } ->
            word ~op:1 ~sup:(sup own) ~wire:(wire_index prog wire) ~lvl:(level = L1)
              ~a:min_after ~b:max_after ()
        | IPut { wire; field; bit; own; inv; _ } ->
            word ~op:2 ~sup:(sup own) ~wire:(wire_index prog wire) ~lvl:inv ~ad:(addr field bit) ()
        | ISample { wire; field; bit; own; _ } ->
            word ~op:3 ~sup:(sup own) ~wire:(wire_index prog wire) ~lit:(is_lit field)
              ~ad:(addr field bit) ()
        | IAfter { ticks; _ } -> word ~op:4 ~a:ticks ()
        | IToggle { wire; field; bit; own; nominal; min_after; max_after; _ } ->
            word ~op:5 ~sup:(sup own) ~wire:(wire_index prog wire) ~ad:(addr field bit)
              ~a:nominal ~b:min_after ~c:max_after ()
        | IBranch { cond = Run { wire; n; set }; skip } ->
            word ~op:6 ~wire:(wire_index prog wire) ~lit:(set <> None)
              ~ad:(match set with Some f -> addr f 0 | None -> 0) ~a:n ~b:skip ()
        | IBranch { cond = Bit { field; bit; level }; skip } ->
            word ~op:7 ~lvl:(level = L1) ~ad:(addr field bit) ~b:skip ()
        | IJump n -> word ~op:8 ~b:n ())
      (specialize prog assignment)
  in
  Array.append words [| 0L |]

let data_bits prog (a : agent) =
  let lay = layout prog in
  let bits = Array.make 128 None in
  List.iter
    (fun (name, base) -> Array.iteri (fun i v -> bits.(base + i) <- v) (Hashtbl.find a.env name))
    lay;
  bits

let hex128 bits =
  String.init 32 (fun k ->
      let nib = 31 - k in
      let v = ref 0 in
      for j = 0 to 3 do
        if bits.(nib * 4 + j) = Some L1 then v := !v lor (1 lsl j)
      done;
      "0123456789abcdef".[!v])

let outcome_code = function
  | Match -> 1 | Peer_asserted -> 2 | Arbitration_lost -> 3 | Deadline_missed -> 4
  | Early_edge -> 5 | Bad_wire -> 6 | Mismatch -> 7

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
let write_case dir prog (cores : (assignment * agent) list) =
  write_file (Filename.concat dir "cfg.hex")
    (List.map
       (fun s ->
         let r = match s.resolution with PushPull -> 0 | DominantLow -> 1 | DominantHigh -> 2 in
         let b = match s.bias with PullDown -> 0 | PullUp -> 1 | Floating -> 2 in
         Printf.sprintf "%02x" (r lor (b lsl 4)))
       prog.wires
    @ List.init (4 - List.length prog.wires) (fun _ -> "20"));
  List.iteri
    (fun i (asg, _) ->
      write_file (Filename.concat dir (Printf.sprintf "prog%d.hex" i))
        (Array.to_list (Array.map (Printf.sprintf "%016Lx") (encode prog asg))))
    cores;
  for i = List.length cores to 3 do
    write_file (Filename.concat dir (Printf.sprintf "prog%d.hex" i)) [ "0000000000000000" ]
  done;
  write_file (Filename.concat dir "data.hex")
    (List.map (fun (_, a) -> hex128 (data_bits prog a)) cores
    @ List.init (4 - List.length cores) (fun _ -> String.make 32 '0'))

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
