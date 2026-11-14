(* Randomized certificate check. For random well-formed programs (both Gen
   families) with random timing constraints, certify every role, run all the
   roles together, and measure on the simulated bus when each timed event
   really happened. Every measured pair must lie inside the certificate's
   guaranteed interval: lo <= distance, and distance <= hi when hi is finite.

   Event times come from the simulator, not from the certificate's own
   reasoning: an edge or toggle is the tick its owner saw it complete, a
   timer mark the tick the certified role's timer fired, a put the tick its
   owner drove plus one (when the wire resolves). Each measured edge is
   checked against the wire trace (the level really changed at that tick),
   and each put against the level the wire shows, so the measurement itself
   is tested. Pairs are formed the way the certificate walks: from an
   occurrence of [from_] to the next occurrence of [to_], unless another
   [from_] comes first; only pairs where the certified role owns an end.

   Ideal io (no synchronizer or pad delay) under both issue models: unlimited
   and one instruction per tick. *)
open Relwire

let failures = ref 0
let check name cond =
  if not cond then begin
    incr failures;
    Printf.printf "  [FAIL] %s\n%!" name
  end

let clean a =
  List.for_all (fun o -> outcomes a o = [])
    [ Deadline_missed; Early_edge; Bad_wire; Mismatch; Collision; Arbitration_lost ]

let done_ a = a.pc = Array.length a.machine

type ev = { pc : int; label : event_ref; t : int; mine : bool }

let label_of = function
  | IEdge { time; _ } | IToggle { time; _ } | IAfter { time; _ } -> Some (At time)
  | IPut { wire; _ } -> Some (Change wire)
  | _ -> None

let own_of = function
  | IEdge { own; _ } | IToggle { own; _ } | IPut { own; _ } -> Some own
  | IAfter _ -> None
  | _ -> None

(* chronological (pc, tick) completions of one agent *)
let trail a = List.rev a.trail

let random_constraints st labels =
  let n = 1 + Random.State.int st 6 in
  let pick () = List.nth labels (Random.State.int st (List.length labels)) in
  List.init n (fun i ->
      { cname = Printf.sprintf "c%d" i; from_ = pick (); to_ = pick (); min_ticks = 0; spec_ns = None })

(* totals across the run *)
let pairs_checked = ref 0 and lines_checked = ref 0 and tight = ref 0 and with_pairs = ref 0

let run_case ~family st seed ~issue =
  let spec, labels =
    match family with
    | `Bus ->
        (Gen.generate st, [ At "start"; At "rise"; At "fall"; Change "SDA" ])
    | `Toggle ->
        (Gen.generate_toggle st, [ At "sof"; At "mid"; At "boundary"; At "end"; Change "LINE" ])
  in
  let constraints = random_constraints st labels in
  let p = { spec.prog with constraints } in
  let tag = Printf.sprintf "%s seed %d%s" (match family with `Bus -> "bus" | `Toggle -> "toggle") seed
      (if issue = None then "" else " issue1") in
  let inputs = List.map (fun r -> (r, Gen.random_inputs st spec r)) Gen.roles in
  let agents =
    List.map (fun r -> make_agent ?issue ~name:r p (Run_as [ r ]) ~inputs:(List.assoc r inputs)) Gen.roles in
  let tr = Array.of_list (simulate ~wires:p.wires ~agents ~ticks:40_000 ()) in
  check (tag ^ ": all finish cleanly") (List.for_all (fun a -> done_ a && clean a) agents);
  let m = (List.hd agents).machine in
  check (tag ^ ": every role's machine has the same shape")
    (List.for_all (fun a -> Array.length a.machine = Array.length m) agents);
  let level t w = List.assoc_opt w (snd tr.(t)) in
  (* the n-th completion of [pc] in agent [a] *)
  let nth_tick a =
    let tbl = Hashtbl.create 64 in
    List.iter (fun (pc, t) ->
        let l = Option.value (Hashtbl.find_opt tbl pc) ~default:[] in
        Hashtbl.replace tbl pc (t :: l)) (trail a);
    fun pc n -> List.nth_opt (List.rev (Option.value (Hashtbl.find_opt tbl pc) ~default:[])) n
  in
  let ticks_of = List.map (fun a -> (a, nth_tick a)) agents in
  let owner pc = List.find_opt (fun a -> own_of a.machine.(pc) = Some Supplied) agents in
  List.iter
    (fun role ->
      let me = List.find (fun a -> a.aname = role) agents in
      (* this role's events in program order, with measured bus times *)
      let seen = Hashtbl.create 64 in
      let evs =
        List.filter_map
          (fun (pc, t_me) ->
            match label_of me.machine.(pc) with
            | None -> None
            | Some label ->
                let n = Option.value (Hashtbl.find_opt seen pc) ~default:0 in
                Hashtbl.replace seen pc (n + 1);
                let mine = match me.machine.(pc) with
                  | IAfter _ -> true | ins -> own_of ins = Some Supplied in
                let src = match me.machine.(pc) with IAfter _ -> Some me | _ -> owner pc in
                let t_src = match src with
                  | Some a -> (List.assq a ticks_of) pc n | None -> Some t_me in
                match t_src with
                | None -> check (tag ^ ": owner ran the same path") false; None
                | Some t ->
                    let t =
                      match me.machine.(pc) with
                      | IPut { wire; field; bit; inv; _ } ->
                          let bus = t + 1 in
                          (* the wire shows the owner's level (derived bits are
                             rewritten as the run goes, so not those) *)
                          (match src with
                           | Some o when field <> "stuff" ->
                               (match (Hashtbl.find o.env field).(bit) with
                                | Some lv ->
                                    let lv = if inv then (if lv = L0 then L1 else L0) else lv in
                                    check (Printf.sprintf "%s: put %s[%d] shows on %s at %d" tag field bit wire bus)
                                      (level bus wire = Some lv)
                                | None -> ())
                           | _ -> ());
                          bus
                      | IEdge { wire; level = lv; _ } ->
                          check (Printf.sprintf "%s: edge at pc %d is on %s at %d" tag pc wire t)
                            (level t wire = Some lv && level (t - 1) wire <> Some lv);
                          t
                      | IToggle { wire; _ } ->
                          check (Printf.sprintf "%s: toggle at pc %d is on %s at %d" tag pc wire t)
                            (level t wire <> level (t - 1) wire);
                          t
                      | _ -> t
                    in
                    Some { pc; label; t; mine })
          (trail me)
        |> Array.of_list
      in
      let cert = certify ?issue p (Run_as [ role ]) in
      List.iter
        (fun (l : cert_line) ->
          let c = l.constr in
          incr lines_checked;
          let ds = ref [] in
          Array.iteri
            (fun i a ->
              if a.label = c.from_ then begin
                let j = ref (i + 1) in
                while !j < Array.length evs && evs.(!j).label <> c.to_ && evs.(!j).label <> c.from_ do incr j done;
                if !j < Array.length evs && evs.(!j).label = c.to_ then begin
                  let b = evs.(!j) in
                  if a.mine || b.mine then ds := (b.t - a.t, a, b) :: !ds
                end
              end)
            evs;
          let g = l.guaranteed in
          if !ds <> [] then begin
            incr with_pairs;
            check (Printf.sprintf "%s: role %s %s measured pairs but the certificate found none" tag role c.cname)
              (l.instances > 0)
          end;
          List.iter
            (fun (d, a, b) ->
              incr pairs_checked;
              let ok = d >= g.lo && (match g.hi with Some h -> d <= h | None -> true) in
              let show = function At s -> "@" ^ s | Change w -> "put " ^ w in
              check
                (Printf.sprintf "%s: role %s %s (%s -> %s): %s at t=%d -> %s at t=%d is %d, certified [%d, %s]"
                   tag role c.cname (show c.from_) (show c.to_) (show a.label) a.t (show b.label) b.t d g.lo
                   (match g.hi with Some h -> string_of_int h | None -> "inf"))
                ok)
            !ds;
          (match !ds with
           | [] -> ()
           | (d0, _, _) :: rest ->
               if List.fold_left (fun m (d, _, _) -> min m d) d0 rest = g.lo then incr tight))
        cert)
    Gen.roles;
  (* the certificate assumes no reaction hazard under single issue *)
  if issue <> None then
    List.iter
      (fun r -> check (tag ^ ": no reaction hazard for " ^ r) (reaction_hazards p (Run_as [ r ]) = []))
      Gen.roles

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 100 in
  for seed = 1 to n do
    List.iter
      (fun issue ->
        run_case ~family:`Bus (Random.State.make [| seed; 11 |]) seed ~issue;
        run_case ~family:`Toggle (Random.State.make [| seed; 13 |]) seed ~issue)
      [ None; Some 1 ]
  done;
  Printf.printf
    "certificate fuzz: %d seeds x 2 program families x 2 issue models x 3 roles: %d certificate lines, %d with measured pairs, %d pairs inside the guarantee, %d lines tight (measured minimum = certified lower bound)\n"
    n !lines_checked !with_pairs !pairs_checked !tight;
  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
