(* RelWire semantic spike.

   A source program denotes a relation over (pre-state, wire trace,
   post-state). [specialize] turns it, under a role assignment, into an
   ordinary deterministic machine. The hardware (here: [step]) never solves
   anything.

   Design invariant: all specialization is static except ownership transfer
   caused by an observed wire outcome. The source AST has no way to ask what
   mode it runs in or which protocol it is. *)

(* ---------- wires ---------- *)

type level = L0 | L1
type drive = Strong0 | Strong1 | HighZ
type resolution = PushPull | DominantLow | DominantHigh
type bias = PullUp | PullDown | Floating
type wire_spec = { name : string; resolution : resolution; bias : bias }
type value = V of level | Contention | Float

let resolve spec drives =
  let has0 = List.mem Strong0 drives and has1 = List.mem Strong1 drives in
  let idle =
    match spec.bias with PullUp -> V L1 | PullDown -> V L0 | Floating -> Float
  in
  match spec.resolution with
  | DominantLow -> if has0 then V L0 else if has1 then V L1 else idle
  | DominantHigh -> if has1 then V L1 else if has0 then V L0 else idle
  | PushPull -> (
      match (has0, has1) with
      | true, true -> Contention
      | true, false -> V L0
      | false, true -> V L1
      | false, false -> idle)

(* How a supplier puts a logical level on a wire of a given resolution. *)
let drive_for spec lv =
  match (spec.resolution, lv) with
  | DominantLow, L0 -> Strong0
  | DominantLow, L1 -> HighZ
  | DominantHigh, L1 -> Strong1
  | DominantHigh, L0 -> HighZ
  | PushPull, L0 -> Strong0
  | PushPull, L1 -> Strong1

(* ---------- source language ---------- *)

type role = string
type index = Const of int | Loop of string

type decl =
  | Field of { name : string; width : int; owner : role }
  | Time of { name : string; owner : role }
  (* A fixed bit (framing, e.g. STOP's low phase). Suppliers drive it,
     observers check it. *)
  | Lit of { name : string; value : level; owner : role }

(* Constraint endpoints: an edge (by time variable) or a Put onto a wire. *)
type event_ref = At of string | Change of string

type constr = { cname : string; from_ : event_ref; to_ : event_ref; min_ticks : int }

type stmt =
  (* The edge's time is a variable: its owner binds it, everyone else
     observes it. Window is relative to the previous edge. *)
  | Edge of { wire : string; level : level; time : string;
              min_after : int; max_after : int }
  (* Field bit is carried on the wire from now until the matching Sample. *)
  | Put of { wire : string; field : string; idx : index }
  | Sample of { wire : string; field : string; idx : index }
  | Repeat of { var : string; count : int; body : stmt list }
  (* A time point fixed by the agreed rate, not by any wire: [ticks] after
     the previous anchor. Every agent places it with its own timer. *)
  | After of { time : string; ticks : int }
  (* Put the complement of a field bit: the level before a data edge. *)
  | Put_not of { wire : string; field : string; idx : index }
  (* An edge whose new level IS the data. Its owner drives it at [nominal];
     an observer accepts any transition in [min_after, max_after] (earlier
     ones are blanked), binds the bit to the new level, and re-anchors. *)
  | Toggle of { wire : string; field : string; idx : index; time : string;
                nominal : int; min_after : int; max_after : int }

type program = {
  wires : wire_spec list;
  decls : decl list;
  body : stmt list;
  constraints : constr list;
}

(* ---------- specialization ---------- *)

type assignment = Run_as of role list | Observe_all
type own = Supplied | Observed

type instr =
  | IEdge of { wire : string; level : level; time : string; role : role;
               own : own; min_after : int; max_after : int }
  | IPut of { wire : string; field : string; bit : int; role : role; own : own; inv : bool }
  | ISample of { wire : string; field : string; bit : int; role : role; own : own }
  | IAfter of { time : string; ticks : int }
  | IToggle of { wire : string; field : string; bit : int; time : string; role : role;
                 own : own; nominal : int; min_after : int; max_after : int }

let owner_of prog name =
  List.find_map
    (function
      | Field { name = n; owner; _ } | Time { name = n; owner } | Lit { name = n; owner; _ } ->
          if n = name then Some owner else None)
    prog.decls
  |> function
  | Some r -> r
  | None -> failwith ("undeclared variable " ^ name)

let specialize prog assignment =
  let own_of role =
    match assignment with
    | Observe_all -> Observed
    | Run_as roles -> if List.mem role roles then Supplied else Observed
  in
  let ix env = function
    | Const n -> n
    | Loop v -> List.assoc v env
  in
  let rec go env stmts =
    List.concat_map
      (function
        | Edge { wire; level; time; min_after; max_after } ->
            let role = owner_of prog time in
            [ IEdge { wire; level; time; role; own = own_of role; min_after; max_after } ]
        | Put { wire; field; idx } ->
            let role = owner_of prog field in
            [ IPut { wire; field; bit = ix env idx; role; own = own_of role; inv = false } ]
        | Put_not { wire; field; idx } ->
            let role = owner_of prog field in
            [ IPut { wire; field; bit = ix env idx; role; own = own_of role; inv = true } ]
        | Toggle { wire; field; idx; time; nominal; min_after; max_after } ->
            let role = owner_of prog field in
            [ IToggle { wire; field; bit = ix env idx; time; role; own = own_of role;
                        nominal; min_after; max_after } ]
        | Sample { wire; field; idx } ->
            let role = owner_of prog field in
            [ ISample { wire; field; bit = ix env idx; role; own = own_of role } ]
        | After { time; ticks } -> [ IAfter { time; ticks } ]
        | Repeat { var; count; body } ->
            List.concat (List.init count (fun i -> go ((var, i) :: env) body)))
      stmts
  in
  Array.of_list (go [] prog.body)

(* ---------- machine ---------- *)

type outcome =
  | Match
  | Peer_asserted     (* supplied time; peer bound it instead *)
  | Arbitration_lost  (* supplied data; wire carried something else *)
  | Deadline_missed
  | Early_edge
  | Bad_wire          (* sampled Float or Contention *)
  | Mismatch          (* observed wire contradicts a literal *)

type event = { tick : int; outcome : outcome; var : string; bit : int }

type phase = Ready | Driven of int | Awaiting

type agent = {
  aname : string;
  machine : instr array;
  specs : (string * wire_spec) list;
  mutable pc : int;
  mutable phase : phase;
  mutable anchor : int;
  mutable demoted : role list;
  drives : (string, drive) Hashtbl.t;
  env : (string, level option array) Hashtbl.t;
  mutable events : event list;
  lits : (string * unit) list;
  clock : float;  (* this agent's tick length relative to nominal *)
  in_sync : int;  (* ticks from bus to what this agent sees *)
  out_delay : int;  (* extra ticks from this agent's drive to the bus *)
}

let make_agent ?(clock = 1.0) ?(in_sync = 0) ?(out_delay = 0) ~name prog assignment ~inputs =
  let env = Hashtbl.create 8 in
  List.iter
    (function
      | Field { name; width; _ } ->
          let a = Array.make width None in
          (match List.assoc_opt name inputs with
           | Some bits -> List.iteri (fun i b -> a.(i) <- Some b) bits
           | None -> ());
          Hashtbl.replace env name a
      | Lit { name; value; _ } -> Hashtbl.replace env name [| Some value |]
      | Time _ -> ())
    prog.decls;
  { aname = name; machine = specialize prog assignment;
    specs = List.map (fun w -> (w.name, w)) prog.wires;
    pc = 0; phase = Ready; anchor = 0; demoted = [];
    drives = Hashtbl.create 4; env; events = [];
    clock; in_sync; out_delay;
    lits = List.filter_map (function Lit { name; _ } -> Some (name, ()) | _ -> None) prog.decls }

let log a tick outcome var bit = a.events <- { tick; outcome; var; bit } :: a.events

(* The one dynamic rule: losing authorship of data demotes the whole role. *)
let effective a role own =
  if own = Supplied && List.mem role a.demoted then Observed else own

let scaled a n = if n >= 1_000_000 then n else int_of_float (Float.round (float n /. a.clock))

let release_all a = Hashtbl.filter_map_inplace (fun _ _ -> Some HighZ) a.drives

(* Run until blocked. [now] and [prev] are resolved wire values. *)
let step a ~tick ~now ~prev =
  let continue = ref true in
  while !continue && a.pc < Array.length a.machine do
    (match a.machine.(a.pc) with
     | IPut { wire; field; bit; role; own; inv } ->
         let d =
           match effective a role own with
           | Supplied -> (
               match (Hashtbl.find a.env field).(bit) with
               | Some lv ->
                   let lv = if inv then (if lv = L0 then L1 else L0) else lv in
                   drive_for (List.assoc wire a.specs) lv
               | None -> failwith ("no input for " ^ field))
           | Observed -> HighZ
         in
         Hashtbl.replace a.drives wire d;
         a.pc <- a.pc + 1
     | ISample { wire; field; bit; role; own } ->
         let cell = Hashtbl.find a.env field in
         (match (now wire, effective a role own) with
          | V lv, Supplied ->
              if cell.(bit) = Some lv then log a tick Match field bit
              else begin
                cell.(bit) <- Some lv;
                log a tick Arbitration_lost field bit;
                a.demoted <- role :: a.demoted;
                release_all a
              end
          | V lv, Observed -> (
              match (List.mem_assoc field a.lits, cell.(bit)) with
              | true, Some expect when expect <> lv -> log a tick Mismatch field bit
              | _ -> cell.(bit) <- Some lv)
          | (Float | Contention), _ -> log a tick Bad_wire field bit);
         a.pc <- a.pc + 1
     | IAfter { ticks; _ } ->
         if tick >= a.anchor + scaled a ticks then begin
           a.anchor <- tick; a.pc <- a.pc + 1
         end
         else continue := false
     | IToggle { wire; field; bit; time; role; own; nominal; min_after; max_after } -> (
         let nominal = scaled a nominal and min_after = scaled a min_after
         and max_after = scaled a max_after in
         let cell = Hashtbl.find a.env field in
         let finish () = a.anchor <- tick; a.phase <- Ready; a.pc <- a.pc + 1 in
         match (a.phase, effective a role own) with
         | Ready, Supplied ->
             if tick >= a.anchor + nominal then begin
               Hashtbl.replace a.drives wire
                 (drive_for (List.assoc wire a.specs) (Option.get cell.(bit)));
               a.phase <- Driven tick
             end;
             continue := false
         | Driven t0, _ when tick > t0 + a.out_delay + a.in_sync ->
             (match now wire with
              | V lv when cell.(bit) = Some lv -> log a tick Match field bit
              | V lv ->
                  cell.(bit) <- Some lv;
                  log a tick Arbitration_lost field bit;
                  a.demoted <- role :: a.demoted;
                  release_all a
              | _ -> log a tick Bad_wire field bit);
             finish ()
         | Driven _, _ -> continue := false
         | _, Observed ->
             let d = tick - a.anchor in
             if d > max_after then (log a tick Deadline_missed time 0; finish ())
             else if d >= min_after && now wire <> prev wire then begin
               (match now wire with
                | V lv -> cell.(bit) <- Some lv
                | _ -> log a tick Bad_wire field bit);
               finish ()
             end
             else continue := false
         | Awaiting, Supplied -> continue := false)
     | IEdge { wire; level; time; role; own; min_after; max_after } -> (
         let min_after = scaled a min_after and max_after = scaled a max_after in
         let finish () =
           a.anchor <- tick; a.phase <- Ready; a.pc <- a.pc + 1
         in
         let await () =
           if now wire = V level && prev wire <> V level then begin
             if tick - a.anchor < min_after then log a tick Early_edge time 0;
             finish ()
           end
           else if tick - a.anchor > max_after then begin
             log a tick Deadline_missed time 0;
             finish ()
           end
           else continue := false
         in
         match (a.phase, effective a role own) with
         | Ready, Supplied ->
             if tick >= a.anchor + min_after then begin
               Hashtbl.replace a.drives wire
                 (drive_for (List.assoc wire a.specs) level);
               a.phase <- Driven tick
             end;
             continue := false
         | Driven t0, _ when tick > t0 + a.out_delay + a.in_sync ->
             if now wire = V level then (log a tick Match time 0; finish ())
             else begin
               (* Peer holds the wire: the edge time is now the peer's. *)
               log a tick Peer_asserted time 0;
               a.phase <- Awaiting;
               await ()
             end
         | Driven _, _ -> continue := false
         | (Ready | Awaiting), Observed | Awaiting, Supplied -> await ()))
  done;
  if a.pc >= Array.length a.machine then release_all a

(* ---------- simulation ---------- *)

type raw = tick:int -> (string * drive) list

type trace = (int * (string * level) list) list

let simulate ~(wires : wire_spec list) ~(agents : agent list)
    ?(raw : raw list = []) ~ticks () : trace =
  let hist = Array.make ticks (Hashtbl.create 1) in
  (* snaps.(i).(t): agent i's drives at the end of tick t *)
  let snaps = List.map (fun _ -> Array.make ticks []) agents in
  let trace = ref [] in
  for tick = 0 to ticks - 1 do
    let cur = Hashtbl.create 4 in
    List.iter
      (fun spec ->
        let ds =
          List.concat
            (List.map2
               (fun a snap ->
                 let k = tick - 1 - a.out_delay in
                 if k < 0 then []
                 else List.filter_map (fun (w, d) -> if w = spec.name then Some d else None) snap.(k))
               agents snaps)
          @ List.concat_map
              (fun r ->
                List.filter_map
                  (fun (w, d) -> if w = spec.name then Some d else None)
                  (r ~tick))
              raw
        in
        Hashtbl.replace cur spec.name (resolve spec ds))
      wires;
    hist.(tick) <- cur;
    List.iter2
      (fun a snap ->
        let at k w = Hashtbl.find hist.(max 0 k) w in
        step a ~tick ~now:(at (tick - a.in_sync)) ~prev:(at (tick - a.in_sync - 1));
        snap.(tick) <- Hashtbl.fold (fun w d acc -> (w, d) :: acc) a.drives [])
      agents snaps;
    trace :=
      ( tick,
        List.filter_map
          (fun s -> match Hashtbl.find cur s.name with V l -> Some (s.name, l) | _ -> None)
          wires )
      :: !trace
  done;
  List.rev !trace

(* Replay a recorded trace by driving dominant levels back onto the bus. *)
let replay (trace : trace) wires : raw =
  let tbl = Hashtbl.create 1024 in
  List.iter (fun (t, vs) -> Hashtbl.replace tbl t vs) trace;
  fun ~tick ->
    match Hashtbl.find_opt tbl tick with
    | None -> []
    | Some vs ->
        List.map
          (fun (w, lv) -> (w, drive_for (List.find (fun s -> s.name = w) wires) lv))
          vs

let field_value a name =
  Array.to_list (Hashtbl.find a.env name)
  |> List.map (function Some L1 -> "1" | Some L0 -> "0" | None -> "?")
  |> String.concat ""

let outcomes a o = List.filter (fun e -> e.outcome = o) (List.rev a.events)

(* ---------- timing certificate ----------

   Static schedule of one role's specialized machine, in that machine's own
   timeline (the ticks at which it acts). Each event also carries [delta]:
   bus time minus machine time. With an io model (input synchronizer
   [sync], output delay [out], sampling [jitter], inter-wire [skew]):
     - supplied dominant edge:  offset exactly min + 1 + out + sync
                                (drive, resolve, pad out, synchronize back)
     - supplied recessive edge: [min + 1 + out + sync, inf), a peer may hold it
     - observed edge:           the program's window [min, max]
     - put:                     at its anchor; reaches the bus 1 + out later
   Supplied and observed edges are seen [sync, sync + jitter] after the bus.
   Distances along the anchor chain are summed as intervals, so a stretched
   rise leaves the following fall's offset exact. *)

type bound = { lo : int; hi : int option }

type io = { sync : int; out : int; skew : int; jitter : int }

let ideal_io = { sync = 0; out = 0; skew = 0; jitter = 0 }

type tev = {
  label : event_ref;
  wire : string option;
  base : int;
  off : bound;
  delta : int * int;
  is_edge : bool;
  mine : bool;
}

let schedule ?(io = ideal_io) prog assignment =
  let evs = ref [] and n = ref 0 and anchor = ref (-1) in
  let push e = evs := e :: !evs; incr n; !n - 1 in
  let lat = 1 + io.out + io.sync in
  let seen_mine = (-io.sync, -io.sync) and seen_peer = (-io.sync - io.jitter, -io.sync) in
  Array.iter
    (function
      | IEdge { wire; level; time; own; min_after; max_after; _ } ->
          let spec = List.find (fun s -> s.name = wire) prog.wires in
          let off, delta =
            match own with
            | Observed -> ({ lo = min_after; hi = Some max_after }, seen_peer)
            | Supplied ->
                if spec.resolution <> PushPull && drive_for spec level = HighZ
                then ({ lo = min_after + lat; hi = None }, seen_mine)
                else ({ lo = min_after + lat; hi = Some (min_after + lat) }, seen_mine)
          in
          anchor :=
            push { label = At time; wire = Some wire; base = !anchor; off; delta;
                   is_edge = true; mine = own = Supplied }
      | IPut { wire; own; _ } ->
          ignore
            (push { label = Change wire; wire = Some wire; base = !anchor;
                    off = { lo = 0; hi = Some 0 }; delta = (1 + io.out, 1 + io.out);
                    is_edge = false; mine = own = Supplied })
      | IToggle { wire; time; own; nominal; min_after; max_after; _ } ->
          let off, delta = match own with
            | Supplied -> ({ lo = nominal + lat; hi = Some (nominal + lat) }, seen_mine)
            | Observed -> ({ lo = min_after; hi = Some max_after }, seen_peer) in
          anchor := push { label = At time; wire = Some wire; base = !anchor; off; delta;
                           is_edge = true; mine = own = Supplied }
      | IAfter { time; ticks } ->
          anchor :=
            push { label = At time; wire = None; base = !anchor;
                   off = { lo = ticks; hi = Some ticks }; delta = (0, 0);
                   is_edge = true; mine = true }
      | ISample _ -> ())
    (specialize prog assignment);
  Array.of_list (List.rev !evs)

(* Bus-time distance a -> b (a before b). Second component: relies on a
   peer event. *)
let distance ?(io = ideal_io) evs a b =
  let ea = if evs.(a).is_edge then a else evs.(a).base in
  let add x y = { lo = x.lo + y.lo;
                  hi = (match (x.hi, y.hi) with Some p, Some q -> Some (p + q) | _ -> None) } in
  let rec up i acc peer =
    if i = ea then (acc, peer)
    else up evs.(i).base (add acc evs.(i).off) (peer || not evs.(i).mine)
  in
  let d, peer = up b { lo = 0; hi = Some 0 } false in
  let off_a = if evs.(a).is_edge then 0 else evs.(a).off.lo in
  let da_lo, da_hi = evs.(a).delta and db_lo, db_hi = evs.(b).delta in
  let skew =
    match (evs.(a).wire, evs.(b).wire) with
    | Some x, Some y when x <> y -> io.skew
    | _ -> 0
  in
  ( { lo = d.lo - off_a + db_lo - da_hi - skew;
      hi = Option.map (fun h -> h - off_a + db_hi - da_lo + skew) d.hi },
    peer || not evs.(a).mine )

type cert_line = {
  constr : constr;
  instances : int;
  guaranteed : bound;
  assumes_peer : bool;
  pass : bool;
}

let certify ?(io = ideal_io) prog assignment =
  let evs = schedule ~io prog assignment in
  List.map
    (fun c ->
      let acc = ref None and peer = ref false and k = ref 0 in
      (* Pair each [from] with the next [to], unless another [from] comes first. *)
      Array.iteri
        (fun a ea ->
          if ea.label = c.from_ then begin
            let b = ref (a + 1) in
            while !b < Array.length evs && evs.(!b).label <> c.to_
                  && evs.(!b).label <> c.from_ do incr b done;
            if !b < Array.length evs && evs.(!b).label = c.to_
               && (ea.mine || evs.(!b).mine) then begin
              let b = !b in
              let d, p = distance ~io evs a b in
              incr k;
              peer := !peer || p;
              acc :=
                Some
                  (match !acc with
                   | None -> d
                   | Some x ->
                       { lo = min x.lo d.lo;
                         hi = (match (x.hi, d.hi) with
                               | Some p, Some q -> Some (max p q) | _ -> None) })
            end
          end)
        evs;
      let g = Option.value !acc ~default:{ lo = 0; hi = Some 0 } in
      { constr = c; instances = !k; guaranteed = g; assumes_peer = !peer;
        pass = !k > 0 && g.lo >= c.min_ticks })
    prog.constraints

let print_certificate ?(tick_ns = 20) title lines =
  let ns t = float_of_int (t * tick_ns) in
  Printf.printf "%s\n  %-9s %10s %22s %10s  %s\n" title "constraint" "spec" "guaranteed" "margin" "";
  List.iter
    (fun l ->
      let hi = match l.guaranteed.hi with
        | Some h when h = l.guaranteed.lo -> ""
        | Some h -> Printf.sprintf "..%.0f" (ns h)
        | None -> "..inf" in
      Printf.printf "  %-9s %8.0fns %14.0fns%-8s %+8.0fns  %s%s (%d)\n" l.constr.cname
        (ns l.constr.min_ticks) (ns l.guaranteed.lo) hi
        (ns (l.guaranteed.lo - l.constr.min_ticks))
        (if l.pass then "PASS" else "FAIL")
        (if l.assumes_peer then " assumes peer" else "") l.instances)
    lines;
  Printf.printf "  %s\n" (if List.for_all (fun l -> l.pass) lines then "PASS" else "FAIL")
