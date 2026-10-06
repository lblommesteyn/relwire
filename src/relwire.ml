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
  (* A checked bit whose value a branch computes from wire history
     (e.g. a stuff bit = complement of the last bit on the wire). *)
  | Derived of { name : string; owner : role }

(* Constraint endpoints: an edge (by time variable) or a Put onto a wire. *)
type event_ref = At of string | Change of string

(* [spec_ns]: the requirement as written, when it was given in time units;
   [min_ticks] is it rounded up to whole ticks. *)
type constr = {
  cname : string;
  from_ : event_ref;
  to_ : event_ref;
  min_ticks : int;
  spec_ns : int option;
}

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
  (* Branches may only test what every role has already sampled, so every
     specialization takes the same path. Checked in [specialize]. *)
  | If_run of { wire : string; n : int; set : string option; body : stmt list }
  | If_bit of { field : string; idx : index; level : level;
                then_ : stmt list; else_ : stmt list }

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
  | IBranch of { cond : cond; skip : int }  (* false: skip the next [skip] *)
  | INop  (* where the hardware sets up a loop: free, or one tick at single issue *)
  | IJump of int

and cond =
  | Run of { wire : string; n : int; set : string option }
      (* last [n] samples on [wire] are equal; [set] := their complement *)
  | Bit of { field : string; bit : int; level : level }

let owner_of prog name =
  List.find_map
    (function
      | Field { name = n; owner; _ } | Time { name = n; owner } | Lit { name = n; owner; _ }
      | Derived { name = n; owner } ->
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
  (* [sampled]: field bits bound on every path so far (the branch lint). *)
  (* [looped]: inside a repeat the hardware runs as its loop (the outermost
     one); deeper repeats are unrolled by the compiler too *)
  let looped = ref false in
  let rec go env sampled stmts =
    List.fold_left
      (fun (acc, sampled) st ->
        let out, sampled = one env sampled st in
        (acc @ out, sampled))
      ([], sampled) stmts
  and one env sampled = function
    | Edge { wire; level; time; min_after; max_after } ->
        let role = owner_of prog time in
        ([ IEdge { wire; level; time; role; own = own_of role; min_after; max_after } ], sampled)
    | Put { wire; field; idx } ->
        let role = owner_of prog field in
        ([ IPut { wire; field; bit = ix env idx; role; own = own_of role; inv = false } ], sampled)
    | Put_not { wire; field; idx } ->
        let role = owner_of prog field in
        ([ IPut { wire; field; bit = ix env idx; role; own = own_of role; inv = true } ], sampled)
    | Toggle { wire; field; idx; time; nominal; min_after; max_after } ->
        let role = owner_of prog field in
        let bit = ix env idx in
        ( [ IToggle { wire; field; bit; time; role; own = own_of role;
                      nominal; min_after; max_after } ],
          (field, bit) :: sampled )
    | Sample { wire; field; idx } ->
        let role = owner_of prog field in
        let bit = ix env idx in
        ([ ISample { wire; field; bit; role; own = own_of role } ], (field, bit) :: sampled)
    | After { time; ticks } -> ([ IAfter { time; ticks } ], sampled)
    | Repeat { var; count; body } ->
        let outer = not !looped in
        looped := true;
        let rec loop i acc sampled =
          if i = count then (acc, sampled)
          else
            let out, sampled = go ((var, i) :: env) sampled body in
            loop (i + 1) (acc @ out) sampled
        in
        let out, sampled = loop 0 [] sampled in
        if outer then looped := false;
        ((if outer then [ INop ] else []) @ out, sampled)
    | If_run { wire; n; set; body } ->
        let b, _ = go env sampled body in
        (IBranch { cond = Run { wire; n; set }; skip = List.length b } :: b, sampled)
    | If_bit { field; idx; level; then_; else_ } ->
        let bit = ix env idx in
        if not (List.mem (field, bit) sampled) then
          failwith
            (Printf.sprintf "branch on %s[%d] before every role has sampled it" field bit);
        let t, st = go env sampled then_ and e, se = go env sampled else_ in
        let both = List.filter (fun x -> List.mem x se) st in
        let cond = Bit { field; bit; level } in
        if e = [] then (IBranch { cond; skip = List.length t } :: t, sampled)
        else
          ( (IBranch { cond; skip = List.length t + 1 } :: t) @ (IJump (List.length e) :: e),
            both )
  in
  Array.of_list (fst (go [] [] prog.body))

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
  hist : (string, level list) Hashtbl.t;  (* samples per wire, newest first *)
  mutable events : event list;
  lits : (string * unit) list;
  clock : float;  (* this agent's tick length relative to nominal *)
  in_sync : int;  (* ticks from bus to what this agent sees *)
  out_delay : int;  (* extra ticks from this agent's drive to the bus *)
  issue : int option;  (* instructions completed per tick; None = run until blocked *)
}

let make_agent ?(clock = 1.0) ?(in_sync = 0) ?(out_delay = 0) ?issue ~name prog assignment ~inputs =
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
      | Derived { name; _ } -> Hashtbl.replace env name [| None |]
      | Time _ -> ())
    prog.decls;
  { aname = name; machine = specialize prog assignment;
    specs = List.map (fun w -> (w.name, w)) prog.wires;
    pc = 0; phase = Ready; anchor = 0; demoted = [];
    drives = Hashtbl.create 4; env; hist = Hashtbl.create 4; events = [];
    clock; in_sync; out_delay; issue;
    lits =
      List.filter_map
        (function Lit { name; _ } | Derived { name; _ } -> Some (name, ()) | _ -> None)
        prog.decls }

let log a tick outcome var bit = a.events <- { tick; outcome; var; bit } :: a.events

(* The one dynamic rule: losing authorship of data demotes the whole role. *)
let effective a role own =
  if own = Supplied && List.mem role a.demoted then Observed else own

let scaled a n = if n >= 1_000_000 then n else int_of_float (Float.round (float n /. a.clock))

let release_all a = Hashtbl.filter_map_inplace (fun _ _ -> Some HighZ) a.drives

(* Run until blocked. [now] and [prev] are resolved wire values. *)
let step a ~tick ~now ~prev =
  let continue = ref true and issued = ref 0 in
  while !continue && a.pc < Array.length a.machine do
    let pc0 = a.pc in
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
         (match now wire with
          | V lv ->
              Hashtbl.replace a.hist wire
                (lv :: Option.value (Hashtbl.find_opt a.hist wire) ~default:[])
          | _ -> ());
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
     | IBranch { cond; skip } ->
         let taken =
           match cond with
           | Bit { field; bit; level } -> (Hashtbl.find a.env field).(bit) = Some level
           | Run { wire; n; set } -> (
               let h = Option.value (Hashtbl.find_opt a.hist wire) ~default:[] in
               match List.filteri (fun i _ -> i < n) h with
               | x :: rest when List.length rest = n - 1 && List.for_all (( = ) x) rest ->
                   Option.iter
                     (fun f -> (Hashtbl.find a.env f).(0) <- Some (if x = L0 then L1 else L0))
                     set;
                   true
               | _ -> false)
         in
         a.pc <- a.pc + (if taken then 1 else skip + 1)
     | IJump n -> a.pc <- a.pc + n + 1
     | INop -> a.pc <- a.pc + 1
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
         | (Ready | Awaiting), Observed | Awaiting, Supplied -> await ()));
    (* a completed instruction uses an issue slot *)
    if a.pc <> pc0 then begin
      incr issued;
      match a.issue with Some k when !issued >= k -> continue := false | _ -> ()
    end
  done;
  (* finishing is an implicit halt: it needs an issue slot of its own *)
  let slot_left = match a.issue with Some k -> !issued < k | None -> true in
  if a.pc >= Array.length a.machine && slot_left then release_all a

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
   Distances along each path are summed as intervals, so a stretched rise
   leaves the following fall's offset exact. *)

type bound = { lo : int; hi : int option }

type io = { sync : int; out : int; skew : int; jitter : int }

let ideal_io = { sync = 0; out = 0; skew = 0; jitter = 0 }

(* Events form a DAG over control flow: a branch gives an event several
   successors. Each graph edge carries [gap], the number of instructions
   executed from one event to the next (the event itself included). With
   unlimited issue, zero-time instructions are free and gaps are ignored;
   with one instruction per tick ([issue] = Some 1), every instruction
   costs a tick, so an event happens no earlier than its gap allows:
     - put:            k ticks after its anchor (k = instructions since it)
     - supplied edge:  max(k, min) + 1 + out + sync after the anchor
     - observed edge:  the window [min, max]; and the core must have reached
                       the wait by then (k <= min), or it can miss the edge
     - after mark:     max(k, ticks) *)
type kind =
  | KPut
  | KDrive of { wait : int; lat : int; recessive : bool }
  | KWatch of { lo : int; hi : int }
  | KTimer of int

type tev = {
  label : event_ref;
  wire : string option;
  kind : kind;
  delta : int * int;
  mine : bool;
  mutable succ : (int * int) list;  (* (node, gap) *)
}

let schedule ?(io = ideal_io) prog assignment =
  let m = specialize prog assignment in
  let n = Array.length m in
  let node_of = Array.make n (-1) in
  let evs = ref [] and k = ref 0 in
  let lat = 1 + io.out + io.sync in
  let seen_mine = (-io.sync, -io.sync) and seen_peer = (-io.sync - io.jitter, -io.sync) in
  let push pc e = node_of.(pc) <- !k; incr k; evs := e :: !evs in
  Array.iteri
    (fun pc ins ->
      match ins with
      | IEdge { wire; level; time; own; min_after; max_after; _ } ->
          let spec = List.find (fun s -> s.name = wire) prog.wires in
          let kind, delta =
            match own with
            | Observed -> (KWatch { lo = min_after; hi = max_after }, seen_peer)
            | Supplied ->
                let recessive = spec.resolution <> PushPull && drive_for spec level = HighZ in
                (KDrive { wait = min_after; lat; recessive }, seen_mine)
          in
          push pc { label = At time; wire = Some wire; kind; delta; mine = own = Supplied; succ = [] }
      | IPut { wire; own; _ } ->
          push pc { label = Change wire; wire = Some wire; kind = KPut;
                    delta = (1 + io.out, 1 + io.out); mine = own = Supplied; succ = [] }
      | IToggle { wire; time; own; nominal; min_after; max_after; _ } ->
          let kind, delta = match own with
            | Supplied -> (KDrive { wait = nominal; lat; recessive = false }, seen_mine)
            | Observed -> (KWatch { lo = min_after; hi = max_after }, seen_peer) in
          push pc { label = At time; wire = Some wire; kind; delta; mine = own = Supplied; succ = [] }
      | IAfter { time; ticks } ->
          push pc { label = At time; wire = None; kind = KTimer ticks; delta = (0, 0);
                    mine = true; succ = [] }
      | ISample _ | IBranch _ | IJump _ | INop -> ())
    m;
  let evs = Array.of_list (List.rev !evs) in
  let flow pc =
    match m.(pc) with
    | IBranch { skip; _ } -> [ pc + 1; pc + 1 + skip ]
    | IJump j -> [ pc + 1 + j ]
    | _ -> [ pc + 1 ]
  in
  (* first event on each path from instruction [pc], with instructions counted *)
  let memo = Hashtbl.create 64 in
  let rec firsts pc =
    if pc >= n then []
    else if node_of.(pc) >= 0 then [ (node_of.(pc), 1) ]
    else
      match Hashtbl.find_opt memo pc with
      | Some r -> r
      | None ->
          let r =
            List.sort_uniq compare
              (List.concat_map (fun q -> List.map (fun (v, g) -> (v, g + 1)) (firsts q)) (flow pc))
          in
          Hashtbl.replace memo pc r;
          r
  in
  Array.iteri
    (fun pc v -> if v >= 0 then evs.(v).succ <- List.sort_uniq compare (List.concat_map firsts (flow pc)))
    node_of;
  evs

type cert_line = {
  constr : constr;
  instances : int;
  guaranteed : bound;
  assumes_peer : bool;
  pass : bool;
}

let join x y =
  { lo = min x.lo y.lo;
    hi = (match (x.hi, y.hi) with Some p, Some q -> Some (max p q) | _ -> None) }

let add x y = { lo = x.lo + y.lo;
                hi = (match (x.hi, y.hi) with Some p, Some q -> Some (p + q) | _ -> None) }

let exact v = { lo = v; hi = Some v }

(* Instructions since the anchor at each put, over every incoming path. *)
let put_k evs ~issue =
  let k = Array.make (Array.length evs) None in
  let anchor e = match e.kind with KPut -> false | _ -> true in
  Array.iteri
    (fun v ev ->
      let here = match (k.(v), anchor ev) with
        | _, true -> (0, 0)
        | Some x, false -> x
        | None, false -> (0, 0) in
      List.iter
        (fun (s, g) ->
          let g = if issue = None then 0 else g in
          let lo, hi = here in
          k.(s) <- Some (match k.(s) with
            | None -> (lo + g, hi + g)
            | Some (a, b) -> (min a (lo + g), max b (hi + g))))
        ev.succ)
    evs;
  Array.map (Option.value ~default:(0, 0)) k

(* Time of node [ev] relative to the walk start, given the last anchor's
   relative time and the instructions (k) since it. Returns the event time
   and whether it re-anchors. *)
let event_time ev anchor (klo, khi) =
  match ev.kind with
  | KPut -> (add anchor { lo = klo; hi = Some khi }, false)
  | KDrive { wait; lat; recessive } ->
      let lo = max klo wait + lat in
      (add anchor { lo; hi = (if recessive then None else Some (max khi wait + lat)) }, true)
  | KWatch { lo; hi } -> (add anchor { lo; hi = Some hi }, true)
  | KTimer t -> (add anchor { lo = max klo t; hi = Some (max khi t) }, true)

let certify ?(io = ideal_io) ?issue prog assignment =
  let evs = schedule ~io prog assignment in
  let pk = put_k evs ~issue in
  List.map
    (fun c ->
      let pairs = Hashtbl.create 16 and peer = ref false in
      Array.iteri
        (fun a ea ->
          if ea.label = c.from_ then begin
            (* the start's own anchor sits k_a before it when it is a put *)
            let anchor0, k0 =
              match ea.kind with
              | KPut -> let kl, kh = pk.(a) in ({ lo = -kh; hi = Some (-kl) }, (kl, kh))
              | _ -> (exact 0, (0, 0))
            in
            let rec walk v anchor (kl, kh) p g =
              let ev = evs.(v) in
              let g = if issue = None then 0 else g in
              let k = (kl + g, kh + g) in
              let t, reanchor = event_time ev anchor k in
              let contributes = ev.kind <> KPut || ev.label = c.to_ in
              let p = p || (contributes && not ev.mine) in
              if ev.label = c.to_ then begin
                if ea.mine || ev.mine then begin
                  let da_lo, da_hi = ea.delta and db_lo, db_hi = ev.delta in
                  let skew = match (ea.wire, ev.wire) with
                    | Some x, Some y when x <> y -> io.skew | _ -> 0 in
                  let d = { lo = t.lo + db_lo - da_hi - skew;
                            hi = Option.map (fun h -> h + db_hi - da_lo + skew) t.hi } in
                  peer := !peer || p || not ea.mine;
                  Hashtbl.replace pairs (a, v)
                    (match Hashtbl.find_opt pairs (a, v) with Some x -> join x d | None -> d)
                end
              end
              else if ev.label <> c.from_ then begin
                let anchor, k = if reanchor then (t, (0, 0)) else (anchor, k) in
                List.iter (fun (s, g) -> walk s anchor k p g) ev.succ
              end
            in
            List.iter (fun (s, g) -> walk s anchor0 k0 false g) ea.succ
          end)
        evs;
      let g = Hashtbl.fold (fun _ d acc -> Some (match acc with Some x -> join x d | None -> d)) pairs None in
      let k = Hashtbl.length pairs in
      let g = Option.value g ~default:{ lo = 0; hi = Some 0 } in
      { constr = c; instances = k; guaranteed = g; assumes_peer = !peer;
        pass = k > 0 && g.lo >= c.min_ticks })
    prog.constraints

(* Reaction hazards under single issue: an observed edge whose earliest
   legal time comes before the core can reach the wait instruction. *)
let reaction_hazards ?(io = ideal_io) ?(issue = 1) prog assignment =
  ignore issue;
  let evs = schedule ~io prog assignment in
  let worst = Array.make (Array.length evs) 0 in
  let pk = put_k evs ~issue:(Some 1) in
  Array.iteri
    (fun v ev ->
      let base = match ev.kind with KPut -> snd pk.(v) | _ -> 0 in
      List.iter (fun (s, g) -> worst.(s) <- max worst.(s) (base + g)) ev.succ)
    evs;
  Array.to_list
    (Array.mapi
       (fun v ev ->
         match ev.kind with
         | KWatch { lo; _ } when worst.(v) > lo ->
             Some ((match ev.label with At t -> t | Change w -> w), worst.(v), lo)
         | _ -> None)
       evs)
  |> List.filter_map Fun.id

let print_certificate ?(tick_ns = 20) title lines =
  let ns t = float_of_int (t * tick_ns) in
  Printf.printf "%s\n  %-9s %10s %22s %10s  %s\n" title "constraint" "spec" "guaranteed" "margin" "";
  List.iter
    (fun l ->
      let hi = match l.guaranteed.hi with
        | Some h when h = l.guaranteed.lo -> ""
        | Some h -> Printf.sprintf "..%.0f" (ns h)
        | None -> "..inf" in
      let spec = match l.constr.spec_ns with
        | Some v -> float_of_int v | None -> ns l.constr.min_ticks in
      Printf.printf "  %-9s %8.0fns %14.0fns%-8s %+8.0fns  %s%s (%d)\n" l.constr.cname
        spec (ns l.guaranteed.lo) hi
        (ns l.guaranteed.lo -. spec)
        (if l.pass then "PASS" else "FAIL")
        (if l.assumes_peer then " assumes peer" else "") l.instances)
    lines;
  Printf.printf "  %s\n" (if List.for_all (fun l -> l.pass) lines then "PASS" else "FAIL")
