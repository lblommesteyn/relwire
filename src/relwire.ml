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

type stmt =
  (* The edge's time is a variable: its owner binds it, everyone else
     observes it. Window is relative to the previous edge. *)
  | Edge of { wire : string; level : level; time : string;
              min_after : int; max_after : int }
  (* Field bit is carried on the wire from now until the matching Sample. *)
  | Put of { wire : string; field : string; idx : index }
  | Sample of { wire : string; field : string; idx : index }
  | Repeat of { var : string; count : int; body : stmt list }

type program = { wires : wire_spec list; decls : decl list; body : stmt list }

(* ---------- specialization ---------- *)

type assignment = Run_as of role list | Observe_all
type own = Supplied | Observed

type instr =
  | IEdge of { wire : string; level : level; time : string; role : role;
               own : own; min_after : int; max_after : int }
  | IPut of { wire : string; field : string; bit : int; role : role; own : own }
  | ISample of { wire : string; field : string; bit : int; role : role; own : own }

let owner_of prog name =
  List.find_map
    (function
      | Field { name = n; owner; _ } | Time { name = n; owner } ->
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
            [ IPut { wire; field; bit = ix env idx; role; own = own_of role } ]
        | Sample { wire; field; idx } ->
            let role = owner_of prog field in
            [ ISample { wire; field; bit = ix env idx; role; own = own_of role } ]
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
}

let make_agent ~name prog assignment ~inputs =
  let env = Hashtbl.create 8 in
  List.iter
    (function
      | Field { name; width; _ } ->
          let a = Array.make width None in
          (match List.assoc_opt name inputs with
           | Some bits -> List.iteri (fun i b -> a.(i) <- Some b) bits
           | None -> ());
          Hashtbl.replace env name a
      | Time _ -> ())
    prog.decls;
  { aname = name; machine = specialize prog assignment;
    specs = List.map (fun w -> (w.name, w)) prog.wires;
    pc = 0; phase = Ready; anchor = 0; demoted = [];
    drives = Hashtbl.create 4; env; events = [] }

let log a tick outcome var bit = a.events <- { tick; outcome; var; bit } :: a.events

(* The one dynamic rule: losing authorship of data demotes the whole role. *)
let effective a role own =
  if own = Supplied && List.mem role a.demoted then Observed else own

let release_all a = Hashtbl.filter_map_inplace (fun _ _ -> Some HighZ) a.drives

(* Run until blocked. [now] and [prev] are resolved wire values. *)
let step a ~tick ~now ~prev =
  let continue = ref true in
  while !continue && a.pc < Array.length a.machine do
    (match a.machine.(a.pc) with
     | IPut { wire; field; bit; role; own } ->
         let d =
           match effective a role own with
           | Supplied -> (
               match (Hashtbl.find a.env field).(bit) with
               | Some lv -> drive_for (List.assoc wire a.specs) lv
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
          | V lv, Observed -> cell.(bit) <- Some lv
          | (Float | Contention), _ -> log a tick Bad_wire field bit);
         a.pc <- a.pc + 1
     | IEdge { wire; level; time; role; own; min_after; max_after } -> (
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
         | Driven t0, _ when tick > t0 ->
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
  let prev = Hashtbl.create 4 in
  let trace = ref [] in
  for tick = 0 to ticks - 1 do
    let cur = Hashtbl.create 4 in
    List.iter
      (fun spec ->
        let ds =
          List.filter_map (fun a -> Hashtbl.find_opt a.drives spec.name) agents
          @ List.concat_map
              (fun r ->
                List.filter_map
                  (fun (w, d) -> if w = spec.name then Some d else None)
                  (r ~tick))
              raw
        in
        Hashtbl.replace cur spec.name (resolve spec ds))
      wires;
    if tick = 0 then Hashtbl.iter (Hashtbl.replace prev) cur;
    let now w = Hashtbl.find cur w and prev_v w = Hashtbl.find prev w in
    List.iter (fun a -> step a ~tick ~now ~prev:prev_v) agents;
    trace :=
      ( tick,
        List.filter_map
          (fun s -> match Hashtbl.find cur s.name with V l -> Some (s.name, l) | _ -> None)
          wires )
      :: !trace;
    Hashtbl.reset prev;
    Hashtbl.iter (Hashtbl.replace prev) cur
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
