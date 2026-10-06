(* relwirec: specialize and certify RelWire programs.

     relwirec FILE.rw                    summary
     relwirec FILE.rw --role R [...]     machine for role(s) R
     relwirec FILE.rw --observe          passive analyzer machine
     relwirec FILE.rw --certify R [--io SYNC,OUT,SKEW,JITTER] [--tick-ns N] [--issue N]
   --issue 1 certifies the chip's timing (one instruction per tick); the
   default certifies the idealized run-until-blocked semantics. *)
open Relwire

let usage () =
  prerr_endline
    "usage: relwirec FILE.rw [--role R]... [--observe] [--certify R] \
     [--io SYNC,OUT,SKEW,JITTER] [--tick-ns N] [--issue N]";
  exit 2

let lvl = function L0 -> "0" | L1 -> "1"
let own = function Supplied -> "drive " | Observed -> "watch "
let hi n = if n >= 1_000_000 then "inf" else string_of_int n

let pp_instr = function
  | IEdge { wire; level; time; own = o; min_after; max_after; _ } ->
      Printf.sprintf "%sedge   %s -> %s @%s in [%d, %s]" (own o) wire (lvl level) time
        min_after (hi max_after)
  | IPut { wire; field; bit; own = Observed; _ } ->
      Printf.sprintf "      release %s (%s[%d] is not ours)" wire field bit
  | IPut { wire; field; bit; own = o; inv; _ } ->
      Printf.sprintf "%s%s %s %s[%d]" (own o) (if inv then "put_not" else "put    ") wire field bit
  | ISample { wire; field; bit; own = o; _ } ->
      Printf.sprintf "%s%s %s %s[%d]" (own o)
        (if o = Supplied then "check  " else "bind   ") wire field bit
  | IAfter { time; ticks } -> Printf.sprintf "      after  %s %d" time ticks
  | IToggle { wire; field; bit; time; own = o; nominal; min_after; max_after; _ } ->
      Printf.sprintf "%stoggle %s %s[%d] @%s at %d in [%d, %d]" (own o) wire field bit time
        nominal min_after max_after
  | IBranch _ | IJump _ | INop -> assert false

let pp_instr = function
  | IBranch { cond = Run { wire; n; set }; skip } ->
      Printf.sprintf "      if last %d on %s equal%s else skip %d" n wire
        (match set with Some f -> " (" ^ f ^ " := complement)" | None -> "") skip
  | IBranch { cond = Bit { field; bit; level }; skip } ->
      Printf.sprintf "      if %s[%d] == %s else skip %d" field bit (lvl level) skip
  | IJump n -> Printf.sprintf "      skip %d" n
  | INop -> "      loop setup"
  | i -> pp_instr i

let print_machine title m =
  Printf.printf "%s (%d instructions)\n" title (Array.length m);
  Array.iteri (fun i ins -> Printf.printf "  %4d  %s\n" i (pp_instr ins)) m

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let file = ref None and roles = ref [] and observe = ref false
  and cert = ref None and io = ref ideal_io and tick_ns = ref 20 and issue = ref None in
  let rec go = function
    | [] -> ()
    | "--role" :: r :: rest -> roles := !roles @ [ r ]; go rest
    | "--observe" :: rest -> observe := true; go rest
    | "--certify" :: r :: rest -> cert := Some r; go rest
    | "--tick-ns" :: n :: rest -> tick_ns := int_of_string n; go rest
    | "--issue" :: n :: rest -> issue := Some (int_of_string n); go rest
    | "--io" :: spec :: rest ->
        (match List.map int_of_string (String.split_on_char ',' spec) with
         | [ sync; out; skew; jitter ] -> io := { sync; out; skew; jitter }
         | _ -> usage ());
        go rest
    | f :: rest when !file = None && String.length f > 0 && f.[0] <> '-' -> file := Some f; go rest
    | _ -> usage ()
  in
  go args;
  let path = match !file with Some f -> f | None -> usage () in
  let prog =
    try Rw_parse.program_of_file ~tick_ns:!tick_ns path with
    | Rw_parse.Parse_error (line, msg) ->
        Printf.eprintf "%s:%d: %s\n" path line msg; exit 1
    | Failure msg -> Printf.eprintf "%s: %s\n" path msg; exit 1
  in
  let declared = Rw_parse.roles prog in
  let check_role r =
    if not (List.mem r declared) then begin
      Printf.eprintf "%s: no role '%s' (roles: %s)\n" path r (String.concat ", " declared);
      exit 1
    end
  in
  List.iter check_role !roles;
  let did = ref false in
  if !roles <> [] then begin
    print_machine ("role " ^ String.concat "+" !roles) (specialize prog (Run_as !roles));
    did := true
  end;
  if !observe then begin
    print_machine "observer" (specialize prog Observe_all);
    did := true
  end;
  (match !cert with
   | None -> ()
   | Some r ->
       check_role r;
       if prog.constraints = [] then (Printf.eprintf "%s: no constraints to certify\n" path; exit 1);
       let lines = certify ~io:!io ?issue:!issue prog (Run_as [ r ]) in
       let hz = match !issue with
         | Some k -> reaction_hazards ~issue:k prog (Run_as [ r ]) | None -> [] in
       List.iter
         (fun (t, k, lo) ->
           Printf.printf "  HAZARD: @%s may occur %d ticks after its anchor, but the core needs %d to reach it
" t lo k)
         hz;
       let i = !io in
       print_certificate ~tick_ns:!tick_ns
         (Printf.sprintf "%s, role %s, %d ns/tick, io sync=%d out=%d skew=%d jitter=%d%s"
            (Filename.basename path) r !tick_ns i.sync i.out i.skew i.jitter
            (match !issue with Some k -> Printf.sprintf ", issue %d" k | None -> ""))
         lines;
       if hz <> [] || not (List.for_all (fun l -> l.pass) lines) then exit 1;
       did := true);
  if not !did then begin
    Printf.printf "%s: %d wires, roles: %s, %d constraints\n" (Filename.basename path)
      (List.length prog.wires) (String.concat ", " declared) (List.length prog.constraints);
    List.iter
      (fun r ->
        Printf.printf "  --role %-10s %d instructions\n" r
          (Array.length (specialize prog (Run_as [ r ]))))
      declared;
    Printf.printf "  --observe          %d instructions\n"
      (Array.length (specialize prog Observe_all))
  end
