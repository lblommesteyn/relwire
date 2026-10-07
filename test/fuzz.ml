(* Randomized projection check. For random well-formed programs (Gen) and
   random inputs, run every role's specialization together with a passive
   observer and require one consistent execution of the source program:
     1. everyone finishes, with no violation, mismatch or bad wire;
     2. all agents agree on every bound bit (and on which bits are bound);
     3. each bound bit equals what its owning role supplied;
     4. replaying the recorded bus into a fresh observer decodes the same.
   The arbitration variant runs two agents for role "a" with different
   inputs: exactly the agent that lost must hold the winner's values, and
   everything else must still agree. Both issue models are covered. *)
open Relwire

let failures = ref 0
let check name cond =
  if not cond then begin
    incr failures;
    Printf.printf "  [FAIL] %s\n%!" name
  end

let values a (spec : Gen.spec) =
  List.map (fun (f, _, _) -> (f, field_value a f)) spec.fields

let clean a =
  List.for_all
    (fun o -> outcomes a o = [])
    [ Deadline_missed; Early_edge; Bad_wire; Mismatch ]

let done_ a = a.pc = Array.length a.machine

let bits_of l = String.concat "" (List.map (function L1 -> "1" | L0 -> "0") l)

(* each bound bit must equal the owner's input; unbound bits ('?') must be
   unbound for everyone (checked by agreement) *)
let fidelity (spec : Gen.spec) inputs_of vals =
  List.for_all
    (fun (f, _, owner) ->
      let got = List.assoc f vals in
      match List.assoc_opt f (inputs_of owner) with
      | None -> true
      | Some want ->
          let want = bits_of want in
          String.length got = String.length want
          && List.for_all
               (fun k -> got.[k] = '?' || got.[k] = want.[k])
               (List.init (String.length got) Fun.id))
    spec.fields

let run_case st seed ~issue ~arbitrate =
  let spec = Gen.generate st in
  let p = spec.prog in
  let inputs = List.map (fun r -> (r, Gen.random_inputs st spec r)) Gen.roles in
  let a2_inputs = Gen.random_inputs st spec "a" in
  let mk name role inp = make_agent ?issue ~name p (Run_as [ role ]) ~inputs:inp in
  let a = mk "a" "a" (List.assoc "a" inputs) in
  let a2 = if arbitrate then [ mk "a2" "a" a2_inputs ] else [] in
  let b = mk "b" "b" (List.assoc "b" inputs) and c = mk "c" "c" (List.assoc "c" inputs) in
  let o = make_agent ?issue ~name:"o" p Observe_all ~inputs:[] in
  let agents = (a :: a2) @ [ b; c; o ] in
  let ticks = 40_000 in
  let tr = simulate ~wires:p.wires ~agents ~ticks () in
  let tag = Printf.sprintf "seed %d%s%s" seed
      (if issue = None then "" else " issue1") (if arbitrate then " arb" else "") in
  check (tag ^ ": all finish cleanly") (List.for_all (fun x -> done_ x && clean x) agents);
  let v0 = values o spec in
  (* agreement on what crossed the wire: where the observer bound a bit,
     every agent holds the same value; where it did not, only the owning
     role's agents may hold one (their own unsent input) *)
  let owner_of f = let _, _, r = List.find (fun (n, _, _) -> n = f) spec.fields in r in
  let role_of x = if x == o then None else Some (if x.aname = "a2" then "a" else x.aname) in
  let agrees x =
    List.for_all
      (fun (f, ov) ->
        let xv = List.assoc f (values x spec) in
        List.for_all
          (fun k ->
            if ov.[k] <> '?' then xv.[k] = ov.[k]
            else xv.[k] = '?' || role_of x = Some (owner_of f))
          (List.init (String.length ov) Fun.id))
      v0
  in
  List.iter (fun x -> check (tag ^ ": agent " ^ x.aname ^ " agrees with the wire") (agrees x)) agents;
  let winner_inputs =
    match a2 with
    | [ a2 ] when outcomes a Arbitration_lost <> [] && outcomes a2 Arbitration_lost = [] -> a2_inputs
    | _ -> List.assoc "a" inputs
  in
  let inputs_of r = if r = "a" then winner_inputs else List.assoc r inputs in
  check (tag ^ ": values are the owners' inputs") (fidelity spec inputs_of v0);
  (match a2 with
   | [ a2 ] ->
       check (tag ^ ": at most one controller loses")
         (outcomes a Arbitration_lost = [] || outcomes a2 Arbitration_lost = [])
   | _ -> ());
  let o2 = make_agent ?issue ~name:"o2" p Observe_all ~inputs:[] in
  ignore (simulate ~wires:p.wires ~agents:[ o2 ] ~raw:[ replay tr p.wires ] ~ticks:(ticks + 10) ());
  check (tag ^ ": replayed bus decodes the same") (values o2 spec = v0 && clean o2);
  let lost = List.exists (fun x -> outcomes x Arbitration_lost <> []) agents in
  lost

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 100 in  (* dune exec ./test/fuzz.exe 300 for the full run *)
  let losses = ref 0 and cases = ref 0 in
  for seed = 1 to n do
    List.iter
      (fun (issue, arbitrate) ->
        let st = Random.State.make [| seed |] in
        incr cases;
        if run_case st seed ~issue ~arbitrate then incr losses)
      [ (None, false); (Some 1, false); (None, true); (Some 1, true) ]
  done;
  Printf.printf "projection fuzz: %d programs x 4 configurations = %d runs, %d with a lost arbitration\n"
    n !cases !losses;
  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
