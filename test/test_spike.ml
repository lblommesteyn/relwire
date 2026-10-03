(* Semantic spike: one I2C program, four runs, no mode conditionals. *)
open Relwire

let failures = ref 0

let check name cond =
  Printf.printf "  [%s] %s\n" (if cond then "PASS" else "FAIL") name;
  if not cond then incr failures

let bits s =
  List.init (String.length s) (fun i -> if s.[i] = '1' then L1 else L0)

let prog = I2c.address_byte
let ctrl name addr = make_agent ~name prog (Run_as [ "controller" ]) ~inputs:[ ("addr", bits addr) ]
let target () = make_agent ~name:"T" prog (Run_as [ "target" ]) ~inputs:[ ("ack", bits "0") ]
let observer name = make_agent ~name prog Observe_all ~inputs:[]

let clean a =
  outcomes a Deadline_missed = [] && outcomes a Early_edge = [] && outcomes a Bad_wire = []

let done_ a = a.pc = Array.length a.machine

let () =
  (* A = 1011011+W, B = 1010011+W; they differ at bit 3, A sends recessive 1. *)
  print_endline "1. two controllers collide";
  let a = ctrl "A" "10110110" and b = ctrl "B" "10100110" in
  let t = target () and o = observer "O" in
  let tr = simulate ~wires:I2c.wires ~agents:[ a; b; t; o ] ~ticks:2000 () in
  let lost = outcomes a Arbitration_lost in
  check "A loses exactly once, at addr[3]"
    (match lost with [ { var = "addr"; bit = 3; _ } ] -> true | _ -> false);
  check "B never loses" (outcomes b Arbitration_lost = []);
  check "A now holds B's address" (field_value a "addr" = "10100110");
  check "observer decodes B's address" (field_value o "addr" = "10100110");
  check "everyone sees ACK=0"
    (List.for_all (fun x -> field_value x "ack" = "0") [ a; b; o ]);
  check "no timing violations" (List.for_all clean [ a; b; t; o ]);
  check "all agents finished" (List.for_all done_ [ a; b; t; o ]);

  print_endline "2. passive decode of the recorded capture";
  let o2 = observer "O2" in
  ignore (simulate ~wires:I2c.wires ~agents:[ o2 ] ~raw:[ replay tr I2c.wires ] ~ticks:2100 ());
  check "replay decodes B's address" (field_value o2 "addr" = "10100110");
  check "replay decodes ACK=0" (field_value o2 "ack" = "0");
  check "replay clean and finished" (clean o2 && done_ o2);

  print_endline "3. target stretches SCL before ACK";
  let c = ctrl "C" "10100110" and t3 = target () and o3 = observer "O3" in
  let stretch ~tick = if tick >= 820 && tick < 1000 then [ ("SCL", Strong0) ] else [] in
  ignore (simulate ~wires:I2c.wires ~agents:[ c; t3; o3 ] ~raw:[ stretch ] ~ticks:2500 ());
  let pa = outcomes c Peer_asserted in
  check "controller's SCL rise was bound by the peer"
    (match pa with [ { var = "scl_rise"; tick; _ } ] -> tick < 1000 | _ -> false);
  check "no data lost, ACK still read"
    (outcomes c Arbitration_lost = [] && field_value c "ack" = "0");
  check "observer decodes address and ACK"
    (field_value o3 "addr" = "10100110" && field_value o3 "ack" = "0");
  check "no timing violations" (List.for_all clean [ c; t3; o3 ]);
  check "all agents finished" (List.for_all done_ [ c; t3; o3 ]);

  print_endline "4. single controller, ideal target (baseline)";
  let c4 = ctrl "C4" "11110000" and t4 = target () in
  ignore (simulate ~wires:I2c.wires ~agents:[ c4; t4 ] ~ticks:2000 ());
  check "target observes the address" (field_value t4 "addr" = "11110000");
  check "controller reads ACK" (field_value c4 "ack" = "0");
  check "only Match outcomes"
    (List.for_all (fun e -> e.outcome = Match) c4.events && clean t4);

  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
