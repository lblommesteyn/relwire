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

  print_endline "5. collision with roles swapped";
  let a = ctrl "A" "10100110" and b = ctrl "B" "10110110" in
  ignore (simulate ~wires:I2c.wires ~agents:[ a; b; target () ] ~ticks:2000 ());
  check "now B loses at addr[3] and A never loses"
    ((match outcomes b Arbitration_lost with [ { bit = 3; _ } ] -> true | _ -> false)
     && outcomes a Arbitration_lost = []);

  print_endline "6. full write transaction (START addr ACK data ACK STOP)";
  let wt = I2c.write_transaction in
  let c = make_agent ~name:"C" wt (Run_as [ "controller" ])
      ~inputs:[ ("addr", bits "10100110"); ("data", bits "01011101") ] in
  let t6 = make_agent ~name:"T" wt (Run_as [ "target" ])
      ~inputs:[ ("ack", bits "0"); ("ack2", bits "0") ] in
  let o6 = make_agent ~name:"O" wt Observe_all ~inputs:[] in
  let tr6 = simulate ~wires:I2c.wires ~agents:[ c; t6; o6 ] ~ticks:2500 () in
  check "observer decodes addr, data, both ACKs"
    (field_value o6 "addr" = "10100110" && field_value o6 "data" = "01011101"
     && field_value o6 "ack" = "0" && field_value o6 "ack2" = "0");
  check "target receives data" (field_value t6 "data" = "01011101");
  check "no mismatches or violations, all finished"
    (List.for_all (fun x -> clean x && outcomes x Mismatch = [] && done_ x) [ c; t6; o6 ]);

  print_endline "7. timing certificate for the controller";
  let cert = certify wt (Run_as [ "controller" ]) in
  print_certificate "  I2C Fast-mode @ 50 MHz, role controller" cert;
  check "certificate passes" (List.for_all (fun l -> l.pass) cert);
  let inst n = (List.find (fun l -> l.constr.cname = n) cert).instances in
  check "START and STOP constraints have exactly one instance each"
    (inst "tHD;STA" = 1 && inst "tSU;STO" = 1);
  check "tHIGH is exact: falls are dominant, nobody can delay them"
    (let g = (List.find (fun l -> l.constr.cname = "tHIGH") cert).guaranteed in
     g.hi = Some g.lo);
  (* Cross-check: measured minima in the simulated trace must equal the
     certificate's lower bounds (no peer stretched in run 6). *)
  let edges w lv =
    let rec go acc prev = function
      | [] -> List.rev acc
      | (t, vs) :: rest ->
          let v = List.assoc w vs in
          go (if prev <> Some v && v = lv then t :: acc else acc) (Some v) rest
    in
    go [] None tr6
  in
  let level_at w t = List.assoc w (List.assoc t tr6) in
  let gaps froms tos =
    List.filter_map
      (fun t ->
        match List.rev (List.filter (fun f -> f < t) froms) with
        | f :: _ -> Some (t - f) | [] -> None)
      tos
  in
  let minl = List.fold_left min max_int in
  let cert_lo name = (List.find (fun l -> l.constr.cname = name) cert).guaranteed.lo in
  let scl_f = edges "SCL" L0 and scl_r = edges "SCL" L1 in
  let sda_f = edges "SDA" L0 and sda_r = edges "SDA" L1 in
  let stop_t = List.nth sda_r (List.length sda_r - 1) in
  let measured =
    [ ("tLOW", minl (gaps scl_f scl_r));
      ("tHIGH", minl (gaps scl_r scl_f));
      ("tHD;STA", List.hd scl_f - List.hd sda_f);
      ("tSU;STO", minl (gaps scl_r [ stop_t ])) ]
  in
  List.iter
    (fun (n, m) -> check (Printf.sprintf "%s measured %d ticks = certified %d" n m (cert_lo n))
        (m = cert_lo n))
    measured;
  let data_changes =
    List.filter (fun t -> level_at "SCL" t = L0) (sda_f @ sda_r) |> List.sort compare in
  let su = minl (gaps data_changes scl_r) in
  check (Printf.sprintf "tSU;DAT measured %d >= certified %d" su (cert_lo "tSU;DAT"))
    (su >= cert_lo "tSU;DAT");

  print_endline "8. mutated captures (record, edit, replay)";
  let mutate tr w (t0, t1) lv =
    List.map (fun (t, vs) ->
        (t, if t >= t0 && t < t1 then (w, lv) :: List.remove_assoc w vs else vs)) tr
  in
  let r = List.nth scl_r 3 in
  let o8 = make_agent ~name:"O8" wt Observe_all ~inputs:[] in
  ignore (simulate ~wires:I2c.wires ~agents:[ o8 ]
            ~raw:[ replay (mutate tr6 "SCL" (r - 20, r) L1) I2c.wires ] ~ticks:2600 ());
  check "SCL rise 400 ns early is flagged Early_edge"
    (List.exists (fun e -> e.var = "scl_rise") (outcomes o8 Early_edge));
  let last_fall = List.nth scl_f (List.length scl_f - 1) in
  let o9 = make_agent ~name:"O9" wt Observe_all ~inputs:[] in
  ignore (simulate ~wires:I2c.wires ~agents:[ o9 ]
            ~raw:[ replay (mutate tr6 "SDA" (last_fall + 2, 2500) L1) I2c.wires ] ~ticks:2600 ());
  check "SDA high before STOP is flagged Mismatch on stop_lo"
    (List.exists (fun e -> e.var = "stop_lo") (outcomes o9 Mismatch));

  print_endline "9. push-pull contention and missed deadlines";
  let tiny max_after =
    { wires = [ { name = "D"; resolution = PushPull; bias = Floating };
                { name = "C"; resolution = DominantLow; bias = PullUp } ];
      decls = [ Field { name = "x"; width = 1; owner = "tx" }; Time { name = "t"; owner = "tx" } ];
      body = [ Put { wire = "D"; field = "x"; idx = Const 0 };
               Edge { wire = "C"; level = L0; time = "t"; min_after = 5; max_after };
               Sample { wire = "D"; field = "x"; idx = Const 0 } ];
      constraints = [] } in
  let p0 = make_agent ~name:"P0" (tiny 100) (Run_as [ "tx" ]) ~inputs:[ ("x", bits "0") ]
  and p1 = make_agent ~name:"P1" (tiny 100) (Run_as [ "tx" ]) ~inputs:[ ("x", bits "1") ] in
  ignore (simulate ~wires:(tiny 100).wires ~agents:[ p0; p1 ] ~ticks:50 ());
  check "two push-pull drivers disagreeing -> Bad_wire"
    (outcomes p0 Bad_wire <> [] && outcomes p1 Bad_wire <> []);
  let q = make_agent ~name:"Q" (tiny 20) Observe_all ~inputs:[] in
  ignore (simulate ~wires:(tiny 20).wires ~agents:[ q ] ~ticks:100 ());
  check "silent bus with 20-tick window -> Deadline_missed" (outcomes q Deadline_missed <> []);

  print_endline "10. UART: same frame program as transmitter, receiver, sniffer";
  let lsb_first = "10110010" (* 0x4D *) in
  let uart_run ?(clock = 1.0) ?(mutate_tr = fun t -> t) () =
    let tx = make_agent ~name:"TX" Uart.frame (Run_as [ "tx" ]) ~inputs:[ ("data", bits lsb_first) ] in
    let rx = make_agent ~clock ~name:"RX" Uart.frame (Run_as [ "rx" ]) ~inputs:[] in
    let tr = simulate ~wires:Uart.wires ~agents:[ tx ] ~ticks:1200 () in
    ignore (simulate ~wires:Uart.wires ~agents:[ rx ] ~raw:[ replay (mutate_tr tr) Uart.wires ] ~ticks:1200 ());
    (tx, rx)
  in
  let tx, rx = uart_run () in
  check "receiver decodes the byte" (field_value rx "data" = lsb_first);
  check "no mismatches, both finished"
    (outcomes rx Mismatch = [] && outcomes tx Arbitration_lost = [] && done_ rx && done_ tx);
  let _, rx3 = uart_run ~clock:1.03 () in
  let _, rx3s = uart_run ~clock:0.97 () in
  check "receiver clock +/-3% still decodes"
    (List.for_all (fun r -> field_value r "data" = lsb_first && outcomes r Mismatch = []) [ rx3; rx3s ]);
  let _, rx8 = uart_run ~clock:1.08 () in
  check "receiver clock +8% fails visibly (wrong data or Mismatch)"
    (field_value rx8 "data" <> lsb_first || outcomes rx8 Mismatch <> []);
  let hold_low tr = List.map (fun (t, vs) -> (t, if t >= 900 then [ ("TXD", L0) ] else vs)) tr in
  let _, rxb = uart_run ~mutate_tr:hold_low () in
  check "line held low through stop bit -> framing Mismatch on stop_hi"
    (List.exists (fun e -> e.var = "stop_hi") (outcomes rxb Mismatch));

  print_endline "11. SPI mode 0: full duplex, one program";
  let sc = make_agent ~name:"SC" Spi.exchange (Run_as [ "controller" ]) ~inputs:[ ("mosi", bits "10100101") ]
  and st = make_agent ~name:"ST" Spi.exchange (Run_as [ "target" ]) ~inputs:[ ("miso", bits "00111100") ]
  and so = make_agent ~name:"SO" Spi.exchange Observe_all ~inputs:[] in
  ignore (simulate ~wires:Spi.wires ~agents:[ sc; st; so ] ~ticks:800 ());
  check "target receives MOSI, controller receives MISO"
    (field_value st "mosi" = "10100101" && field_value sc "miso" = "00111100");
  check "sniffer sees both directions"
    (field_value so "mosi" = "10100101" && field_value so "miso" = "00111100");
  check "no violations, all finished"
    (List.for_all (fun x -> clean x && outcomes x Arbitration_lost = [] && done_ x) [ sc; st; so ]);

  print_endline "12. Manchester: per-bit resync";
  let word = "10110010011100001111010100110101" in
  let m = Manchester.frame 32 in
  let man_run ?(clock = 1.0) ?(mutate_tr = fun t -> t) () =
    let tx = make_agent ~name:"MTX" m (Run_as [ "tx" ]) ~inputs:[ ("data", bits word) ] in
    let rx = make_agent ~clock ~name:"MRX" m Observe_all ~inputs:[] in
    let tr = simulate ~wires:Manchester.wires ~agents:[ tx ] ~ticks:3600 () in
    ignore (simulate ~wires:Manchester.wires ~agents:[ rx ]
              ~raw:[ replay (mutate_tr tr) Manchester.wires ] ~ticks:3600 ());
    (tx, rx)
  in
  let mtx, mrx = man_run () in
  check "32-bit frame decodes" (field_value mrx "data" = word && done_ mrx && done_ mtx);
  check "receiver clock off by 8% and 15% still decodes 32 bits"
    (List.for_all
       (fun c -> let _, r = man_run ~clock:c () in
         field_value r "data" = word && outcomes r Deadline_missed = [])
       [ 0.85; 0.92; 1.08; 1.15 ]);
  let _, r40 = man_run ~clock:1.4 () in
  check "receiver clock off by 40% fails visibly"
    (field_value r40 "data" <> word || not (clean r40) || not (done_ r40));
  (* A 3-tick glitch inside a blanking window is ignored. *)
  let glitch tr =
    let t0 = 10 + 1 + 50 + 1 + (5 * 100) + 50 + 10 in
    List.map
      (fun (t, vs) ->
        if t >= t0 && t < t0 + 3 then
          (t, [ ("LINE", if List.assoc "LINE" vs = L0 then L1 else L0) ])
        else (t, vs))
      tr
  in
  let _, rg = man_run ~mutate_tr:glitch () in
  check "glitch inside the blanking window is ignored" (field_value rg "data" = word);

  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
