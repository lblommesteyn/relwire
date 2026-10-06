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
let observer_of p name = make_agent ~name p Observe_all ~inputs:[]

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

  print_endline "13. io model: 2-cycle synchronizer, 1-cycle output delay";
  let io = { sync = 2; out = 1; skew = 0; jitter = 0 } in
  let ci = make_agent ~in_sync:2 ~out_delay:1 ~name:"CI" wt (Run_as [ "controller" ])
      ~inputs:[ ("addr", bits "10100110"); ("data", bits "01011101") ] in
  let ti = make_agent ~in_sync:2 ~out_delay:1 ~name:"TI" wt (Run_as [ "target" ])
      ~inputs:[ ("ack", bits "0"); ("ack2", bits "0") ] in
  let oi = make_agent ~name:"OI" wt Observe_all ~inputs:[] in
  let tri = simulate ~wires:I2c.wires ~agents:[ ci; ti; oi ] ~ticks:3000 () in
  check "transaction still decodes with io delays on both ends"
    (field_value oi "data" = "01011101" && field_value ti "data" = "01011101"
     && field_value ci "ack2" = "0"
     && List.for_all (fun x -> clean x && outcomes x Mismatch = [] && done_ x) [ ci; ti; oi ]);
  let certi = certify ~io wt (Run_as [ "controller" ]) in
  let edges_in tr w lv =
    let rec go acc prev = function
      | [] -> List.rev acc
      | (t, vs) :: rest ->
          let v = List.assoc w vs in
          go (if prev <> Some v && v = lv then t :: acc else acc) (Some v) rest
    in
    go [] None tr
  in
  let lo name = (List.find (fun l -> l.constr.cname = name) certi).guaranteed.lo in
  let sf = edges_in tri "SCL" L0 and sr = edges_in tri "SCL" L1 in
  let df = edges_in tri "SDA" L0 and dr = edges_in tri "SDA" L1 in
  let lvl t w = List.assoc w (List.assoc t tri) in
  let changes = List.filter (fun t -> lvl t "SCL" = L0) (df @ dr) |> List.sort compare in
  let exact =
    [ ("tLOW", minl (gaps sf sr)); ("tHIGH", minl (gaps sr sf));
      ("tHD;STA", List.hd sf - List.hd df);
      ("tSU;STO", minl (gaps sr [ List.nth dr (List.length dr - 1) ])) ] in
  List.iter
    (fun (n, m) -> check (Printf.sprintf "%s on the bus %d ticks = certified %d" n m (lo n)) (m = lo n))
    exact;
  let su = minl (gaps changes sr) and hd = minl (gaps sf changes) in
  check (Printf.sprintf "tSU;DAT on the bus %d >= certified %d" su (lo "tSU;DAT")) (su >= lo "tSU;DAT");
  check (Printf.sprintf "tHD;DAT on the bus %d >= certified %d" hd (lo "tHD;DAT")) (hd >= lo "tHD;DAT");
  print_certificate "  I2C Fast-mode, controller, io sync=2 out=1 skew=1 jitter=1"
    (certify ~io:{ sync = 2; out = 1; skew = 1; jitter = 1 } wt (Run_as [ "controller" ]));

  print_endline "14. .rw sources parse to exactly the OCaml-built programs";
  let ex f = Rw_parse.program_of_file ("../examples/" ^ f) in
  check "i2c.rw = I2c.write_transaction" (ex "i2c.rw" = I2c.write_transaction);
  check "uart.rw = Uart.frame" (ex "uart.rw" = Uart.frame);
  check "spi.rw = Spi.exchange" (ex "spi.rw" = Spi.exchange);
  check "manchester.rw = Manchester.frame 32" (ex "manchester.rw" = Manchester.frame 32);
  let bad src =
    match Rw_parse.program_of_string src with
    | _ -> None
    | exception Rw_parse.Parse_error (line, msg) -> Some (line, msg)
  in
  check "undeclared field is a parse error with its line"
    (bad "wire D push_pull floating\nprotocol {\n  put D x[0]\n}" = Some (3, "undeclared field x"));
  check "there is no syntax for asking the mode"
    (bad "wire D push_pull floating\nprotocol {\n  if mode {\n}\n}" <> None);

  print_endline "15. CAN: bit stuffing as a branch on shared wire history";
  let crc15 lvls =
    List.fold_left
      (fun crc b ->
        let nxt = (if b = L1 then 1 else 0) lxor ((crc lsr 14) land 1) in
        let crc = (crc lsl 1) land 0x7fff in
        if nxt = 1 then crc lxor 0x4599 else crc)
      0 lvls
  in
  let to_bits n v = String.init n (fun i -> if (v lsr (n - 1 - i)) land 1 = 1 then '1' else '0') in
  let header id dlc data = bits ("0" ^ id ^ "000" ^ dlc ^ data) in
  let crc_of id dlc data = to_bits 15 (crc15 (header id dlc data)) in
  (* reference stuffer: stuff bits inserted into SOF..CRC *)
  let stuff_count lvls =
    let rec go cnt last run = function
      | [] -> cnt
      | b :: r ->
          if Some b = last then
            if run + 1 = 5 then go (cnt + 1) (Some (if b = L0 then L1 else L0)) 1 r
            else go cnt last (run + 1) r
          else go cnt (Some b) 1 r
    in
    go 0 None 0 lvls
  in
  let can_tx name id data =
    make_agent ~name Can.frame (Run_as [ "tx" ])
      ~inputs:[ ("id", bits id); ("dlc", bits "0001"); ("data", bits data);
                ("crc", bits (crc_of id "0001" data)) ]
  in
  let can_rx name = make_agent ~name Can.frame (Run_as [ "rx" ]) ~inputs:[ ("ack", bits "0") ] in
  let id1 = "00000111111" and d1 = "11111111" in
  let t1 = can_tx "TX" id1 d1 and r1 = can_rx "RX" and o1 = observer_of Can.frame "CO" in
  let trc = simulate ~wires:Can.wires ~agents:[ t1; r1; o1 ] ~ticks:8000 () in
  let expected_stuff =
    stuff_count (header id1 "0001" d1 @ bits (crc_of id1 "0001" d1)) in
  let got_stuff = List.length (List.filter (fun e -> e.var = "stuff") t1.events) in
  check (Printf.sprintf "transmitter inserts %d stuff bits, reference stuffer says %d"
           got_stuff expected_stuff)
    (got_stuff = expected_stuff && got_stuff > 0);
  check "receiver and observer de-stuff: id, data, CRC decode"
    (List.for_all
       (fun x -> field_value x "id" = id1 && field_value x "data" = d1
                 && field_value x "crc" = crc_of id1 "0001" d1)
       [ r1; o1 ]);
  check "transmitter sees the ACK; no mismatches; all finished"
    (field_value t1 "ack" = "0"
     && List.for_all (fun x -> outcomes x Mismatch = [] && clean x && done_ x) [ t1; r1; o1 ]);

  let ida = "00000111111" and idb = "00000101111" in
  let ta = can_tx "A" ida "10101010" and tb = can_tx "B" idb "11110000" in
  let r2 = can_rx "RX2" and o2 = observer_of Can.frame "CO2" in
  ignore (simulate ~wires:Can.wires ~agents:[ ta; tb; r2; o2 ] ~ticks:8000 ());
  check "A loses arbitration at id[6], B never loses"
    ((match outcomes ta Arbitration_lost with [ { var = "id"; bit = 6; _ } ] -> true | _ -> false)
     && outcomes tb Arbitration_lost = []);
  check "loser and observer decode B's frame, and its CRC checks"
    (List.for_all
       (fun x -> field_value x "id" = idb && field_value x "data" = "11110000"
                 && field_value x "crc" = crc_of idb "0001" "11110000")
       [ ta; o2 ]);
  check "stuffing stays in lockstep across the collision (no Mismatch anywhere)"
    (List.for_all (fun x -> outcomes x Mismatch = [] && done_ x) [ ta; tb; r2; o2 ]);

  let first_stuff = (List.find (fun e -> e.var = "stuff") (List.rev t1.events)).tick in
  let o3 = observer_of Can.frame "CO3" in
  let flip tr =
    List.map
      (fun (t, vs) ->
        if t >= first_stuff - 74 && t < first_stuff + 26 then
          (t, [ ("CAN", if List.assoc "CAN" vs = L0 then L1 else L0) ])
        else (t, vs))
      tr
  in
  ignore (simulate ~wires:Can.wires ~agents:[ o3 ] ~raw:[ replay (flip trc) Can.wires ] ~ticks:8000 ());
  check "a flipped stuff bit is reported as a stuff error (Mismatch on stuff)"
    (List.exists (fun e -> e.var = "stuff") (outcomes o3 Mismatch));

  print_endline "16. branch lint and I2C read-or-write";
  let early =
    { (I2c.address_byte) with
      body = If_bit { field = "addr"; idx = Const 7; level = L1; then_ = []; else_ = [] }
             :: I2c.address_byte.body } in
  check "branching on a bit nobody has sampled yet is rejected"
    (match specialize early (Run_as [ "controller" ]) with
     | _ -> false
     | exception Failure _ -> true);
  check "can.rw = Can.frame" ({ (Rw_parse.program_of_file "../examples/can.rw") with constraints = [] } = Can.frame);
  let rw = Rw_parse.program_of_file "../examples/i2c_rw.rw" in
  let run_rw addr =
    let c = make_agent ~name:"C" rw (Run_as [ "controller" ])
        ~inputs:[ ("addr", bits addr); ("wdata", bits "11001010"); ("rack", bits "1") ] in
    let t = make_agent ~name:"T" rw (Run_as [ "target" ])
        ~inputs:[ ("ack", bits "0"); ("wack", bits "0"); ("rdata", bits "00110101") ] in
    let o = observer_of rw "O" in
    ignore (simulate ~wires:I2c.wires ~agents:[ c; t; o ] ~ticks:3000 ());
    (c, t, o)
  in
  let c, t, o = run_rw "10100110" in
  check "write: target receives wdata, read branch untouched"
    (field_value t "wdata" = "11001010" && field_value o "rdata" = "????????"
     && List.for_all (fun x -> clean x && outcomes x Mismatch = [] && done_ x) [ c; t; o ]);
  let c, t, o = run_rw "10100111" in
  check "read: controller receives rdata from the target, NACKs it"
    (field_value c "rdata" = "00110101" && field_value o "rack" = "1"
     && field_value o "wdata" = "????????"
     && List.for_all (fun x -> clean x && outcomes x Mismatch = [] && done_ x) [ c; t; o ]);

  print_endline "17. certificates over branching programs";
  let rwp = Rw_parse.program_of_file "../examples/i2c_rw.rw" in
  let crw = certify rwp (Run_as [ "controller" ]) in
  print_certificate "  i2c_rw.rw, role controller (both R/W arms)" crw;
  let lw = certify wt (Run_as [ "controller" ]) in
  let g n ls = (List.find (fun l -> l.constr.cname = n) ls).guaranteed in
  check "read-or-write passes on both arms" (List.for_all (fun l -> l.pass) crw);
  check "same tLOW/tHIGH guarantee as the write-only program"
    (g "tLOW" crw = g "tLOW" lw && g "tHIGH" crw = g "tHIGH" lw);
  check "the read arm's data comes from the target: tSU;DAT assumes peer"
    ((List.find (fun l -> l.constr.cname = "tSU;DAT") crw).assumes_peer);
  let canp = Rw_parse.program_of_file "../examples/can.rw" in
  let ccan = certify canp (Run_as [ "tx" ]) in
  print_certificate "  can.rw, role tx (stuffed and unstuffed paths)" ccan;
  check "CAN bit timing exact on every path: bit = 100, seg1 = 75, seg2 = 25"
    (g "bit" ccan = { lo = 100; hi = Some 100 } && g "tSEG1" ccan = { lo = 75; hi = Some 75 }
     && g "tSEG2" ccan = { lo = 25; hi = Some 25 });
  let bad = { canp with constraints = [ { cname = "bit"; from_ = At "boundary"; to_ = At "boundary"; min_ticks = 101 } ] } in
  check "and a 101-tick bit requirement fails" (not (List.hd (certify bad (Run_as [ "tx" ]))).pass);

  print_endline "18. single issue (one instruction per tick): certificate vs simulation";
  let c1 = make_agent ~issue:1 ~name:"C1" wt (Run_as [ "controller" ])
      ~inputs:[ ("addr", bits "10100110"); ("data", bits "01011101") ] in
  let t1 = make_agent ~issue:1 ~name:"T1" wt (Run_as [ "target" ])
      ~inputs:[ ("ack", bits "0"); ("ack2", bits "0") ] in
  let o1 = make_agent ~issue:1 ~name:"O1" wt Observe_all ~inputs:[] in
  let tr1 = simulate ~wires:I2c.wires ~agents:[ c1; t1; o1 ] ~ticks:3000 () in
  check "single-issue transaction decodes cleanly"
    (field_value o1 "data" = "01011101" && field_value c1 "ack2" = "0"
     && List.for_all (fun x -> clean x && outcomes x Mismatch = [] && done_ x) [ c1; t1; o1 ]);
  let cs = certify ~issue:1 wt (Run_as [ "controller" ]) in
  print_certificate "  I2C Fast-mode, controller, single issue" cs;
  let lo1 n = (List.find (fun l -> l.constr.cname = n) cs).guaranteed.lo in
  let ed w lv =
    let rec go acc prev = function
      | [] -> List.rev acc
      | (t, vs) :: rest ->
          let v = List.assoc w vs in
          go (if prev <> Some v && v = lv then t :: acc else acc) (Some v) rest
    in
    go [] None tr1
  in
  let sf = ed "SCL" L0 and sr = ed "SCL" L1 and df = ed "SDA" L0 and dr = ed "SDA" L1 in
  let lv t w = List.assoc w (List.assoc t tr1) in
  let ch = List.filter (fun t -> lv t "SCL" = L0) (df @ dr) |> List.sort compare in
  List.iter
    (fun (n, m) -> check (Printf.sprintf "%s simulated %d = certified %d" n m (lo1 n)) (m = lo1 n))
    [ ("tLOW", minl (gaps sf sr)); ("tHIGH", minl (gaps sr sf));
      ("tHD;STA", List.hd sf - List.hd df);
      ("tSU;STO", minl (gaps sr [ List.nth dr (List.length dr - 1) ])) ];
  let su1 = minl (gaps ch sr) and hd1 = minl (gaps sf ch) in
  check (Printf.sprintf "tSU;DAT simulated %d >= certified %d" su1 (lo1 "tSU;DAT")) (su1 >= lo1 "tSU;DAT");
  check (Printf.sprintf "tHD;DAT simulated %d >= certified %d" hd1 (lo1 "tHD;DAT")) (hd1 >= lo1 "tHD;DAT");
  let progs =
    [ ("i2c", wt); ("i2c_rw", rwp); ("spi", Spi.exchange); ("uart", Uart.frame);
      ("manchester", Manchester.frame 32); ("can", canp) ] in
  let hazards =
    List.concat_map
      (fun (n, p) ->
        List.concat_map
          (fun r -> List.map (fun h -> (n, r, h)) (reaction_hazards p (Run_as [ r ])))
          (Rw_parse.roles p @ [ "nobody" ]))
      progs
  in
  check "no reaction hazards in any example, any role" (hazards = []);
  let fast = Rw_parse.program_of_string
      "wire CS push_pull pull_up\nwire SCLK push_pull pull_down\nwire MOSI push_pull floating\nwire MISO push_pull floating\nfield mosi[8] from controller\nfield miso[8] from target\ntime cs_fall, cs_rise, sclk_rise, sclk_fall from controller\nprotocol {\n  edge CS -> 0 @cs_fall in [10, inf]\n  repeat i 8 {\n    put MOSI mosi[i]\n    put MISO miso[i]\n    edge SCLK -> 1 @sclk_rise in [1, inf]\n    sample MOSI mosi[i]\n    sample MISO miso[i]\n    edge SCLK -> 0 @sclk_fall in [1, inf]\n  }\n}\n" in
  let hz = reaction_hazards fast (Run_as [ "target" ]) in
  check (Printf.sprintf "SPI at 1-tick half period: target cannot keep up (%d hazards)" (List.length hz))
    (List.exists (fun (t, k, lo) -> t = "sclk_fall" && k > lo) hz);

  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
