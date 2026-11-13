(* Differential test: every scenario runs on the OCaml reference model and on
   the RTL core (iverilog), and the two must agree tick for tick on the bus,
   event for event per core, and bit for bit in final data memory. *)
open Relwire

let failures = ref 0

let check name cond =
  Printf.printf "  [%s] %s\n%!" (if cond then "PASS" else "FAIL") name;
  if not cond then incr failures

let bits s = List.init (String.length s) (fun i -> if s.[i] = '1' then L1 else L0)

let read_lines path =
  let ic = open_in_bin path in
  let rec go acc =
    match input_line ic with
    | l -> go (String.trim l :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  go []

(* RELWIRE_HW_DIR keeps the cases (dune gives a test its own temporary
   directory, which goes away) for tapeout/collect_gl_cases.sh *)
let root =
  match Sys.getenv_opt "RELWIRE_HW_DIR" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat (Filename.get_temp_dir_name ()) "relwire_hw"
let run cmd = if Sys.command cmd <> 0 then failwith ("command failed: " ^ cmd)

let first_diff a b =
  let rec go i = function
    | x :: xs, y :: ys -> if x = y then go (i + 1) (xs, ys) else Some (i, x, y)
    | [], [] -> None
    | x :: _, [] -> Some (i, x, "<missing>")
    | [], y :: _ -> Some (i, "<missing>", y)
  in
  go 0 (a, b)

let report what = function
  | None -> true
  | Some (i, ref_, hw) ->
      Printf.printf "      %s differs at line %d: model '%s' vs rtl '%s'\n" what i ref_ hw;
      false

(* [pull]: (wire index, tick) holds that wire low from that tick on, from
   outside the chip (an external device), in the model and on the pins *)
let scenario ?pull name prog ~ticks (cores : (assignment * (string * string) list) list) =
  let dir = Filename.concat root name in
  run (Printf.sprintf "mkdir -p %s" dir);
  let mk () =
    List.mapi
      (fun i (asg, inputs) -> (asg, make_agent ~issue:1 ~name:(string_of_int i) prog asg ~inputs:(List.map (fun (f, v) -> (f, bits v)) inputs)))
      cores
  in
  let fresh = mk () in
  let compiled, nbytes = Hw.write_case dir prog fresh in
  let agents = List.map snd fresh in
  let raw =
    match pull with
    | None -> []
    | Some (w, from) ->
        let name = (List.nth prog.wires w).name in
        [ (fun ~tick -> if tick >= from then [ (name, Strong0) ] else []) ]
  in
  let tr = simulate ~wires:prog.wires ~agents ~raw ~ticks () in
  run
    (Printf.sprintf "vvp -n %s/sim +dir=%s +ticks=%d +wires=%d +bytes=%d%s > %s/vvp.log" root dir
       ticks (List.length prog.wires) nbytes
       (match pull with Some (w, t) -> Printf.sprintf " +pull_wire=%d +pull_from=%d" w t | None -> "")
       dir);
  let tr_ok =
    report "bus trace"
      (first_diff (Hw.trace_lines prog tr) (read_lines (Filename.concat dir "trace_hw.txt")))
  in
  let hw_ev = read_lines (Filename.concat dir "events_hw.txt") in
  let ev_ok =
    List.for_all Fun.id
      (List.mapi
         (fun i a ->
           let mine =
             List.filter_map
               (fun l ->
                 match String.index_opt l ' ' with
                 | Some k when String.sub l 0 k = string_of_int i ->
                     Some (String.sub l (k + 1) (String.length l - k - 1))
                 | _ -> None)
               hw_ev
           in
           report (Printf.sprintf "core %d events" i) (first_diff (Hw.events prog a) mine))
         agents)
  in
  let dm = read_lines (Filename.concat dir "dmem_hw.txt") in
  let dm_ok =
    List.for_all Fun.id
      (List.mapi
         (fun i a ->
           let hw = List.hd (String.split_on_char ' ' (List.nth dm i)) in
           let model = Hw.data_bits prog a in
           let ok = ref true in
           Array.iteri
             (fun b v ->
               let nib = int_of_string ("0x" ^ String.make 1 hw.[(Hw.dw / 4) - 1 - (b / 4)]) in
               let hb = if (nib lsr (b mod 4)) land 1 = 1 then L1 else L0 in
               match v with Some l when l <> hb -> ok := false | _ -> ())
             model;
           if not !ok then Printf.printf "      core %d data memory differs\n" i;
           !ok)
         agents)
  in
  let nev = List.length hw_ev in
  let rb_ok =
    match List.find_opt (fun l -> String.length l > 9 && String.sub l 0 9 = "readback ") dm with
    | Some l -> String.sub l 9 (String.length l - 9) = List.hd (String.split_on_char ' ' (List.hd dm))
    | None -> false
  in
  if not rb_ok then print_endline "      pin readback of core 0 differs from its data memory";
  let words = Array.length compiled.Hw.words in
  check
    (Printf.sprintf "%s: %d ticks, %d events, one %d-instr binary: bus, events, data memory agree"
       name ticks nev words)
    (tr_ok && ev_ok && dm_ok && rb_ok && nev > 0)

let () =
  run (Printf.sprintf "mkdir -p %s" root);
  run
    (Printf.sprintf
       "iverilog -g2012 -o %s/sim ../hw/tb_top.v ../hw/rpm_top.v ../hw/rpm_core.v         ../hw/sim/RM_IHPSG13_1P_256x48_c2_bm_bist.v"
       root);
  let ctrl a = (Run_as [ "controller" ], [ ("addr", a) ]) in
  let tgt = (Run_as [ "target" ], [ ("ack", "0") ]) in
  print_endline "RTL (tt_um_relwire: 4 cores, single issue, synchronized pins, shared SRAM, programmed over pins) vs single-issue reference model";
  scenario "i2c_arbitration" I2c.address_byte ~ticks:2000
    [ ctrl "10110110"; ctrl "10100110"; tgt; (Observe_all, []) ];
  scenario "i2c_write" I2c.write_transaction ~ticks:2500
    [ (Run_as [ "controller" ], [ ("addr", "10100110"); ("data", "01011101") ]);
      (Run_as [ "target" ], [ ("ack", "0"); ("ack2", "0") ]);
      (Observe_all, []) ];
  let rw = Rw_parse.program_of_file "../examples/i2c_rw.rw" in
  scenario "i2c_read_branch" rw ~ticks:3000
    [ (Run_as [ "controller" ], [ ("addr", "10100111"); ("rack", "1") ]);
      (Run_as [ "target" ], [ ("ack", "0"); ("rdata", "00110101") ]);
      (Observe_all, []) ];
  scenario "spi_full_duplex" Spi.exchange ~ticks:800
    [ (Run_as [ "controller" ], [ ("mosi", "10100101") ]);
      (Run_as [ "target" ], [ ("miso", "00111100") ]);
      (Observe_all, []) ];
  scenario "uart" Uart.frame ~ticks:1200
    [ (Run_as [ "tx" ], [ ("data", "10110010") ]); (Run_as [ "rx" ], []) ];
  scenario "manchester32" (Manchester.frame 32) ~ticks:3600
    [ (Run_as [ "tx" ], [ ("data", "10110010011100001111010100110101") ]); (Observe_all, []) ];
  let crc = "011100101101001" in
  scenario "can_stuffing" Can.frame ~ticks:8000
    [ (Run_as [ "tx" ], [ ("id", "00000111111"); ("dlc", "0001"); ("data", "11111111"); ("crc", crc) ]);
      (Run_as [ "rx" ], [ ("ack", "0") ]);
      (Observe_all, []) ];
  scenario "can_arbitration" Can.frame ~ticks:8000
    [ (Run_as [ "tx" ], [ ("id", "00000111111"); ("dlc", "0001"); ("data", "10101010"); ("crc", crc) ]);
      (Run_as [ "tx" ], [ ("id", "00000101111"); ("dlc", "0001"); ("data", "11110000"); ("crc", crc) ]);
      (Run_as [ "rx" ], [ ("ack", "0") ]);
      (Observe_all, []) ];
  (* the checksum computed on the chip: no core is given the CRC *)
  let canc = Rw_parse.program_of_file "../examples/can_crc.rw" in
  scenario "can_crc" canc ~ticks:8000
    [ (Run_as [ "tx" ], [ ("id", "00000111111"); ("dlc", "0001"); ("data", "11111111") ]);
      (Run_as [ "rx" ], [ ("ack", "0") ]);
      (Observe_all, []) ];
  scenario "can_crc_arbitration" canc ~ticks:8000
    [ (Run_as [ "tx" ], [ ("id", "00000111111"); ("dlc", "0001"); ("data", "10101010") ]);
      (Run_as [ "tx" ], [ ("id", "00000101111"); ("dlc", "0001"); ("data", "11110000") ]);
      (Run_as [ "rx" ], [ ("ack", "0") ]);
      (Observe_all, []) ];
  (* protocols added as programs only, at the chip's real 220 ns tick *)
  let ex220 f = Rw_parse.program_of_file ~tick_ns:220 ("../examples/" ^ f) in
  scenario "jtag" (ex220 "jtag.rw") ~ticks:1200
    [ (Run_as [ "probe" ], [ ("ir", "1010"); ("dr_in", "11001010") ]);
      (Run_as [ "target" ], [ ("ir_cap", "0001"); ("dr_out", "01011100") ]);
      (Observe_all, []) ];
  scenario "onewire" (ex220 "onewire.rw") ~ticks:9500
    [ (Run_as [ "master" ], [ ("cmd", "11001100") ]);
      (Run_as [ "device" ], [ ("rom", "00010100") ]);
      (Observe_all, []) ];
  scenario "onewire_no_device" (ex220 "onewire.rw") ~ticks:4000
    [ (Run_as [ "master" ], [ ("cmd", "11001100") ]); (Observe_all, []) ];
  scenario "ps2" (ex220 "ps2.rw") ~ticks:4800
    [ (Run_as [ "device" ], [ ("data", "01101000") ]); (Run_as [ "host" ], []); (Observe_all, []) ];
  scenario "ps2_host" (ex220 "ps2_host.rw") ~ticks:7000
    [ (Run_as [ "host" ], [ ("cmd", "10110111") ]); (Run_as [ "device" ], []); (Observe_all, []) ];
  (* checksums that start at all ones (a SETCRC instruction) and are sent complemented *)
  scenario "modbus" (ex220 "modbus.rw") ~ticks:12000
    [ (Run_as [ "master" ], [ ("addr", "10001000"); ("func", "11100000") ]);
      (Run_as [ "slave" ], []);
      (Observe_all, []) ];
  scenario "usb_token" (ex220 "usb_token.rw") ~ticks:1200
    [ (Run_as [ "host" ], [ ("pid", "10000111"); ("addr", "1010100"); ("endp", "0111") ]);
      (Run_as [ "device" ], []);
      (Observe_all, []) ];
  (* audit regressions: programs where the RTL and the model used to differ,
     or agreed on a wrong execution *)
  let ps = Rw_parse.program_of_string in
  scenario "zero_repeat"
    (ps "wire W push_pull pull_down lit x = 1 from tx protocol { repeat i 0 { put W x[0] after mark 5 sample W x[0] } after go 3 put W x[0] after m2 5 sample W x[0] }")
    ~ticks:50 [ (Run_as [ "tx" ], []); (Observe_all, []) ];
  scenario "shadowed_loop"
    (ps "wire W push_pull pull_down field x[2] from tx protocol { repeat i 2 { repeat i 2 { put W x[i] after mark 5 sample W x[i] } } }")
    ~ticks:100 [ (Run_as [ "tx" ], [ ("x", "01") ]); (Observe_all, []) ];
  scenario "shadowed_then_nested"
    (ps "wire W push_pull pull_down field x[2] from tx protocol { repeat i 2 { repeat i 2 { repeat j 2 { put W x[j] after m 5 sample W x[j] } } } }")
    ~ticks:200 [ (Run_as [ "tx" ], [ ("x", "01") ]); (Observe_all, []) ];
  (* an unbounded wait across the 16-bit tick counter's wrap: the edge comes
     from outside the chip at tick 65537 and is on time *)
  scenario "wait_past_wrap" ~pull:(0, 65537)
    (ps "wire W dominant_low pull_up wire V push_pull pull_down time t from dev lit one = 1 from tx protocol { edge W -> 0 @t in [1, inf] put V one[0] after m 2 sample V one[0] }")
    ~ticks:65560 [ (Run_as [ "tx" ], []); (Observe_all, []) ];
  scenario "crc_into_itself"
    (ps "wire W push_pull pull_down crc c[1] poly 1 init 1 from tx protocol { put W c[0] after g 5 sample W c[0] into c }")
    ~ticks:30 [ (Run_as [ "tx" ], []); (Observe_all, []) ];
  scenario "push_pull_contention"
    (ps "wire W push_pull pull_down field x[1] from tx protocol { put W x[0] after mark 5 sample W x[0] }")
    ~ticks:30 [ (Run_as [ "tx" ], [ ("x", "0") ]); (Run_as [ "tx" ], [ ("x", "1") ]); (Observe_all, []) ];
  scenario "crc_branch"
    (ps "wire W push_pull pull_down crc c[1] poly 1 init 1 from tx lit one = 1 from tx protocol { put W c[0] after gap 5 sample W c[0] if c[0] == 1 { put W one[0] after mark 5 sample W one[0] } }")
    ~ticks:30 [ (Run_as [ "tx" ], []); (Observe_all, []) ];
  (* Random programs (the projection fuzzer's generator) through the RTL:
     every role as a core plus an observer, single issue, compiled to one
     shared binary; with and without a second controller in arbitration. *)
  print_endline "random programs through the RTL";
  let bits_s l = String.concat "" (List.map (function L1 -> "1" | L0 -> "0") l) in
  let ran = ref 0 and skipped = ref 0 in
  for seed = 1 to 15 do
    List.iter
      (fun arbitrate ->
        let st = Random.State.make [| seed |] in
        let spec = Gen.generate st in
        let inp r = List.map (fun (f, v) -> (f, bits_s v)) (Gen.random_inputs st spec r) in
        let ia = inp "a" and ib = inp "b" and ic = inp "c" and ia2 = inp "a" in
        match Hw.compile spec.prog with
        | exception Failure _ -> incr skipped
        | _ ->
            incr ran;
            let cores =
              [ (Run_as [ "a" ], ia) ]
              @ (if arbitrate then [ (Run_as [ "a" ], ia2) ] else [ (Run_as [ "b" ], ib) ])
              @ [ (Run_as [ "c" ], ic); (Observe_all, []) ]
            in
            scenario (Printf.sprintf "rand%02d%s" seed (if arbitrate then "_arb" else "")) spec.prog
              ~ticks:4000 cores)
      [ false; true ]
  done;
  (* the Manchester-style family: data edges, collisions instead of arbitration *)
  for seed = 1 to 8 do
    List.iter
      (fun arbitrate ->
        let st = Random.State.make [| seed; 7 |] in
        let spec = Gen.generate_toggle st in
        let inp r = List.map (fun (f, v) -> (f, bits_s v)) (Gen.random_inputs st spec r) in
        let ia = inp "a" and ib = inp "b" and ic = inp "c" and ia2 = inp "a" in
        incr ran;
        let cores =
          [ (Run_as [ "a" ], ia) ]
          @ (if arbitrate then [ (Run_as [ "a" ], ia2) ] else [ (Run_as [ "b" ], ib) ])
          @ [ (Run_as [ "c" ], ic); (Observe_all, []) ]
        in
        scenario (Printf.sprintf "tog%02d%s" seed (if arbitrate then "_coll" else "")) spec.prog
          ~ticks:3000 cores)
      [ false; true ]
  done;
  Printf.printf "  %d random programs on the RTL, %d skipped (larger than 128 words)
" !ran !skipped;

  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
