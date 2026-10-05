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

let root = Filename.concat (Filename.get_temp_dir_name ()) "relwire_hw"
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

let scenario name prog ~ticks (cores : (assignment * (string * string) list) list) =
  let dir = Filename.concat root name in
  run (Printf.sprintf "mkdir -p %s" dir);
  let mk () =
    List.mapi
      (fun i (asg, inputs) -> (asg, make_agent ~name:(string_of_int i) prog asg ~inputs:(List.map (fun (f, v) -> (f, bits v)) inputs)))
      cores
  in
  let fresh = mk () in
  let compiled, nbytes = Hw.write_case dir prog fresh in
  let agents = List.map snd fresh in
  let tr = simulate ~wires:prog.wires ~agents ~ticks () in
  run
    (Printf.sprintf "vvp -n %s/sim +dir=%s +ticks=%d +wires=%d +bytes=%d > %s/vvp.log" root dir
       ticks (List.length prog.wires) nbytes dir);
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
  print_endline "RTL (tt_um_relwire, 4 cores, shared SRAM, programmed over pins) vs reference model";
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
  if !failures > 0 then (Printf.printf "%d FAILED\n" !failures; exit 1)
  else print_endline "ALL PASS"
