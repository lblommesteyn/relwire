(* Generates the Tiny Tapeout cocotb test vectors: the loader byte stream for
   a program and the data memory every core must hold afterwards, per the
   single-issue reference model. Two cases:
     OUT_DIR/            an I2C write between three cores (controller, target,
                         sniffer); the symmetry tests relabel this one
     OUT_DIR/usb_token/  the USB token fields with their CRC-5, which nobody
                         is given: SETCRC and the complemented send, on the
                         netlist
   Usage (from the repository root): gen_tt_test OUT_DIR *)
open Relwire

let bits s = List.init (String.length s) (fun i -> if s.[i] = '1' then L1 else L0)

let gen dir p cores ~ticks =
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  let mk () =
    List.map
      (fun (asg, inp) ->
        (asg, make_agent ~issue:1 ~name:"c" p asg ~inputs:(List.map (fun (f, v) -> (f, bits v)) inp)))
      cores
  in
  let _, nbytes = Hw.write_case dir p (mk ()) in
  let run = mk () in
  ignore (simulate ~wires:p.wires ~agents:(List.map snd run) ~ticks ());
  let oc = open_out_bin (Filename.concat dir "expected.txt") in
  Printf.fprintf oc "# ticks %d bytes %d\n" ticks nbytes;
  (* one line per core: data-memory bits 0..63 as 0/1, '-' where the model
     leaves a bit unbound (not checked) *)
  List.iteri
    (fun i (_, a) ->
      let b = Hw.data_bits p a in
      Printf.fprintf oc "%d %s\n" i
        (String.init 64 (fun j -> match b.(j) with Some L1 -> '1' | Some L0 -> '0' | None -> '-')))
    run;
  close_out oc

let () =
  let dir = Sys.argv.(1) in
  gen dir I2c.write_transaction ~ticks:3000
    [ (Run_as [ "controller" ], [ ("addr", "10100110"); ("data", "01011101") ]);
      (Run_as [ "target" ], [ ("ack", "0"); ("ack2", "0") ]);
      (Observe_all, []) ];
  gen (Filename.concat dir "usb_token")
    (Rw_parse.program_of_file ~tick_ns:220 "examples/usb_token.rw")
    ~ticks:1200
    [ (Run_as [ "host" ], [ ("pid", "10000111"); ("addr", "1010100"); ("endp", "0111") ]);
      (Run_as [ "device" ], []);
      (Observe_all, []) ]
