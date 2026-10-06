(* Generates the Tiny Tapeout cocotb test vectors: the loader byte stream for
   an I2C write between three cores (controller, target, sniffer) and the data
   memory every core must hold afterwards, per the single-issue reference
   model.  Usage: gen_tt_test OUT_DIR *)
open Relwire

let bits s = List.init (String.length s) (fun i -> if s.[i] = '1' then L1 else L0)

let () =
  let dir = Sys.argv.(1) in
  let p = I2c.write_transaction in
  let cores =
    [ (Run_as [ "controller" ], [ ("addr", "10100110"); ("data", "01011101") ]);
      (Run_as [ "target" ], [ ("ack", "0"); ("ack2", "0") ]);
      (Observe_all, []) ]
  in
  let mk () =
    List.map
      (fun (asg, inp) ->
        (asg, make_agent ~issue:1 ~name:"c" p asg ~inputs:(List.map (fun (f, v) -> (f, bits v)) inp)))
      cores
  in
  let _, nbytes = Hw.write_case dir p (mk ()) in
  let run = mk () in
  let ticks = 3000 in
  ignore (simulate ~wires:p.wires ~agents:(List.map snd run) ~ticks ());
  let oc = open_out_bin (Filename.concat dir "expected.txt") in
  Printf.fprintf oc "# ticks %d bytes %d\n" ticks nbytes;
  (* one line per core: data-memory bits 0..63 as 0/1, '-' where the model
     leaves a bit unbound (not checked) *)
  List.iteri
    (fun i (_, a) ->
      let b = Hw.data_bits p a in
      Printf.fprintf oc "%d %s
" i
        (String.init 64 (fun j -> match b.(j) with Some L1 -> '1' | Some L0 -> '0' | None -> '-')))
    run;
  close_out oc
