(* I2C, written once. 1 tick = 20 ns (50 MHz). Fast-mode limits from
   UM10204 Table 10, rounded up to whole ticks. *)
open Relwire

let t_hd_sta = 30 (* 0.6 us *)
let t_low = 65 (* 1.3 us *)
let t_high = 30 (* 0.6 us *)
let t_su_dat = 5 (* 100 ns *)
let t_su_sto = 30 (* 0.6 us *)
let forever = 1_000_000

let wires =
  [ { name = "SCL"; resolution = DominantLow; bias = PullUp };
    { name = "SDA"; resolution = DominantLow; bias = PullUp } ]

let fast_mode =
  [ { cname = "tHD;STA"; from_ = At "start"; to_ = At "scl_fall"; min_ticks = t_hd_sta };
    { cname = "tLOW"; from_ = At "scl_fall"; to_ = At "scl_rise"; min_ticks = t_low };
    { cname = "tHIGH"; from_ = At "scl_rise"; to_ = At "scl_fall"; min_ticks = t_high };
    { cname = "tSU;DAT"; from_ = Change "SDA"; to_ = At "scl_rise"; min_ticks = t_su_dat };
    { cname = "tHD;DAT"; from_ = At "scl_fall"; to_ = Change "SDA"; min_ticks = 0 };
    { cname = "tSU;STO"; from_ = At "scl_rise"; to_ = At "stop"; min_ticks = t_su_sto } ]

let bit_cell field idx =
  [ Put { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L1; time = "scl_rise";
           min_after = t_low; max_after = forever };
    Sample { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L0; time = "scl_fall";
           min_after = t_high; max_after = forever } ]

let byte field = Repeat { var = "i"; count = 8; body = bit_cell field (Loop "i") }

let start =
  [ Edge { wire = "SDA"; level = L0; time = "start"; min_after = 10; max_after = forever };
    Edge { wire = "SCL"; level = L0; time = "scl_fall";
           min_after = t_hd_sta; max_after = forever } ]

(* SDA must be low at the last SCL rise, then rise while SCL is high. *)
let stop =
  [ Put { wire = "SDA"; field = "stop_lo"; idx = Const 0 };
    Edge { wire = "SCL"; level = L1; time = "scl_rise";
           min_after = t_low; max_after = forever };
    Sample { wire = "SDA"; field = "stop_lo"; idx = Const 0 };
    Edge { wire = "SDA"; level = L1; time = "stop";
           min_after = t_su_sto; max_after = forever } ]

let timing_decls =
  [ Time { name = "start"; owner = "controller" };
    Time { name = "scl_rise"; owner = "controller" };
    Time { name = "scl_fall"; owner = "controller" } ]

let address_byte =
  { wires;
    decls =
      [ Field { name = "addr"; width = 8; owner = "controller" };
        Field { name = "ack"; width = 1; owner = "target" } ]
      @ timing_decls;
    body = start @ [ byte "addr" ] @ bit_cell "ack" (Const 0);
    constraints = [] }

(* START, address, ACK, one written data byte, ACK, STOP. *)
let write_transaction =
  { wires;
    decls =
      [ Field { name = "addr"; width = 8; owner = "controller" };
        Field { name = "ack"; width = 1; owner = "target" };
        Field { name = "data"; width = 8; owner = "controller" };
        Field { name = "ack2"; width = 1; owner = "target" };
        Lit { name = "stop_lo"; value = L0; owner = "controller" };
        Time { name = "stop"; owner = "controller" } ]
      @ timing_decls;
    body =
      start @ [ byte "addr" ] @ bit_cell "ack" (Const 0)
      @ [ byte "data" ] @ bit_cell "ack2" (Const 0) @ stop;
    constraints = fast_mode }
