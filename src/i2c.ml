(* I2C address byte + ACK, written once. 1 tick = 20 ns (50 MHz).
   Fast-mode: tHD;STA >= 0.6 us, tLOW >= 1.3 us, tHIGH >= 0.6 us. *)
open Relwire

let t_hd_sta = 30
let t_low = 65
let t_high = 30
let forever = 1_000_000

let wires =
  [ { name = "SCL"; resolution = DominantLow; bias = PullUp };
    { name = "SDA"; resolution = DominantLow; bias = PullUp } ]

let bit_cell field idx =
  [ Put { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L1; time = "scl_rise";
           min_after = t_low; max_after = forever };
    Sample { wire = "SDA"; field; idx };
    Edge { wire = "SCL"; level = L0; time = "scl_fall";
           min_after = t_high; max_after = forever } ]

let address_byte =
  { wires;
    decls =
      [ Field { name = "addr"; width = 8; owner = "controller" };
        Field { name = "ack"; width = 1; owner = "target" };
        Time { name = "start"; owner = "controller" };
        Time { name = "scl_rise"; owner = "controller" };
        Time { name = "scl_fall"; owner = "controller" } ];
    body =
      [ Edge { wire = "SDA"; level = L0; time = "start";
               min_after = 10; max_after = forever };
        Edge { wire = "SCL"; level = L0; time = "scl_fall";
               min_after = t_hd_sta; max_after = forever };
        Repeat { var = "i"; count = 8; body = bit_cell "addr" (Loop "i") } ]
      @ bit_cell "ack" (Const 0) }
