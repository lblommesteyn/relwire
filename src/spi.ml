(* SPI mode 0, full duplex, written once. Both roles supply a bit in the
   same cell: MOSI from the controller, MISO from the target. *)
open Relwire

let half = 25 (* 1 MHz SCLK at 50 MHz *)
let forever = 1_000_000

let wires =
  [ { name = "CS"; resolution = PushPull; bias = PullUp };
    { name = "SCLK"; resolution = PushPull; bias = PullDown };
    { name = "MOSI"; resolution = PushPull; bias = Floating };
    { name = "MISO"; resolution = PushPull; bias = Floating } ]

let exchange =
  { wires;
    decls =
      [ Field { name = "mosi"; width = 8; owner = "controller" };
        Field { name = "miso"; width = 8; owner = "target" };
        Time { name = "cs_fall"; owner = "controller" };
        Time { name = "cs_rise"; owner = "controller" };
        Time { name = "sclk_rise"; owner = "controller" };
        Time { name = "sclk_fall"; owner = "controller" } ];
    body =
      [ Edge { wire = "CS"; level = L0; time = "cs_fall"; min_after = 10; max_after = forever };
        Repeat { var = "i"; count = 8;
                 body =
                   [ Put { wire = "MOSI"; field = "mosi"; idx = Loop "i" };
                     Put { wire = "MISO"; field = "miso"; idx = Loop "i" };
                     Edge { wire = "SCLK"; level = L1; time = "sclk_rise";
                            min_after = half; max_after = forever };
                     Sample { wire = "MOSI"; field = "mosi"; idx = Loop "i" };
                     Sample { wire = "MISO"; field = "miso"; idx = Loop "i" };
                     Edge { wire = "SCLK"; level = L0; time = "sclk_fall";
                            min_after = half; max_after = forever } ] };
        Edge { wire = "CS"; level = L1; time = "cs_rise"; min_after = half; max_after = forever } ];
    constraints = [] }
