(* UART 8N1, written once. No clock wire: sample points are After marks at
   mid-bit, counted from the start bit's falling edge. 100 ticks per bit
   (500 kbaud at 50 MHz). *)
open Relwire

let bit = 100
let half = bit / 2
let forever = 1_000_000

let wires = [ { name = "TXD"; resolution = PushPull; bias = PullUp } ]

let frame =
  { wires;
    decls =
      [ Field { name = "data"; width = 8; owner = "tx" };
        Lit { name = "start_lo"; value = L0; owner = "tx" };
        Lit { name = "stop_hi"; value = L1; owner = "tx" };
        Time { name = "start"; owner = "tx" } ];
    body =
      [ Edge { wire = "TXD"; level = L0; time = "start"; min_after = 10; max_after = forever };
        After { time = "mid"; ticks = half };
        Sample { wire = "TXD"; field = "start_lo"; idx = Const 0 };
        Repeat { var = "i"; count = 8;
                 body =
                   [ After { time = "boundary"; ticks = half };
                     Put { wire = "TXD"; field = "data"; idx = Loop "i" };
                     After { time = "mid"; ticks = half };
                     Sample { wire = "TXD"; field = "data"; idx = Loop "i" } ] };
        After { time = "boundary"; ticks = half };
        Put { wire = "TXD"; field = "stop_hi"; idx = Const 0 };
        After { time = "mid"; ticks = half };
        Sample { wire = "TXD"; field = "stop_hi"; idx = Const 0 } ];
    constraints = [] }
