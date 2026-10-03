(* Manchester (IEEE 802.3 polarity: 1 = low->high at mid-bit). Every bit's
   mid-cell transition re-anchors the receiver, so clock error does not
   accumulate across the frame. 100 ticks per bit. *)
open Relwire

let half = 50
let forever = 1_000_000

let wires = [ { name = "LINE"; resolution = PushPull; bias = PullUp } ]

let frame n =
  { wires;
    decls =
      [ Field { name = "data"; width = n; owner = "tx" };
        Lit { name = "sync"; value = L1; owner = "tx" };
        Time { name = "sof"; owner = "tx" } ];
    body =
      [ Edge { wire = "LINE"; level = L0; time = "sof"; min_after = 10; max_after = forever };
        Toggle { wire = "LINE"; field = "sync"; idx = Const 0; time = "mid";
                 nominal = half; min_after = half / 2; max_after = half + (half / 2) };
        Repeat { var = "i"; count = n;
                 body =
                   [ After { time = "boundary"; ticks = half };
                     Put_not { wire = "LINE"; field = "data"; idx = Loop "i" };
                     Toggle { wire = "LINE"; field = "data"; idx = Loop "i"; time = "mid";
                              nominal = half; min_after = half / 2;
                              max_after = half + (half / 2) } ] };
        After { time = "end"; ticks = half } ];
    constraints = [] }
