(* CAN 2.0A base frame, one data byte. NRZ bits sampled at 75%, hard sync
   on SOF only. Bit stuffing: after five equal bits on the wire, a stuff bit
   of the opposite level. The branch tests wire history every role has
   sampled, so all specializations take it together. 100 ticks per bit. *)
open Relwire

let seg1 = 75
let seg2 = 25
let forever = 1_000_000

let wires = [ { name = "CAN"; resolution = DominantLow; bias = PullUp } ]

let stuff_check =
  If_run { wire = "CAN"; n = 5; set = Some "stuff";
           body =
             [ Put { wire = "CAN"; field = "stuff"; idx = Const 0 };
               After { time = "sample"; ticks = seg1 };
               Sample { wire = "CAN"; field = "stuff"; idx = Const 0 };
               After { time = "boundary"; ticks = seg2 } ] }

let plain field idx =
  [ Put { wire = "CAN"; field; idx };
    After { time = "sample"; ticks = seg1 };
    Sample { wire = "CAN"; field; idx };
    After { time = "boundary"; ticks = seg2 } ]

let stuffed field idx = plain field idx @ [ stuff_check ]
let each field n = Repeat { var = "i"; count = n; body = stuffed field (Loop "i") }

let frame =
  { wires;
    decls =
      [ Field { name = "id"; width = 11; owner = "tx" };
        Field { name = "dlc"; width = 4; owner = "tx" };
        Field { name = "data"; width = 8; owner = "tx" };
        Field { name = "crc"; width = 15; owner = "tx" };
        Field { name = "ack"; width = 1; owner = "rx" };
        Lit { name = "dominant"; value = L0; owner = "tx" };
        Lit { name = "recessive"; value = L1; owner = "tx" };
        Derived { name = "stuff"; owner = "tx" };
        Time { name = "sof"; owner = "tx" } ];
    body =
      [ Edge { wire = "CAN"; level = L0; time = "sof"; min_after = 10; max_after = forever };
        After { time = "sample"; ticks = seg1 };
        Sample { wire = "CAN"; field = "dominant"; idx = Const 0 };
        After { time = "boundary"; ticks = seg2 };
        stuff_check;
        each "id" 11 ]
      @ stuffed "dominant" (Const 0) (* RTR *)
      @ stuffed "dominant" (Const 0) (* IDE *)
      @ stuffed "dominant" (Const 0) (* r0 *)
      @ [ each "dlc" 4; each "data" 8; each "crc" 15 ]
      @ plain "recessive" (Const 0) (* CRC delimiter *)
      @ plain "ack" (Const 0)
      @ plain "recessive" (Const 0) (* ACK delimiter *)
      @ [ Repeat { var = "i"; count = 7; body = plain "recessive" (Const 0) } ];
    constraints = [] }
