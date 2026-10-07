# RelWire

A relational protocol language for a programmable protocol-emulator ASIC
(Jane Street / Tiny Tapeout IHP 130nm competition, deadline Jan 18 2027).

A RelWire program denotes a relation over (pre-state, wire trace, post-state).
Under a role assignment, the compiler specializes it into an ordinary
deterministic machine. The hardware never solves anything.

**Invariant:** all specialization is static except ownership transfer caused by
an observed wire outcome (lost arbitration, or a peer stretching an edge). The
source language has no way to ask which mode it runs in.

```
$ relwirec examples/i2c.rw --role controller     # controller machine
$ relwirec examples/i2c.rw --role target         # target machine
$ relwirec examples/i2c.rw --observe             # passive analyzer / checker
$ relwirec examples/i2c.rw --certify controller --io 2,1,1,1
i2c.rw, role controller, 20 ns/tick, io sync=2 out=1 skew=1 jitter=1
  constraint       spec             guaranteed     margin
  tHD;STA        600ns            660ns..700         +60ns  PASS (1)
  tLOW          1300ns           1380ns..inf         +80ns  PASS (19)
  tHIGH          600ns            680ns              +80ns  PASS (18)
  tSU;DAT        100ns           1280ns..inf       +1180ns  PASS assumes peer (19)
  tHD;DAT          0ns             60ns..100         +60ns  PASS assumes peer (19)
  tSU;STO        600ns            660ns..inf         +60ns  PASS (1)
  PASS
```

Same `i2c.rw` every time.

## Language

| Statement | Owner supplies | Others observe |
|---|---|---|
| `edge W -> L @t in [a, b]` | drives W to L, `a` after the anchor | wait for the transition, check it falls in the window |
| `put W f[i]` / `put_not W f[i]` | drive the bit (or its complement) | release W |
| `sample W f[i]` | check the wire; a mismatch means arbitration lost | bind the bit (literals: check, else `Mismatch`) |
| `toggle W f[i] @t at n in [a, b]` | edge whose new level is the data | accept any transition in the window, bind, re-anchor |
| `after t n` | timer mark `n` after the anchor (no wire) | same |
| `repeat i n { }`, `use macro(args)` | | |

Wires declare a resolution (`push_pull`, `dominant_low`, `dominant_high`) and a
bias (`pull_up`, `pull_down`, `floating`).

## Status

`dune test` runs the semantic suite: I2C (arbitration, stretching, full write,
replay and mutated replay), UART (baud tolerance), full-duplex SPI, Manchester
(per-bit resync), CAN (bit stuffing as a branch on shared wire history,
arbitration, stuff-error detection), I2C read-or-write on the R/W bit, the timing certificate cross-checked against simulation with
and without io delays, and the `.rw` sources checked against the OCaml-built
programs.

### Hardware (ISA v1)

`hw/rpm_core.v` runs compiled RelWire (`src/hw.ml`): 26-bit instructions, one
level of zero-overhead loop with an index register, timing values in an
8-entry constant table shared by all cores (a program's speed is data, not
code), 64-bit data memory.

**One binary per protocol.** Owned instructions carry their owner's role id;
each core has a 4-bit role mask. Controller, target and sniffer run the same
instructions and differ only in that register.

`test/hw_diff.ml` runs eight scenarios (I2C arbitration, write, read branch, SPI,
UART, Manchester, CAN stuffing, CAN arbitration) on the reference model
(interpreting the unrolled machine) and on the RTL under iverilog (executing the
looped binary). They must agree tick for tick on the bus, event for event per
core, and bit for bit in data memory. Planted bugs (stuff bit not complemented;
loop one iteration short) are caught.

| | v0 | v1 |
|---|---|---|
| CAN binary | 418 x 64 bit, per role | 94 x 26 bit, shared |
| I2C write binary | 78 x 64 bit, per role | 25 x 26 bit, shared |
| core (Yosys, sg13g2 typ) | 1,919 cells, 27.7k um2 | 2,102 cells, 27.7k um2 |

### Top level and physical design

`hw/rpm_top.v` (`tt_um_relwire`) puts 4 cores on one single-port program memory
(128 x 26 flip-flops) with time-multiplexed fetch, a shared constant table, a
2-flop synchronizer on the protocol pins, a byte-wide loader on
`ui_in`/`uio_in[7:6]`, and readback on `uo_out`.

**Single issue.** Each core executes one instruction per tick; a tick is
4 + 2 (sync) + 4 (cores) = 10 cycles, 250 ns at 40 MHz (one cycle parks each
core's instruction in a local register, so decode does not run off a bus that spans
the die). Every instruction,
including loop setup and the final halt, costs a tick, and the timing
certificate charges for it (`relwirec FILE --certify ROLE --issue 1`). It also
checks reaction hazards: a core watching for an edge must reach the wait
instruction before the earliest time the edge may legally occur.

The differential test drives this top level at the pins against the
single-issue reference model: it programs the chip through the loader, runs,
and reads data memory back.

### Tiny Tapeout CMOS5L hardening

`tapeout/` is the Tiny Tapeout project (6x4 tiles, `ihp-sg13cmos5l`). `setup_local.sh`
and `harden.sh` mirror `TinyTapeout/tt-gds-action@ihp-cmos5l` locally (pinned
IHP-Open-PDK, LibreLane 3.1.0.dev3 in Docker). The program memory is flip-flops,
not an IHP SRAM macro: CMOS5L user designs may not use TopMetal1, so the macro's
Metal4 power pins cannot reach the Metal4-only power grid.

| | |
|---|---|
| Tiny Tapeout precheck | **passed** (KLayout CMOS5L DRC, pins, boundary, layers, cells) |
| DRC (router, Magic) | 0 |
| LVS | 0 errors |
| setup, slow corner 1.08 V / 125 C, 50 MHz | met, +1.14 ns |
| hold, fast corner | met, +0.10 ns |
| utilization | 64% of 1289 x 711 um |
| antenna | 0 |
| open warnings | 242 max-slew, 7 max-cap (slow corner) |

Open questions: a data override demotes the whole role while a time override
flips only that edge;
CAN resync within a frame (only hard sync on SOF is modelled).

Build: `dune build` (OCaml 4.14, dune 3).
