# RelWire

**One protocol program. One binary. Every role.**

```
                    examples/i2c.rw  ->  relwirec  ->  one 25-word binary
                                                          |
             +--------------------------------+-----------+------------------+
             |                                |                              |
   core 0, role mask = controller   core 1, role mask = target    core 2, role mask = none
   drives START, address, data      watches, drives the ACKs      drives nothing,
                                                                  reconstructs everything
             +-------------------- the same two pins (SCL, SDA) -------------+
```

RelWire is a protocol language and a protocol-emulator chip (Jane Street ASIC
competition, Tiny Tapeout, IHP 130 nm CMOS5L). You write a protocol **once**, as
the legal interactions on the wire, with every value and every edge owned by a
role. The compiler turns it into one binary; each core's role mask decides what
that core drives and what it only watches. A controller, a target and a sniffer
are three projections of the same program, running the same instructions.

When the wire contradicts a core, ownership moves at run time, and only then:

* a controller that **loses arbitration** (I2C, CAN) becomes an observer and
  decodes the winner's message;
* a peer that **stretches a clock edge** takes that one edge's time, nothing else;
* transmitters whose **data edges collide** (Manchester) all report it, and
  nobody adopts a value from the destroyed symbol.

The language has no way to ask which role it is running as, and it only allows
branches on bits every role has already seen, so the projections cannot
silently diverge. [docs/semantics.md](docs/semantics.md) states the
consistency claim and sketches why it holds.

## Try it

```
$ relwirec examples/i2c.rw                       # summary: wires, roles, sizes
$ relwirec examples/i2c.rw --role controller     # what the controller executes
$ relwirec examples/i2c.rw --role target
$ relwirec examples/i2c.rw --observe             # passive analyzer / checker
$ relwirec examples/i2c_ns.rw --tick-ns 250 --certify controller --issue 1
i2c_ns.rw, role controller, 250 ns/tick, io sync=0 out=0 skew=0 jitter=0, issue 1
  constraint       spec             guaranteed     margin
  tHD;STA        600ns           1000ns             +400ns  PASS (1)
  tLOW          1300ns           1750ns..inf        +450ns  PASS (19)
  tHIGH          600ns           1000ns             +400ns  PASS (18)
  tSU;DAT        100ns           1000ns..inf        +900ns  PASS assumes peer (19)
  tHD;DAT          0ns            500ns..750        +500ns  PASS assumes peer (19)
  tSU;STO        600ns           1000ns..inf        +400ns  PASS (1)
  PASS
```

The last command proves that the chip, running this program at its real
timing (40 MHz, one instruction per 250 ns tick), meets the I2C Fast-mode spec
on every path, including the cost of its own instructions. Times in the source
are physical (`1300ns`); `--tick-ns` compiles them for a clock.

Build with `dune build` (OCaml 4.14, dune 3); `dune test` runs everything below.

## How it relates to prior work

Projecting one global description into per-participant behaviour is endpoint
projection, the core of multiparty session types and choreographic
programming. RelWire takes it somewhere those usually do not go: a **shared
physical medium** with resolution rules (wired-AND, contention) instead of
message channels; **time as an owned variable** that a peer can take; **ownership
that transfers at run time** on observed wire outcomes; and projections that
compile to **one binary for a small hardware machine**, where the role is a
register. See [docs/semantics.md](docs/semantics.md).

## Language

| statement | owner (supplied) | everyone else (observed) |
|---|---|---|
| `edge W -> L @t in [a, b]` | drive W to L, `a` after the anchor | wait for the transition, check its window |
| `put W f[i]` / `put_not W f[i]` | drive the bit (or its complement) | release W |
| `sample W f[i]` | check the wire: a mismatch is lost arbitration | bind the bit (literals: check) |
| `toggle W f[i] @t at n in [a, b]` | edge whose new level is the bit | accept a transition in the window, bind, re-anchor |
| `after t n` | timer mark `n` after the anchor | same |
| `if f[i] == v { }`, `if_run W n { }` | branch on bits everyone has sampled | same branch |
| `repeat i n { }`, `use macro(args)` | | |

Wires declare a resolution (`push_pull`, `dominant_low`, `dominant_high`) and a
bias (`pull_up`, `pull_down`, `floating`). Examples: I2C (write, read-or-write on
the R/W bit), SPI (full duplex), UART, Manchester, CAN (bit stuffing as a branch
on shared wire history, arbitration, stuff errors).

## How it is checked

| layer | check |
|---|---|
| semantics | `test/test_spike.ml`: every protocol and rule, record/mutate/replay, timing certificates cross-checked against simulation |
| projections | `test/fuzz.ml`: random well-formed programs (clocked bus with loops, literals, branches, stuffing; Manchester-style data edges), every role plus an observer, both issue models, with and without a second supplier; 2,400 runs at 300 seeds |
| RTL vs model | `test/hw_diff.ml`: tick for tick on the bus, event for event, bit for bit in data memory; 8 protocol scenarios plus random programs, through the pin-level top and its loader |
| netlist vs RTL | `hw/gl/`: the routed netlist, cycle for cycle on every output pin |
| in CI | [tt-relwire](https://github.com/lblommesteyn/tt-relwire): Tiny Tapeout's GDS build, precheck, and cocotb tests on RTL and gate level |

Planted bugs are caught at each layer: a stuff bit not complemented, a loop one
iteration short, misaligned fetch, a controller that is not demoted after
losing arbitration. The randomized checker also found a real semantic bug: the
first rule for data edges let the "winner" of a Manchester collision report
success while every other node failed to decode the cell. That became the
collision rule above.

## Hardware

`hw/rpm_core.v` is the RelWire Protocol Machine; `hw/rpm_top.v` is the Tiny
Tapeout top `tt_um_relwire`.

* 26-bit instructions, one level of zero-overhead loop with an index register,
  64-bit data memory per core.
* **One binary per protocol**: owned instructions carry their owner's role id;
  each core has a 4-bit role mask.
* Timing values live in an 8-entry table shared by the cores, so a program's
  speed is data, not code.
* 4 cores, a 128 x 26 flip-flop program memory with time-multiplexed fetch, a
  2-flop synchronizer on the protocol pins, a byte-wide loader and readback.
* **Single issue**: one instruction per core per tick, a tick being 10 cycles
  (250 ns at 40 MHz). Every instruction, including loop setup and the final
  halt, costs a tick; the timing certificate charges for it and checks
  reaction hazards (a core must reach a wait before its edge can occur).

| | v0 | v1 |
|---|---|---|
| CAN binary | 418 x 64 bit, per role | 94 x 26 bit, shared |
| I2C write binary | 78 x 64 bit, per role | 25 x 26 bit, shared |

`host/` runs it on the Tiny Tapeout demo board: `relwirec --emit-image` makes a
program image, `relwire_tt.py` (MicroPython) loads it with a role and inputs
per core, runs the chip and decodes what each core saw. Its loader stream is
byte-identical to the one the RTL and gate-level tests run. It has not run on a
board: there is no silicon yet.

## Physical design (Tiny Tapeout, IHP CMOS5L, 6x4 tiles)

`tapeout/` holds the Tiny Tapeout project; `export_tt_repo.sh` generates the
submission repository, and `setup_local.sh`/`harden.sh` mirror its CI locally.

What the physical flow taught the architecture:

* The IHP SRAM macro does not fit CMOS5L's rules (user designs may not use
  TopMetal1, and the macro's Metal4 power pins cannot reach the Metal4-only
  grid without it), so the program memory is flip-flops.
* A shared constant-table read port steered by the executing core gave static
  timing a die-wide path from one core's instruction into another core's
  registers. Each core now has its own read ports, and its own instruction
  register.
* At 50 MHz the slow corner (1.08 V, 125 C) missed setup by 4.5 ns while the
  typical and fast corners met it. The target is now 40 MHz.

An earlier revision hardened clean and passed Tiny Tapeout's precheck (DRC 0,
LVS 0, antenna 0, 64% utilization, 50 MHz met at the slow corner before the
reset and timing changes). The current revision's 40 MHz results come from
[tt-relwire's CI](https://github.com/lblommesteyn/tt-relwire/actions). About 240
max-slew and a few max-cap warnings at the slow corner remain open, driven by
the flip-flop program memory.

## Open questions

* CAN is modelled with hard synchronization on start of frame only, not
  in-frame resynchronization.
* The consistency claim is argued and fuzzed, not mechanized.
* The program memory, as flip-flops, dominates area, hold buffering and the
  slew warnings; a smaller or latch-based memory is the path back to 50 MHz.
