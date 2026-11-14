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
$ relwirec examples/i2c_ns.rw --tick-ns 220 --certify controller --issue 1
i2c_ns.rw, role controller, 220 ns/tick, io sync=0 out=0 skew=0 jitter=0, issue 1
  constraint       spec             guaranteed     margin
  tHD;STA        600ns            880ns             +280ns  PASS (1)
  tLOW          1300ns           1540ns..inf        +240ns  PASS (19)
  tHIGH          600ns           1100ns             +500ns  PASS (18)
  tSU;DAT        100ns            880ns..inf        +780ns  PASS assumes peer (19)
  tHD;DAT          0ns            440ns..660        +440ns  PASS assumes peer (19)
  tSU;STO        600ns            880ns..inf        +280ns  PASS (1)
  fSCL          2500ns           2640ns..inf        +140ns  PASS (18)
  PASS
```

The last command proves that the chip, running this program at its real
timing (50 MHz, one instruction per 220 ns tick), meets the I2C Fast-mode spec
on every path, including the cost of its own instructions, at about 379 kHz.
It also caught a real mistake: at 50 MHz, driving only the minimum SCL high
time would have run the bus at 413 kHz, over Fast-mode's 400 kHz limit, which
the spec file had not stated; with the `fSCL` constraint added, the certificate
rejects that. Times in the source are physical (`1300ns`); `--tick-ns`
compiles them for a clock.

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
| `sample W f[i] into c` | the same, and fold the sampled level into checksum `c` | same |
| `put W c[i]`, `sample W c[i]` for a `crc c[n] poly P [init V] [invert]` | drive the checksum register's top bit (complemented with `invert`) | check the wire against their own register |
| `toggle W f[i] @t at n in [a, b]` | edge whose new level is the bit | accept a transition in the window, bind, re-anchor |
| `after t n` | timer mark `n` after the anchor | same |
| `if f[i] == v { }`, `if_run W n { }` | branch on bits everyone has sampled | same branch |
| `repeat i n { }`, `use macro(args)` | | |

A `crc` is a field nobody supplies. Every role folds the same sampled wire
levels into its own shift register, so every role holds the same value, by the
same argument that lets a branch test wire history. The owner sends it and the
rest check it. One instruction per 220 ns tick leaves a CAN bit about four
instructions at 1 Mbit/s, far too few to compute a CRC-15 in the program, so
the fold is part of `sample` and costs no instructions.

A CRC is named by more than its polynomial. `init V` starts the register at
`V` instead of zero (all ones for Modbus, USB and HDLC) and `invert` sends it
complemented (USB, HDLC). Bit order needs no option: the register is sent top
bit first, and a CRC described as "LSB first, polynomial reversed" is the same
register read from the other end. The reference model reproduces the published
check value of eleven named CRCs from 5 to 16 bits (`test/test_spike.ml`, 15d).

Wires declare a resolution (`push_pull`, `dominant_low`, `dominant_high`) and a
bias (`pull_up`, `pull_down`, `floating`). Examples: I2C (write, read-or-write on
the R/W bit), SPI (full duplex), UART, Manchester, CAN (bit stuffing as a branch
on shared wire history, arbitration, stuff errors; `can_crc.rw` computes the
CRC-15 on the chip).

Four more were added after the hardware was fixed, as programs only, with no
change to the chip or the compiler:

* **JTAG** (`jtag.rw`): an IR scan and a DR scan on all four wires. TMS bits
  are literals, so an observer checks the TAP walk.
* **1-Wire** (`onewire.rw`): reset, presence, a byte written, a byte read, at
  standard-speed timing. A write slot and a read slot are the same statements;
  who owns the bit decides who holds the wire. Presence is a time the device
  owns, so a missing device is a missed deadline.
* **PS/2** (`ps2.rw`): device to host. Its odd parity is the checksum register
  at degree 1.
* **PS/2, host to device** (`ps2_host.rw`): the host inhibits the clock,
  requests to send and lets go; from then on every clock edge is the device's,
  and the device ends the frame with an ACK. Clock ownership moves mid-frame
  with nothing in the program but the owner of each edge. With no device the
  host reports the first clock edge as a missed deadline.

Two more came with the checksum's start value and complemented send:

* **Modbus RTU** (`modbus.rw`): a Read Exception Status request, four UART
  characters. The CRC-16 starts at all ones and goes out low byte first; for
  slave 0x11 the frame is `11 07 4C 22`.
* **USB token fields** (`usb_token.rw`): PID, address, endpoint and the CRC-5,
  which starts at all ones and is sent complemented. This is the packet on a
  clock and a data wire, not the USB line code.

All six are certified at the chip's 220 ns tick with one instruction per tick
(`test/test_spike.ml`, 15e): every `constraint` holds on every path, and no
role can fall behind an edge it must watch. The certificate checks the ticks
the source rounds to, so the Modbus bit is 238 ticks, 52.36 us against 52.08
us at 19200 baud, 0.5% slow and inside a UART's tolerance.

## How it is checked

| layer | check |
|---|---|
| semantics | `test/test_spike.ml`: every protocol and rule, record/mutate/replay, timing certificates cross-checked against simulation, eleven named CRCs against their published check values |
| projections | `test/fuzz.ml`: random well-formed programs (clocked bus with loops, literals, branches, stuffing; Manchester-style data edges), every role plus an observer, both issue models, with and without a second supplier, and every bound bit must agree across roles; 800 runs at 100 seeds in CI, 2,400 at 300 seeds with `dune exec ./test/fuzz.exe 300` |
| RTL vs model | `test/hw_diff.ml`: tick for tick on the bus, event for event, bit for bit in data memory; 17 protocol scenarios, 8 audit regressions and random programs, through the pin-level top and its loader |
| symmetry | `tapeout/test/test.py`: relabelling the cores, the wires or the role ids in the loader stream must relabel the result and change nothing else. Six metamorphic runs, one swap and one 4-cycle per action (the generators of S4), on RTL and on the gate-level netlist. They test each generator on the I2C configuration; they are not a proof for all 13,824 relabellings, which would need the property at every intermediate configuration |
| reload | `tapeout/test/test.py`: a reload in the middle of a transaction stops the cores, and no core keeps a role from the earlier load |
| netlist vs RTL | `hw/gl/run_gl.sh`: the routed netlist against the RTL, every output pin on every cycle, for thirteen programs chosen to cover each mechanism once (`tapeout/collect_gl_cases.sh`); runs in tt-relwire's CI as `gl_pins` |
| checksum on the netlist | `tapeout/test/test.py`: a USB token whose CRC-5 nobody is given (start at all ones, sent complemented); the receiving cores must check it and bind `10111`, on RTL and gate level |
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
* A 16-bit checksum register per core steps as an LFSR on a folding sample.
  The polynomial is data too (table entry 7, left-aligned), so one register
  serves CRC-5, CRC-15 and CRC-16. A `SETCRC` instruction gives it a start
  value; the level bit of a checksum `put` or `sample` complements it on the
  wire.
* 4 cores, a 128 x 26 flip-flop program memory with time-multiplexed fetch, a
  2-flop synchronizer on the protocol pins, a byte-wide loader and readback.
* **Single issue**: one instruction per core per tick, a tick being 11 cycles
  (220 ns at 50 MHz). Each instruction is fetched, parked in the core's own
  register, predecoded into registers, then executed, so the execute cycle is a
  short next-state mux. Every instruction, including loop setup and the final
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

## Physical design (Tiny Tapeout, IHP CMOS5L, 8x4 tiles)

`tapeout/` holds the Tiny Tapeout project; `export_tt_repo.sh` generates the
submission repository. CI builds with the March 2027 shuttle's action tag,
`ttihp27a` (LibreLane 3.1.0.dev4). `setup_local.sh`/`harden.sh` mirror the
earlier `ihp-cmos5l` action and have not been moved to the new pins.

What the physical flow taught the architecture:

* We could not integrate the IHP SRAM macro in this design on CMOS5L: its
  Metal4 power pins did not reach Tiny Tapeout's Metal4-only power grid
  without TopMetal1, which we could not use here. Tiny Tapeout allows SRAM
  macros in general; this was our integration problem, and the program memory
  is flip-flops.
* A shared constant-table read port steered by the executing core gave static
  timing a die-wide path from one core's instruction into another core's
  registers. Each core now has its own read ports, and its own instruction
  register.
* At 50 MHz the slow corner (1.08 V, 125 C) missed setup by 4.5 ns while the
  typical and fast corners met it, and dropping to 40 MHz alone did not close it:
  the paths were too deep (about 15 levels of weak gates on long wires, with each
  core's flip-flops spread over a few hundred micrometres). A **predecode stage**
  closed it: everything the execute cycle tests is stable for a whole tick, so it
  is computed one cycle earlier into registers.
* The gate-level test caught registers with no reset that RTL simulation hides;
  every state register now resets.
* With timing healthy, placement-stage repair margins (aim 30% inside the
  slew/cap limits, buffer long wires) cut the slew warnings by a third and the
  cap warnings almost entirely, and 50 MHz closes.
* Moving to the shuttle's toolchain grew the netlist by 10%: the cell library
  has no flop without a reset pin, the resets were synchronous, and the new
  flow ties each unused pin high with its own tie cell (4,526 of them). At 6x4
  that put the floorplan at 60% and placement failed after hold repair. The
  design moved to 8x4, the confirmed maximum.
* On the new netlist the gate-level test read X. Synthesis had folded the
  synchronous clear of one loop-index bit into `X | ~X`: always 0 in silicon,
  but X in simulation when the flop starts unknown, and the bit never left X.
  A three-valued replay of the netlist traced it to that bit; forcing the one
  bit to 0 made the test pass. The fix is structural, not a nudge to
  synthesis: `rst_n` goes through a two-flop synchronizer (asynchronous
  assert, synchronous release) to the flops' reset pins, so every state
  register is known in reset however the logic is built. The run/stop clear
  stays synchronous. Only the program memory and the constant table, which the
  loader writes, are not reset.

Current result, with the audit fixes (CI on [tt-relwire](https://github.com/lblommesteyn/tt-relwire/actions)):

| | |
|---|---|
| Tiny Tapeout precheck | passed |
| gate-level cocotb tests (9: I2C, six symmetry, the CRC-5 token, reload) | passed |
| netlist vs RTL, every pin every cycle (`gl_pins`, 13 programs) | identical |
| DRC (router, Magic) / LVS | 0 / 0 |
| setup, slow corner 1.08 V / 125 C, 50 MHz | met, +8.18 ns |
| setup, typical corner | met, +10.97 ns |
| hold, fast corner | met, +0.11 ns |
| utilization | 53% of 1724 x 711 um, 103,353 instances |
| max-slew / max-cap violations | 0 / 0 in every corner |
| antenna | 0 nets |

## Open questions

* CAN is modelled with hard synchronization on start of frame only, not
  in-frame resynchronization.
* The consistency claim is argued and fuzzed, not mechanized. An external
  audit (below) found that model/RTL agreement alone can miss a shared bug, so
  the tests now also check invariants of their own: every role holds the same
  bound bits and ends at the same instruction.
* Timing certificates assume an ideal pin interface unless given `--io`
  (synchronizer, output delay, skew, jitter); they are not a guarantee for pad
  delay, pull-up rise time or metastability, none of which has been measured.
* Nothing has run against an independent implementation of a protocol (a
  real I2C or SPI peripheral, or an FPGA): every peer in the tests is another
  RelWire core or the reference model.
* The program memory, as flip-flops, dominates area and hold buffering.
* One checksum per program, at most 16 bits: no CRC-32, and no USB data
  packet next to its token (two polynomials).

## External audit (November 2026)

An independent review ran extra programs against the model and the RTL and
found eleven problems that the existing tests passed over; a second pass
found two edge cases in the first fixes, and a third found six more,
including a timing certificate that promised more than the chip does. All
are fixed and each has a regression test (sections 21 and 22 of
`test/test_spike.ml` for the third pass) (`test/test_spike.ml` section 21, the audit
scenarios in `test/hw_diff.ml`, the reload test in `tapeout/test/test.py`):

| finding | fix |
|---|---|
| a branch on a sampled checksum bit went different ways in sender and observer (model and RTL agreed on the wrong run) | every role keeps the wire's level of a checksum bit; branches on literals are refused |
| timing values of 65,535 ticks or more were shortened to 65,535 after certification, and a finite maximum became "unbounded" | refused at compile time. `inf` is its own value, which only the keyword produces: arithmetic on it is a parse error, so no finite number or expression (`1000000`, `500000+500000`) can become unbounded |
| a reload left omitted cores with their old roles, and the frame reset did not stop a running chip | the frame reset now stops the cores and clears every role mask; the host library also sends STOP |
| `repeat i 0` ran its body once in hardware | zero-count loops are removed in the model and the compiler |
| a nested `repeat i` that reused the name read the outer index | the inner binding shadows the hardware loop variable; only the outermost repeat is the hardware loop, however deep the nesting (a follow-up review found a third level reused it) |
| a fifth wire and out-of-range field indices were accepted and aliased other resources | one validation boundary: wire count, index bounds, unique names, widths, ordered windows, nonnegative counts and a positive tick |
| an edge arriving on the first tick after its deadline was accepted | the deadline is checked first, in the model and the RTL, as for data edges |
| reaction-hazard analysis ignored the instructions before the first timed event | the schedule has an entry node |
| two cores driving a push-pull wire to opposite levels gave the 1 the wire | the chip detects it, releases the pin and reports a bad wire to every core, as the model's Contention |
| the symmetry tests were described as covering all 13,824 relabellings | reworded: six metamorphic checks of the generators |
| long vector paths were truncated by the testbench, which then hung | 1,024-character paths, missing inputs are a fatal error, and a watchdog |
| *third pass:* the certificate dropped the instructions before a program's first put, promising 11 ticks where the model and RTL give 8 | the put's offset from the program start counts, for certificates and reaction hazards |
| an unbounded wait reported a valid edge as early once the 16-bit tick counter wrapped (after 14.4 ms) | each core counts ticks since its anchor in a saturating counter; tested with an edge at tick 65,537 from outside the chip |
| a product could overflow to 0, and the number `max_int` read as `inf` | every literal and every intermediate result is below 2^40, with products checked before they are formed |
| `sample W c[0] into c` on the checksum itself compiled as a data sample | compiled as a checksum sample |
| `if_run W 0` skipped its body in the model and ran it in the RTL | a run of at least one sample is required |
| a maximum written in time units rounded up, so a 500 ns deadline accepted an edge at 660 ns | maximums round down to whole ticks, minimums up |

