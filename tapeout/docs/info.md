## How it works

RelWire is a protocol emulator built around one idea: **a protocol is written
once, for every participant at the same time, and the chip runs that one
description as whichever participant you ask for.**

A RelWire program reads like a timing diagram. Each statement says what happens
on a wire and *who owns it*:

```
field addr[8] from controller        # the controller supplies these bits
field ack[1]  from target            # the target supplies this one
put    SDA addr[i]                   # the owner drives the bit; everyone else lets go
edge   SCL -> 1 @rise in [t_low, inf]
sample SDA addr[i]                   # the owner checks it; everyone else records it
```

There is no "if I am the controller" anywhere. A core that owns a value drives
it and checks it on the wire; a core that does not own it observes it. The same
binary is a controller on one core, a target on another, and a protocol
analyser on a third, and all three can share the same pins. If two cores both
own a value and the wire disagrees with one of them (lost arbitration on I2C or
CAN), that core is demoted to an observer at run time and carries on decoding.

**The chip.** Four identical cores run one shared binary from a 128 x 26-bit
program memory. A 4-bit role mask per core says which roles it plays. Timing
values (bit times, setup and hold limits, deadlines) live in an 8-entry table
shared by all cores, so the same program runs at a different speed by changing
data, not code. Each core has 64 bits of data memory for the fields it sends
and receives, a zero-overhead loop, branches on wire history that every role
has seen (CAN bit stuffing, I2C read-or-write), and a 16-bit checksum register.

**The checksum.** Every role folds the same wire levels into its own LFSR, so
every role holds the same CRC without anyone being given it. The owner sends
it; everyone else checks the wire against their own copy and reports a
mismatch. The polynomial (up to 16 bits), the start value and whether it is
sent complemented are all part of the program: the same hardware computes
CAN's CRC-15, Modbus's CRC-16, USB's CRC-5 and PS/2's parity.

**Timing.** A tick is 11 clock cycles (220 ns at 50 MHz); each core executes
one instruction per tick. Pins pass a 2-flop synchronizer, so external devices
may be asynchronous. Every core reports events: matched, a peer held the
wire, lost arbitration, missed deadline, early edge, collision, mismatch.

**Protocols written for it** (each one program, every role from the same
binary, checked against a reference model at RTL): I2C (write, read, and
read-or-write chosen by the R/W bit; arbitration), SPI (full duplex), UART,
Manchester, CAN (bit stuffing, arbitration, on-chip CRC-15), JTAG (IR and DR
scans on all four wires), 1-Wire (reset, presence, write and read slots), PS/2
in both directions (the clock changes owner mid-frame), Modbus RTU (CRC-16)
and USB token fields (CRC-5). The last six were added as programs only, with
no change to the hardware.

**How it is checked.** A reference model in OCaml defines the semantics. The
RTL is compared with it tick for tick on the bus and bit for bit in data
memory, over 17 protocol scenarios and randomly generated programs. Timing
certificates prove, for each program, that every timing constraint holds on
every path at the chip's real tick. The cocotb tests below run on the RTL and
on the routed gate-level netlist, and the netlist is also compared with the
RTL on every output pin on every cycle for thirteen programs. Six of the cocotb
tests are symmetry checks: renaming the cores, the wires or the role ids must
rename the result and change nothing else (one swap and one 4-cycle, the
generators of each group of permutations, tested on the I2C configuration).
Another reloads the chip in the middle of a transaction and checks that no core
keeps a role from the earlier load.

Source, language reference, compiler and the full test suites:
https://github.com/lblommesteyn/relwire

## How to test

**Programming.** The chip is programmed through a byte-wide loader: put a byte
on `ui_in` and pulse `uio[7]` (rising edge). `uio[6]` high resets the loader's
framing, stops the cores and clears every core's role mask: start every load
with it. Commands (first byte, then arguments):

| bytes | meaning |
|---|---|
| `01 addr b3 b2 b1 b0` | program word `addr` (low 26 bits) |
| `02 i hi lo` | timing constant `i` (16 bits) |
| `03 core byte data` | data-memory byte of one core |
| `04 core mask` | role mask of one core |
| `05 wire mode` | wire resolution: 0 push-pull, 1 open-drain low, 2 open-drain high |
| `06` | run |
| `07` | stop |
| `08 core byte` | show a data-memory byte on `uo_out` |

The compiler, `relwirec FILE.rw --tick-ns N --emit-image OUT.py`, produces the
program words, constants and data layout; `host/relwire_tt.py` in the source
repository builds the loader stream from them and drives it from the Tiny
Tapeout demo board (MicroPython). Compile for the tick your clock gives:
11 clock cycles per tick, so `--tick-ns 1000` at 11 MHz.

**Status.** While running, `uo_out` shows `[7]` running, `[6]` all cores
halted, `[5:3]` the code of the last event and `[2:1]` the core that raised it
(1 match, 2 peer held the wire, 3 lost arbitration, 4 missed deadline, 5 early
edge, 6 collision or bad wire, 7 mismatch). Two cores driving a push-pull wire
to opposite levels is a bad wire: the pin is released and every core is told.

**A first test (I2C, no external parts).** Load `examples/i2c_ns.rw` with three
cores: core 0 as controller (address and data byte in its data memory), core 1
as target, core 2 with an empty role mask (sniffer). Enable the demo board's
pull-ups on `uio[1:0]`, send `06`, wait for `uo_out[6]`, send `07`, then read
each core's data memory with `08`. All three cores hold the same address, data
and ACK bits. `host/demo_i2c.py` does exactly this.

**Against real devices.** Wire a device to `uio[3:0]`, give its role to no
core, and the chip plays the other side: for example a controller against a
real I2C or SPI peripheral, or a sniffer that decodes and checks a real bus
without driving it (role mask 0).

## External hardware

None for the built-in I2C test (the demo board's pull-ups are enough).
Open-drain buses (I2C, CAN-style, 1-Wire, PS/2) need pull-up resistors on the
protocol wires, about 4.7 kOhm. Any device that speaks one of the protocols
above can be connected to `uio[3:0]` at the demo board's 3.3 V logic levels.
