## How it works

RelWire is a protocol language in which one program describes a protocol for every
participant at once. Each statement says which role owns a value or an edge; a core
drives what its roles own and observes the rest. The chip has four cores that run the
same binary from one shared SRAM. A 4-bit role mask per core picks controller,
target, sniffer or any mix, so the same I2C program is a controller on one core, a
target on another, and a protocol checker on a third. Lost arbitration hands a core's
role over to observation at run time. Timing values live in a shared table, so a
program's speed is data.

Each protocol tick is 10 clock cycles (200 ns at 50 MHz); every instruction takes one tick.

## How to test

Send the loader byte stream produced by `relwirec` (program words, timing constants,
data bytes, role masks, wire modes, then `06` to run) on `ui_in`, strobing
`uio_in[7]` for each byte. The protocol wires are `uio[3:0]`; add pull-ups for I2C or
CAN. Read data memory back with command `08 core byte` on `uo_out`.

## External hardware

Pull-up resistors on the protocol wires for open-drain buses (I2C, CAN-style).
