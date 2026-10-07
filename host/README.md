# Running RelWire on the Tiny Tapeout demo board

`relwire_tt.py` drives the chip's loader over the TT MicroPython SDK; `demo_i2c.py`
runs one I2C program on three cores (controller, target, sniffer) sharing the
chip's pins, and prints what each core saw.

1. Compile the program for the tick your clock gives (10 clock cycles per tick):

       relwirec examples/i2c_ns.rw --tick-ns 1000 --emit-image host/i2c_image.py   # 10 MHz

2. Copy `relwire_tt.py`, `i2c_image.py` and `demo_i2c.py` to the board
   (`mpremote cp ...`), then `import demo_i2c` at the REPL.

`test_host.py` checks, under CPython, that the stream this library builds is byte
for byte the stream the reference toolchain produces, which is the stream the cocotb
tests run on the RTL and on the gate-level netlist. What has not been tested is the
board itself: no silicon exists yet.
