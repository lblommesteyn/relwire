# SPDX-License-Identifier: Apache-2.0
"""On-board demo for the Tiny Tapeout demo board (MicroPython, TT SDK).

One I2C program, one binary, three cores: a controller writes address 0x53
(R/W = 0) and data 0xBA, a target acknowledges, and a sniffer with no roles
reconstructs the whole transaction. All three share the chip's uio[1:0]
pins, with the RP2's pull-ups as the bus pull-ups.

Copy to the board with relwire_tt.py and i2c_image.py, then:  import demo_i2c
The image must be compiled for the tick this clock gives (10 cycles/tick):
  relwirec examples/i2c_ns.rw --tick-ns 1000 --emit-image host/i2c_image.py
"""
from ttboard.demoboard import DemoBoard
from ttboard.mode import RPMode
import i2c_image as img
from relwire_tt import Core, RelWireChip, TTBoardPort

CLOCK_HZ = 10_000_000  # 1 us tick: I2C at about 70 kHz, gentle on pull-ups

tt = DemoBoard.get()
tt.shuttle.tt_um_relwire.enable()
tt.mode = RPMode.ASIC_RP_CONTROL
tt.clock_project_PWM(CLOCK_HZ)
tt.reset_project(True)
tt.reset_project(False)

chip = RelWireChip(TTBoardPort(tt, CLOCK_HZ))
cores = [
    Core(["controller"], {"addr": "10100110", "data": "01011101"}),
    Core(["target"], {"ack": "0", "ack2": "0"}),
    Core([], {}),  # sniffer: owns nothing, observes everything
]
chip.load(img, cores)
chip.run_until_halted()
names = ["controller", "target", "sniffer"]
for i, name in enumerate(names):
    f = chip.fields(img, i)
    print("%-10s addr=%s ack=%s data=%s ack2=%s" % (name, f["addr"], f["ack"], f["data"], f["ack2"]))
print("all three should show addr=10100110 ack=0 data=01011101 ack2=0")
