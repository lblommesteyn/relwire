# SPDX-License-Identifier: Apache-2.0
"""Three cores on one chip run the same I2C program as controller, target and
sniffer. The test programs the chip over its pins, lets the transaction run,
then reads every core's data memory back and compares it, bit by bit, with
the RelWire reference model (expected.txt, from tools/gen_tt_test)."""

import os

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles, FallingEdge

HERE = os.path.dirname(os.path.abspath(__file__))
TICK_CYCLES = 10  # 4 + 2 (pin synchronizer) + 4 cores


def read_vectors():
    with open(os.path.join(HERE, "load.hex")) as f:
        stream = [int(l, 16) for l in f if l.strip()]
    with open(os.path.join(HERE, "expected.txt")) as f:
        lines = [l.split() for l in f if l.strip()]
    ticks = int(lines[0][2])
    expected = {int(c): bits for c, bits in lines[1:]}
    return stream, ticks, expected


async def send(dut, byte):
    # change inputs on the falling edge so RTL and gate-level runs agree on
    # which rising edge first sees them
    await FallingEdge(dut.clk)
    dut.ui_in.value = byte
    await ClockCycles(dut.clk, 2, rising=False)
    dut.load_strobe.value = 1
    await ClockCycles(dut.clk, 3, rising=False)
    dut.load_strobe.value = 0
    await ClockCycles(dut.clk, 3, rising=False)


@cocotb.test()
async def test_i2c_three_roles_one_binary(dut):
    stream, ticks, expected = read_vectors()
    cocotb.start_soon(Clock(dut.clk, 25, unit="ns").start())
    dut.ena.value = 1
    dut.ui_in.value = 0
    dut.load_strobe.value = 0
    dut.load_frame.value = 0
    dut.rst_n.value = 0
    await ClockCycles(dut.clk, 10)
    dut.rst_n.value = 1
    await ClockCycles(dut.clk, 4)

    for b in stream:  # ends with 06: run
        await send(dut, b)
    await ClockCycles(dut.clk, ticks * TICK_CYCLES)
    assert int(dut.uo_out.value) & 0x40, "all cores should have halted"
    await send(dut, 0x07)  # stop

    for core, want in expected.items():
        got = []
        for byte in range(8):
            await send(dut, 0x08)
            await send(dut, core)
            await send(dut, byte)
            await ClockCycles(dut.clk, 2, rising=False)
            v = int(dut.uo_out.value)
            got += [(v >> j) & 1 for j in range(8)]
        for j, w in enumerate(want):
            if w != "-":
                assert got[j] == int(w), f"core {core} bit {j}: got {got[j]}, model says {w}"
        dut._log.info(f"core {core}: {sum(w != '-' for w in want)} bits match the model")
