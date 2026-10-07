# SPDX-License-Identifier: Apache-2.0
"""Host library for the RelWire chip (tt_um_relwire) on a Tiny Tapeout demo board.

Runs under MicroPython on the board's RP2 (with the TT SDK) and under CPython
(for tests). It turns a compiled program image (relwirec --emit-image) plus a
role assignment per core into the loader byte stream, runs the chip, and
decodes each core's data memory back into named fields.

    import i2c_image as img
    chip = RelWireChip(TTBoardPort(tt))
    chip.load(img, [Core(["controller"], {"addr": "10100110", "data": "01011101"}),
                    Core(["target"], {"ack": "0", "ack2": "0"}),
                    Core([], {})])            # no roles: a sniffer
    chip.run_until_halted()
    print(chip.fields(img, 2))               # what the sniffer saw

Bit strings are written in transmission order: character i is field bit i.
"""

CMD_WORD, CMD_CONST, CMD_DATA, CMD_MASK, CMD_WIRE, CMD_RUN, CMD_STOP, CMD_READ = 1, 2, 3, 4, 5, 6, 7, 8
DATA_BYTES = 8  # 64-bit data memory per core
NCORES = 4


class Core:
    def __init__(self, roles, inputs):
        self.roles = list(roles)
        self.inputs = dict(inputs)


def role_mask(image, roles):
    m = 0
    for r in roles:
        if r in image.ROLES:  # a role that owns nothing contributes no bit
            m |= 1 << image.ROLES[r]
    return m


def data_bits(image, inputs):
    """Initial data memory (64 bits, LSB = bit 0) from field inputs and literals."""
    bits = 0
    for name, (base, width) in image.LAYOUT.items():
        if name in image.LITS:
            if image.LITS[name]:
                bits |= 1 << base
        elif name in inputs:
            s = inputs[name]
            if len(s) != width:
                raise ValueError("%s needs %d bits, got %d" % (name, width, len(s)))
            for i, ch in enumerate(s):
                if ch == "1":
                    bits |= 1 << (base + i)
    return bits


def load_stream(image, cores):
    """The loader byte stream: program, constants, per-core data and role
    masks, wire modes, then RUN. Same bytes as the reference toolchain."""
    if len(cores) > NCORES:
        raise ValueError("at most %d cores" % NCORES)
    out = []
    for a, w in enumerate(image.WORDS):
        out += [CMD_WORD, a, (w >> 24) & 0xFF, (w >> 16) & 0xFF, (w >> 8) & 0xFF, w & 0xFF]
    for i, v in enumerate(image.CONSTS):
        out += [CMD_CONST, i, v >> 8, v & 0xFF]
    for i, core in enumerate(cores):
        d = data_bits(image, core.inputs)
        for b in range(DATA_BYTES):
            out += [CMD_DATA, i, b, (d >> (8 * b)) & 0xFF]
        out += [CMD_MASK, i, role_mask(image, core.roles)]
    for i, (_, res) in enumerate(image.WIRES):
        out += [CMD_WIRE, i, res]
    out += [CMD_RUN]
    return out


class RelWireChip:
    """Drives the loader protocol through a port object with:
    set_data(byte), set_strobe(0/1), set_frame(0/1), read_out() -> int,
    wait(cycles) (project clock cycles)."""

    def __init__(self, port):
        self.port = port

    def send(self, byte):
        p = self.port
        p.set_data(byte)
        p.wait(2)
        p.set_strobe(1)
        p.wait(3)
        p.set_strobe(0)
        p.wait(3)

    def load(self, image, cores):
        self.port.set_frame(1)
        self.port.wait(4)
        self.port.set_frame(0)
        self.port.wait(4)
        for b in load_stream(image, cores):
            self.send(b)

    def status(self):
        v = self.port.read_out()
        return {"running": (v >> 7) & 1, "halted": (v >> 6) & 1,
                "last_code": (v >> 3) & 7, "last_core": (v >> 1) & 3}

    def run_until_halted(self, max_wait=10_000_000, step=10_000):
        waited = 0
        while not self.status()["halted"]:
            if waited >= max_wait:
                raise RuntimeError("cores did not halt")
            self.port.wait(step)
            waited += step
        self.send(CMD_STOP)

    def read_data(self, core):
        d = 0
        for b in range(DATA_BYTES):
            self.send(CMD_READ)
            self.send(core)
            self.send(b)
            self.port.wait(2)
            d |= self.port.read_out() << (8 * b)
        self.send(CMD_STOP)  # leave readback mode
        return d

    def fields(self, image, core):
        d = self.read_data(core)
        return decode(image, d)


def decode(image, d):
    out = {}
    for name, (base, width) in image.LAYOUT.items():
        out[name] = "".join("1" if (d >> (base + i)) & 1 else "0" for i in range(width))
    return out


class TTBoardPort:
    """Port for the Tiny Tapeout demo board (TT MicroPython SDK).
    Protocol wires are uio[3:0] (open-drain buses get the RP2's pull-ups);
    the loader strobe and frame are uio[7] and uio[6], driven by the RP2."""

    def __init__(self, tt, clock_hz=40_000_000):
        from machine import Pin  # MicroPython only
        import time
        self.tt, self.time, self.clock_hz = tt, time, clock_hz
        tt.uio_oe_pico.value = 0b11000000
        for n in range(4):
            getattr(tt.pins, "uio%d" % n).pull = Pin.PULL_UP
        tt.uio_in[7] = 0
        tt.uio_in[6] = 0

    def set_data(self, byte):
        self.tt.ui_in.value = byte

    def set_strobe(self, v):
        self.tt.uio_in[7] = v

    def set_frame(self, v):
        self.tt.uio_in[6] = v

    def read_out(self):
        return int(self.tt.uo_out.value)

    def wait(self, cycles):
        us = (cycles * 1_000_000) // self.clock_hz
        self.time.sleep_us(us if us > 0 else 1)
