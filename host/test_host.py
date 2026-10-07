# CPython check of relwire_tt.py against the reference toolchain: the loader
# stream it builds must equal, byte for byte, the stream tools/gen_tt_test
# produces (the one the cocotb tests run on the RTL and the gate-level
# netlist), and decoding the model's expected data memory must give the
# transmitted fields.
import importlib.util
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import relwire_tt as rw


def load(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


img = load(sys.argv[1], "img")
vec = sys.argv[2]
cores = [
    rw.Core(["controller"], {"addr": "10100110", "data": "01011101"}),
    rw.Core(["target"], {"ack": "0", "ack2": "0"}),
    rw.Core([], {}),
]
ours = rw.load_stream(img, cores)
with open(os.path.join(vec, "load.hex")) as f:
    ref = [int(l, 16) for l in f if l.strip()]
assert ours == ref, "stream differs at byte %d" % next(i for i, (a, b) in enumerate(zip(ours, ref)) if a != b)
print("loader stream: %d bytes, identical to the reference toolchain" % len(ours))

with open(os.path.join(vec, "expected.txt")) as f:
    rows = [l.split() for l in f if l.strip() and not l.startswith("#")]
for core, bits in rows:
    d = sum(1 << j for j, c in enumerate(bits) if c == "1")
    got = rw.decode(img, d)
    assert got["addr"] == "10100110" and got["data"] == "01011101", got
    assert got["ack"] == "0" and got["ack2"] == "0", got
print("decode: all %d cores read back addr, data and both ACKs" % len(rows))
