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
(per-bit resync), the timing certificate cross-checked against simulation with
and without io delays, and the `.rw` sources checked against the OCaml-built
programs.

Open questions: a data override demotes the whole role while a time override
flips only that edge; CAN needs bit stuffing (data-dependent insertion), which
the language cannot express yet.

Build: `dune build` (OCaml 4.14, dune 3).
