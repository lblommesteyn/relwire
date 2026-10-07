# RelWire semantics

This note states what a RelWire program means, what specialization does, what
property the projections of one program are claimed to have when they run
together, and why. It is a proof sketch, not a mechanized proof; the
randomized checker (`test/fuzz.ml`) is the empirical side of the same claim.

## 1. Programs

A program declares **wires**, **variables** and a **body**.

A wire has a *resolution*, which says what the bus carries given every agent's
drive, and a *bias* for when nobody drives:

| resolution | drives combine as |
|---|---|
| `dominant_low` | any 0 wins (open drain, I2C, CAN) |
| `dominant_high` | any 1 wins |
| `push_pull` | one driver; two disagreeing drivers are contention |

Every variable has an **owner**, a role name:

* `field f[n] from r`: data bits that role `r` supplies;
* `lit l = v from r`: a fixed bit (framing) that `r` drives and everyone checks;
* `derived d from r`: a checked bit whose value a branch computes from wire
  history (a stuff bit);
* `time t from r`: an edge time that `r` produces.

Statements, with what they mean for the owner and for everyone else:

| statement | owner (supplied) | everyone else (observed) |
|---|---|---|
| `edge W -> L @t in [a, b]` | drive `W` to `L` at `a` after the anchor | wait for the transition; must fall in `[a, b]` |
| `put W f[i]` / `put_not W f[i]` | drive the bit (or its complement) | release `W` |
| `sample W f[i]` | read `W`; it must equal the bit | bind the bit to `W` (a literal or derived bit: check it) |
| `toggle W f[i] @t at n in [a, b]` | edge whose new level is the bit, at `n` | accept a transition in `[a, b]`, bind the bit to its level |
| `after t n` | timer mark `n` after the anchor | same |
| `repeat i n { }` | unrolled (the outermost one becomes a hardware loop) | same |
| `if f[i] == v { } else { }`, `if_run W n { }` | branch | same branch |

## 2. Denotation

A program `P` denotes a relation `R(P)` between an initial data state, a bus
trace (the resolved value of every wire at every tick) and a final data state.
A trace is in the relation when there is an assignment of every variable such
that each statement's reading holds: each `sample` reads the bit's value off
the wire, each `edge` happens in its window, each literal has its value, and
branches follow the bits they test. `R(P)` mentions no roles. It describes the
legal interactions, not any participant.

## 3. Specialization

A **role assignment** `rho` is the set of roles one agent plays. Specialization
`specialize(P, rho)` resolves, for every statement, whether the variable it
touches is *supplied* (its owner is in `rho`) or *observed*, and produces an
ordinary deterministic machine. Nothing about the mode is left to run time
except one rule (section 5). In particular the source language has no way to
ask which role it is running as; there is no syntax for it.

On the chip, specialization is not even a compile step: every instruction
carries its owner's role id, and a core's 4-bit role mask is `rho`. All roles
run the same binary.

## 4. The branch rule

`if f[i] == v` is accepted only if `f[i]` has been **sampled on every path**
to the branch, and `if_run W n` tests only the history of samples on `W`.
`specialize` rejects anything else.

**Lemma 1 (same branch).** If all agents' bindings of every sampled bit agree,
all agents take the same side of every branch.

Branches test only sampled bits or wire history, and every agent samples the
same wire at the same program point, so the tested values are equal. Without
the rule an owner could branch on a bit it supplies but has not sent, and its
machine would silently diverge from everyone else's.

## 5. Ownership transfer: the one dynamic rule

A supplied value can be contradicted by the wire. What transfers is exactly
**what the wire contradicted**:

* **A data bit** (`sample` reads something other than the supplied bit, on a
  dominant wire): the wire is now carrying another agent's message. Any further
  bit we supply would be spliced into someone else's message, so the agent
  loses authorship of the *message*: its whole role is demoted to observed for
  the rest of the run, it releases every wire, and it binds the bit to what
  the wire carried. (I2C and CAN arbitration.)
* **An edge time** (a released recessive edge does not appear, because a
  peer holds the wire): no data is contradicted; a peer only delayed one event.
  Only that edge's time transfers. The agent waits for the edge, re-anchors on
  it and keeps supplying everything else. (I2C clock stretching.)
* **A data edge** (`toggle`, whose bit *is* the transition): if the
  transition does not happen at all, another transmitter held the line through
  the cell, so no symbol reached the wire. That is a **collision**, not an
  arbitration: every transmitter in it reports `Collision`, stops, and adopts
  no value from the cell. Level-encoded bits can arbitrate; transition-encoded
  bits can only collide. This is why CAN arbitrates on NRZ bits while
  Manchester networks detect collisions instead.

The three cases are one principle at three granularities: message, event,
symbol. The third case was found by the randomized checker (section 8): the
first implementation let the "winner" of a Manchester collision report success
while every other node failed to decode the cell.

## 6. The consistency claim

Run agents `A_1 ... A_k`, with role assignments `rho_1 ... rho_k`, on one bus.
Assume the environment is the agents themselves (no outside driver), and for
each value at most one *distinct* supplied version reaches the wire (true for
one supplier per role; with several suppliers of one role, the dominant wire
picks one).

**Claim (projection consistency).** Every agent finishes; at every `sample`
all agents bind the same bit, namely the resolved wire value; all take the
same branches; an observer's bindings equal the bits the surviving supplier
sent; and the recorded bus trace, replayed into a fresh observer, decodes the
same. For data edges that collide, the weaker property holds: no divergence is
silent (every transmitter in the collision reports it) and agents that finish
without any reported fault agree.

**Proof sketch** (induction over the instruction stream). Invariant: all agents
are at the same statement of the unrolled program (up to waiting), and their
bindings of all sampled bits agree.

* `put`: only owners drive; observers release. The resolved wire value is the
  surviving supplier's bit (one supplier) or the dominant bit (several).
* `sample`: everyone reads the same resolved value at the same anchor-relative
  time. Observers bind it. A supplier whose bit matches keeps its binding,
  which equals the wire. A supplier whose bit differs binds the wire value and
  is demoted (section 5). In all cases the bindings agree, so the invariant is
  preserved.
* `edge`, `after`: they move the anchor. Every agent anchors on the same
  transition (or the same timer mark from that anchor), so they stay aligned;
  a stretched edge only delays everyone together.
* `toggle`: either a transition occurs and everyone binds its level (as for
  `sample`), or none occurs and every transmitter reports a collision.
* Branches: Lemma 1.

Observers' bindings equal the supplier's bits because, absent a lost
arbitration, the wire carries exactly the supplied bits. Replay decodes the
same because an observer's behaviour is a function of the trace alone.

What the sketch leaves informal: timing (that every agent reaches each wait
before its edge can legally occur). That is the separate **reaction-hazard**
check under single issue (`reaction_hazards`), and the timing certificate.

## 7. Timing

Two issue models share the semantics above.

* **Run until blocked** (idealized): zero-time statements are free.
* **Single issue** (the chip): every instruction, including loop setup and the
  final halt, costs one tick.

`certify` computes, for each constraint, bus-time bounds over every
control-flow path (branches give a DAG), from the wire resolution (dominant
edges are exact, recessive ones may be held back by a peer), the io model
(synchronizer, output delay, skew, jitter) and the issue model.
`reaction_hazards` reports any observed edge whose earliest legal time comes
before the core can reach the wait.

## 8. Evidence

* `test/test_spike.ml`: hand-written scenarios for every protocol and rule,
  including record, mutate and replay.
* `test/fuzz.ml`: random well-formed programs from two families (clocked bus
  with loops, literals, branches and stuffing; Manchester-style data edges),
  every role as an agent plus an observer, both issue models, with and without
  a second supplier of one role. 2,400 runs at 300 seeds.
* `test/hw_diff.ml`: the RTL against the model, tick for tick on the bus,
  event for event and bit for bit, on hand-written and random programs.
* `hw/gl/`: the routed netlist against the RTL, cycle for cycle on every pin.

## 9. Relation to prior work

Projecting one global description into per-participant behaviour is the core
idea of multiparty session types and choreographic programming, where it is
called endpoint projection. RelWire applies that idea where those systems do
not usually go:

* a **shared physical medium** with resolution rules (wired-AND, contention)
  instead of point-to-point message channels;
* **time as a relational variable**: an edge's time has an owner, and a peer
  can take it (clock stretching);
* **dynamic ownership transfer** on observed wire outcomes (arbitration,
  collision), where classical projections are static;
* projections that compile to **one binary for a small hardware machine**,
  with the role as a register, checked down to the routed netlist.

## 10. Limitations

* The consistency claim is argued, not mechanized.
* Only one level of hardware loop; deeper repeats are unrolled.
* CAN is modelled with hard synchronization on start of frame only, not
  in-frame resynchronization.
* The environment is assumed to be the agents themselves. An arbitrary
  external device is only *checked* against the program by an observer, not
  constrained by it.
