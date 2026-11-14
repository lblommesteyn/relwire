#!/usr/bin/env bash
# Collects RTL-vs-netlist pin cases from the cases test/hw_diff.ml writes
# (load.hex, cfg.hex per scenario, after `dune test`) into DEST/cases, with
# DEST/cases.txt listing "name bytes cycles". hw/gl/run_gl.sh then runs each
# on the RTL and on the netlist and requires every output pin to agree on
# every cycle. The set covers each mechanism once: arbitration, branches, full
# duplex, timer sampling, data edges, stuffing with the CRC, all four wires,
# a missed deadline, ownership moving mid-frame, SETCRC and inverted send,
# push-pull contention between cores, a branch on a checksum bit.
# Usage: collect_gl_cases.sh DEST [SRC]
set -euo pipefail
DEST=$1
SRC=${2:-${RELWIRE_HW_DIR:-${TMPDIR:-/tmp}/relwire_hw}}
SRC=${SRC%/}
CASES="i2c_arbitration:2000 i2c_read_branch:3000 spi_full_duplex:800 uart:1200
manchester32:3600 can_crc_arbitration:8000 jtag:1200 onewire_no_device:4000
ps2_host:7000 usb_token:1200 push_pull_contention:30 crc_branch:30
crc_into_itself:30"
mkdir -p "$DEST/cases"
: > "$DEST/cases.txt"
for c in $CASES; do
  name=${c%%:*} ticks=${c##*:}
  mkdir -p "$DEST/cases/$name"
  cp "$SRC/$name/load.hex" "$SRC/$name/cfg.hex" "$DEST/cases/$name/"
  echo "$name $(grep -c . "$SRC/$name/load.hex") $((ticks * 11))" >> "$DEST/cases.txt"
done
cat "$DEST/cases.txt"
