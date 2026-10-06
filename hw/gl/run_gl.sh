#!/usr/bin/env bash
# Gate-level check: run the same pin stimulus on the RTL and on the hardened
# netlist, and require identical outputs on every cycle plus identical
# readback. Usage: run_gl.sh NETLIST CASE_DIR BYTES CYCLES
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
NET=$1 CASE=$2 BYTES=$3 CYCLES=$4
PDK_ROOT=${PDK_ROOT:-/root/tt/pdk}
M=$PDK_ROOT/ihp-sg13cmos5l/libs.ref/sg13cmos5l_stdcell/verilog
OUT=$(mktemp -d)
iverilog -g2012 -o "$OUT/rtl" "$HERE/tb_pins.v" "$HERE/../rpm_top.v" "$HERE/../rpm_core.v"
iverilog -g2012 -DFUNCTIONAL -DUNIT_DELAY=#0 -o "$OUT/gl" "$HERE/tb_pins.v" "$NET" "$M/sg13cmos5l_stdcell.v" "$M/sg13cmos5l_udp.v"
cp -r "$CASE" "$OUT/rtl_case"; cp -r "$CASE" "$OUT/gl_case"
vvp -n "$OUT/rtl" +dir="$OUT/rtl_case" +bytes=$BYTES +cycles=$CYCLES > /dev/null
vvp -n "$OUT/gl" +dir="$OUT/gl_case" +bytes=$BYTES +cycles=$CYCLES > "$OUT/gl.log" 2>&1
n=$(wc -l < "$OUT/rtl_case/pins.txt")
if cmp -s "$OUT/rtl_case/pins.txt" "$OUT/gl_case/pins.txt"; then
  echo "GL MATCH $(basename "$CASE"): $n lines identical"
else
  echo "GL MISMATCH $(basename "$CASE")"
  diff "$OUT/rtl_case/pins.txt" "$OUT/gl_case/pins.txt" | grep -E "^[<>]" | sort | uniq -c | sort -rn | head -12
  exit 1
fi
