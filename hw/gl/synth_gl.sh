#!/usr/bin/env bash
# Quick gate-level netlist (synthesis only, IHP CMOS5L cells) for X-checking
# the RTL with hw/gl/run_gl.sh, without a full harden. Usage: synth_gl.sh OUT.v
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
PDK_ROOT=${PDK_ROOT:-/root/tt/pdk}
LIB=$PDK_ROOT/ihp-sg13cmos5l/libs.ref/sg13cmos5l_stdcell/lib/sg13cmos5l_stdcell_typ_1p20V_25C.lib
${YOSYS:-/root/oss-cad-suite/bin/yosys} -q -p "read_verilog -sv $HERE/../rpm_core.v $HERE/../rpm_top.v; \
  synth -top tt_um_relwire -flatten; dfflibmap -liberty $LIB; abc -liberty $LIB; \
  setundef -zero; hilomap -singleton -hicell sg13cmos5l_tiehi L_HI -locell sg13cmos5l_tielo L_LO; \
  opt_clean -purge; write_verilog -noattr $1"
