#!/bin/bash
# Usage (WSL Ubuntu-22.04, root): bash run.sh
set -e
cd "$(dirname "$0")"
PLAT=${PLAT:-/root/OpenROAD-flow-scripts/flow/platforms/ihp-sg13g2}
YOSYS=${YOSYS:-/root/oss-cad-suite/bin/yosys}
mkdir -p out
sed "s#PLAT#$PLAT#g" synth.ys > out/synth.ys
$YOSYS -q -l out/synth.log -s out/synth.ys
PLAT=$PLAT openroad -no_init -exit pnr.tcl > out/pnr.log 2>&1
