#!/usr/bin/env bash
# Harden tt_um_relwire with the Tiny Tapeout CMOS5L flow, locally.
# WSL Ubuntu-22.04, root, Docker running, after setup_local.sh.
# Works in a native-filesystem copy and copies the reports back.
set -euo pipefail
TT_HOME=${TT_HOME:-/root/tt}
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK=$TT_HOME/work
export PDK_ROOT=$TT_HOME/pdk PDK=ihp-sg13cmos5l
rm -rf "$WORK" && mkdir -p "$WORK"
cp -r "$HERE/info.yaml" "$HERE/src" "$HERE/docs" "$WORK/"
cp "$HERE/../hw/rpm_top.v" "$WORK/src/tt_um_relwire.v"
cp "$HERE/../hw/rpm_core.v" "$WORK/src/rpm_core.v"
ln -sfn "$TT_HOME/tools" "$WORK/tt"
cd "$WORK"
git init -q && git add -A && git -c user.email=local@local -c user.name=local commit -qm local
git remote add origin https://github.com/lblommesteyn/relwire.git
. "$TT_HOME/venv/bin/activate"
./tt/tt_tool.py --create-user-config --ihp
./tt/tt_tool.py --harden --ihp
./tt/tt_tool.py --print-stats --ihp || true
