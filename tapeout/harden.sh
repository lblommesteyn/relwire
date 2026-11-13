#!/usr/bin/env bash
# Harden tt_um_relwire with the Tiny Tapeout CMOS5L flow, locally, the way
# tt-gds-action@ttihp27a does it in CI. Run setup_local.sh first.
# Works in a scratch copy under $TT_HOME/work.
set -euo pipefail
TT_HOME=${TT_HOME:-$HOME/tt}
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK=$TT_HOME/work
export PDK_ROOT=${PDK_ROOT:-$TT_HOME/pdk} PDK=ihp-sg13cmos5l
TT_ARGS="--pdk $PDK"
rm -rf "$WORK" && mkdir -p "$WORK"
cp -r "$HERE/info.yaml" "$HERE/src" "$HERE/docs" "$WORK/"
cp "$HERE/../hw/rpm_top.v" "$WORK/src/tt_um_relwire.v"
cp "$HERE/../hw/rpm_core.v" "$WORK/src/rpm_core.v"
ln -sfn "$TT_HOME/tools" "$WORK/tt"
cd "$WORK"
git init -q && git add -A && git -c user.email=local@local -c user.name=local commit -qm local
git remote add origin https://github.com/lblommesteyn/relwire.git
. "$TT_HOME/venv/bin/activate"
./tt/tt_tool.py --create-user-config $TT_ARGS
./tt/tt_tool.py --harden $TT_ARGS
./tt/tt_tool.py --print-stats $TT_ARGS || true
