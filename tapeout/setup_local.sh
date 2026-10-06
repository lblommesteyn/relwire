#!/usr/bin/env bash
# Local mirror of TinyTapeout/tt-gds-action@ihp-cmos5l (WSL Ubuntu-22.04, root,
# Docker running). Installs the PDK, the TT tools and LibreLane under $TT_HOME.
set -euo pipefail
TT_HOME=${TT_HOME:-/root/tt}
PDK_ROOT=$TT_HOME/pdk
IHP_PDK_REV="2bbec755dc67ca3db0261c3d6163e15735d66710"   # same pin as CI
LIBRELANE_VERSION="3.1.0.dev3"
mkdir -p "$TT_HOME"
if [ ! -d "$PDK_ROOT/ihp-sg13cmos5l" ]; then
  mkdir -p "$PDK_ROOT"
  git -C "$PDK_ROOT" init -q
  git -C "$PDK_ROOT" fetch -q --depth 1 https://github.com/IHP-GmbH/IHP-Open-PDK.git "$IHP_PDK_REV"
  git -C "$PDK_ROOT" checkout -q FETCH_HEAD
  echo "IHP-Open-PDK $IHP_PDK_REV" > "$PDK_ROOT/ihp-sg13cmos5l/SOURCES"
fi
[ -d "$TT_HOME/tools" ] || git clone -q --depth 1 -b ihp-sg13cmos5l https://github.com/TinyTapeout/tt-support-tools.git "$TT_HOME/tools"
[ -d "$TT_HOME/venv" ] || python3.11 -m venv "$TT_HOME/venv"
"$TT_HOME/venv/bin/pip" install -q --upgrade pip
"$TT_HOME/venv/bin/pip" install -q -r "$TT_HOME/tools/requirements.txt"
"$TT_HOME/venv/bin/pip" install -q "librelane==$LIBRELANE_VERSION"
echo SETUP_OK
