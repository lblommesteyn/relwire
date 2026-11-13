#!/usr/bin/env bash
# Local mirror of TinyTapeout/tt-gds-action@ttihp27a, the action CI uses
# (Linux, Python 3.11, Docker running). Installs the TT tools and LibreLane
# under $TT_HOME. The PDK is not installed here: LibreLane fetches the
# IHP-Open-PDK revision it is pinned to (through ciel) on the first harden,
# into $PDK_ROOT, exactly as in CI.
set -euo pipefail
TT_HOME=${TT_HOME:-$HOME/tt}
LIBRELANE_VERSION="3.1.0.dev4"   # the action's default for ihp-sg13cmos5l
TOOLS_REF=${TOOLS_REF:-main}      # the action's default tools-ref
mkdir -p "$TT_HOME/pdk"
[ -d "$TT_HOME/tools" ] || git clone -q --depth 1 -b "$TOOLS_REF" https://github.com/TinyTapeout/tt-support-tools.git "$TT_HOME/tools"
[ -d "$TT_HOME/venv" ] || python3.11 -m venv "$TT_HOME/venv"
"$TT_HOME/venv/bin/pip" install -q --upgrade pip
"$TT_HOME/venv/bin/pip" install -q -r "$TT_HOME/tools/requirements.txt"
"$TT_HOME/venv/bin/pip" install -q "librelane==$LIBRELANE_VERSION"
echo SETUP_OK
