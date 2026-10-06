#!/usr/bin/env bash
# Assemble the Tiny Tapeout submission repository (tt-relwire) from this
# project: TT expects info.yaml, src/, docs/, test/ and the workflows at the
# repository root. Test vectors come from the reference model.
# Usage: export_tt_repo.sh DEST   (run where `dune` works)
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
DEST=$1
mkdir -p "$DEST/src" "$DEST/test" "$DEST/docs" "$DEST/.github/workflows"
cp "$HERE/info.yaml" "$DEST/"
cp "$HERE/docs/info.md" "$DEST/docs/"
cp "$HERE/src/config.json" "$DEST/src/"
cp "$ROOT/hw/rpm_top.v" "$DEST/src/tt_um_relwire.v"
cp "$ROOT/hw/rpm_core.v" "$DEST/src/rpm_core.v"
cp "$HERE/test/tb.v" "$HERE/test/test.py" "$HERE/test/Makefile" "$HERE/test/requirements.txt" "$DEST/test/"
cp "$HERE/workflows/"*.yaml "$DEST/.github/workflows/"
cp "$ROOT/LICENSE" "$DEST/"
(cd "$ROOT" && dune exec ./tools/gen_tt_test.exe "$DEST/test" >/dev/null)
rm -f "$DEST/test/cfg.hex"
cat > "$DEST/.gitignore" <<'GI'
runs/
tt_submission/
test/sim_build/
test/results.xml
test/*.fst
test/__pycache__/
test/gate_level_netlist.v
GI
cat > "$DEST/README.md" <<'MD'
# RelWire protocol emulator (Tiny Tapeout, IHP CMOS5L)

Tiny Tapeout submission for the Jane Street protocol-emulator ASIC competition.
Four cores run one relational protocol program; a role mask per core makes the
same binary a controller, a target or a sniffer.

This repository is generated from the main project,
[lblommesteyn/relwire](https://github.com/lblommesteyn/relwire), by
`tapeout/export_tt_repo.sh`: the language, reference model, compiler, timing
certificates and the full test suites live there. See [docs/info.md](docs/info.md)
for how the chip works and how to test it.
MD
echo "exported to $DEST"
