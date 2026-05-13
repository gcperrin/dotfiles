#!/usr/bin/env bash
# Run the Neovim configuration test suite via plenary-busted.
#
# Requires:
#   - nvim (>= 0.10 — the suite uses vim.lsp.get_clients)
#   - plenary.nvim installed under $XDG_DATA_HOME/nvim/lazy/plenary.nvim
#     (run nvim once with the full config to let lazy.nvim install plugins).

set -euo pipefail

# Resolve the nvim config directory (parent of this script's dir).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NVIM_DIR="$(dirname "$SCRIPT_DIR")"

PLENARY_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/lazy/plenary.nvim"
if [ ! -d "$PLENARY_PATH" ]; then
  echo "ERROR: plenary.nvim not found at $PLENARY_PATH"
  echo "Run \`nvim\` once with the full config to let lazy.nvim install plugins."
  exit 2
fi

cd "$NVIM_DIR"

# PlenaryBustedDirectory exits with a non-zero shell error on failure.
nvim --headless --noplugin -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ { minimal_init = 'tests/minimal_init.lua', sequential = true }"
