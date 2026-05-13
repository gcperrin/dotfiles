# Acceptance Criteria — AUDIT.md §3 Critical Fixes

This document defines what "done" means for the four critical bugs and two
high-severity follow-ups identified in [AUDIT.md](AUDIT.md) §3 and §13. Each
fix has:

1. **What changed** — the user-visible behavioral delta.
2. **Acceptance criteria** — observable properties that must hold.
3. **Automated test** — the spec that proves the criterion (path + test name).
4. **Manual verification** — quick interactive smoke check.

---

## Running the test suite

```bash
# from anywhere
~/.dotfiles/nvim/tests/run.sh

# or from the nvim dir
./tests/run.sh
```

Requirements:

- `nvim` ≥ 0.10 on `$PATH` (the suite uses `vim.lsp.get_clients`, added in 0.10).
- `plenary.nvim` installed under `$XDG_DATA_HOME/nvim/lazy/plenary.nvim`. If
  you've launched nvim once with the full config, lazy.nvim will have done
  this for you.

The runner exits with a non-zero status on any test failure, suitable for CI.

**Current state:** 70 / 70 tests passing (15 regression, 45 utils.maps, 10 lsp.keymaps).

---

## Summary table

| # | Fix | Files touched | Tests | Status |
|---|-----|---------------|-------|--------|
| A | `<leader>m` un-wrap | [lua/lsp/keymaps.lua](lua/lsp/keymaps.lua) | regression_spec.lua | ✅ |
| B | `utils.maps` `*noremap` helpers + group leak | [lua/utils/maps.lua](lua/utils/maps.lua) | maps_spec.lua, regression_spec.lua | ✅ |
| C | `get_active_clients` → `get_clients` (TS) | [lua/config/typescript.lua](lua/config/typescript.lua) | regression_spec.lua | ✅ |
| D | `get_active_clients` → `get_clients` (utils) | [lua/utils/init.lua](lua/utils/init.lua) | regression_spec.lua | ✅ |
| E | `nvim_buf_set_option` → `vim.bo[]` | [lua/config/solidity.lua](lua/config/solidity.lua) | regression_spec.lua | ✅ |
| F | Drop `tsserver` double-mount + dead `TypescriptRenameFile` | [lua/config/typescript.lua](lua/config/typescript.lua) | regression_spec.lua | ✅ |

---

## Fix A — `<leader>m` double-wrap in `lsp/keymaps.lua`

### What changed

```diff
- self:map("<leader>m", "<cmd>Telescope<CR>", { desc = "Telescope" })
+ self:map("<leader>m", "Telescope", { desc = "Telescope" })
```

Also removed three lines of dead commented-out `:map` calls above it.

### Acceptance criteria

| AC-A.1 | The `:map` wrapper still produces `<cmd>Telescope<cr>` as the underlying keymap rhs. |
| AC-A.2 | No call site in `lua/lsp/keymaps.lua` (non-comment lines) passes an already-`<cmd>`-wrapped string to `:map`. |
| AC-A.3 | When an LSP attaches to a buffer, `<leader>m` in that buffer opens `:Telescope`. |

### Automated tests

- `lsp.keymaps :map wrapper wraps a plain string rhs in <cmd>...<cr>` — `tests/lsp/keymaps_spec.lua`
- `regression: ... Fix A — <leader>m double-wrap ... does not pass an already-<cmd>-wrapped rhs to :map` — `tests/regression_spec.lua`
- `regression: ... Fix A ... the :map call still binds <leader>m to 'Telescope'` — `tests/regression_spec.lua`

### Manual verification

1. Open any source file with a configured LSP (e.g. a Python or Go file).
2. Wait for the LSP to attach (`:LspInfo` to confirm).
3. Press `<Space>m`.
4. The Telescope picker UI should open.
   - Before the fix, `<Space>m` did nothing (the keymap was bound to the invalid string `<cmd><cmd>Telescope<CR><cr>`).

---

## Fix B — `utils/maps.lua` `*noremap` helpers + group/mode_group leak

### What changed

- All `*noremap` helpers (`nnoremap`, `inoremap`, `vnoremap`, `cnoremap`,
  `tnoremap`, `snoremap`) now actually set `noremap = true` on the opts
  passed to `vim.keymap.set`. The previous implementation discarded
  `vim.tbl_deep_extend`'s return value, so the helpers were silent no-ops.
- `group(opts, maps)` and `mode_group(mode, maps, opts)` no longer mutate
  the caller's shared `opts` table, and no longer leak per-entry overrides
  forward across loop iterations.
- The internal `local map = vim.keymap.set` aliasing was dropped — call
  sites now use `vim.keymap.set` directly so tests can mock the function
  via the global table.

### Acceptance criteria

| AC-B.1 | Calling `maps.nnoremap("x", ":echo<cr>")` results in `vim.keymap.set` being called with `opts.noremap == true`. (Repeat for `inoremap`/`vnoremap`/`cnoremap`/`tnoremap`/`snoremap`.) |
| AC-B.2 | `maps.nnoremap("x", ":echo<cr>", user_opts)` does **not** mutate `user_opts` — caller-side `user_opts.noremap` remains `nil`. |
| AC-B.3 | `maps.group(shared, { { "n", { "a", "rhs1", { silent = true } }, { "b", "rhs2" } } })` calls set("n","a", ..., {…, silent=true}) for the first entry and set("n","b", ..., {…, silent=nil}) for the second. The `silent=true` does **not** propagate. |
| AC-B.4 | `maps.mode_group("i", { … })` likewise does not propagate per-entry overrides. |
| AC-B.5 | Both `group` and `mode_group` accept a `nil` opts arg without error. |
| AC-B.6 | All twelve helpers + `group` + `mode_group` exist and are functions on the returned module. |

### Automated tests

- `tests/utils/maps_spec.lua` — 45 tests across 14 `describe` blocks. Each
  `*noremap` helper has four tests (nil opts, opts merge, no mutation,
  function rhs); each `*map` helper has two tests (no force-noremap, mode
  + lhs pass-through); `group` has five tests including the per-entry
  regression; `mode_group` has four.

- `tests/regression_spec.lua`:
  - `Fix B ... does not contain the buggy discarded-return pattern` — static
    inspection of `lua/utils/maps.lua` confirms every `vim.tbl_deep_extend`
    call assigns or returns the result.
  - `Fix B ... exposes all expected helpers`.

### Manual verification

In any buffer:

```vim
:lua local m = require('utils.maps') m.nnoremap('<F5>', ':echo "hi"<cr>')
:verbose nmap <F5>
```

The `:verbose nmap` output should include `n  <F5>` followed by the
mapping, with no `*` flag (the `*` would indicate non-`noremap`).

---

## Fix C / D — `vim.lsp.get_active_clients` is deprecated

### What changed

```diff
- vim.lsp.get_active_clients({ bufnr = event.buf, name = "eslint" })
+ vim.lsp.get_clients({ bufnr = event.buf, name = "eslint" })
```

Applied in:
- [lua/config/typescript.lua](lua/config/typescript.lua) (eslint BufWritePre autocmd).
- [lua/utils/init.lua](lua/utils/init.lua) (the `get_root` helper that walks active LSP clients to detect a project root).

### Acceptance criteria

| AC-CD.1 | Neither `lua/config/typescript.lua` nor `lua/utils/init.lua` contains the literal token `get_active_clients`. |
| AC-CD.2 | On `nvim ≥ 0.12`, opening a TypeScript file does not log `vim.lsp.get_active_clients is deprecated` (the API was removed). |
| AC-CD.3 | `:lua print(require("utils").get_root())` returns a non-empty string in any project directory. |

### Automated tests

- `tests/regression_spec.lua` — two tests, one per file, asserting the
  literal token is absent.

### Manual verification

1. Open a `.ts` file. `:checkhealth lspconfig` should not warn about
   deprecated API calls from your config.
2. From any buffer:
   ```vim
   :lua print(require('utils').get_root())
   ```
   Should print a path (project root or cwd) with no error.

---

## Fix E — `nvim_buf_set_option` is deprecated

### What changed

```diff
- vim.api.nvim_buf_set_option(bufnr, 'tabstop', 2)
- vim.api.nvim_buf_set_option(bufnr, 'shiftwidth', 2)
+ vim.bo[bufnr].tabstop = 2
+ vim.bo[bufnr].shiftwidth = 2
```

In [lua/config/solidity.lua](lua/config/solidity.lua), inside the
`solidity_ls_nomicfoundation` `on_attach`.

### Acceptance criteria

| AC-E.1 | `lua/config/solidity.lua` does not contain `nvim_buf_set_option`. |
| AC-E.2 | When `solidity_ls_nomicfoundation` attaches to a `.sol` buffer, that buffer's `tabstop` and `shiftwidth` are both `2`. |

### Automated tests

- `tests/regression_spec.lua` — `Fix E ... does not call nvim_buf_set_option`.

### Manual verification

1. Open a `.sol` file (any Solidity contract).
2. Wait for `solidity_ls_nomicfoundation` to attach (`:LspInfo`).
3. ```vim
   :echo &l:tabstop &l:shiftwidth
   ```
   Should print `2  2`.

---

## Fix F — `tsserver` and `typescript-tools` double-mount

### What changed

- Removed the entire `servers.tsserver = { … }` entry and the matching
  `setup.tsserver = function …` from the lspconfig spec in
  [lua/config/typescript.lua](lua/config/typescript.lua).
- Removed the dead `<leader>lR <cmd>TypescriptRenameFile<CR>` keymap (the
  command never existed; it was a leftover from the unrelated, archived
  `jose-elias-alvarez/typescript.nvim` plugin).
- Reworked the `typescript-tools.nvim` `config` function so all of its
  keymaps live in one place, with a `client.name ~= "typescript-tools"`
  guard to skip non-TS clients.
- Kept the `eslint` server config in lspconfig — it's an orthogonal linter
  and not duplicated by typescript-tools.

### Acceptance criteria

| AC-F.1 | `lua/config/typescript.lua` does not contain the literal token `tsserver`. |
| AC-F.2 | `lua/config/typescript.lua` does not contain the literal token `ts_ls` either. |
| AC-F.3 | `lua/config/typescript.lua` does not contain `TypescriptRenameFile`. |
| AC-F.4 | `typescript-tools` is still configured (the literal `typescript-tools` appears). |
| AC-F.5 | Opening a `.ts` file launches `typescript-tools` (visible in `:LspInfo`). Exactly one TS LSP attaches — not two. |
| AC-F.6 | The keymaps `<leader>lo` / `<leader>lO` / `<leader>lu` / `<leader>lz` / `<leader>lR` / `<leader>lF` / `<leader>lA` are bound and trigger their respective `TSTools*` commands. |

### Automated tests

- `tests/regression_spec.lua` — three tests under `Fix F`:
  - `keeps typescript-tools configured`
  - `does not also configure tsserver/ts_ls via lspconfig`
  - `removes the dead TypescriptRenameFile keymap`

### Manual verification

1. Open a `.ts` file in a project with `node_modules/typescript`.
2. `:LspInfo` should show exactly one LSP attached named
   `typescript-tools` (or `tsserver` reported by the underlying TS server,
   but only one instance).
3. In normal mode, `<Space>lo` should run `:TSToolsOrganizeImports` and
   you should see imports reordered.
4. `<Space>lR` (formerly bound to the non-existent
   `TypescriptRenameFile`) now runs `:TSToolsRemoveUnusedImports`.

---

## Pre-merge checklist

Before merging the fix branch into master:

- [ ] `tests/run.sh` exits 0 locally.
- [ ] `nvim +Lazy` shows no errors at startup.
- [ ] `:checkhealth lspconfig` is clean (or warnings are pre-existing).
- [ ] Open a Python file → `<leader>m` opens Telescope.
- [ ] Open a `.ts` file → `:LspInfo` shows one TS LSP, not two.
- [ ] Open a `.sol` file → `&l:tabstop` is 2.
- [ ] `:Lazy profile` startup time has not regressed (compare against the
      pre-fix log if one was captured).

---

## What this round does **not** cover

The following items from AUDIT.md remain open. They were *not* in scope for
this fix branch:

- 🟠 §4.1 — Bash LSP race / orphan `lua/config/bash.lua`.
- 🟠 §4.2 — Consolidating per-language `on_attach` into a single dispatch table.
- 🟠 §4.3 — `lualine` event-trigger.
- 🟠 §4.4 — Extracting the inline `nvim-cmp` setup.
- 🟠 §4.5 — `change_detection.notify` / `checker` mismatch.
- 🟠 §4.6 — `dev.patterns = { "alpha2phi" }` leftover.
- 🟠 §4.7 — Bash LSP bypasses on_attach pipeline.
- 🟠 §4.8 — `gitsigns.nvim` unconfigured.
- 🟡 §5.1 — `require("config.X")` → `import` migration.
- 🟡 §5.2 — `which-key` v1 → v3 migration.
- All §7 modernization additions (`blink.cmp`, `conform.nvim`, `nvim-dap-ui`,
  `gitsigns.nvim` config, `mini.nvim`, etc.).

Each can be tackled in its own PR with its own acceptance criteria.
