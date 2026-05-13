# Neovim Configuration Audit

**Reviewer:** Senior Engineer (audit pass)
**Date:** 2026-05-12
**Target:** `~/.dotfiles/nvim/` (commit `50e1630`, branch `claude/nervous-jones-952f60`)
**Scope:** Full configuration — bootstrap, plugin specs, LSP layer, language modules, utility libraries, lockfile.

---

## 0. TL;DR — Executive Summary

The configuration is a mid-size, lazy.nvim-based Neovim setup with reasonable bones but accumulated drift. It is **functional but leaks performance and harbors at least one always-broken keymap, several silently-broken helper functions, dead code, deprecated API calls, and a misused plugin-spec idiom**. There are also clear modernization opportunities (formatter pipeline, deprecated LSP server names, the cmp → blink.cmp ecosystem).

**Severity tally:**

| Severity   | Count | Example                                                                 |
| ---------- | ----- | ----------------------------------------------------------------------- |
| 🔴 Critical | 4     | `<leader>m` double-`<cmd>` wrap; `vim.tbl_deep_extend` mis-mutation     |
| 🟠 High     | 8     | Deprecated `vim.lsp.get_active_clients`; `tsserver` rename; LSP race    |
| 🟡 Medium   | 14    | `require("config.X)` inside spec; orphan `bash.lua`; vimtex no-op       |
| 🔵 Low      | 11    | Commented-out telescope keymaps; stale README TODOs; quote inconsistency |

If you only have an hour, fix the four 🔴 items in §3, then prune dead code in §6. If you have an afternoon, do §3, §4, §7 (consolidation), and apply the modernization roadmap in §10.

---

## 1. Repository Layout

```
nvim/
├── init.lua                # bootstrap, options, leader maps, lazy setup
├── lazy-lock.json          # plugin commit pins (49 plugins)
├── README.md               # 14-line stub w/ stale TODO list
└── lua/
    ├── plugins/
    │   └── init.lua        # 433 lines — main plugin specs + cmp + treesitter + neotree...
    ├── config/             # per-language / per-feature plugin "modules"
    │   ├── ai.lua          # claude-code.nvim + (disabled) copilot
    │   ├── bash.lua        # ORPHAN — never required
    │   ├── go.lua
    │   ├── html.lua
    │   ├── picker.lua      # telescope live_grep custom actions (set_extension/set_folders)
    │   ├── proto.lua
    │   ├── python.lua
    │   ├── solidity.lua
    │   ├── telescope.lua   # 307 lines, ~15 telescope deps
    │   ├── typescript.lua
    │   └── vimtex.lua      # entire file commented; `return {}` — no-op
    ├── lsp/
    │   ├── keymaps.lua     # on_attach handler + map wrapper (HAS A BUG, see §3.1)
    │   ├── servers.lua     # generic setup() that drives mason-lspconfig
    │   └── utils.lua       # capabilities() + LspAttach autocmd helper + diag toggle
    └── utils/
        ├── init.lua        # has(), get_root(), telescope(), open_term()
        ├── icons.lua       # Nerd-font icon dictionary
        └── maps.lua        # keymap helpers — SEVERAL SILENTLY BROKEN (see §3.2)
```

### Strengths

- Clear separation of LSP infra (`lua/lsp`), language modules (`lua/config`), shared utilities (`lua/utils`), and plugin specs (`lua/plugins`).
- Lazy.nvim-native plugin spec spreading per filetype (`opts = function(_, opts) vim.list_extend(...) end`) — idiomatic and clean.
- Mason → mason-lspconfig → lspconfig pipeline is wired correctly enough.
- Treesitter ensure_installed is consolidated.
- A real `on_attach` factory pattern in `lsp/utils.lua` rather than copy/pasting per server.

### Weaknesses

- **No `plugins/lang/<filetype>.lua` directory convention** — language modules sit under `config/` and are pulled in via `require()` rather than lazy's preferred `{ import = ... }`. This eagerly evaluates every language spec at startup.
- `config/` mixes telescope + AI + per-language LSP + a stub picker module. Three unrelated concerns living under one folder name.
- Two parallel `on_attach` implementations: `lsp/utils.on_attach(cb)` (autocmd registrar) and `lsp/keymaps.on_attach(client, buf)` (key setter). The naming collision is confusing — they aren't variants of the same function, but the names suggest they are.
- README is essentially empty. There is no documented mental model for what is bound where.

---

## 2. Plugin Inventory (from `lazy-lock.json`)

49 plugins. Below, grouped by concern, with **load behavior** and **utilization assessment**:

### Core / Bootstrap (3)

| Plugin       | Loading      | Notes                                           |
| ------------ | ------------ | ----------------------------------------------- |
| lazy.nvim    | bootstrap    | Stable branch — fine.                           |
| plenary.nvim | transitive   | Required by telescope/claude-code/etc.          |
| nui.nvim     | transitive   | Only used by neo-tree.                          |

### UI / Look (5)

| Plugin             | Loading                  | Issue                                                                 |
| ------------------ | ------------------------ | --------------------------------------------------------------------- |
| github-nvim-theme  | `lazy=false, priority=1000` | Fine.                                                              |
| lualine.nvim       | startup (`config = fn`)  | **No `event`/`lazy`** — loads eagerly. Sections are mostly empty.    |
| barbar.nvim        | `event = "VeryLazy"`     | Pinned `^1.0.0` — fine. Could be `cmd = "Buffer*"`.                  |
| nvim-web-devicons  | transitive               | Fine.                                                                 |
| fidget.nvim        | dep of lspconfig         | `config = true` — uses defaults, not customized.                     |

### Editing / Movement (7)

| Plugin                     | Loading                | Issue                                                              |
| -------------------------- | ---------------------- | ------------------------------------------------------------------ |
| nvim-treesitter            | `BufReadPost/BufNewFile` | Good. `ensure_installed` deduplicated cleanly.                   |
| nvim-treesitter-textobjects | dep                    | Used (`af/if/ac/ic/aa/ia`).                                       |
| nvim-ts-context-commentstring | `lazy = true`        | Wired into Comment.nvim — fine.                                   |
| Comment.nvim               | `keys = {gc, gcc, gbc}` | Good.                                                            |
| nvim-surround              | `event = "VeryLazy"`   | Fine. Could be `keys = {"ys","cs","ds"}` for tighter laziness.   |
| nvim-autopairs             | `event = "InsertEnter"` | Fine, but no cmp integration (`on_confirm_done`) — missing.       |
| vim-matchup                | `event = "BufReadPost"` | Fine.                                                            |
| vim-abolish                | **eager**              | No `event`/`cmd` → loads at startup. Should be `cmd = {"Abolish","Subvert","S"}`. |

### Telescope ecosystem (16)

| Plugin                       | Used?                                                                |
| ---------------------------- | -------------------------------------------------------------------- |
| telescope.nvim               | yes (`<leader>ff`, `<leader>su`, `<leader>b`)                        |
| telescope-fzf-native.nvim    | yes (`load_extension "fzf"`)                                         |
| telescope-file-browser.nvim  | yes (custom `<A-f>` action)                                          |
| telescope-project.nvim       | partial — extension loaded but no keymap exposes it                  |
| project.nvim                 | yes (`projects` extension + autosession root detection)              |
| telescope-repo.nvim          | **dead** — all keymaps commented out                                 |
| aerial.nvim                  | extension loaded; **no keymap** binds it                             |
| telescope-frecency.nvim      | **dead** — keymap commented out                                      |
| sqlite.lua                   | required by frecency/neocomposer; frecency is dead → reconsider     |
| advanced-git-search.nvim     | **dead** — never used (and no extension loaded)                      |
| telescope-luasnip.nvim       | **dead** — keymap commented out                                      |
| telescope-cc.nvim            | extension loaded (`conventional_commits`); **no keymap**             |
| telescope-lazy.nvim          | extension loaded; **no keymap** (commented out)                      |
| scope.nvim                   | extension loaded; only useful if you keep tabs+barbar separately     |
| telescope-zoxide             | extension loaded; **no keymap** (commented out)                      |
| telescope-live-grep-args     | extension loaded; **no keymap** (`<leader>fg` commented out)         |

**Bottom line: ~9 of 16 telescope-related plugins are vestigial.** They add startup work, lock churn, and noise. See §6.

### LSP / Completion (8)

| Plugin                 | Notes                                                            |
| ---------------------- | ---------------------------------------------------------------- |
| nvim-lspconfig         | wired via `lua/lsp/servers.lua`                                 |
| mason.nvim             | wired                                                             |
| mason-lspconfig.nvim   | wired (passes ensure_installed)                                 |
| nvim-cmp               | configured in `plugins/init.lua` inline (130+ lines)            |
| cmp-nvim-lsp           | dep                                                              |
| cmp-nvim-lsp-signature-help | dep                                                         |
| cmp-buffer / cmp-path  | dep                                                              |
| cmp_luasnip            | dep                                                              |
| LuaSnip                | configured                                                       |
| friendly-snippets      | loaded via vscode-snippets loader                                |
| typescript-tools.nvim  | configured **+** `tsserver` is also configured in lspconfig — **double mount, see §3.4** |

### Languages (4)

| Plugin    | Notes                                                                          |
| --------- | ------------------------------------------------------------------------------ |
| go.nvim   | configured `ft={go,gomod}` + sync binary install on `build`                    |
| guihua.lua | dep of go.nvim                                                                 |
| vimtex    | `ft = {tex, bib}` — config-fn requires `config/vimtex.lua` which is **a no-op** |
| nvim-dap  | configured for go + python; no UI plugin → poor debug UX                      |

### Other (6)

| Plugin            | Notes                                                                |
| ----------------- | -------------------------------------------------------------------- |
| neo-tree.nvim     | configured                                                           |
| which-key.nvim    | configured **3 times** in 3 files with **mixed v1/v3 API** — see §5 |
| gitsigns.nvim     | declared only as a dep of barbar; **never configured**, no keymaps   |
| claude-code.nvim  | configured                                                           |
| copilot.lua       | `enabled = false` — dead spec                                        |
| nvim-dap-go       | dep, fine                                                            |
| nvim-dap-python   | configured                                                           |

---

## 3. 🔴 Critical Bugs (must fix)

### 3.1 `lsp/keymaps.lua:9` — `<leader>m` is double-wrapped in `<cmd>...<cr>`

[lua/lsp/keymaps.lua:42-54](nvim/lua/lsp/keymaps.lua:42) defines:

```lua
function M:map(lhs, rhs, opts)
  ...
  vim.keymap.set(
    opts.mode or "n",
    lhs,
    type(rhs) == "string" and ("<cmd>%s<cr>"):format(rhs) or rhs,
    ...
  )
end
```

The helper auto-wraps any string `rhs` in `<cmd>…<cr>`. But line 9 passes an *already-wrapped* string:

```lua
self:map("<leader>m", "<cmd>Telescope<CR>", { desc = "Telescope" })
```

This expands to `<cmd><cmd>Telescope<CR><cr>`, which is invalid and silently breaks `<leader>m`. The line directly above it (`gr`, `gI`, `gb`) follows the *correct* unwrapped convention.

**Fix:**
```lua
self:map("<leader>m", "Telescope", { desc = "Telescope" })
```

### 3.2 `utils/maps.lua` — every `*noremap` helper silently no-ops `noremap=true`

[lua/utils/maps.lua:47-55](nvim/lua/utils/maps.lua:47):

```lua
function M.nnoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })   -- ❌ return value discarded
  else
    opts = {}
  end
  map('n', key, cmd, opts)
end
```

`vim.tbl_deep_extend` **returns a new table** — it does not mutate `opts`. So `opts` is unchanged and `noremap=true` is never applied. The same bug exists in `inoremap`, `vnoremap`, `cnoremap`, `tnoremap`, `snoremap`, and **also in the higher-level `mode_group`/`group` functions** at lines 11-15 and 23-33.

In practice this is mostly harmless on modern Neovim because `vim.keymap.set` defaults to `noremap = true` already — but the helper does not deliver what its name promises, which means any future caller passing a *recursive* mapping (i.e. wanting `noremap=false`) will get behavior they didn't ask for, and any caller relying on this helper to "force" noremap is being lied to.

**Fix:**
```lua
function M.nnoremap(key, cmd, opts)
  opts = vim.tbl_deep_extend('force', opts or {}, { noremap = true })
  map('n', key, cmd, opts)
end
```

The `group()` and `mode_group()` functions have the same pattern at lines 12-14, 24-26, 31-33. They also mutate a shared `opts` table across loop iterations, so per-entry option overrides "leak" forward across the iteration. Rewrite to clone per-iteration:

```lua
function M.mode_group(mode, maps, opts)
  for _, v in ipairs(maps) do
    local local_opts = vim.tbl_deep_extend('force', opts or {}, v[3] or {})
    map(mode, v[1], v[2], local_opts)
  end
end
```

### 3.3 Deprecated `vim.lsp.get_active_clients` (Neovim 0.10+ removed in 0.12)

Two call sites:

- [lua/config/typescript.lua:84](nvim/lua/config/typescript.lua:84):
  ```lua
  local client = vim.lsp.get_active_clients({ bufnr = event.buf, name = "eslint" })[1]
  ```
- [lua/utils/init.lua:16](nvim/lua/utils/init.lua:16):
  ```lua
  for _, client in pairs(vim.lsp.get_active_clients { bufnr = 0 }) do
  ```

`vim.lsp.get_active_clients` was deprecated in 0.10 and removed in 0.12. Use `vim.lsp.get_clients`. Also `nvim_buf_set_option` (used in [solidity.lua:40-41](nvim/lua/config/solidity.lua:40)) is deprecated; use `vim.bo[bufnr].tabstop = 2` or `vim.api.nvim_set_option_value("tabstop", 2, {buf=bufnr})`.

### 3.4 `tsserver` is named `ts_ls` in modern nvim-lspconfig (+ double-mounted with typescript-tools)

[lua/config/typescript.lua:39](nvim/lua/config/typescript.lua:39) configures `tsserver`, but as of nvim-lspconfig commits in mid-2024 the entry was renamed to `ts_ls`. On a current lspconfig this server will silently fail to attach (you'll see a warning from your own `servers.lua:71` — *"LSP server 'tsserver' not found in lspconfig"*).

Worse, **the file also installs `typescript-tools.nvim`**, which is itself a tsserver alternative — they shouldn't both manage TypeScript. Pick one:

- **Recommended:** keep `typescript-tools.nvim` (faster, no Node JSON-RPC overhead), and **remove the `tsserver`/`ts_ls` lspconfig entry entirely**. Delete the `setup.tsserver` function too.
- Or: drop `typescript-tools.nvim` and use `ts_ls` in lspconfig with proper inlay-hint settings.

Either way, you cannot keep both without one shadowing the other and producing duplicate code actions.

Additionally: [typescript.lua:74](nvim/lua/config/typescript.lua:74) maps `<leader>lR` to `<cmd>TypescriptRenameFile<CR>`. That command **does not exist** in either lspconfig or typescript-tools.nvim (it was a `jose-elias-alvarez/typescript.nvim` command, an unrelated archived plugin). Dead binding.

---

## 4. 🟠 High-Priority Issues

### 4.1 Bash LSP race condition / double-start

[init.lua:157-165](nvim/init.lua:157) creates an autocmd that starts `bash-language-server` on `FileType sh`. There is **also** [lua/config/bash.lua](nvim/lua/config/bash.lua) that defines the same autocmd inside a `return { ... }`:

```lua
return {
  vim.api.nvim_create_autocmd('FileType', { ... })
}
```

This is broken in two ways:
1. The `return { vim.api.nvim_create_autocmd(...) }` *evaluates the autocmd creation when the module loads* and stores the autocmd ID in the table — it does not return a lazy.nvim plugin spec.
2. The module is **not** required anywhere in `plugins/init.lua`, so it's dead code today. But if you ever wire it in, you'll get a duplicate `bash-language-server` started per buffer.

**Fix:** Delete `lua/config/bash.lua`. Either keep the `init.lua` block or replace with mason-installed `bash-language-server` configured through `lsp/servers.lua` like the other LSPs.

### 4.2 LSP `on_attach` is registered as a **global** `LspAttach` autocmd — not buffer-scoped

[lua/lsp/utils.lua:12-19](nvim/lua/lsp/utils.lua:12):

```lua
function M.on_attach(on_attach)
  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
      local bufnr = args.buf
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      on_attach(client, bufnr)
    end,
  })
end
```

Every call to `lsp_utils.on_attach(fn)` creates a *new global autocmd*. Look at the call sites:

- `lsp/servers.lua:39` — registers the generic keymap handler
- `config/python.lua:46` — registers a pyright-specific handler
- `config/typescript.lua:70` — registers a tsserver-specific handler
- `config/typescript.lua:12` — registers another tsserver-specific handler (typescript-tools)
- `config/go.lua:61` — registers a gopls-specific handler
- `config/solidity.lua:31` — registers a solidity-specific handler
- `config/proto.lua:11` — registers a buf_ls-specific handler

That's **7 separate global `LspAttach` autocmds**. Every time *any* LSP attaches to *any* buffer, all 7 fire. Each callback re-checks `client.name == "<this>"` and exits — so it's not *incorrect* — but it is wasteful and makes the dispatch O(N) in number of servers.

**Fix:** Either (a) add an `augroup` with `clear=true` and a stable name so re-sourcing doesn't multiply them, or (b) consolidate all the per-server keymap blocks into a single `on_attach` dispatch table keyed by client name. The second is cleaner and is what most modern starters do.

Sketch:
```lua
-- lsp/keymaps.lua
local handlers = {
  pyright = function(c, b) ... end,
  gopls   = function(c, b) ... end,
  ...
}
function M.on_attach(client, bufnr)
  -- common maps
  ...
  local h = handlers[client.name]
  if h then h(client, bufnr) end
end
```

Then the per-language files just **add to `handlers`** rather than registering more autocmds.

### 4.3 `lualine` has no `event`, so it loads at startup

[plugins/init.lua:31-46](nvim/lua/plugins/init.lua:31) declares lualine with a `config` function but no `event`/`cmd`/`keys`. Because the top-level `defaults = { lazy = true }` is set in [init.lua:74](nvim/init.lua:74), lazy.nvim should still defer it — **but** the `config` function references `require("lualine").setup(...)` which forces the load. The result is that lualine ends up loading early on the first buffer event anyway. Make it explicit:

```lua
{
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  ...
}
```

Also, the current sections are essentially **empty** (`lualine_a = {}`, `lualine_y = {}`). You're paying lualine's startup cost for what amounts to "filename + encoding + fileformat + filetype." That's barely more than the default statusline. Either commit to a real statusline (mode indicator, branch, diagnostics, LSP) or drop lualine entirely and live with `set laststatus=3` + a one-line custom statusline.

### 4.4 `nvim-cmp` is configured inline in `plugins/init.lua` (130 lines)

[plugins/init.lua:234-348](nvim/lua/plugins/init.lua:234) is a wall of cmp setup that doesn't belong in the master plugin spec list. It buries the architecture. Move it to `lua/plugins/cmp.lua` or `lua/config/cmp.lua` and import.

While you're there: the `<C-j>` / `<C-k>` mapping in cmp also tries `c` (command) mode. `cmp.config.mapping.preset.insert{}` is the wrong wrapper for command-mode mappings — use `cmp.config.mapping.preset.cmdline()` and set up cmdline sources via `cmp.setup.cmdline(":", ...)`. As-is, the command-mode behavior is partly bypassed.

### 4.5 `change_detection.notify = false` + `checker.enabled = true`

[init.lua:77-80](nvim/init.lua:77) silently rebuilds the plugin graph on changes while *also* periodically pinging git for updates. The combination means you get update spam in `:Lazy` notifications even though you've muted change notifications. Either:

- `checker = { enabled = true, notify = false }` (silent update checks)
- Or `checker = { enabled = false }` if you don't want auto-checks at all.

The current state is the worst of both.

### 4.6 `dev.patterns = { "alpha2phi" }`

[init.lua:76](nvim/init.lua:76):
```lua
dev = { patterns = jit.os:find "Windows" and {} or { "alpha2phi" } },
```

This tells lazy.nvim to load any plugin authored by `alpha2phi` from `~/projects/alpha2phi/<plugin>` instead of from GitHub. You almost certainly **forked this from a `Modern-Neovim` starter** by user alpha2phi and never trimmed this leftover. Remove unless you're actively developing their plugins locally.

### 4.7 Bash autocmd does no `on_attach` / no keymaps / no capabilities

[init.lua:157-165](nvim/init.lua:157) starts the bash LSP raw via `vim.lsp.start({...})`. This bypasses your entire `lsp/utils.capabilities()` (cmp completion) and `lsp/keymaps.on_attach` pipeline. Result: in `.sh` files you get an LSP attached but no `K`/`gr`/`gd`/`<leader>la` etc.

Register bash via mason+lspconfig like every other language so it inherits the on-attach handler.

### 4.8 `gitsigns.nvim` listed only as a dep, never configured

[plugins/init.lua:413](nvim/lua/plugins/init.lua:413) loads gitsigns as a barbar dep but never calls its setup. You're paying the disk hit but getting no gutter signs, no `]c`/`[c` hunk nav, no `<leader>hp` preview, etc. Either configure it properly or drop it entirely.

---

## 5. 🟡 Medium Issues — Architecture & Maintenance

### 5.1 `{ require("config.X") }` is the wrong way to import lazy spec modules

[plugins/init.lua:388-409](nvim/lua/plugins/init.lua:388):

```lua
{
  require("config.python")    -- returns { {spec1}, {spec2}, ... }
},
{
  require("config.html")
},
...
```

Two problems:

1. **It evaluates eagerly.** `require()` is called at the moment `plugins/init.lua` is read, which is at every Neovim startup. Lazy.nvim never gets to defer the module load. Every language module's top-level code runs always.
2. **It bypasses lazy.nvim's import optimization.** The idiomatic equivalent is:
   ```lua
   { import = "config.python" },
   { import = "config.html" },
   ...
   ```
   Lazy stat-caches imports and skips re-evaluation across the bytecode cache.

The cleaner refactor is to rename `lua/config/<lang>.lua` → `lua/plugins/lang/<lang>.lua` and replace this whole block with a single:

```lua
-- in lazy setup
spec = { { import = "plugins" }, { import = "plugins.lang" } },
```

### 5.2 `which-key.nvim` configured three times with mismatched API versions

Three call sites:
- [plugins/init.lua:141-148](nvim/lua/plugins/init.lua:141) — uses **v1 API** (`defaults = { ["<leader>l"] = { name = "+LSP" } }`)
- [config/html.lua:40-47](nvim/lua/config/html.lua:40) — also **v1 API** (`defaults = { ["<leader>lz"] = { name = "+Color" } }`)
- [config/ai.lua:71-83](nvim/lua/config/ai.lua:71) — uses **v3 API** (`spec = { { "<leader>a", group = "AI" } }`)

Recent which-key (`3aab2147…` in your lock = v3) **stopped supporting the v1 syntax**. The first two entries are silently no-ops on your current version. Migrate everything to the v3 `spec = {...}` form.

### 5.3 `vimtex` config function loads an empty module

[plugins/init.lua:62-70](nvim/lua/plugins/init.lua:62):

```lua
{
  "lervag/vimtex",
  config = function() require("config.vimtex") end,
  ft = { "tex", "bib" },
},
```

[`config/vimtex.lua`](nvim/lua/config/vimtex.lua) is **`return {}` with 110 lines of commented-out vim globals**. The `config` function does nothing useful. Either:
- Delete the `config = function`, since vimtex doesn't need setup.
- Or actually configure it.

### 5.4 `lua/config/bash.lua` — orphan file

Confirmed via grep: no `require("config.bash")` anywhere in the codebase. The autocmd it tries to register would also be a duplicate of the one in `init.lua`. **Delete this file.**

### 5.5 Dead/inactive plugins still in lock and dependency tree

These contribute to startup work and lock-file churn with no payoff:

| Plugin                          | Reason                                                |
| ------------------------------- | ----------------------------------------------------- |
| advanced-git-search.nvim        | No keymap, no extension loaded                         |
| telescope-repo.nvim             | All keymaps commented out                              |
| telescope-frecency.nvim         | Keymap commented out (`<leader>fo`)                    |
| sqlite.lua                      | Only used by frecency (above) and disabled NeoComposer |
| telescope-luasnip.nvim          | Keymap commented out                                   |
| telescope-cc.nvim               | No keymap                                              |
| telescope-lazy.nvim             | Keymap commented out                                   |
| telescope-zoxide                | Keymap commented out                                   |
| telescope-live-grep-args.nvim   | Keymap commented out                                   |
| scope.nvim                      | Only useful if you split tabs/buffers — overkill here  |
| copilot.lua                     | `enabled = false`                                      |
| NeoComposer.nvim                | `enabled = false`                                      |

**Prune action:** delete or re-enable. ~12 fewer entries in `lazy-lock.json`, faster startup, less ongoing maintenance.

### 5.6 Telescope `pickers` configuration mixes `theme = "dropdown"` + `previewer = false` defaults

[config/telescope.lua:232-245](nvim/lua/config/telescope.lua:232) gives `find_files`, `git_files`, and `buffers` the `dropdown` theme without previewer. This is a strong UX choice — you've signed up for *no preview when finding files* — fine if intentional, but worth surfacing. Live grep keeps the default horizontal layout. **Inconsistency** between "I want previews to write code" (`live_grep`) and "I don't want previews to navigate" (`find_files`). I'd recommend either:

- Drop `dropdown` from pickers and use it via per-call themes (`telescope.themes.get_dropdown()` for one-offs).
- Or commit to it everywhere and skip previewer plumbing for live_grep too.

### 5.7 `telescope.load_extension` ordering / silent failure

`config/telescope.lua:269-283` calls `load_extension` for **15 extensions** without `pcall`. If any extension fails to load (e.g. after a partial install or version bump), the whole setup throws and the rest of your telescope config is unbound. Wrap in `pcall` or use lazy.nvim's `init = function() ... end` per-extension to make failures isolated.

### 5.8 LSP fall-through path in `lsp/servers.lua` calls `mlsp.setup_handlers` *after* `mlsp.setup`

[lsp/servers.lua:121-128](nvim/lua/lsp/servers.lua:121):

```lua
if have_mason and mlsp then
  if type(mlsp.setup) == "function" then
    mlsp.setup { ensure_installed = ensure_installed }
    if type(mlsp.setup_handlers) == "function" then
      mlsp.setup_handlers { setup }
    end
  end
end
```

`mason-lspconfig 2.x` removed `setup_handlers`. The `type(...) == "function"` check correctly skips it on new versions, **but**: on new mason-lspconfig there is no automatic server setup from `setup{ensure_installed=...}` either. Result: servers that pass through `ensure_installed[#ensure_installed+1] = server` at line 116 (i.e. servers that *are* in `all_mslp_servers`) will be installed but **never have `setup()` called on them**, because the handler loop never runs.

Test this by checking `:LspInfo` in a Python file — `pyright`/`ruff` should be active. If they aren't, this is why. The fix is to iterate the ensure_installed list and call `setup(server)` explicitly, or migrate to mason-lspconfig 2.x's new `handlers` opt:

```lua
mlsp.setup({
  ensure_installed = ensure_installed,
  handlers = { setup },  -- runs `setup` for every installed server
})
```

(See mason-lspconfig README — the API changed.)

### 5.9 No `vim.opt.termguicolors = true`

[init.lua:35](nvim/init.lua:35) — commented out. `tokyonight`/`catppuccin`/`github_dark` all assume true-color. Without it many highlights render as approximations and some plugins disable themselves. **Enable it.**

### 5.10 No `signcolumn = "yes"`

[init.lua:32](nvim/init.lua:32) — commented out. With LSP + gitsigns (if you enable it) + diagnostics, your text horizontally jitters every time a sign appears. Setting `vim.opt.signcolumn = "yes"` is the standard fix.

### 5.11 `init.lua` mixes options + plugin bootstrap + keymaps + autocmds in one file

It's 170 lines and growing. Split into:
- `lua/core/options.lua`
- `lua/core/keymaps.lua`
- `lua/core/autocmds.lua`
- `init.lua` — only does `require` + lazy bootstrap

This is the convention every popular distro/starter has settled on (kickstart, LazyVim, Astronvim). Easier to navigate and easier for `:edit`-ing the right concern.

### 5.12 README's stated TODOs are mostly done

[nvim/README.md](nvim/README.md):

```
- Add telescope shortcuts to config with "modern neovim" alpha2phi
- GOTO def (telescope)
- ChatGPT plugin
- Copilot with hotkey
- Other AI tools?
- Check Proto config
```

- "Telescope shortcuts" — done, mostly commented out (§5.5).
- "GOTO def (telescope)" — bound via `lsp/keymaps.lua` (`gr`, `gI`, `gb`).
- "ChatGPT plugin" — superseded by claude-code.nvim.
- "Copilot" — declared but disabled.
- "Proto" — `buf_ls` is configured.

Either delete the file or replace with a real README documenting your leader-key map, your file-tree position, your AI keys, etc.

### 5.13 The dotfiles repo also contains a stale `init.vim`

[init.vim](init.vim) at the dotfiles root (`/Users/gcperrin/.dotfiles/init.vim` once symlinked) is a **vim-plug-era Vimscript config** referencing NERDTree, deoplete, tern, neomake, ultisnips — all superseded by the Neovim Lua config. If this gets sourced by anything, it's pure confusion. Verify with `ls -la ~/.config/nvim` and delete the file if nothing points to it.

### 5.14 `lazy.nvim` performance config disables `matchparen` but you load `vim-matchup`

[init.lua:88](nvim/init.lua:88) lists `"matchparen"` in `disabled_plugins`. That's correct — vim-matchup is its replacement. But you should also disable `"matchit"` similarly — and you do. Good. Worth noting that `"netrwPlugin"` is *not* disabled even though you have neo-tree with `hijack_netrw_behavior = "open_default"`. Add `"netrwPlugin"` to the disabled list to save a small bit of startup time.

---

## 6. 🔵 Low-Priority / Style Issues

| # | File | Issue |
|---|------|-------|
| L1 | `plugins/init.lua` | Mixed quote styles (`"..."` and `'...'`) within the same file |
| L2 | `plugins/init.lua` | Mixed `require("foo")` vs `require "foo"` (Lua sugar) |
| L3 | `init.lua` | Trailing whitespace on several lines (`leader = ' '`) |
| L4 | `config/telescope.lua` | ~24 lines of commented-out keymaps — drift |
| L5 | `lua/utils/icons.lua` | Some icon entries empty strings (`Bug = ""`) — codepoints missing |
| L6 | `config/telescope.lua:286-290` | Commented-out `fg_bg` color overrides for telescope titles — dead code |
| L7 | `lsp/keymaps.lua` | Commented-out alternate gd/gD lines (above the working `gr` block) |
| L8 | `lsp/servers.lua` | `vim.notify(... vim.log.levels.WARN)` should probably be `ERROR` (it indicates a config/lock mismatch) |
| L9 | `lsp/keymaps.lua:28` | `<leader>la` uses `mode = { "n", "v" }` but other LSP keymaps don't — inconsistent |
| L10 | `init.lua:152` | `'<C-w v>'` — should be `'<C-w>v'` (space breaks the chord) |
| L11 | `plugins/init.lua` | `"GOTO def (telescope)"` etc. left as TODO comments inline |

**L10 is functionally broken:** `<leader>\` is supposed to split vertically, but the rhs `'<C-w v>'` is parsed as `<C-w>` + literal `v` + space — `<C-w>` followed by `v` opens a vertical split, the trailing space is dropped, so it might actually still work in practice. But `<leader>v` two lines up uses the correct `'<c-w>v'`. Make them consistent.

---

## 7. Plugin-by-Plugin Recommendations

### 7.1 Replace `nvim-cmp` ecosystem with `blink.cmp`

Your cmp stack is **6 plugins** (`nvim-cmp`, `cmp-nvim-lsp`, `cmp-nvim-lsp-signature-help`, `cmp-buffer`, `cmp-path`, `cmp_luasnip`). `blink.cmp` (Saghen/blink.cmp) replaces all of them with a single Rust-backed plugin that's faster, has built-in fuzzy matching (no telescope-fzf-native needed for completion), and converges with native nvim 0.11 completion. As of 2025 it's the de-facto modern completion plugin.

Migration is ~30 lines. Strong recommend.

### 7.2 Add `conform.nvim` for formatting (and remove the format-on-save scattered in language files)

Currently formatting is implicit (via LSP) in three places:
- `config/go.lua:77-82` — BufWritePre `vim.lsp.buf.format()` for gopls
- `config/proto.lua:23-28` — same for buf_ls
- `config/typescript.lua:82-92` — auto `EslintFixAll` on save

This is ad-hoc and won't unify on, e.g., `prettierd` for JS. Use `conform.nvim` (already a Mason-installable, you're installing `prettierd` in `html.lua:11`):

```lua
{
  "stevearc/conform.nvim",
  event = "BufWritePre",
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "ruff_format" },
      javascript = { "prettierd" },
      typescript = { "prettierd" },
      go = { "goimports", "gofumpt" },
      sh = { "shfmt" },
      proto = { "buf" },
    },
    format_on_save = { lsp_fallback = true, timeout_ms = 500 },
  },
}
```

Then **delete** the BufWritePre autocmds in the language files.

### 7.3 Add `nvim-lint` (or stick with LSP linters)

You're using ruff (lsp-as-linter), eslint (lsp), golangci-lint-langserver (lsp). That covers most cases. **No action required**, but if you ever need standalone linting for languages without a linter-LSP (e.g. `markdownlint`, `actionlint`), `mfussenegger/nvim-lint` is the right choice.

### 7.4 Replace `vim-abolish` with `johmsalas/text-case.nvim`

Native Lua, faster startup, lazy by default. abolish's `Subvert`/`Abolish` features map cleanly.

### 7.5 Replace `barbar.nvim` with `bufferline.nvim` (or no buffer-line at all)

Barbar is fine but has heavier dependencies. `akinsho/bufferline.nvim` is lighter and the de-facto standard. Or skip the bufferline entirely if you only navigate via telescope buffers (`<leader>b`).

### 7.6 Add `nvim-dap-ui` and `nvim-nio` for a usable debug experience

You have nvim-dap + dap-go + dap-python but no UI. A debug session right now is "manual breakpoint, manual continue, no scope view." Add:

```lua
{ "rcarriga/nvim-dap-ui", dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio" }, ... }
```

### 7.7 Add `folke/trouble.nvim` or `folke/snacks.nvim`

For diagnostic/symbol lists. Replaces the LSP quickfix flow with a UX that doesn't fight Telescope.

### 7.8 Add `lewis6991/gitsigns.nvim` (configured)

You already have it as a transitive dep. Add a proper spec:

```lua
{
  "lewis6991/gitsigns.nvim",
  event = { "BufReadPre", "BufNewFile" },
  opts = {
    signs = { add = {text="┃"}, change = {text="┃"}, ... },
    on_attach = function(buf)
      local gs = require("gitsigns")
      vim.keymap.set("n", "]c", gs.next_hunk, { buffer = buf })
      vim.keymap.set("n", "[c", gs.prev_hunk, { buffer = buf })
      vim.keymap.set("n", "<leader>hp", gs.preview_hunk, { buffer = buf })
      vim.keymap.set("n", "<leader>hs", gs.stage_hunk, { buffer = buf })
      vim.keymap.set("n", "<leader>hb", function() gs.blame_line{full=true} end, { buffer = buf })
    end,
  },
}
```

This is high-value, low-cost.

### 7.9 Add `mini.nvim` modules as one-shot replacements

`echasnovski/mini.nvim` is a meta-plugin where each module is independently usable. Strong candidates:

- `mini.surround` replaces `nvim-surround`
- `mini.pairs` replaces `nvim-autopairs`
- `mini.comment` replaces `Comment.nvim`
- `mini.ai` adds better text objects (complements treesitter-textobjects)
- `mini.indentscope` adds animated indent guides
- `mini.bracketed` adds `]/[` bracket motions (LSP diagnostic / buffer / file / etc.)

Net effect: you can drop 3-4 of your existing plugins with a single dep.

### 7.10 Consider `folke/flash.nvim` for jump motions

Replaces vim's `f`/`t`/`/` with a better experience. Adds `S` for tree-sitter scope jumping.

---

## 8. Keybinding Map (current effective state)

Below is what is *actually* bound right now, ignoring broken/commented entries.

### Leader (`<Space>`)

| Map         | Action                          | Defined in                          |
| ----------- | ------------------------------- | ----------------------------------- |
| `<leader>w` | `:w<CR>`                        | init.lua                            |
| `<leader>q` | `:q<CR>`                        | init.lua                            |
| `<leader>v` | `<C-w>v` (vsplit)               | init.lua                            |
| `<leader>\` | (broken — see L10) ≈ vsplit     | init.lua                            |
| `<leader>t` | Neotree toggle                  | plugins/init.lua                    |
| `<leader>n` | Next buffer (barbar)            | plugins/init.lua                    |
| `<leader>p` | Prev buffer (barbar)            | plugins/init.lua                    |
| `<leader>c` | Close buffer (barbar)           | plugins/init.lua                    |
| `<leader>b` | Telescope buffers               | config/telescope.lua                |
| `<leader>ff` | Telescope find files            | config/telescope.lua                |
| `<leader>su` | Telescope live_grep             | config/telescope.lua                |
| `<leader>m` | `:Telescope` ❌ **BROKEN**       | lsp/keymaps.lua (§3.1)              |
| `<leader>aa/ac/ar/av` | Claude Code commands   | config/ai.lua                       |
| `<leader>la` | Code action                    | lsp/keymaps.lua                     |
| `<leader>ls` | Document symbols               | lsp/keymaps.lua                     |
| `<leader>lS` | Workspace symbols              | lsp/keymaps.lua                     |
| `<leader>lw` | Toggle inline diagnostics      | lsp/keymaps.lua                     |
| `<leader>lo` | Organize imports (per-LSP)     | config/python.lua, typescript.lua   |
| `<leader>lO` | Sort imports (TS)              | config/typescript.lua               |
| `<leader>lu` | Remove unused (TS)             | config/typescript.lua               |
| `<leader>lF` | Fix all (TS)                   | config/typescript.lua               |
| `<leader>lA` | Add missing imports (TS)       | config/typescript.lua               |
| `<leader>lR` | Rename file (TS) ❌ **BROKEN**  | config/typescript.lua (cmd doesn't exist) |
| `<leader>lz` | Color picker (CCC)             | config/html.lua                     |
| `<leader>ly` | GoModTidy                      | config/go.lua                       |
| `<leader>lc` | GoCoverage                     | config/go.lua                       |
| `<leader>lt` | GoTest                         | config/go.lua                       |
| `<leader>dT` | Debug test (Go)                | config/go.lua                       |
| `<leader>lC/lM/lE` | Python dap test            | config/python.lua                   |

### Non-leader

| Map         | Action                                       |
| ----------- | -------------------------------------------- |
| `fd`/`fD`/`Fd`/`FD` | Escape from insert                   |
| `<C-j>` / `<C-k>` | 15gj / 15gk in normal/visual; cmp/cmdline up/down |
| `H` / `L`   | `^` / `$`                                    |
| `j` / `k`   | `gj` / `gk` (display-line aware)             |
| `gd`        | LSP definition (in `config/go.lua`, **go only**) |
| `gD`        | LSP declaration (Go and Solidity only)       |
| `gr`        | LSP references (Telescope)                   |
| `gI`        | LSP implementations (Telescope)              |
| `gb`        | LSP type definition (Telescope)              |
| `K`         | LSP hover                                    |
| `gK`        | LSP signature help                           |
| `[d`/`]d`   | Prev/next diagnostic                         |
| `[e`/`]e`   | Prev/next error                              |
| `[w`/`]w`   | Prev/next warning                            |
| `gc` / `gcc` / `gbc` | Comment                               |

### Conspicuous gaps

- **No leader-binding for `find files in cwd` (only "root dir")** — sometimes you want to scope to where you are.
- **No buffer-fuzzy-find (`<leader>sb`)** — commented out.
- **No help tags (`<leader>hs`)** — commented out.
- **No git status / git diff bindings** — gitsigns not configured.
- **No global rename / global format** — LSP rename not bound (commented out at `lsp/keymaps.lua:27`).
- **`<leader>f` namespace is shared between "find" (telescope) and nothing** — but the typescript LSP `<leader>l` group has `<leader>lF` for "Fix All" which is fine.
- **`gd` (definition) is only set in `go.lua` and `proto.lua`** — Python, TypeScript, etc. fall back to Neovim's default omnifunc (which works but isn't telescope-driven). Lift this to the global `lsp/keymaps.lua`.

---

## 9. Performance Notes

Measure first: `nvim --startuptime startup.log +q` and look for outliers > 5ms. But based on the spec read, the likely top contributors at startup are:

1. **`{ require("config.python") }` (and siblings)** — these run eagerly even though the underlying plugin specs are themselves lazy. The `require` of each `config/*.lua` file pulls in `lsp.utils` transitively, which is fine, but the eager call means none of lazy.nvim's bytecode cache applies. **Switch to `{ import = ... }`** (§5.1).

2. **15 Telescope extension `load_extension` calls** in `config/telescope.lua:269-283`. Each loads a plugin synchronously the first time Telescope is invoked, but they're called inside `setup`, so they all fire on first telescope open. With most extensions being dead (§5.5), drop them.

3. **`lualine` config without `event`** (§4.3).

4. **`vim-abolish` without `cmd`/`event`** — loaded at startup.

5. **`treesitter` syncing** — `sync_install = false` is correct. Good.

6. **Cache** — `performance.cache.enabled = true` is set. Good.

After §5.1 + §5.5 + §4.3 + L7.4, you should see startup drop visibly (typically 30-80ms on macOS depending on cold/warm).

---

## 10. Modernization Roadmap

Suggested order:

1. **Fix the 🔴 bugs** (§3) — 30 minutes.
2. **Consolidate `on_attach`** (§4.2) — keymap dispatch table by client name. 45 minutes.
3. **Convert `{ require("config.X") }` → `{ import = ... }`** and rename `config/<lang>.lua` → `plugins/lang/<lang>.lua` (§5.1). 30 minutes.
4. **Delete dead plugins** (§5.5) and the orphan `bash.lua` / no-op `vimtex.lua`. 15 minutes.
5. **Resolve `tsserver` vs `typescript-tools`** (§3.4). Pick one. 30 minutes.
6. **Migrate which-key v1 → v3** (§5.2). 15 minutes.
7. **Enable termguicolors + signcolumn=yes** (§5.9, §5.10). 2 minutes.
8. **Split `init.lua`** into `core/options.lua` + `core/keymaps.lua` + `core/autocmds.lua` (§5.11). 30 minutes.
9. **Add `conform.nvim` + remove per-language BufWritePre format hooks** (§7.2). 30 minutes.
10. **Add `gitsigns.nvim` with proper config** (§7.8). 15 minutes.
11. *(Optional but high-impact)* **Migrate `nvim-cmp` → `blink.cmp`** (§7.1). 45 minutes.
12. *(Optional)* **Add `nvim-dap-ui` + `nvim-nio`** for a real debug UX (§7.6). 20 minutes.

Total: a focused afternoon yields a measurably faster, cleaner config with fewer footguns.

---

## 11. Specific File-by-File Findings (Cheat Sheet)

### `nvim/init.lua`
- L19: `clipboard:prepend({'unnamedplus'})` — fine on macOS, but consider `vim.opt.clipboard = "unnamedplus"` (idiomatic).
- L29, L32, L35: useful options commented out — re-enable `termguicolors` and `signcolumn`.
- L76: `dev.patterns = { "alpha2phi" }` — leftover from starter fork.
- L77-80: `change_detection` / `checker` mismatch (see §4.5).
- L86-95: disabled_plugins missing `"netrwPlugin"`.
- L107-112: insert-escape mappings — `fD`/`Fd`/`FD` cases are usually overkill; `fd` is the canonical one.
- L132-133: `<C-j>`/`<C-k>` line-jump — overrides cmp's mappings in normal mode only, OK.
- L143: commented `<leader>r` to reload — lazy does support `:Lazy reload` which is a better answer.
- L152: `'<C-w v>'` — should be `'<c-w>v'` (typo).
- L157-165: bash autocmd that bypasses LSP infra (§4.7).

### `nvim/lua/plugins/init.lua`
- L37, L42-43: empty lualine sections.
- L62-70: vimtex `config` calls a no-op module (§5.3).
- L80-94: treesitter `ensure_installed` doesn't include `lua`, `c`, or `tsx` — add them, you're editing Lua right now.
- L141-148: which-key v1 syntax (§5.2).
- L150-175: mason setup — fine but `mr.refresh(ensure_installed)` callback signature changed in newer mason; verify.
- L181-197: lspconfig spec — relies on `lua/lsp/servers.lua` having a working setup (§5.8).
- L234-348: cmp config inline — extract (§4.4).
- L388-409: `{ require("config.X") }` antipattern (§5.1).
- L430-432: `vim-abolish` eager load.

### `nvim/lua/config/telescope.lua`
- L29-53: ~24 commented-out keymaps. Either uncomment or delete.
- L62-110: custom action `visidata` requires `vd` binary — fine but undocumented.
- L267-284: 15 `load_extension` calls — half of them dead (§5.5).
- L286-290: commented-out highlight overrides.

### `nvim/lua/config/python.lua`
- L8-14: commented null-ls block — delete.
- L40-44: `setup.ruff` returns `false` — that means "fall through to default lspconfig setup," which is the inverse of what your `lsp/servers.lua:55` documents (`return true` to skip). Cross-check intent.
- L47-61: ruff doesn't need explicit on_attach; pyright's keymaps are fine but should live in the language handlers table (§4.2).

### `nvim/lua/config/typescript.lua`
- L8-25: typescript-tools setup.
- L33-95: lspconfig.tsserver setup — conflicts with typescript-tools (§3.4).
- L75: `<cmd>TypescriptRenameFile<CR>` — command doesn't exist (§3.4).
- L84: deprecated `vim.lsp.get_active_clients` (§3.3).
- L78-80: `return true` from `setup.tsserver` is correct ("skip lspconfig auto-setup"). Good.

### `nvim/lua/config/go.lua`
- L13: ensure_installed has `golangci-lint-langserver` as a Mason package name — that should be `golangci-lint-langserver` (Mason registry name). Verify.
- L36-55: gopls config is solid. `semanticTokens = true` is good for treesitter coexistence.
- L77-82: BufWritePre LSP format — move to conform.nvim (§7.2).

### `nvim/lua/config/html.lua`
- L21: `html` LSP attached to JS/TS/JSX/TSX filetypes — emmet-style. OK but emmet_ls below also covers JSX. There's overlap. Consider:
  - HTML LSP only on `html`
  - emmet_ls on `html, css, javascriptreact, typescriptreact, vue, svelte`.

### `nvim/lua/config/solidity.lua`
- L11-21: Mason `ensure_installed` lists both `nomicfoundation-solidity-language-server` AND `solidity-ls` — they're different servers. You then configure `solidity_ls_nomicfoundation`. Drop the `solidity-ls` install.
- L40-41: deprecated `nvim_buf_set_option` (§3.3).

### `nvim/lua/config/proto.lua`
- L23-28: BufWritePre format — move to conform.nvim with `buf` formatter.
- L21-22: `nvim_set_keymap` is global, not buffer-scoped — should be `vim.keymap.set` with `buffer = bufnr` (consistent with the rest of the file).

### `nvim/lua/config/ai.lua`
- L2-13: `copilot.lua` disabled spec — delete if you've committed to claude-code.
- L14-70: claude-code.nvim spec — fine.
- L71-83: which-key v3 spec — correct, but the *rest* of the codebase uses v1. Standardize.

### `nvim/lua/config/picker.lua`
- Solid implementation. Used by `live_grep` for `<C-f>` (extension filter) and `<C-l>` (folder filter) within telescope. Keep.

### `nvim/lua/utils/init.lua`
- L16: deprecated `vim.lsp.get_active_clients` (§3.3).
- L44-70: `telescope()` factory — useful, retains opts by closure. Good.
- L72-93: `open_term()` is a simple impl, called by visidata/toggle_term telescope actions. Could be replaced by `akinsho/toggleterm.nvim` for a richer terminal experience.

### `nvim/lua/utils/maps.lua`
- §3.2 — broken `*noremap` helpers.
- The whole file is largely obsoleted by `vim.keymap.set` defaulting to `noremap = true` since 0.7. You could delete `maps.lua` entirely and use `vim.keymap.set` directly with `<5 lines of refactoring in `init.lua`.

### `nvim/lua/utils/icons.lua`
- Fine. Some keys have empty strings (`Bug = ""`) because the Nerd Font glyphs didn't render in the source — fix when you next touch the file.

### `nvim/lua/lsp/keymaps.lua`
- §3.1 — broken `<leader>m`.
- L24-26: format-on-save and rename commented out — re-enable rename (`vim.lsp.buf.rename`), keep format under conform.nvim.
- L29-31: telescope lsp_document_symbols / dynamic_workspace_symbols / toggle_diagnostics — all sensible.

### `nvim/lua/lsp/servers.lua`
- §5.8 — mason-lspconfig 2.x compatibility hole.
- L65-86: defensive checks for `lspconfig[server]` — useful, but the cluster of `vim.notify` calls is verbose; collapse with a single error message.

### `nvim/lua/lsp/utils.lua`
- §4.2 — global LspAttach autocmd registration per call.
- `toggle_diagnostics` uses a module-level `diagnostics_active` boolean — works, but doesn't track per-buffer. Acceptable.

### `nvim/README.md`
- §5.12 — replace with real documentation.

### `nvim/lazy-lock.json`
- 49 entries; 12 are dead (§5.5). Trim by deleting the corresponding plugin specs and running `:Lazy clean`.

---

## 12. Sanity-Check / Verification Steps

After fixes, run:

```vim
:Lazy
:Lazy profile
:checkhealth
:checkhealth lspconfig
:checkhealth mason
:LspInfo                            " in each filetype
:Telescope                          " confirm <leader>m once §3.1 is fixed
:lua print(vim.lsp.get_clients()[1].name)
```

And from the shell:

```bash
nvim --startuptime /tmp/start.log +q && sort -nrk2 /tmp/start.log | head -30
```

You're looking for plugin-load times >5ms that you didn't expect.

---

## 13. Appendix — Quick Diff Sketch for the 🔴 Fixes

### A. `lsp/keymaps.lua` line 9
```diff
-  self:map("<leader>m", "<cmd>Telescope<CR>", { desc = "Telescope" })
+  self:map("<leader>m", "Telescope", { desc = "Telescope" })
```

### B. `utils/maps.lua` all `*noremap` helpers
```diff
 function M.nnoremap(key, cmd, opts)
-  if opts ~= nil then
-    vim.tbl_deep_extend('force', opts, { noremap = true })
-  else
-    opts = {}
-  end
+  opts = vim.tbl_deep_extend('force', opts or {}, { noremap = true })
   map('n', key, cmd, opts)
 end
```
(Repeat for `inoremap`, `vnoremap`, `cnoremap`, `tnoremap`, `snoremap`.)

For `mode_group`/`group`: clone per-iteration as shown in §3.2.

### C. `config/typescript.lua` — deprecated API
```diff
-          local client = vim.lsp.get_active_clients({ bufnr = event.buf, name = "eslint" })[1]
+          local client = vim.lsp.get_clients({ bufnr = event.buf, name = "eslint" })[1]
```

### D. `utils/init.lua` — deprecated API
```diff
-    for _, client in pairs(vim.lsp.get_active_clients { bufnr = 0 }) do
+    for _, client in pairs(vim.lsp.get_clients { bufnr = 0 }) do
```

### E. `config/solidity.lua` — deprecated API
```diff
-              vim.api.nvim_buf_set_option(bufnr, 'tabstop', 2)
-              vim.api.nvim_buf_set_option(bufnr, 'shiftwidth', 2)
+              vim.bo[bufnr].tabstop = 2
+              vim.bo[bufnr].shiftwidth = 2
```

### F. `config/typescript.lua` — pick one (tsserver or typescript-tools)
Remove the `tsserver` entry from `servers = {}` and the `setup.tsserver` function. Keep only typescript-tools. *Or* the inverse — but not both.

---

*End of audit. Happy to apply any subset of these as commits — recommend starting with §13 (Critical bugs) then §5.1 (`require` → `import`).*
