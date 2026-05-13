-- Bootstrap a minimal Neovim runtime for plenary-busted tests.
-- Invoked as:
--   nvim --headless --noplugin -u tests/minimal_init.lua \
--     -c "PlenaryBustedDirectory tests/ { minimal_init = 'tests/minimal_init.lua' }"

local function exists(path)
  return vim.uv.fs_stat(path) ~= nil
end

local lazy_data = vim.fn.stdpath('data') .. '/lazy'
local plenary_path = lazy_data .. '/plenary.nvim'

if not exists(plenary_path) then
  error(table.concat({
    'plenary.nvim not found at ' .. plenary_path,
    'Run nvim once with the full config to let lazy.nvim install plugins,',
    'or set XDG_DATA_HOME to point at a different lazy install.',
  }, '\n'))
end

vim.opt.runtimepath:prepend(plenary_path)

-- The parent of `tests/` is the nvim config directory; adding it makes
-- `require("utils.maps")`, `require("lsp.keymaps")`, etc. resolve to the
-- files under `lua/`.
local this_file = debug.getinfo(1).source:sub(2)
local nvim_root = vim.fn.fnamemodify(this_file, ':p:h:h')
vim.opt.runtimepath:prepend(nvim_root)

vim.cmd('runtime plugin/plenary.vim')

-- Expose the nvim root to specs that need to read source files for static
-- regression checks. Specs read this via _G.NVIM_ROOT.
_G.NVIM_ROOT = nvim_root
