-- Regression tests for the AUDIT.md §3 critical fixes.
--
-- These tests inspect source files directly rather than executing them.
-- They are intentionally cheap and dependency-free so that a future edit
-- that re-introduces a deprecated API or the original bug will fail CI
-- before any runtime testing kicks in.

local function read(rel_path)
  local root = _G.NVIM_ROOT or vim.fn.getcwd()
  return table.concat(vim.fn.readfile(root .. '/' .. rel_path), '\n')
end

local function loads(mod)
  local ok, err = pcall(require, mod)
  return ok, err
end

describe('regression: critical bug fixes from AUDIT §3', function()
  describe('Fix A — <leader>m double-wrap in lsp/keymaps.lua', function()
    it('does not pass an already-<cmd>-wrapped rhs to :map', function()
      local nvim_root = _G.NVIM_ROOT or vim.fn.getcwd()
      for _, line in ipairs(vim.fn.readfile(nvim_root .. '/lua/lsp/keymaps.lua')) do
        local is_comment = line:match('^%s*%-%-')
        if not is_comment then
          assert.is_nil(line:find('"<leader>m"%s*,%s*"<cmd>'),
            '<leader>m must be passed unwrapped to :map (line: ' .. line .. ')')
        end
      end
    end)

    it("the :map call still binds <leader>m to 'Telescope'", function()
      local src = read('lua/lsp/keymaps.lua')
      assert.is_truthy(src:find('"<leader>m"%s*,%s*"Telescope"'),
        'expected the un-wrapped form: self:map("<leader>m", "Telescope", ...)')
    end)
  end)

  describe('Fix B — utils/maps.lua noremap helpers', function()
    it('does not contain the buggy discarded-return pattern', function()
      -- The old buggy line was:
      --   vim.tbl_deep_extend('force', opts, { noremap = true })
      -- (return value discarded). Valid uses must either assign the result
      -- (`x = vim.tbl_deep_extend(...)`) or return it (`return vim.tbl_deep_extend(...)`).
      local nvim_root = _G.NVIM_ROOT or vim.fn.getcwd()
      for _, line in ipairs(vim.fn.readfile(nvim_root .. '/lua/utils/maps.lua')) do
        local is_comment = line:match('^%s*%-%-')
        if not is_comment and line:find('tbl_deep_extend') then
          local is_assignment = line:find('=%s*vim%.tbl_deep_extend')
          local is_return = line:find('return%s+vim%.tbl_deep_extend')
          assert.is_truthy(is_assignment or is_return,
            'vim.tbl_deep_extend return value must be used: ' .. line)
        end
      end
    end)

    it('exposes all expected helpers', function()
      local maps = require('utils.maps')
      for _, name in ipairs({
        'group', 'mode_group',
        'nmap', 'nnoremap',
        'imap', 'inoremap',
        'vmap', 'vnoremap',
        'cmap', 'cnoremap',
        'tmap', 'tnoremap',
        'smap', 'snoremap',
      }) do
        assert.is_function(maps[name], 'utils.maps.' .. name .. ' must be a function')
      end
    end)
  end)

  describe('Fix C/D — deprecated vim.lsp.get_active_clients', function()
    local files = {
      'lua/config/typescript.lua',
      'lua/utils/init.lua',
    }
    for _, f in ipairs(files) do
      it(f .. ' does not call vim.lsp.get_active_clients', function()
        local src = read(f)
        assert.is_nil(src:find('get_active_clients'),
          f .. ' still references the deprecated API; use vim.lsp.get_clients')
      end)
    end
  end)

  describe('Fix E — deprecated nvim_buf_set_option in solidity.lua', function()
    it('does not call nvim_buf_set_option', function()
      local src = read('lua/config/solidity.lua')
      assert.is_nil(src:find('nvim_buf_set_option'),
        'use vim.bo[bufnr].<opt> = <val> or vim.api.nvim_set_option_value instead')
    end)
  end)

  describe('Fix F — typescript.lua tsserver/typescript-tools conflict', function()
    it('keeps typescript-tools configured', function()
      local src = read('lua/config/typescript.lua')
      assert.is_truthy(src:find('typescript%-tools'),
        'typescript-tools.nvim should remain as the TypeScript LSP handler')
    end)

    it('does not also configure tsserver/ts_ls via lspconfig', function()
      local src = read('lua/config/typescript.lua')
      assert.is_nil(src:find('tsserver'),
        'tsserver should not be configured alongside typescript-tools')
      assert.is_nil(src:find('ts_ls'),
        'ts_ls should not be configured alongside typescript-tools')
    end)

    it('removes the dead TypescriptRenameFile keymap', function()
      local src = read('lua/config/typescript.lua')
      assert.is_nil(src:find('TypescriptRenameFile'),
        'TypescriptRenameFile is not a real command in lspconfig or typescript-tools')
    end)
  end)
end)

describe('smoke: pure-Lua modules load without error', function()
  -- These modules have no plugin-runtime dependencies at require time.
  local modules = {
    'utils.maps',
    'utils.init',
    'utils.icons',
    'lsp.keymaps',
    'lsp.utils',
  }
  for _, mod in ipairs(modules) do
    it('loads ' .. mod, function()
      package.loaded[mod] = nil
      local ok, err = loads(mod)
      assert.is_true(ok, 'failed to require("' .. mod .. '"): ' .. tostring(err))
    end)
  end
end)
