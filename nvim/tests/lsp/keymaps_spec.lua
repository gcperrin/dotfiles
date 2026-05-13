-- Unit tests for lua/lsp/keymaps.lua.
--
-- Focused on the `:map` wrapper, which auto-wraps a string rhs in
-- `<cmd>...<cr>`. The bug-of-record was a call site that *also* wrapped
-- the rhs, producing `<cmd><cmd>Telescope<CR><cr>` (invalid).
--
-- We test the wrapper's contract here. The "no double-wrap" guarantee on
-- the actual call sites is covered separately in regression_spec.lua via
-- static source inspection.

local keymaps = require('lsp.keymaps')

local function fake_client(capabilities)
  return {
    name = 'test_lsp',
    server_capabilities = capabilities or {},
  }
end

describe('lsp.keymaps', function()
  describe(':map wrapper', function()
    local calls, original_set

    before_each(function()
      calls = {}
      original_set = vim.keymap.set
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.keymap.set = function(mode, lhs, rhs, opts)
        table.insert(calls, { mode = mode, lhs = lhs, rhs = rhs, opts = opts })
      end
    end)

    after_each(function()
      vim.keymap.set = original_set
    end)

    it('wraps a plain string rhs in <cmd>...<cr>', function()
      local self = keymaps.new(fake_client(), 0)
      self:map('X', 'Telescope', { desc = 'test' })
      assert.equals(1, #calls)
      assert.equals('<cmd>Telescope<cr>', calls[1].rhs)
      assert.equals('X', calls[1].lhs)
    end)

    it('passes a function rhs through unchanged', function()
      local self = keymaps.new(fake_client(), 0)
      local fn = function() end
      self:map('X', fn, { desc = 'test' })
      assert.equals(fn, calls[1].rhs)
    end)

    it('defaults to normal mode', function()
      local self = keymaps.new(fake_client(), 0)
      self:map('X', 'Telescope')
      assert.equals('n', calls[1].mode)
    end)

    it('honors a custom mode in opts', function()
      local self = keymaps.new(fake_client(), 0)
      self:map('X', 'Telescope', { mode = { 'n', 'v' } })
      assert.same({ 'n', 'v' }, calls[1].mode)
    end)

    it('attaches the buffer from new()', function()
      local self = keymaps.new(fake_client(), 42)
      self:map('X', 'Telescope')
      assert.equals(42, calls[1].opts.buffer)
    end)

    it('marks the mapping silent', function()
      local self = keymaps.new(fake_client(), 0)
      self:map('X', 'Telescope')
      assert.is_true(calls[1].opts.silent)
    end)

    it("skips mappings whose 'has' capability is missing", function()
      local self = keymaps.new(fake_client({}), 0)
      self:map('X', 'Whatever', { has = 'rename' })
      assert.equals(0, #calls,
        ':map should be a no-op when the LSP lacks the requested capability')
    end)

    it("invokes mappings whose 'has' capability is present", function()
      local self = keymaps.new(fake_client({ renameProvider = true }), 0)
      self:map('X', 'Whatever', { has = 'rename' })
      assert.equals(1, #calls)
    end)
  end)

  describe('diagnostic_goto', function()
    it('returns a function that calls vim.diagnostic.goto_next', function()
      local captured
      local original = vim.diagnostic.goto_next
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.diagnostic.goto_next = function(opts) captured = opts end
      keymaps.diagnostic_goto(true)()
      vim.diagnostic.goto_next = original
      assert.is_not_nil(captured)
    end)

    it('forwards a severity filter', function()
      local captured
      local original = vim.diagnostic.goto_prev
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.diagnostic.goto_prev = function(opts) captured = opts end
      keymaps.diagnostic_goto(false, 'ERROR')()
      vim.diagnostic.goto_prev = original
      assert.equals(vim.diagnostic.severity.ERROR, captured.severity)
    end)
  end)
end)
