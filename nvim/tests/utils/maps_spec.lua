-- Unit tests for lua/utils/maps.lua.
--
-- These specs verify the contract of the keymap helpers — most importantly
-- that `*noremap` variants actually set `noremap = true` on the resulting
-- opts table (which they silently failed to do before, because
-- vim.tbl_deep_extend returns a new table and the previous code threw
-- the return value away).
--
-- Also covers the "no leaking of per-entry opts overrides" property of
-- mode_group/group, which was broken by mutating the shared opts table
-- across loop iterations.

local maps = require('utils.maps')

describe('utils.maps', function()
  local calls
  local original_set

  before_each(function()
    calls = {}
    original_set = vim.keymap.set
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.keymap.set = function(mode, lhs, rhs, opts)
      table.insert(calls, {
        mode = mode,
        lhs = lhs,
        rhs = rhs,
        opts = vim.deepcopy(opts or {}),
      })
    end
  end)

  after_each(function()
    vim.keymap.set = original_set
  end)

  ---------------------------------------------------------------------------
  -- *noremap helpers
  ---------------------------------------------------------------------------

  local noremap_cases = {
    { fn = 'nnoremap', mode = 'n' },
    { fn = 'inoremap', mode = 'i' },
    { fn = 'vnoremap', mode = 'v' },
    { fn = 'cnoremap', mode = 'c' },
    { fn = 'tnoremap', mode = 't' },
    { fn = 'snoremap', mode = 's' },
  }

  for _, case in ipairs(noremap_cases) do
    describe(case.fn, function()
      it('sets noremap=true with nil opts', function()
        maps[case.fn]('x', ':echo<cr>')
        assert.equals(1, #calls)
        assert.equals(case.mode, calls[1].mode)
        assert.equals('x', calls[1].lhs)
        assert.equals(':echo<cr>', calls[1].rhs)
        assert.is_true(calls[1].opts.noremap)
      end)

      it('preserves caller-provided opts and adds noremap=true', function()
        maps[case.fn]('x', ':echo<cr>', { desc = 'test', silent = true })
        assert.is_true(calls[1].opts.noremap)
        assert.equals('test', calls[1].opts.desc)
        assert.is_true(calls[1].opts.silent)
      end)

      it('does not mutate the caller-provided opts table', function()
        local user_opts = { desc = 'test' }
        maps[case.fn]('x', ':echo<cr>', user_opts)
        assert.is_nil(user_opts.noremap,
          case.fn .. ' should not mutate the caller-provided opts')
      end)

      it('accepts a function as the rhs', function()
        local fn = function() end
        maps[case.fn]('x', fn)
        assert.equals(fn, calls[1].rhs)
      end)
    end)
  end

  ---------------------------------------------------------------------------
  -- plain *map helpers (no implicit noremap)
  ---------------------------------------------------------------------------

  local plain_cases = {
    { fn = 'nmap', mode = 'n' },
    { fn = 'imap', mode = 'i' },
    { fn = 'vmap', mode = 'v' },
    { fn = 'cmap', mode = 'c' },
    { fn = 'tmap', mode = 't' },
    { fn = 'smap', mode = 's' },
  }

  for _, case in ipairs(plain_cases) do
    describe(case.fn, function()
      it('does not force noremap=true', function()
        maps[case.fn]('x', ':echo<cr>')
        assert.is_nil(calls[1].opts.noremap)
      end)

      it('passes the mode and lhs through to vim.keymap.set', function()
        maps[case.fn]('x', ':echo<cr>', { desc = 'test' })
        assert.equals(case.mode, calls[1].mode)
        assert.equals('x', calls[1].lhs)
        assert.equals('test', calls[1].opts.desc)
      end)
    end)
  end

  ---------------------------------------------------------------------------
  -- group
  ---------------------------------------------------------------------------

  describe('group', function()
    it('applies shared opts to every entry', function()
      maps.group({ noremap = true, silent = true }, {
        { 'n',
          { 'a', ':echo a<cr>' },
          { 'b', ':echo b<cr>' },
        },
      })
      assert.equals(2, #calls)
      assert.is_true(calls[1].opts.noremap)
      assert.is_true(calls[1].opts.silent)
      assert.is_true(calls[2].opts.noremap)
      assert.is_true(calls[2].opts.silent)
    end)

    it('supports multiple modes in one call', function()
      maps.group({ noremap = true }, {
        { 'c',
          { '<c-j>', '<down>' },
          { '<c-k>', '<up>' },
        },
        { 'n',
          { 'j', 'gj' },
        },
      })
      assert.equals(3, #calls)
      assert.equals('c', calls[1].mode)
      assert.equals('<c-j>', calls[1].lhs)
      assert.equals('<down>', calls[1].rhs)
      assert.equals('c', calls[2].mode)
      assert.equals('n', calls[3].mode)
      assert.equals('j', calls[3].lhs)
      assert.equals('gj', calls[3].rhs)
    end)

    -- This was the bug: per-entry overrides mutated the shared opts table,
    -- so once silent=true was set on one entry, every later entry inherited
    -- it even when it shouldn't.
    it('does not leak per-entry opts overrides to later entries (regression)', function()
      maps.group({ noremap = true }, {
        { 'n',
          { 'a', ':echo a<cr>', { silent = true } },  -- per-entry override
          { 'b', ':echo b<cr>' },                      -- no override
        },
      })
      assert.equals(2, #calls)
      assert.is_true(calls[1].opts.silent,
        'first entry should have silent=true from its override')
      assert.is_nil(calls[2].opts.silent,
        'second entry should NOT inherit silent=true from the previous override')
    end)

    it('does not mutate the caller-provided shared opts table', function()
      local shared = { noremap = true }
      maps.group(shared, {
        { 'n',
          { 'a', ':echo a<cr>', { silent = true } },
        },
      })
      assert.is_nil(shared.silent,
        'group() must not mutate the shared opts table')
    end)

    it('handles nil opts', function()
      maps.group(nil, {
        { 'n', { 'a', ':echo a<cr>' } },
      })
      assert.equals(1, #calls)
      assert.equals('n', calls[1].mode)
    end)
  end)

  ---------------------------------------------------------------------------
  -- mode_group
  ---------------------------------------------------------------------------

  describe('mode_group', function()
    it('applies the same mode to every entry', function()
      maps.mode_group('i', {
        { 'fd', '<Esc>' },
        { 'FD', '<Esc>' },
      })
      assert.equals(2, #calls)
      assert.equals('i', calls[1].mode)
      assert.equals('fd', calls[1].lhs)
      assert.equals('<Esc>', calls[1].rhs)
      assert.equals('i', calls[2].mode)
      assert.equals('FD', calls[2].lhs)
    end)

    it('does not leak per-entry opts overrides (regression)', function()
      maps.mode_group('n', {
        { 'a', ':echo a<cr>', { silent = true } },
        { 'b', ':echo b<cr>' },
      })
      assert.is_true(calls[1].opts.silent)
      assert.is_nil(calls[2].opts.silent,
        'mode_group must not propagate per-entry overrides forward')
    end)

    it('does not mutate the caller-provided shared opts table', function()
      local shared = { noremap = true }
      maps.mode_group('n', {
        { 'a', ':echo a<cr>', { silent = true } },
      }, shared)
      assert.is_nil(shared.silent)
    end)

    it('handles nil opts', function()
      maps.mode_group('i', {
        { 'fd', '<Esc>' },
      })
      assert.equals(1, #calls)
    end)
  end)
end)
