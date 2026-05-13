----------------------
-------- MAPS --------
----------------------
local M = {}

-- Merge { noremap = true } into a (possibly nil) opts table without mutating it.
-- `vim.tbl_deep_extend` returns a new table; the caller must assign the result.
local function with_noremap(opts)
  return vim.tbl_deep_extend('force', opts or {}, { noremap = true })
end

-- Build a per-entry options table by layering an entry's override (if any) on
-- top of the group's shared opts. Critically, this never mutates `opts` —
-- previous bugs in this file leaked overrides forward across iterations.
local function merge_opts(opts, override)
  if override == nil then
    return opts
  end
  return vim.tbl_deep_extend('force', opts, override)
end

function M.mode_group(mode, maps, opts)
  opts = opts or {}
  for _, v in ipairs(maps) do
    local local_opts = merge_opts(opts, v[3])
    vim.keymap.set(mode, v[1], v[2], local_opts)
  end
end

function M.group(opts, maps)
  opts = opts or {}
  for _, v in ipairs(maps) do
    local mode = v[1]
    if type(v[2]) == 'table' then
      for i = 2, #v do
        local v_in = v[i]
        local local_opts = merge_opts(opts, v_in[3])
        vim.keymap.set(mode, v_in[1], v_in[2], local_opts)
      end
    else
      local local_opts = merge_opts(opts, v[4])
      vim.keymap.set(mode, v[2], v[3], local_opts)
    end
  end
end

-- ez maps
-- normal map
function M.nmap(key, cmd, opts)
  vim.keymap.set('n', key, cmd, opts or {})
end

function M.nnoremap(key, cmd, opts)
  vim.keymap.set('n', key, cmd, with_noremap(opts))
end

-- insert map
function M.imap(key, cmd, opts)
  vim.keymap.set('i', key, cmd, opts or {})
end

function M.inoremap(key, cmd, opts)
  vim.keymap.set('i', key, cmd, with_noremap(opts))
end

-- visual map
function M.vmap(key, cmd, opts)
  vim.keymap.set('v', key, cmd, opts or {})
end

function M.vnoremap(key, cmd, opts)
  vim.keymap.set('v', key, cmd, with_noremap(opts))
end

-- command map
function M.cmap(key, cmd, opts)
  vim.keymap.set('c', key, cmd, opts or {})
end

function M.cnoremap(key, cmd, opts)
  vim.keymap.set('c', key, cmd, with_noremap(opts))
end

-- terminal map
function M.tmap(key, cmd, opts)
  vim.keymap.set('t', key, cmd, opts or {})
end

function M.tnoremap(key, cmd, opts)
  vim.keymap.set('t', key, cmd, with_noremap(opts))
end

-- select map
function M.smap(key, cmd, opts)
  vim.keymap.set('s', key, cmd, opts or {})
end

function M.snoremap(key, cmd, opts)
  vim.keymap.set('s', key, cmd, with_noremap(opts))
end

return M
