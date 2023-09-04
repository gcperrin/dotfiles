----------------------
-------- MAPS --------
----------------------
local M = {}

local map = vim.keymap.set

function M.mode_group(mode, maps, opts)
  opts = opts or {}
  for _, v in ipairs(maps) do
    if v[3] then
      vim.tbl_deep_extend('force', opts, v[3])
    end
    map(mode, v[1], v[2], opts)
  end
end

function M.group(opts, maps)
  for _, v in ipairs(maps) do
    local mode = v[1]
    if type(v[2]) == 'table' then
      for i = 2, #v do
        local v_in = v[i]
        if v_in[3] then
          vim.tbl_deep_extend('force', opts, v_in[3])
        end
        map(mode, v_in[1], v_in[2], opts)
      end
    else
      if v[4] then
        vim.tbl_deep_extend('force', opts, v[4])
      end
      map(mode, v[2], v[3], opts)
    end
  end
end

-- ez maps
-- normal map
function M.nmap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('n', key, cmd, opts)
end

function M.nnoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end

  map('n', key, cmd, opts)
end

-- insert map
function M.imap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('i', key, cmd, opts)
end

function M.inoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end
  map('i', key, cmd, opts)
end

-- visual map
function M.vmap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('v', key, cmd, opts)
end

function M.vnoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end
  map('v', key, cmd, opts)
end

-- command map
function M.cmap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('c', key, cmd, opts)
end

function M.cnoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end

  map('c', key, cmd, opts)
end

-- terminal map
function M.tmap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('t', key, cmd, opts)
end

function M.tnoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end

  map('t', key, cmd, opts)
end

-- select map
function M.smap(key, cmd, opts)
  if opts == nil then
    opts = {}
  end
  map('s', key, cmd, opts)
end

function M.snoremap(key, cmd, opts)
  if opts ~= nil then
    vim.tbl_deep_extend('force', opts, { noremap = true })
  else
    opts = {}
  end

  map('s', key, cmd, opts)
end

return M
