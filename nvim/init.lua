-----------------------------
-- BASIC LUA NEOVIM CONFIG --
-----------------------------

-- SPACE as leader key
vim.g.mapleader = ' ' 

-- long live zsh
vim.opt.shell = '/bin/zsh'

-- remove pauses after j in insert mode
vim.opt.timeoutlen = 1000
vim.opt.ttimeoutlen = 0

-- HoldCursor faster than 4s
vim.opt.updatetime = 800

-- use system clipboard
vim.opt.clipboard:prepend({ 'unnamedplus' })

-- better completion actions
vim.opt.completeopt = { 'menuone', 'noinsert', 'noselect' }

-- cleaner completions
vim.opt.shortmess:append('c')

-- Line number in gutter`
vim.opt.number = true

-- Ignore case in search
vim.opt.ignorecase = true

-- add sign column always
-- vim.opt.signcolumn = 'yes'

-- numbas
vim.opt.number = true
-- vim.opt.relativenumber = true

-- allow more complicated font/color stuff
-- vim.opt.termguicolors = true

-- No swapfile
vim.opt.swapfile = false
vim.opt.backup = false

-- Search case ignore
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- use 2 spaces as tabs and always
-- expand to spaces
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

------------------------
-- PACKAGE MANAGEMENT --
------------------------

-- ensure lazy is installed
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup('plugins', {
  change_detection = {
    notify = false,
  },
})

require("lazy").setup {
  spec = {
    { import = "plugins" },
    { import = "lsp" },
    { import = "config" },
  },
  defaults = { lazy = true, version = nil },
  install = { missing = true, colorscheme = { "tokyonight", "catppuccin" } },
  dev = { patterns = jit.os:find "Windows" and {} or { "alpha2phi" } },
  checker = { enabled = true },
  performance = {
    cache = {
      enabled = true,
    },
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
}

------------------
-- KEY MAPPINGS --
------------------

-- Mapping functions import
local map = require('utils.maps')

-- Easy escape from insert
map.mode_group('i', {
  { 'fd', '<Esc>' },
  { 'fD', '<Esc>' },
  { 'Fd', '<Esc>' },
  { 'FD', '<Esc>' },
})

-- Keymaps for all modes
map.group({ noremap = true }, {
  {
    'c',
    -- Allow for homerow up and down in command mode
    { '<c-j>', '<down>' },
    { '<c-k>', '<up>' },
  },
  {
    'n',
    -- Allow for innerline navagation
    { 'j', 'gj' },
    { 'k', 'gk' },
    -- End and beg of line easier
    { 'H', '^' },
    { 'L', '$' },
  },
  -- Line jumping
  { 'n', { '<c-j>', '15gj' }, { '<c-k>', '15gk' } },
  { 'v', { '<c-j>', '15gj' }, { '<c-k>', '15gk' } },

  -- Save file
  { 'n', { '<leader>w', ':w<ENTER>' } },

  -- Quit file
  { 'n', { '<leader>q', ':q<ENTER>' } },

  -- Reload NVIM config
  -- currently resourcing is not supported with lazy
  -- { 'n', { '<leader>r', ':so %<CR>' } },

  { 'n', { '<leader>v', '<c-w>v' } },

  -- Bracket jumping (<%>)
  -- TODO
  
  -- Buffer nav
  { 'n', { '<leader>n', '<Cmd>BufferNext<CR>' } },
  { 'n', { '<leader>p', '<Cmd>BufferPrevious<CR>' } },
  { 'n', { '<leader>c', '<Cmd>BufferClose<CR>' } },
  { 'n', { '<leader>\\', '<C-w v>' } },
  -- { 'n', { 'gd', '<Cmd>lua vim.lsp.buf.declaration()<CR>' } },
  
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'sh',
    callback = function()
      vim.lsp.start({
        name = 'bash-language-server',
        cmd = { 'bash-language-server', 'start' },
      })
    end,
  })


-- Fix all indents (DOESN'T WORK FOR NOW)
-- map.nnoremap('<leader>><cr>', 'mzgg=G`z:w<cr>')
