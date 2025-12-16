local lsp_utils = require "lsp.utils"

return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "solidity" })
    end
  },
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      vim.list_extend(
        opts.ensure_installed,
        { "nomicfoundation-solidity-language-server" }
      )
      vim.list_extend(
        opts.ensure_installed,
        { "solidity-ls" }
      )
    end,
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        solidity_ls_nomicfoundation = {},
      },
      setup = {
        solidity_ls_nomicfoundation = function(_, _)
          local lsp_utils = require "lsp.utils"
          lsp_utils.on_attach(function(client, bufnr)
            local map = function(mode, lhs, rhs, desc)
              if desc then
                desc = desc
              end
              vim.keymap.set(mode, lhs, rhs, { silent = true, desc = desc, buffer = bufnr, noremap = true })
            end
            if client.name == "solidity_ls_nomicfoundation" then
              vim.api.nvim_buf_set_option(bufnr, 'tabstop', 2)
              vim.api.nvim_buf_set_option(bufnr, 'shiftwidth', 2)
              map("n", "gD", vim.lsp.buf.declaration, "Go to Declaration")
              map("n", "gd", vim.lsp.buf.definition, "Go to Definition")
            end
          end)
        end,
      },
    },
  },
}
