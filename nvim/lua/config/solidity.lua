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
        solidity_ls_nomicfoundation = {
          setup = {
            cmd = { 'nomicfoundationssss-language-server', '--stdio' },
            filetypes = { 'solidity' },
            root_dir = lsp_utils.find_git_ancestor,
            single_file_support = true,
          },
        },
      },
    },
    solidity_ls_nomicfoundation = function(_, opts)
      require("lsp.utils").on_attach(function(client, bufnr)
        local map = function(mode, lhs, rhs, desc)
          if desc then
            desc = desc
          end
          vim.keymap.set(mode, lhs, rhs, { silent = true, desc = desc, buffer = bufnr, noremap = true })
        end
        vim.o.tabstop = 2
        vim.o.shiftwidth = 1
        map("n", "gD", "<leader>g vim.lsp.buf.declaration()<cr>", { noremap = true, silent = false })
        map("n", "gd", "<leader>g vim.lsp.buf.definition()<cr>", { noremap = true, silent = false })
        -- vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]
        vim.api.nvim_buf_set_option('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', { noremap=true, silent=true })
      end)
    end
  },
}
