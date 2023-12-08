return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "solidity" })
    end,
  },
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      vim.list_extend(
        opts.ensure_installed,
        { "nomicfoundation-solidity-language-server" }
      )
    end,
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        solidity_ls_nomicfoundation = {
          setup = {
            cmd = { 'nomicfoundation-solidity-language-server', '--stdio' },
            filetypes = { 'solidity' },
            -- root_dir = require("lsp.utils").find_git_ancestor,
            single_file_support = true,
          },
        },
      },
    },
  },
}
