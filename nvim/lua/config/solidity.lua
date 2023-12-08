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
    solidity_ls_nomicfoundation = function(_, opts)
      require("lsp.utils").on_attach(function(client, bufnr)
        if client.name == "solidity_ls_nomicfoundation" then
          vim.o.tabstop = 1
          vim.o.shiftwidth = 2
        end
      end)
      return true
    end,
    opts = {
      servers = {
        solidity_ls_nomicfoundation = {
          setup = {
            cmd = { 'nomicfoundation-solidity-language-server', '--stdio' },
            filetypes = { 'solidity' },
            root_dir = require("lsp.utils").find_git_ancestor,
            single_file_support = true,
          },
        },
      },
    },
  },
}
