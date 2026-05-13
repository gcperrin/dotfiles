return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "javascript", "typescript", "tsx" })
    end,
  },
  {
    "pmizio/typescript-tools.nvim",
    opts = {},
    config = function(_, opts)
      require("lsp.utils").on_attach(function(client, bufnr)
        if client.name ~= "typescript-tools" then
          return
        end
        local set = function(lhs, rhs, desc)
          vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
        end
        set("<leader>lo", "<cmd>TSToolsOrganizeImports<cr>",       "Organize Imports")
        set("<leader>lO", "<cmd>TSToolsSortImports<cr>",           "Sort Imports")
        set("<leader>lu", "<cmd>TSToolsRemoveUnused<cr>",          "Remove Unused")
        set("<leader>lz", "<cmd>TSToolsGoToSourceDefinition<cr>",  "Go To Source Definition")
        set("<leader>lR", "<cmd>TSToolsRemoveUnusedImports<cr>",   "Remove Unused Imports")
        set("<leader>lF", "<cmd>TSToolsFixAll<cr>",                "Fix All")
        set("<leader>lA", "<cmd>TSToolsAddMissingImports<cr>",     "Add Missing Imports")
      end)
      require("typescript-tools").setup(opts)
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = { "pmizio/typescript-tools.nvim" },
    opts = {
      servers = {
        eslint = {
          settings = {
            -- helps eslint find the eslintrc when it's placed in a subfolder instead of the cwd root
            workingDirectory = { mode = "auto" },
          },
        },
      },
      setup = {
        eslint = function()
          vim.api.nvim_create_autocmd("BufWritePre", {
            callback = function(event)
              local client = vim.lsp.get_clients({ bufnr = event.buf, name = "eslint" })[1]
              if client then
                local diag = vim.diagnostic.get(event.buf, { namespace = vim.lsp.diagnostic.get_namespace(client.id) })
                if #diag > 0 then
                  vim.cmd "EslintFixAll"
                end
              end
            end,
          })
        end,
      },
    },
  },
}
