return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        buf_ls = {
        },
      },
      setup = {
        buf_ls = function(_, _)
          local lsp_utils = require "lsp.utils"
          lsp_utils.on_attach(function(client, bufnr)
            local map = function(mode, lhs, rhs, desc)
              if desc then
                desc = desc
              end
              vim.keymap.set(mode, lhs, rhs, { silent = true, desc = desc, buffer = bufnr, noremap = true })
            end
            -- stylua: ignore
            if client.name == "buf_ls" then
              vim.api.nvim_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", { noremap = true, silent = true })
              vim.api.nvim_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true, silent = true })
              vim.api.nvim_create_autocmd("BufWritePre", {
                buffer = bufnr,
                callback = function()
                  vim.lsp.buf.format()
                end,
              })
            end
          end)
        end,
     },
    },
  },

}
