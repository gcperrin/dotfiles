local M = {}

local lsp_utils = require "lsp.utils"

local function lsp_init()
  -- LSP handlers configuration
  local config = {
    float = {
      focusable = true,
      style = "minimal",
      border = "rounded",
    },

    diagnostic = {
      virtual_text = {
        severity = {
          min = vim.diagnostic.severity.WARN,
        },
      },
      underline = false,
      update_in_insert = false,
      severity_sort = true,
      float = {
        focusable = true,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
      },
    },
  }

  -- Diagnostic configuration
  vim.diagnostic.config(config.diagnostic)
end

function M.setup(_, opts)
  lsp_utils.on_attach(function(client, bufnr)
    -- require("lsp.format").on_attach(client, bufnr)
    require("lsp.keymaps").on_attach(client, bufnr)
  end)

  lsp_init() -- diagnostics, handlers

  local servers = opts.servers
  local capabilities = lsp_utils.capabilities()

  local function setup(server)
    local server_opts = vim.tbl_deep_extend("force", {
      capabilities = capabilities,
    }, servers[server] or {})

    if opts.setup[server] then
      if opts.setup[server](server, server_opts) then
        return
      end
    elseif opts.setup["*"] then
      if opts.setup["*"](server, server_opts) then
        return
      end
    end
    
    local ok, lspconfig = pcall(require, "lspconfig")
    if not ok then
      vim.notify("lspconfig not available", vim.log.levels.ERROR)
      return
    end
    
    if not lspconfig[server] then
      vim.notify("LSP server '" .. server .. "' not found in lspconfig", vim.log.levels.WARN)
      return
    end
    
    local server_config = lspconfig[server]
    if type(server_config) ~= "table" then
      vim.notify("LSP server '" .. server .. "' is not a table: " .. type(server_config), vim.log.levels.ERROR)
      return
    end
    
    if type(server_config.setup) ~= "function" then
      vim.notify("LSP server '" .. server .. "'.setup is not a function: " .. type(server_config.setup), vim.log.levels.ERROR)
      return
    end
    
    server_config.setup(server_opts)
  end

  -- get all the servers that are available through mason-lspconfig
  local have_mason, mlsp = pcall(require, "mason-lspconfig")
  local all_mslp_servers = {}
  if have_mason then
    -- Try to get the list of available servers
    local has_get_available, available = pcall(function()
      return mlsp.get_available_servers()
    end)
    if has_get_available then
      all_mslp_servers = available
    else
      -- Fallback: try the old mappings method
      local has_mappings, mappings = pcall(require, "mason-lspconfig.mappings.server")
      if has_mappings then
        all_mslp_servers = vim.tbl_keys(mappings.lspconfig_to_package)
      end
    end
  end

  local ensure_installed = {} ---@type string[]
  for server, server_opts in pairs(servers) do
    if server_opts then
      server_opts = server_opts == true and {} or server_opts
      -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
      if server_opts.mason == false or not vim.tbl_contains(all_mslp_servers, server) then
        setup(server)
      else
        ensure_installed[#ensure_installed + 1] = server
      end
    end
  end

  if have_mason and mlsp then
    if type(mlsp.setup) == "function" then
      mlsp.setup { ensure_installed = ensure_installed }
      if type(mlsp.setup_handlers) == "function" then
        mlsp.setup_handlers { setup }
      end
    end
  end
end

return M
