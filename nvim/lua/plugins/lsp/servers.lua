---@diagnostic disable: missing-fields

-- Add any servers here together with their settings
---@type lspconfig.options
local servers = {
  bashls = {},
  clangd = {},
  cssls = {},
  gopls = {},
  tsserver = {},
  html = {},
  jsonls = {},
  yamlls = {},

  -- JS/TS
  eslint = {
    workingDirectory = { mode = "auto" },
  },
  prettier = {
    workingDirectory = { mode = "auto" },
  },

  -- lua
  lua_ls = {
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim" },
        },
        workspace = {
          checkThirdParty = false,
          library = vim.api.nvim_get_runtime_file("", true),
        },
        completion = {
          callSnippet = "Replace",
        },
      },
    },
  },

  -- rust
  rust_analyzer = {},

  -- python
  pyright = {},
  ruff = {},
}

return servers
