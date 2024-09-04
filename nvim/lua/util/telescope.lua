local conf = require("telescope.config").values
local finders = require "telescope.finders"
local make_entry = require "telescope.make_entry"
local pickers = require "telescope.pickers"

local M = {}

local function get_basename(filename)
  return string.match(filename, "([^/\\]+)$")
end

local function contains(str, substr)
  return string.find(str, substr) ~= nil
end

-- find lsp references, but filter tests
-- mostly taken from telescope source
function M.references_filter_tests(opts)
  local filepath = vim.api.nvim_buf_get_name(opts.bufnr)
  local lnum = vim.api.nvim_win_get_cursor(opts.winnr)[1]
  local params = vim.lsp.util.make_position_params(opts.winnr)
  local include_current_line = vim.F.if_nil(opts.include_current_line, false)
  params.context = { includeDeclaration = vim.F.if_nil(opts.include_declaration, true) }

  vim.lsp.buf_request(opts.bufnr, "textDocument/references", params, function(err, result, ctx, _)
    if err then
      vim.api.nvim_err_writeln("Error when finding references: " .. err.message)
      return
    end

    local locations = {}
    if result then
      local results = vim.lsp.util.locations_to_items(result, vim.lsp.get_client_by_id(ctx.client_id).offset_encoding)
      if include_current_line then
        locations = vim.tbl_filter(function(v)
          -- Remove current line from result
          return not (v.filename == filepath and v.lnum == lnum)
        end, vim.F.if_nil(results, {}))
      else
        locations = vim.F.if_nil(results, {})
      end
    end

    if vim.tbl_isempty(locations) then
      return
    end

    -- Locations look like this:
    -- {
    --   col = 10,
    --   filename = "/Users/eric.siegel/code/ziggy/client/packages/feature/ZarrRnaDataLoader.ts",
    --   lnum = 48,
    --   text = "MySuperCoolFunction(x, y, z)",
    --   user_data = {
    --     uri = "file:///Users/eric.siegel/code/ziggy/client/packages/feature/ZarrRnaDataLoader.ts"
    --     range = {
    --       ["end"] = { character = 23, line = 47 },
    --       start = { character = 9, line = 47 }
    --     },
    --   }
    -- }
    local filtered_locations = {}
    for _, location in pairs(locations) do
      local filename = location["filename"]
      local name = get_basename(filename)
      if not contains(name, "test") then
        table.insert(filtered_locations, location)
      end
    end

    pickers
        .new(opts, {
          prompt_title = "LSP Filtered References",
          finder = finders.new_table {
            results = filtered_locations,
            entry_maker = opts.entry_maker or make_entry.gen_from_quickfix(opts),
          },
          previewer = conf.qflist_previewer(opts),
          sorter = conf.generic_sorter(opts),
          push_cursor_on_edit = true,
          push_tagstack_on_edit = true,
        })
        :find()
  end)
end

return M
