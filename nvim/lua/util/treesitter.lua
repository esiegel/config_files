local M = {}

-- Find all functions in the current TS file
M.find_functions_in_ts = function()
	-- Check if Tree-sitter is available
	local ts = vim.treesitter
	local bufnr = vim.api.nvim_get_current_buf()

	local parser = ts.get_parser(bufnr, "typescript")
	local lang = parser:lang()

	if parser:lang() ~= "typescript" then
		print("The current file is not a TypeScript file.")
		return
	end

	local tree = parser:parse()[1]
	local root = tree:root()

	-- Table to store all function names
	local function_names = {}

	-- Helper function to recursively find functions
	local function find_functions(node)
		-- Check if the node is a function declaration or expression
		if node:type() == "function_declaration" or node:type() == "function_expression" then
			-- Extract function name
			local name_node = node:field("name")[1]
			if name_node then
				local name = ts.get_node_text(name_node, bufnr)
				table.insert(function_names, name)
			end
		end

		-- Recursively visit child nodes
		for child in node:iter_children() do
			find_functions(child)
		end
	end

	-- Start finding functions from the root node
	find_functions(root)

	-- Print all function names
	for _, name in ipairs(function_names) do
		print(name)
	end
end

-- Map the function to a command in Neovim
vim.api.nvim_create_user_command("FindTSFunctions", M.find_functions_in_ts, {})

return M
