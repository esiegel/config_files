local M = {}
local ts_utils = require("nvim-treesitter.ts_utils")

-- === Shared Helpers ===

local function get_node_at_cursor()
	return ts_utils.get_node_at_cursor()
end

local function get_text(node)
	return vim.treesitter.get_node_text(node, 0)
end

-- Walk up the tree to find a specific node type
local function find_ancestor(node, types)
	local current = node
	while current do
		if vim.tbl_contains(types, current:type()) then
			return current
		end
		current = current:parent()
	end
	return nil
end

local function strip_semicolon(str)
	return str:gsub(";%s*$", "")
end

-- === Logic: If -> Ternary ===

local function if_to_ternary(if_node)
	-- 1. Validate structure
	local alternative = if_node:field("alternative")[1]
	if not alternative then
		print("Toggle: 'if' statement is missing an 'else' block.")
		return
	end

	local condition = if_node:field("condition")[1]
	local consequence = if_node:field("consequence")[1]

	-- Handle 'else' clause vs direct statement
	local else_statement = alternative
	if alternative:type() == "else_clause" then
		else_statement = alternative:named_child(0)
	end

	-- Extract internal expressions from blocks
	local function extract_inner(block_node)
		if block_node:type() == "statement_block" then
			local inner = block_node:named_child(0)
			if not inner then
				return nil
			end

			-- Handle "return x;"
			if inner:type() == "return_statement" then
				local ret_val = inner:named_child(0)
				return { type = "return", text = get_text(ret_val) }

			-- Handle "x = y;"
			elseif inner:type() == "expression_statement" then
				local expr = inner:named_child(0)
				if expr:type() == "assignment_expression" then
					local left = get_text(expr:field("left")[1])
					local right = get_text(expr:field("right")[1])
					return { type = "assign", var = left, text = right }
				end
				return { type = "expr", text = strip_semicolon(get_text(inner)) }
			end
		end
		-- Fallback for single-line unbraced statements: "if (x) return y;"
		if block_node:type() == "return_statement" then
			return { type = "return", text = get_text(block_node:named_child(0)) }
		end
		return { type = "expr", text = strip_semicolon(get_text(block_node)) }
	end

	local cons_data = extract_inner(consequence)
	local alt_data = extract_inner(else_statement)

	if not cons_data or not alt_data then
		print("Toggle: Could not parse if/else blocks.")
		return
	end

	-- 2. Build new string
	local cond_text = get_text(condition)
	-- Remove outer parens if TS parser included them
	if cond_text:match("^%(") and cond_text:match("%)$") then
		cond_text = cond_text:sub(2, -2)
	end

	local new_text = ""

	if cons_data.type == "return" and alt_data.type == "return" then
		new_text = string.format("return %s ? %s : %s;", cond_text, cons_data.text, alt_data.text)
	elseif cons_data.type == "assign" and alt_data.type == "assign" and cons_data.var == alt_data.var then
		new_text = string.format("%s = %s ? %s : %s;", cons_data.var, cond_text, cons_data.text, alt_data.text)
	else
		new_text = string.format("%s ? %s : %s;", cond_text, cons_data.text, alt_data.text)
	end

	-- 3. Apply change
	local start_row, start_col, end_row, end_col = if_node:range()
	vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, { new_text })
	print("Toggle: Converted If/Else to Ternary")
end

-- === Logic: Ternary -> If ===

local function ternary_to_if(ternary_node)
	local condition = get_text(ternary_node:field("condition")[1])
	local consequence = get_text(ternary_node:field("consequence")[1])
	local alternative = get_text(ternary_node:field("alternative")[1])

	local parent = ternary_node:parent()
	local prefix = ""
	local target_node = ternary_node -- Node to replace

	-- Detect Context: Return or Assignment?
	if parent:type() == "return_statement" then
		prefix = "return "
		target_node = parent
	elseif parent:type() == "assignment_expression" then
		local var_name = get_text(parent:field("left")[1])
		prefix = var_name .. " = "

		-- Try to capture the full statement (including semicolon)
		local grand_parent = parent:parent()
		if grand_parent:type() == "expression_statement" then
			target_node = grand_parent
		else
			target_node = parent
		end
	elseif parent:type() == "variable_declarator" then
		local var_name = get_text(parent:field("name")[1])
		prefix = var_name .. " = "
		-- Replace the entire declaration line
		target_node = parent:parent() -- lexical_declaration
		print("Toggle: Warning - Check 'const' vs 'let' scope.")
	end

	local new_lines = {
		string.format("if (%s) {", condition),
		string.format("  %s%s;", prefix, consequence),
		"} else {",
		string.format("  %s%s;", prefix, alternative),
		"}",
	}

	local start_row, start_col, end_row, end_col = target_node:range()
	vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, new_lines)

	-- Re-indent using standard vim command
	vim.cmd("normal! ==")
	print("Toggle: Converted Ternary to If/Else")
end

-- === Main Entry Point ===

M.toggle = function()
	local node = get_node_at_cursor()
	if not node then
		return
	end

	-- 1. Check if we are inside a Ternary
	local ternary = find_ancestor(node, { "ternary_expression" })
	if ternary then
		ternary_to_if(ternary)
		return
	end

	-- 2. Check if we are inside an If Statement
	local if_stmt = find_ancestor(node, { "if_statement" })
	if if_stmt then
		if_to_ternary(if_stmt)
		return
	end

	print("Toggle: Cursor is not inside an If/Else or Ternary.")
end

return M
