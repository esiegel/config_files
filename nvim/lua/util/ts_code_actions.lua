local M = {}

local ts = vim.treesitter

local function get_lang(bufnr)
	local ft = vim.bo[bufnr].filetype
	local ok, lang = pcall(vim.treesitter.language.get_lang, ft)
	if ok and lang then
		return lang
	end

	local map = {
		javascript = "javascript",
		javascriptreact = "javascript",
		typescript = "typescript",
		typescriptreact = "tsx",
		tsx = "tsx",
	}
	return map[ft]
end

local function supported_buffer(bufnr)
	local ft = vim.bo[bufnr].filetype
	return ft == "javascript" or ft == "javascriptreact" or ft == "typescript" or ft == "typescriptreact" or ft == "tsx"
end

local function get_node_at_pos(bufnr, lang, row, col)
	local ok, node = pcall(ts.get_node, { bufnr = bufnr, lang = lang, pos = { row, col } })
	if ok and node then
		return node
	end
	if ts.get_node_at_pos then
		return ts.get_node_at_pos(bufnr, row, col, lang)
	end
	return nil
end

local function find_ancestor(node, types)
	local lookup = {}
	for _, t in ipairs(types) do
		lookup[t] = true
	end
	while node do
		if lookup[node:type()] then
			return node
		end
		node = node:parent()
	end
	return nil
end

local function node_text(bufnr, node)
	return ts.get_node_text(node, bufnr)
end

local function node_range(node)
	local sr, sc, er, ec = node:range()
	return {
		start = { line = sr, character = sc },
		["end"] = { line = er, character = ec },
	}
end

local function get_line_indent(bufnr, row)
	local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
	return line:match("^%s*") or ""
end

local function get_indent_unit(bufnr)
	local sw = vim.bo[bufnr].shiftwidth
	if sw == 0 then
		sw = vim.bo[bufnr].tabstop
	end
	if vim.bo[bufnr].expandtab then
		return string.rep(" ", sw)
	end
	return "\t"
end

local function unwrap_single_statement(node)
	if not node then
		return nil
	end

	if node:type() ~= "statement_block" then
		return node
	end

	local statements = {}
	for child in node:iter_children() do
		if child:named() then
			table.insert(statements, child)
		end
	end

	if #statements == 1 then
		return statements[1]
	end

	return nil
end

local function statement_expression(bufnr, node)
	if not node then
		return nil
	end

	if node:type() == "return_statement" then
		local arg = node:field("argument")[1]
		if not arg then
			return nil
		end
		return { kind = "return", text = node_text(bufnr, arg) }
	end

	if node:type() == "expression_statement" then
		local expr = node:field("expression")[1]
		if not expr then
			return nil
		end
		return { kind = "expression", text = node_text(bufnr, expr) }
	end

	return nil
end

local function build_if_to_ternary(bufnr, node)
	if not node or node:type() ~= "if_statement" then
		return nil
	end

	local condition = node:field("condition")[1]
	local consequence = unwrap_single_statement(node:field("consequence")[1])
	local alternative = node:field("alternative")[1]
	if alternative and alternative:type() == "else_clause" then
		alternative = alternative:field("body")[1]
	end
	alternative = unwrap_single_statement(alternative)
	if not (condition and consequence and alternative) then
		return nil
	end

	local then_expr = statement_expression(bufnr, consequence)
	local else_expr = statement_expression(bufnr, alternative)
	if not (then_expr and else_expr) then
		return nil
	end
	if then_expr.kind ~= else_expr.kind then
		return nil
	end

	local sr = node:range()
	local indent = get_line_indent(bufnr, sr)
	local cond_text = node_text(bufnr, condition)
	local body
	if then_expr.kind == "return" then
		body = indent .. "return " .. cond_text .. " ? " .. then_expr.text .. " : " .. else_expr.text .. ";"
	else
		body = indent .. cond_text .. " ? " .. then_expr.text .. " : " .. else_expr.text .. ";"
	end

	return {
		title = "Convert if to ternary",
		kind = "refactor.rewrite",
		edit = {
			changes = {
				[vim.uri_from_bufnr(bufnr)] = {
					{ range = node_range(node), newText = body },
				},
			},
		},
	}
end

local function build_ternary_to_if(bufnr, node)
	if not node or node:type() ~= "conditional_expression" then
		return nil
	end

	local parent = node:parent()
	while parent and parent:type() ~= "expression_statement" and parent:type() ~= "return_statement" do
		parent = parent:parent()
	end
	if not parent then
		return nil
	end

	if parent:type() == "expression_statement" then
		if parent:field("expression")[1] ~= node then
			return nil
		end
	elseif parent:type() == "return_statement" then
		if parent:field("argument")[1] ~= node then
			return nil
		end
	end

	local condition = node:field("condition")[1]
	local consequence = node:field("consequence")[1]
	local alternative = node:field("alternative")[1]
	if not (condition and consequence and alternative) then
		return nil
	end

	local sr = parent:range()
	local indent = get_line_indent(bufnr, sr)
	local inner_indent = indent .. get_indent_unit(bufnr)
	local stmt_prefix = parent:type() == "return_statement" and "return " or ""

	local lines = {
		indent .. "if (" .. node_text(bufnr, condition) .. ") {",
		inner_indent .. stmt_prefix .. node_text(bufnr, consequence) .. ";",
		indent .. "} else {",
		inner_indent .. stmt_prefix .. node_text(bufnr, alternative) .. ";",
		indent .. "}",
	}

	return {
		title = "Convert ternary to if",
		kind = "refactor.rewrite",
		edit = {
			changes = {
				[vim.uri_from_bufnr(bufnr)] = {
					{ range = node_range(parent), newText = table.concat(lines, "\n") },
				},
			},
		},
	}
end

local function build_actions(bufnr, ctx)
	if not supported_buffer(bufnr) then
		return {}
	end

	local lang = get_lang(bufnr)
	if not lang then
		return {}
	end

	local ok, parser = pcall(ts.get_parser, bufnr, lang)
	if not ok or not parser then
		return {}
	end

	local range = ctx.params and ctx.params.range or nil
	local row = range and range.start.line or (vim.api.nvim_win_get_cursor(0)[1] - 1)
	local col = range and range.start.character or vim.api.nvim_win_get_cursor(0)[2]

	local node = get_node_at_pos(bufnr, lang, row, col)
	if not node then
		return {}
	end

	local actions = {}
	local if_node = find_ancestor(node, { "if_statement" })
	local if_action = build_if_to_ternary(bufnr, if_node)
	if if_action then
		table.insert(actions, if_action)
	end

	local ternary_node = find_ancestor(node, { "conditional_expression" })
	local ternary_action = build_ternary_to_if(bufnr, ternary_node)
	if ternary_action then
		table.insert(actions, ternary_action)
	end

	return actions
end

M.build_actions = build_actions
M.build_if_to_ternary = build_if_to_ternary
M.build_ternary_to_if = build_ternary_to_if
M.find_ancestor = find_ancestor
M.get_indent_unit = get_indent_unit
M.get_lang = get_lang
M.get_line_indent = get_line_indent
M.get_node_at_pos = get_node_at_pos
M.node_range = node_range
M.node_text = node_text
M.statement_expression = statement_expression
M.supported_buffer = supported_buffer
M.unwrap_single_statement = unwrap_single_statement

function M.setup()
	if M._setup_done then
		return
	end
	M._setup_done = true

	local original = vim.lsp.handlers["textDocument/codeAction"]
	vim.lsp.handlers["textDocument/codeAction"] = function(err, actions, ctx, config)
		local bufnr = ctx.bufnr
		if bufnr and supported_buffer(bufnr) then
			local extra = build_actions(bufnr, ctx)
			if #extra > 0 then
				actions = actions or {}
				for _, action in ipairs(extra) do
					table.insert(actions, action)
				end
			end
		end
		return original(err, actions, ctx, config)
	end
end

return M
