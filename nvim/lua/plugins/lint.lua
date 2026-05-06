local fs = require("util.fs")

return {
	{
		"mfussenegger/nvim-lint",
		event = {
			"BufReadPre",
			"BufNewFile",
		},
		config = function()
			local lint = require("lint")

			local eslint = lint.linters.eslint
			eslint.args = {
				"--format",
				"json",
				"--stdin",
				"--stdin-filename",
				function()
					if fs.file_exists("packages/eslint-config/index.js") then
						return "--config packages/eslint-config/index.js"
					elseif fs.file_exists(".eslintrc.js") then
						return "--config .eslintrc.js"
					elseif fs.file_exists(".eslintrc.cjs") then
						return "--config .eslintrc.cjs"
					else
						return ""
					end
				end,
				function()
					return vim.api.nvim_buf_get_name(0)
				end,
			}

			-- customize luacheck so that it will use the luacheckrc
			local luacheck = lint.linters.luacheck
			luacheck.args = {
				"--default-config",
				vim.fn.expand("~/.config/nvim/.luacheckrc"),
				"--formatter",
				"plain",
				"--codes",
				"--ranges",
				"-",
			}

			local golangcilint = lint.linters.golangcilint
			golangcilint.cwd = function()
				local filedir = fs.get_parent_path(vim.api.nvim_buf_get_name(0))
				return fs.find_upward("go.mod", filedir) or filedir
			end
			golangcilint.args = {
				"run",
				"--output.json.path=stdout",
				"--output.text.path=",
				"--output.tab.path=",
				"--output.html.path=",
				"--output.checkstyle.path=",
				"--output.code-climate.path=",
				"--output.junit-xml.path=",
				"--output.teamcity.path=",
				"--output.sarif.path=",
				"--issues-exit-code=0",
				"--show-stats=false",
				"--path-mode=abs",
				function()
					local filepath = vim.api.nvim_buf_get_name(0)
					local filedir = fs.get_parent_path(filepath)
					if fs.find_upward("go.mod", filedir) then
						return filedir
					else
						return vim.fn.fnamemodify(filepath, ":p")
					end
				end,
			}

			lint.linters_by_ft = {
				css = { "stylelint" },
				javascript = { "eslint" },
				javascriptreact = { "eslint" },
				go = { "golangcilint" },
				lua = { "luacheck" },
				python = { "ruff" },
				typescript = { "eslint" },
				typescriptreact = { "eslint" },
			}

			local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

			vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
				group = lint_augroup,
				callback = function()
					lint.try_lint(nil, { ignore_errors = true })
				end,
			})
		end,
	},
}
