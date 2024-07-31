-- This module provides Machine specific functions
local M = {}

MACHINE = {
	UNKNOWN = 1,
	HOME_LAPTOP = 2,
	HOME_DESKTOP = 3,
	WORK_LAPTOP = 4,
}

function M.current_machine()
	local hostname = vim.fn.hostname()

	if hostname == "etrans" then
		return MACHINE.HOME_LAPTOP
	elseif hostname == "emachine" then
		return MACHINE.HOME_LAPTOP
	elseif hostname == "fpx-l-US00953" then
		return MACHINE.HOME_LAPTOP
	else
		return MACHINE.UNKNOWN
	end
end

function M.vim_home_dir()
	local machine = M.current_machine()

	if machine == MACHINE.HOME_LAPTOP then
		return "/Users/eric/.vim"
	elseif machine == MACHINE.HOME_DESKTOP then
		return "/Users/eric/.vim"
	elseif machine == MACHINE.WORK_LAPTOP then
		return "/Users/eric.siegel/.vim"
	else
		return vim.fn.expand("$HOME/.vim")
	end
end

function M.vim_tmp_dir()
	local machine = M.current_machine()

	if machine == MACHINE.HOME_LAPTOP then
		return "/Users/eric/.vim/.tmpvim"
	elseif machine == MACHINE.HOME_DESKTOP then
		return "/Users/eric/code/.tmpvim"
	elseif machine == MACHINE.WORK_LAPTOP then
		return "/Users/eric.siegel/.vim/.tmpvim"
	else
		return "/Users/eric/.vim/.tmpvim"
	end
end

function M.make_tmp_dir()
  local tmp_dir = M.vim_tmp_dir()
  if not vim.fn.isdirectory(tmp_dir) then
    vim.fn.mkdir(tmp_dir, "p")
  end
end

return M
