-- TODO: update system info to work with nvim
local system_info = require("config.system_info")
system_info.make_tmp_dir()

vim.g.mapleader = ","
vim.g.maplocalleader = ","

-- read modeline at bottom of files
vim.opt.modeline = true

-- inverse colors for highlight searching
vim.cmd.hi("Search cterm=inverse ctermbg=NONE ctermfg=NONE gui=inverse guibg=NONE guifg=NONE")

-- Allows execution of local vimrc files, and enables project specific vimrc
-- disallows the use of autocmd, shell, and write within the exrc files
vim.opt.exrc = true
vim.opt.secure = true

-- removes annoying beeps when bad command
-- instead flashes screen
-- first one helps with entering
vim.opt.visualbell = true

-- no search wraps
vim.opt.wrapscan = false

-- don't wrap lines
vim.opt.wrap = false

-- magic patterns - extended regular expresions
vim.opt.magic = true

-- case is only important in search if using caps
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- In block mode we can go passed characters into blank space
vim.opt.virtualedit = "block"

-- After this many milliseconds write to the swap.
vim.opt.updatetime = 500

-- wildmode enables better file viewing when opeing new files, like bash
vim.opt.wildmode = "longest,list,list:full"
vim.opt.wildignore:append({ "*.swp", "*.pyc", "*.class", "*.idea*" })

-- When in unclosed parens, ie args, have them line up during indentation.
-- help cinoptions-values
vim.opt.cinoptions:append({ "(0" })

-- new horizontal splits will be placed below
-- new vertical splits will be placed to the right
vim.opt.splitbelow = false
vim.opt.splitright = true

-- formatting when in diff mode
vim.opt.diffopt = "filler,vertical,internal,closeoff,algorithm:patience"

vim.opt.autowrite = false -- disable auto write
vim.opt.clipboard = "unnamedplus" -- sync with system clipboard
vim.opt.cmdheight = 1 -- cmd height
vim.opt.completeopt = "menu,menuone,noselect"
vim.opt.conceallevel = 3 -- Hide * markup for bold and italic
vim.opt.confirm = true -- confirm to save changes before exiting modified buffer
vim.opt.cursorline = true -- Enable highlighting of the current line
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.formatoptions = "jcroqlnt" -- rules that help remove comments during formatting
vim.opt.grepformat = "%f:%l:%c:%m" --
vim.opt.grepprg = "rg --vimgrep" --
vim.opt.guifont = "FiraCode Nerd Font:h11" --
vim.opt.hidden = true -- Enable modified buffers in background
vim.opt.ignorecase = true -- Ignore case
vim.opt.inccommand = "nosplit" -- preview incremental substitute
vim.opt.joinspaces = false -- No double spaces with join after a dot
vim.opt.laststatus = 2 -- Always show a status line
vim.opt.mouse = "a" -- enable mouse mode
vim.opt.number = false -- Print line number
vim.opt.pumblend = 10 -- Popup blend
vim.opt.pumheight = 10 -- Maximum number of entries in a popup
vim.opt.relativenumber = false -- Relative line numbers
vim.opt.scrolloff = 4 -- Lines of context
vim.opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }
vim.opt.shiftround = true -- Round indent
vim.opt.shiftwidth = 2 -- Size of an indent
vim.opt.showmode = false -- dont show mode since we have a statusline
vim.opt.sidescrolloff = 8 -- Columns of context
vim.opt.signcolumn = "yes" -- Always show the signcolumn, otherwise it would shift the text each time
vim.opt.smartcase = true -- Don't ignore case with capitals
vim.opt.smartindent = true -- Insert indents automatically
vim.opt.spelllang = { "en" }
vim.opt.softtabstop = 2
vim.opt.tabstop = 2 -- Number of spaces tabs count for
vim.opt.termguicolors = true -- True color support
vim.opt.timeoutlen = 300 --
vim.opt.undofile = true --
vim.opt.undolevels = 10000 --
vim.opt.updatetime = 200 -- save swap file and trigger CursorHold
vim.opt.wildmode = "longest:full,full" -- Command-line completion mode
vim.opt.wrap = false -- Disable line wrap

if vim.fn.has("nvim-0.9.0") == 1 then
	vim.opt.splitkeep = "screen"
	vim.o.shortmess = "filnxtToOFWIcC"
end

-- fix markdown indentation settings
vim.g.markdown_recommended_style = 0
