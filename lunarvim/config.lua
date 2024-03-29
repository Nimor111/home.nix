--[[
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT

-- general
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.colorscheme = "onedarker"

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"

lvim.builtin.which_key.mappings["z"] = { "<cmd>ZenMode<CR>", "Zen Mode" }

lvim.builtin.telescope.defaults.file_ignore_patterns = { ".git", "node_modules" }

lvim.builtin.which_key.mappings["x"] = { "<cmd>Scratch<CR>", "Scratch buffer" }

-- unmap a default keymapping
-- lvim.keys.normal_mode["<C-Up>"] = ""
-- edit a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>"

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- lvim.builtin.telescope.on_config_done = function()
--   local actions = require "telescope.actions"
--   -- for input mode
--   lvim.builtin.telescope.defaults.mappings.i["<C-j>"] = actions.move_selection_next
--   lvim.builtin.telescope.defaults.mappings.i["<C-k>"] = actions.move_selection_previous
--   lvim.builtin.telescope.defaults.mappings.i["<C-n>"] = actions.cycle_history_next
--   lvim.builtin.telescope.defaults.mappings.i["<C-p>"] = actions.cycle_history_prev
--   -- for normal mode
--   lvim.builtin.telescope.defaults.mappings.n["<C-j>"] = actions.move_selection_next
--   lvim.builtin.telescope.defaults.mappings.n["<C-k>"] = actions.move_selection_previous
-- end

lvim.builtin.telescope.defaults.layout_strategy = "flex"
lvim.builtin.telescope.defaults.layout_config.width = 180

-- Use which-key to add extra bindings with the leader-key prefix
lvim.builtin.which_key.mappings["B"] = { "<cmd>Telescope buffers<CR>", "Buffers"}
lvim.builtin.which_key.mappings["T"] = {
  t = { "<cmd>Telescope<CR>", "Telescope" }
}
lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }
lvim.builtin.which_key.mappings["t"] = {
  name = "+Trouble",
  r = { "<cmd>Trouble lsp_references<cr>", "References" },
  f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
  d = { "<cmd>Trouble lsp_document_diagnostics<cr>", "Diagnostics" },
  q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
  l = { "<cmd>Trouble loclist<cr>", "LocationList" },
  w = { "<cmd>Trouble lsp_workspace_diagnostics<cr>", "Diagnostics" },
}

lvim.builtin.which_key.mappings["w"] = {
   j = { "<cmd>wincmd j<cr>", "Move to window below"},
   k = { "<cmd>wincmd k<cr>", "Move to window above"},
   h = { "<cmd>wincmd h<cr>", "Move to the left window"},
   l = { "<cmd>wincmd l<cr>", "Move to right window"},
 }

-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.dashboard.active = true
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.show_icons.git = 0

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "css",
  "rust",
  "java",
  "yaml",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true

-- generic LSP settings
-- you can set a custom on_attach function that will be used for all the language servers
-- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end
-- you can overwrite the null_ls setup table (useful for setting the root_dir function)
-- lvim.lsp.null_ls.setup = {
--   root_dir = require("lspconfig").util.root_pattern("Makefile", ".git", "node_modules"),
-- }
-- or if you need something more advanced
-- lvim.lsp.null_ls.setup.root_dir = function(fname)
--   if vim.bo.filetype == "javascript" then
--     return require("lspconfig/util").root_pattern("Makefile", ".git", "node_modules")(fname)
--       or require("lspconfig/util").path.dirname(fname)
--   elseif vim.bo.filetype == "php" then
--     return require("lspconfig/util").root_pattern("Makefile", ".git", "composer.json")(fname) or vim.fn.getcwd()
--   else
--     return require("lspconfig/util").root_pattern("Makefile", ".git")(fname) or require("lspconfig/util").path.dirname(fname)
--   end
-- end

-- set a formatter if you want to override the default lsp one (if it exists)
-- lvim.lang.python.formatters = {
--   {
--     exe = "black",
--   }
-- }
-- set an additional linter
-- lvim.lang.python.linters = {
--   {
--     exe = "flake8",
--   }
-- }

-- Additional Plugins
lvim.plugins = {
  {
    "vimwiki/vimwiki",
    config = function()
      vim.cmd("let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md', 'diary_rel_path': './', 'auto_diary_index': 1}]")
      vim.cmd("let g:vimwiki_markdown_link_ext = 1")
      vim.cmd("let g:vimwiki_folding='expr'")
    end,
    branch = "dev"
  },
  {
    "folke/trouble.nvim",
    cmd = "TroubleToggle",
  },
  {
    "junegunn/fzf",
    "junegunn/fzf.vim",
    "michal-h21/vim-zettel",
  },
  {
    "metakirby5/codi.vim",
    cmd = "Codi",
  },
  {
    "purescript-contrib/purescript-vim"
  },
  {
    "folke/twilight.nvim",
    event = "BufWinEnter"
  },
  {
    "folke/zen-mode.nvim"
  },
  {
    "mtth/scratch.vim",
    config = function ()
      vim.cmd("let g:scratch_persistence_file='~/scratch_database'")
    end
  },
  {
    "DougBeney/vim-reddit"
  },
  -- {
    --"mattn/calendar-vim"
    --"itkkchyny/calendar.vim",
    -- config = function ()
    --   vim.cmd('source ~/.config/lvim/calendar_diary.vim')
    -- end
  -- },
  {
    "Olical/aniseed",
    "Olical/conjure"
  },
  {
    "vimoutliner/vimoutliner",
    config = function ()
      vim.cmd("let maplocalleader = ','")
    end
  },
  {
    "ntpeters/vim-better-whitespace",
    config = function ()
      vim.cmd([[
        let g:better_whitespace_enabled=1
        let g:strip_whitespace_on_save=1
        let g:strip_whitespace_confirm=0
        let g:better_whitespace_filetypes_blacklist=['dashboard']
     ]])
    end
  },
  {
    "nicwest/vim-http"
  },
  {
    "mfussenegger/nvim-jdtls",
    ft = "java"
  }
}

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
lvim.autocommands.custom_groups = {
  { "BufWinEnter", "*.*", ":lcd %:p:h"}
}

-- Longer timeout between commands
vim.cmd("set timeoutlen=1000 ttimeoutlen=0")
vim.cmd("set foldlevel=30")

lvim.format_on_save = false

vim.cmd("set colorcolumn=120")
