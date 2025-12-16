return {
  {
    "zbirenbaum/copilot.lua",
    enabled = false,
    cmd = "Copilot",
    build = ":Copilot auth",
    opts = {
      filetypes = {
        markdown = true,
        help = true,
      },
    },
  },
  {
    "greggh/claude-code.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    cmd = { "ClaudeCode", "ClaudeCodeContinue", "ClaudeCodeResume", "ClaudeCodeVerbose" },
    keys = {
      { "<leader>aa", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude Code", mode = "n" },
      { "<leader>ac", "<cmd>ClaudeCodeContinue<cr>", desc = "Claude Continue", mode = "n" },
      { "<leader>ar", "<cmd>ClaudeCodeResume<cr>", desc = "Claude Resume", mode = "n" },
      { "<leader>av", "<cmd>ClaudeCodeVerbose<cr>", desc = "Claude Verbose", mode = "n" },
    },
    opts = {
      window = {
        split_ratio = 0.4,
        position = "rightbelow vertical",
        enter_insert = false,
        hide_numbers = true,
        hide_signcolumn = true,
      },
      refresh = {
        enable = true,
        updatetime = 100,
        timer_interval = 1000,
        show_notifications = true,
      },
      git = {
        use_git_root = true,
      },
      shell = {
        separator = '&&',
        pushd_cmd = 'pushd',
        popd_cmd = 'popd',
      },
      command = "claude",
      command_variants = {
        continue = "--continue",
        resume = "--resume",
        verbose = "--verbose",
      },
      keymaps = {
        toggle = {
          normal = false, -- Disabled, using custom keymaps
          terminal = "<C-,>",
          variants = {
            continue = false, -- Disabled, using custom keymaps
            verbose = false,  -- Disabled, using custom keymaps
          },
        },
        window_navigation = true,
        scrolling = true,
      },
    },
    config = function(_, opts)
      require("claude-code").setup(opts)
    end,
  },
  {
    "folke/which-key.nvim",
    optional = true,
    opts = {
      spec = {
        { "<leader>a", group = "AI" },
        { "<leader>aa", desc = "Toggle Claude Code" },
        { "<leader>ac", desc = "Claude Continue" },
        { "<leader>ar", desc = "Claude Resume" },
        { "<leader>av", desc = "Claude Verbose" },
      },
    },
  },
}
