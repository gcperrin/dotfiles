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
}
