# Alex Tree-sitter grammar

Very incomplete alex tree sitter grammar.
Right now it really only lets you highlight the haskell part of alex files - as long as you have a tree-sitter haskell parser available.

## TODO

- [ ] write grammar for the alex part of the files
- [ ] test that it all works
- [ ] learn more about tree-sitter so I can understand it all better

## installing it in NVIM

This approach is what I had to do to make it work on my system, specifically NvChad + nvim-treesitter, and is not tested in any other setup

1. clone the repo
2. add the following to your `init.lua`:

```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.alex = {
  install_info = {
    url = "PATH_TO_CLONED_REPO",
    files = {"src/parser.c"},
    -- optional entries:
    branch = "main",
    generate_requires_npm = false,
    requires_generate_from_grammar = false,
  },
  highlight = { enable = true }, -- not sure if this is actually needed
  filetype = "x"
}
```

3. symlink `PATH_TO_CLONED_REPO/queries` to `~/.config/nvim/queries/alex`
4. open nvim and run `:TSInstall alex`
5. if nvim doesn't pick up the filetype as `alex` correctly, add the following to `~/.config/nvim/filetype.lua`

```lua
vim.filetype.add({
  extension = {
    x = "alex"
  }
})
```
