# solidity-analyzer

[![Telegram](https://img.shields.io/badge/Join-Telegram-blue?style=for-the-badge&logo=data:image/svg%2bxml;base64,PHN2ZyBlbmFibGUtYmFja2dyb3VuZD0ibmV3IDAgMCAyNCAyNCIgaGVpZ2h0PSI1MTIiIHZpZXdCb3g9IjAgMCAyNCAyNCIgd2lkdGg9IjUxMiIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48cGF0aCBkPSJtOS40MTcgMTUuMTgxLS4zOTcgNS41ODRjLjU2OCAwIC44MTQtLjI0NCAxLjEwOS0uNTM3bDIuNjYzLTIuNTQ1IDUuNTE4IDQuMDQxYzEuMDEyLjU2NCAxLjcyNS4yNjcgMS45OTgtLjkzMWwzLjYyMi0xNi45NzIuMDAxLS4wMDFjLjMyMS0xLjQ5Ni0uNTQxLTIuMDgxLTEuNTI3LTEuNzE0bC0yMS4yOSA4LjE1MWMtMS40NTMuNTY0LTEuNDMxIDEuMzc0LS4yNDcgMS43NDFsNS40NDMgMS42OTMgMTIuNjQzLTcuOTExYy41OTUtLjM5NCAxLjEzNi0uMTc2LjY5MS4yMTh6IiBmaWxsPSIjMDM5YmU1Ii8+PC9zdmc+)](https://t.me/+9RBGRK34dyI2OWM0)


Goal is to create and provide improved devex over current extensions
for foundry projects with maybe hardhat support(through foundry) in future.


## Features

- [x] Formatting (with foundry, same as `forge fmt` while respecting configs)
- [x] Diagnostics (compiler errors and warnings on standard dapptools/foundry structure projects work. Optimizations, configurations, perf, etc. yet to come)
- [x] Outline (could add more details)

## Installation

1. Install the language server:
   ```bash
   # ensure that ~/.cargo/bin is in your path for the extension to be able to find it
   cargo +nightly install --git https://github.com/parmanuxyz/solidity-analyzer --bin solidity-analyzer-ls
   ```
2. Install the companion vscode prerelease extension [from the marketplace](https://marketplace.visualstudio.com/items?itemName=parmanu.solidity-analyzer-language-client-prerelease)

## Language Server

```bash
$ cargo b
```

Check the [configuration path below](#configuration). Logs are located in `~/solidity-analyzer.log`

## VSCode Extension Development/Debug

Open [`extension.ts`](./client/src/extension.ts) in VSCode and run in debug mode
with <kbd>F5</kbd>.

Install the pre-release from VSCode marketplace [here](https://marketplace.visualstudio.com/items?itemName=parmanu.solidity-analyzer-language-client-prerelease).

## Configuration

### VSCode

The extension will try to run the language server from the paths in following order:

1. `solidity-analyzer.languageServerPath` configuration value if it exists
2. `SOLIDITY_ANALYZER_SERVER_PATH` environment variable if it exists
3. Will search the folders in `PATH` to see if `solidity-analyzer-ls` exists in any of them. Will run the first entry it'd find.
4. If it doesn't even exist in the `PATH` it will try `~/bin/solidity-analyzer-ls`.
5. If found nowhere it'd show an error and exit.

### neovim

Configure neovim to use solidity-analyzer-ls as follows:

```lua
local configs = require 'lspconfig.configs'
local lspconfig = require 'lspconfig'

-- Check if the config is already defined (useful when reloading this file)
if not configs.solidity_analyer_lsp then
  configs.solidity_analyzer_lsp = {
    default_config = {
      cmd = {vim.fn.expand('$HOME/.cargo/bin/solidity-analyzer-ls')},
      filetypes = {'solidity'},
      root_dir = lspconfig.util.root_pattern('foundry.toml') or function(fname)
        return lspconfig.util.find_git_ancestor(fname)
      end,
      settings = {},
    },
  }
end

lspconfig.solidity_analyzer_lsp.setup{}
```
