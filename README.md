# solidity-analyzer

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

## Extension

Open [`extension.ts`](./client/src/extension.ts) in VSCode and run in debug mode
with <kbd>F5</kbd>.

Install the pre-release from VSCode marketplace [here](https://marketplace.visualstudio.com/items?itemName=parmanu.solidity-analyzer-language-client-prerelease).

### Configuration

The extension will try to run the language server from the paths in following order:

1. `solidity-analyzer.languageServerPath` configuration value if it exists
2. `SOLIDITY_ANALYZER_SERVER_PATH` environment variable if it exists
3. Will search the folders in `PATH` to see if `solidity-analyzer-ls` exists in any of them. Will run the first entry it'd find.
4. If it doesn't even exist in the `PATH` it will try `~/bin/solidity-analyzer-ls`.
5. If found nowhere it'd show an error and exit.
