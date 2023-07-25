# solidity-analyzer

Goal is to create and provide improved devex over current extensions
for foundry projects with maybe hardhat support(through foundry) in future.

## Features

- [x] Formatting (with foundry, same as `forge fmt` while respecting configs)
- [ ] Diagnostics
- [x] Outline (could add more details)

## Language Server

```bash
$ cargo b
```

Check the [configuration path below](#configuration).

## Extension

Open [`extension.ts`](./client/src/extension.ts) in VSCode and run in debug mode
with <kbd>F5</kbd>.

### Configuration

The extension will try to run the language server from the paths in following order:

1. `solidity-analyzer.languageServerPath` configuration value if it exists
2. `SOLIDITY_ANALYZER_SERVER_PATH` environment variable if it exists
3. Will search the folders in `PATH` to see if `solidity-analyzer-ls` exists in any of them. Will run the first entry it'd find.
4. If it doesn't even exist in the `PATH` it will try `~/bin/solidity-analyzer-ls`.
5. If found nowhere it'd show an error and exit.
