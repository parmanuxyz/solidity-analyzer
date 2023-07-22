# solidity-analyzer

## Features

- [x] Formatting
- [ ] Diagnostics
- [ ] Outline

## Language Server

```bash
$ cargo b
# extension uses ~/bin/solidity-analyzer-ls as the language server
$ ln -s $PWD/target/debug/solidity-analyzer-ls ~/bin/solidity-analyzer-ls
```

## Extension

Open [`extension.ts`](./client/src/extension.ts) in VSCode and run in debug mode
with <kbd>F5</kbd>.
