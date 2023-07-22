import { ExtensionContext, window } from 'vscode';
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  // let disposable = commands.registerCommand("extension.helloworld", async uri => {
  // 	vscode.window.showInformationMessage('Hello World!');
  // });
  //
  // context.subscriptions.push(disposable);

  const traceOutputChannel = window.createOutputChannel(
    'Solidity Analyzer Language Server Trace'
  );
  traceOutputChannel.appendLine('Solidity Analyzer Language Server Trace');
  const command =
    process.env.SERVER_PATH || process.env.HOME + '/bin/solidity-analyzer-ls';
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: 'debug',
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'solidity' }],
    outputChannel: traceOutputChannel,
    traceOutputChannel,
  };

  // Create the language client and start the clien5t.
  client = new LanguageClient(
    'solidity-analyzer-ls',
    'solidity analyzer language server',
    serverOptions,
    clientOptions
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
