import { ExtensionContext, window, workspace } from 'vscode';
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

let client: LanguageClient;

const getFromPath = (bin: string, def: string) => {
  const paths = process.env.PATH?.split(path.delimiter) || [];
  const binPaths = paths.map((x) => path.join(x, bin));
  const found = binPaths.find((x) => {
    return fs.existsSync(x);
  });
  return found || def;
};

const SOLIDITY_ANALYZER_SERVER_BIN_NAME = 'solidity-analyzer-ls';

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

  const serverPathEnv = process.env.SOLIDITY_ANALYZER_SERVER_PATH;
  const serverPathConfig = workspace
    .getConfiguration('solidity-analyzer')
    .get<string>('languageServerPath');
  const defaultPath = getFromPath(
    SOLIDITY_ANALYZER_SERVER_BIN_NAME,
    path.join(os.homedir(), 'bin', SOLIDITY_ANALYZER_SERVER_BIN_NAME)
  );
  const serverPath = serverPathConfig || serverPathEnv || defaultPath;
  if (!fs.existsSync(serverPath)) {
    window.showErrorMessage(
      `Solidity Analyzer Language Server not found at ${serverPath}`
    );
    process.exit(1);
    return;
  }

  const run: Executable = {
    command: serverPath,
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
    'solidity-analyzer',
    'Solidity language support for Visual Studio Code',
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
