/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { PathLike } from 'fs';
import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const debugServerModule = context.asAbsolutePath(
		path.join('..','..','target', 'debug', 'nls')
	);

	const serverConfigutration = workspace.getConfiguration("nls.server")
	const enableDebug: Boolean = serverConfigutration.get("debugLog")
	const traceFile: PathLike = serverConfigutration.get("trace")
	const serverModule: string = serverConfigutration.get("path")


	console.error(debugServerModule)
	console.error(serverModule)

	// The debug options for the server
	const debugOptions = { env: { "RUST_LOG": "trace" } };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {
			command: serverModule, transport: TransportKind.stdio, options: (enableDebug ? debugOptions : {}),
			args: traceFile ? ["--trace", traceFile.toString()] : [],
		},
		debug: {
			command: debugServerModule,
			args: traceFile ? ["--trace", traceFile.toString()] : [],
			transport: TransportKind.stdio,
			options: debugOptions
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'nickel' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.ncl')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'nickelLanguageServerClient',
		'Nickel Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start()
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
