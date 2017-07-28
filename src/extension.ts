'use strict';

import * as path from 'path';

import { ExtensionContext, workspace } from 'vscode';
import { LanguageClient, TransportKind, LanguageClientOptions, RevealOutputChannelOn } from 'vscode-languageclient';

export function activate(context: ExtensionContext) {
	// The server is a Racket application
    const executablExt = process.platform == 'win32' ? '.exe' : '';
    const executable = 'server' + executablExt;
    
    const command = context.asAbsolutePath(path.join('server', executable));
    let serverOptions = {
        command, 
        transport: TransportKind.stdio,
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // diagnosticCollectionName: 'racket',
        outputChannelName: 'racket', // TODO check this and maybe remove it
        revealOutputChannelOn: RevealOutputChannelOn.Info, // TODO check this and maybe remove it
        documentSelector: ['racket', { language: 'racket', pattern: '**∕*.rkt' }],
        synchronize: {
            configurationSection: 'racket',
            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: [
                workspace.createFileSystemWatcher('**/.clientrc'),
                workspace.createFileSystemWatcher('**/.rkt')
            ]
        }
    }

	// Create the language client and start the client.
	const languageClient = new LanguageClient('racket', 'Racket Server', serverOptions, clientOptions);
	
	// Push the disposable to the context's subscriptions so that the 
	// client can be deactivated on extension deactivation
	context.subscriptions.push(languageClient.start());
}