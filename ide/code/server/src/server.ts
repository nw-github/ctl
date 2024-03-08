import {
    createConnection,
    TextDocuments,
    Diagnostic,
    DiagnosticSeverity,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    TextDocumentSyncKind,
    InitializeResult,
} from "vscode-languageserver/node";

import { TextDocument } from "vscode-languageserver-textdocument";

import { spawn } from "child_process";

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents = new TextDocuments<TextDocument>(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

connection.onInitialize((params: InitializeParams) => {
    const capabilities = params.capabilities;

    // Does the client support the `workspace/configuration` request?
    // If not, we fall back using global settings.
    hasConfigurationCapability = !!(
        capabilities.workspace && !!capabilities.workspace.configuration
    );
    hasWorkspaceFolderCapability = !!(
        capabilities.workspace && !!capabilities.workspace.workspaceFolders
    );

    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            // Tell the client that this server supports code completion.
            // completionProvider: {
            //     resolveProvider: true,
            // },
            // diagnosticProvider: {
            //     interFileDependencies: false,
            //     workspaceDiagnostics: false,
            // },
        },
    };
    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true,
            },
        };
    }
    return result;
});

connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        // Register for all configuration changes.
        connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
    if (hasWorkspaceFolderCapability) {
        connection.workspace.onDidChangeWorkspaceFolders((_event) => {
            connection.console.log("Workspace folder change event received.");
        });
    }
});

type Configuration = {
    maxNumberOfProblems: number;
    compiler: { path: string; maxInvocationTime: number; debounceMs: number };
};

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: Configuration = {
    maxNumberOfProblems: 1000,
    compiler: { path: "ctl", maxInvocationTime: 5000, debounceMs: 250 },
};
let globalSettings = defaultSettings;

// Cache the settings of all open documents
const documentSettings = new Map<string, Thenable<Configuration>>();
let validateTextDocument = debounce(realValidateTextDocument, globalSettings.compiler.debounceMs);

connection.onDidChangeConfiguration((change) => {
    if (hasConfigurationCapability) {
        // Reset all cached document settings
        documentSettings.clear();
    }
    globalSettings = (change.settings.ctlsp as Configuration) || defaultSettings;
    validateTextDocument = debounce(realValidateTextDocument, globalSettings.compiler.debounceMs);

    const first = documents.all()[0];
    if (first) {
        validateTextDocument(first);
    }
});

function getDocumentSettings(resource: string) {
    if (!hasConfigurationCapability) {
        return Promise.resolve(globalSettings);
    }

    let result = documentSettings.get(resource);
    if (!result) {
        result = connection.workspace.getConfiguration({
            scopeUri: resource,
            section: "ctlsp",
        });
        documentSettings.set(resource, result);
    }
    return result;
}

documents.onDidClose((e) => {
    documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change) => {
    validateTextDocument(change.document);
});

function debounce(func: (...args: any) => void, wait: number) {
    let timeout: NodeJS.Timeout | undefined;
    return (...args: any) => {
        clearTimeout(timeout);
        if (!timeout) {
            func(...args);
        } else {
            timeout = setTimeout(() => {
                timeout = undefined;
                func(...args);
            }, wait);
        }
    };
}

async function realValidateTextDocument(document: TextDocument) {
    type DiagnosticsResult = {
        files: string[];
        diagnostics: {
            id: number;
            pos: number;
            len: number;
            msg: string;
            sev: "warning" | "error";
        }[];
        input_file: number;
    };

    // In this simple example we get the settings for every validate run.
    const settings = await getDocumentSettings(document.uri);
    const text = document.getText();
    const result = await invokeCompiler<DiagnosticsResult>(
        text,
        ["--diagnostics"],
        settings,
        document.uri
    );
    if (!result) {
        return;
    }

    const allDiagnostics: Record<string, Diagnostic[]> = {};
    for (const document of documents.all()) {
        allDiagnostics[document.uri] = [];
    }

    for (const { id, pos, len, sev, msg } of result.diagnostics) {
        const uri = id === result.input_file ? document.uri : "file://" + result.files[id];

        let diagnostics: Diagnostic[];
        if (!allDiagnostics[uri]) {
            diagnostics = allDiagnostics[uri] = [];
        } else {
            diagnostics = allDiagnostics[uri];
        }

        const doc = documents.get(uri) ?? document;
        diagnostics.push({
            severity: sev === "warning" ? DiagnosticSeverity.Warning : DiagnosticSeverity.Error,
            range: { start: doc.positionAt(pos), end: doc.positionAt(pos + len) },
            message: msg,
            source: "ctlsp",
        });
    }

    console.log("------------");
    for (const uri in allDiagnostics) {
        connection.sendDiagnostics({ uri, diagnostics: allDiagnostics[uri] });
        console.log(`sending ${allDiagnostics[uri].length} diagnostics for ${uri}`);
    }
}

async function invokeCompiler<T>(
    text: string,
    flags: string[],
    settings: Configuration,
    path: string
) {
    if (path.startsWith("file://")) {
        path = path.slice(7);
    }

    const result = spawn(settings.compiler.path, ["lsp", path, "--has-file", ...flags], {
        timeout: settings.compiler.maxInvocationTime,
        shell: true,
    });

    return new Promise<T>((resolve, reject) => {
        let [stdout, stderr] = ["", ""];
        result.stdout.on("data", (data) => (stdout += data));
        result.stderr.on("data", (data) => (stderr += data));
        result.on("error", (err) => {
            reject(err);
        });
        result.on("close", (code) => {
            if (code) {
                console.warn(`compiler returned error code ${code}. stderr: ${stderr}`);
            }
            resolve(JSON.parse(stdout) as T);
        });
        result.stdin.end(text, "utf-8");
    }).catch((err) => {
        console.error(`error invoking compiler: ${err}`);
        return Promise.resolve(null);
    });
}

connection.onDidChangeWatchedFiles((_change) => {
    // Monitored files have change in VSCode
    connection.console.log("We received a file change event");
});

// This handler provides the initial list of the completion items.
// connection.onCompletion(
// 	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
// 		// The pass parameter contains the position of the text document in
// 		// which code complete got requested. For the example we ignore this
// 		// info and always provide the same completion items.
// 		return [
// 			{
// 				label: 'TypeScript',
// 				kind: CompletionItemKind.Text,
// 				data: 1
// 			},
// 			{
// 				label: 'JavaScript',
// 				kind: CompletionItemKind.Text,
// 				data: 2
// 			}
// 		];
// 	}
// );

// This handler resolves additional information for the item selected in
// the completion list.
// connection.onCompletionResolve(
// 	(item: CompletionItem): CompletionItem => {
// 		if (item.data === 1) {
// 			item.detail = 'TypeScript details';
// 			item.documentation = 'TypeScript documentation';
// 		} else if (item.data === 2) {
// 			item.detail = 'JavaScript details';
// 			item.documentation = 'JavaScript documentation';
// 		}
// 		return item;
// 	}
// );

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
