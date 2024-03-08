import * as path from "path";
import { workspace, ExtensionContext, window, StatusBarAlignment, MarkdownString, commands } from "vscode";
import { LanguageClient, ServerOptions, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient;

const CMD_STOP = "ctlsp.stop_server";
const CMD_RESTART = "ctlsp.restart_server";

const statusItem = window.createStatusBarItem(StatusBarAlignment.Left);

export function activate(context: ExtensionContext) {
    // The server is implemented in node
    const serverModule = context.asAbsolutePath(path.join("server", "out", "server.js"));

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
        },
    };

    client = new LanguageClient("ctlsp", "CTL Language Server", serverOptions, {
        documentSelector: [{ scheme: "file", language: "ctl" }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
        },
    });

    context.subscriptions.push(commands.registerCommand(CMD_STOP, async () => {
        await client?.stop();
        updateTooltip();
    }));

    context.subscriptions.push(commands.registerCommand(CMD_RESTART, async () => {
        if (!client) {
            return;
        }

        await client.stop();
        await client.start();
        updateTooltip();
    }));

    (async () => {
        await client.start();
        updateTooltip();
    })();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}

function updateTooltip() {
    const running = client.isRunning();
    statusItem.tooltip = new MarkdownString("", true);
    statusItem.tooltip.isTrusted = true;

    statusItem.tooltip.appendMarkdown(running ? "Running" : "Stopped");
    statusItem.tooltip.appendMarkdown("\n\n---\n\n");
    if (running) {
        statusItem.text = "ctlsp";
        statusItem.tooltip.appendMarkdown(`\n\n[Stop Server](command:${CMD_STOP})`);
        statusItem.command = CMD_STOP;
    } else {
        statusItem.text = "$(stop-circle) ctlsp";
        statusItem.command = CMD_RESTART;
    }

    statusItem.tooltip.appendMarkdown(
        `\n\n[${running ? "Res" : "S"}tart Server](command:${CMD_RESTART})`
    );
    statusItem.show();
}
