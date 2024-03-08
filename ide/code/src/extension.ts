import {
    workspace,
    ExtensionContext,
    window,
    StatusBarAlignment,
    MarkdownString,
    commands,
} from "vscode";
import { LanguageClient, State } from "vscode-languageclient/node";

let client: LanguageClient;

const CMD_STOP = "ctlsp.stop_server";
const CMD_RESTART = "ctlsp.restart_server";
const message = {
    [State.Running]: "Running",
    [State.Stopped]: "Stopped",
    [State.Starting]: "Starting...",
};

const statusItem = window.createStatusBarItem(StatusBarAlignment.Left);

export function activate(context: ExtensionContext) {
    client = new LanguageClient(
        "ctlsp",
        "CTL Language Server",
        {
            debug: {
                command: "$CARGO_TARGET_DIR/debug/ctl",
                args: ["lsp"],
                options: {
                    env: {
                        ...process.env,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        RUST_LOG: "debug",
                    },
                    shell: true,
                },
            },
            run: {
                command: (workspace.getConfiguration("ctlsp").get("compiler") as any).path,
                args: ["lsp"],
                options: {
                    env: {
                        ...process.env,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        RUST_LOG: "debug",
                    },
                },
            },
        },
        {
            documentSelector: [{ scheme: "file", language: "ctl" }],
            synchronize: {
                // Notify the server about file changes to '.clientrc files contained in the workspace
                fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
            },
        }
    );

    context.subscriptions.push(
        commands.registerCommand(CMD_STOP, async () => {
            await client.stop();
        })
    );

    context.subscriptions.push(
        commands.registerCommand(CMD_RESTART, async () => {
            if (client.isRunning()) {
                await client.stop();
            }
            await client.start();
        })
    );

    client.onDidChangeState((e) => updateTooltip(e.newState));
    client.start();
}

export function deactivate() {
    return client?.stop();
}

function updateTooltip(state: State) {
    const running = state === State.Running;
    statusItem.tooltip = new MarkdownString("", true);
    statusItem.tooltip.isTrusted = true;

    statusItem.tooltip.appendMarkdown(message[state]);
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
