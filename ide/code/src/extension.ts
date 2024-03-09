import {
    workspace,
    ExtensionContext,
    window,
    StatusBarAlignment,
    MarkdownString,
    commands,
} from "vscode";
import { LanguageClient, State } from "vscode-languageclient/node";


const CMD_STOP = "ctlsp.stop_server";
const CMD_RESTART = "ctlsp.restart_server";
const message = {
    [State.Running]: "Running",
    [State.Stopped]: "Stopped",
    [State.Starting]: "Starting...",
};

const statusItem = window.createStatusBarItem(StatusBarAlignment.Left);

let client: LanguageClient;

export function activate(context: ExtensionContext) {
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

    initClient();

    workspace.onDidChangeConfiguration(async (e) => {
        if (e.affectsConfiguration("ctlsp.compiler")) {
            const result = await window.showInformationMessage(
                "Compiler path changed. Restart server?",
                "Restart",
                "Cancel"
            );

            if (result === "Restart") {
                if (client.isRunning()) {
                    await client.stop();
                }

                initClient();
            }
        }
    });
}

export function deactivate() {
    return client?.stop();
}

function initClient() {
    client = new LanguageClient(
        "ctlsp",
        "CTL Language",
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
                command: workspace.getConfiguration("ctlsp.compiler").get<string>("path"),
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
    client.onDidChangeState((e) => updateTooltip(e.newState));
    client.start();
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
