import path = require("path");
import * as vscode from "vscode";

type Wave = {
    name: string;
    wave: string;
    data?: string[];
};

type WaveGroup = Wave | [string, WaveGroup];

type WaveJSON = {
    signal: WaveGroup[];
    head: { tock: number };
    foot: { tock: number };
    config: { hscale: number };
};

export default class Extension {
    private static context?: vscode.ExtensionContext = undefined;
    static setContext(context: vscode.ExtensionContext) {
        this.context = context;
    }

    private static outChannel?: vscode.OutputChannel = undefined;
    static initOutputChannel(outChannel: vscode.OutputChannel) {
        this.outChannel = outChannel;
        this.outChannel.hide();
    }
    static sendErrorToOutputChannel(error: string, message: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(message);
        // this.outChannel?.show();
        vscode.window.showErrorMessage(
            `${error}. Check 'VIDE' output log for more details.`
        );
        console.error(message);
    }
    static sendWarningToOutputChannel(warning: string, message: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(message);
        // this.outChannel?.show();
        vscode.window.showWarningMessage(
            `${warning}. Check 'VIDE' output log for more details.`
        );
        console.warn(message);
    }
    static sendInfoToOutputChannel(info: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(info);
        this.outChannel?.show();
        console.log(info);
    }

    private static diagnosticsCollection?: vscode.DiagnosticCollection;
    static initDiagnostics(diagCollection: vscode.DiagnosticCollection) {
        this.diagnosticsCollection = diagCollection;
    }
    static setDiagnostics(diagnostics: [vscode.Uri, vscode.Diagnostic[]][]) {
        this.diagnosticsCollection?.clear();
        diagnostics.forEach(([uri, diags]) => {
            this.diagnosticsCollection?.set(uri, diags);
        });
    }
    static clearDiagnostics() {
        this.diagnosticsCollection?.clear();
    }
    static deleteFromDiagnostics(uri: vscode.Uri) {
        this.diagnosticsCollection?.delete(uri);
    }

    private static waveformView?: vscode.WebviewPanel = undefined;
    private static waveJSON?: WaveJSON = {
        signal: [
            { name: "Wave 1", wave: "10101010" },
            {
                name: "Wave 2",
                wave: "========",
                data: ["A", "B", "C", "D", "E", "F", "G", "H"],
            },
            ["G1", { name: "Wave 3", wave: "01010101" }],
        ],
        head: { tock: 1 },
        foot: { tock: 1 },
        config: { hscale: 1 },
    };
    static setHTML(wave: WaveJSON) {
        if (this.waveformView) {
            this.waveformView.webview.html = `<!DOCTYPE html>
            <html lang="en">
                <head>
                    <script src="https://cdnjs.cloudflare.com/ajax/libs/wavedrom/2.6.8/skins/default.js" type="text/javascript"></script>
                    <script src="https://cdnjs.cloudflare.com/ajax/libs/wavedrom/2.6.8/wavedrom.min.js" type="text/javascript"></script>
                </head>
                <body style="background-color:white;" onload="WaveDrom.ProcessAll()">
                    <script type="WaveDrom">
                    ${JSON.stringify(wave)}
                    </script>
                </body>
            </html>`;
        }
    }
    static zoomInWave() {
        if (this.waveformView && this.waveJSON) {
            this.waveJSON.config.hscale += 1;
            this.setHTML(this.waveJSON);
        }
    }
    static zoomOutWave() {
        if (
            this.waveformView &&
            this.waveJSON &&
            this.waveJSON.config.hscale > 1
        ) {
            this.waveJSON.config.hscale -= 1;
            this.setHTML(this.waveJSON);
        }
    }
    static displayWaveform() {
        if (this.context && this.waveJSON) {
            if (!this.waveformView) {
                this.waveformView = vscode.window.createWebviewPanel(
                    "vide",
                    "Waveform",
                    vscode.ViewColumn.Active,
                    { enableScripts: true }
                );
                vscode.commands.executeCommand(
                    "setContext",
                    "vide.waveformOpen",
                    true
                );
                vscode.commands.executeCommand(
                    "workbench.action.moveEditorToBelowGroup"
                );
                this.waveformView.onDidDispose(() => {
                    this.waveformView = undefined;
                    vscode.commands.executeCommand(
                        "setContext",
                        "vide.waveformOpen",
                        false
                    );
                    console.log("Waveform view disposed.");
                });
            }

            // const defaultScriptPath = this.waveformView.webview.asWebviewUri(
            //     vscode.Uri.file(
            //         path.join(
            //             this.context.extensionPath,
            //             "resources",
            //             "sripts",
            //             "default.js"
            //         )
            //     )
            // );
            // const wavedromScriptPath = this.waveformView.webview.asWebviewUri(
            //     vscode.Uri.file(
            //         path.join(
            //             this.context.extensionPath,
            //             "resources",
            //             "sripts",
            //             "wavedrom.min.js"
            //         )
            //     )
            // );

            this.setHTML(this.waveJSON);
        } else {
            console.error("No waveJSON to visualise...");
        }
    }
}
