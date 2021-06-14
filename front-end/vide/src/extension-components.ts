import * as path from "path";
import * as vscode from "vscode";
import { SimConfig } from "./simconfig";

type Wave = {
    name: string;
    wave: string;
    data?: string[];
};

type WaveGroup = Wave | [string, ...WaveGroup[]];

type WaveJSON = {
    signal: WaveGroup[];
    head: { tock: number };
    foot: { tock: number };
    config: { hscale: number };
};

export type SimulationData = {
    name: string;
    values: string[];
}[];

const getWave = (name: string, values: string[], index?: number): Wave => {
    let wave = "";
    let dataArr: string[] = [];
    let prevValue = "";
    values.forEach((valueStr) => {
        const v =
            index === undefined
                ? valueStr
                : valueStr[valueStr.length - 1 - index];
        if (v === prevValue) {
            wave += ".";
        } else {
            if (v.length > 1) {
                wave += "=";
                dataArr.push(valueStr);
            } else {
                wave += v === "1" ? "h" : v === "0" ? "l" : "x";
            }
            prevValue = v;
        }
    });
    return { name: name, wave: wave, data: dataArr };
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
    private static simulationData?: SimulationData = undefined;
    private static waveJSON: WaveJSON = {
        signal: [],
        head: { tock: 1 },
        foot: { tock: 1 },
        config: { hscale: 2 },
    };
    static setHTML() {
        if (this.waveformView && this.context) {
            const defaultJSUri = this.waveformView.webview.asWebviewUri(
                vscode.Uri.file(
                    path.join(
                        this.context.extensionPath,
                        "resources",
                        "scripts",
                        "default.js"
                    )
                )
            );
            const wavedromJSUri = this.waveformView.webview.asWebviewUri(
                vscode.Uri.file(
                    path.join(
                        this.context.extensionPath,
                        "resources",
                        "scripts",
                        "wavedrom.min.js"
                    )
                )
            );
            this.waveformView.webview.html = `
                <!DOCTYPE html>
                <html lang="en">
                    <head>
                        <script src="${defaultJSUri}" type="text/javascript"></script>
                        <script src="${wavedromJSUri}" type="text/javascript"></script>
                    </head>
                    <body style="background-color:white;" onload="WaveDrom.ProcessAll()">
                        <script type="WaveDrom">
                        ${JSON.stringify(this.waveJSON)}
                        </script>
                    </body>
                </html>`;
        }
    }
    static zoomInWave() {
        if (this.waveformView) {
            this.waveJSON.config.hscale += 1;
            this.setHTML();
        }
    }
    static zoomOutWave() {
        if (this.waveformView && this.waveJSON.config.hscale > 1) {
            this.waveJSON.config.hscale -= 1;
            this.setHTML();
        }
    }
    static setSimulationDataAndDisplay(
        data: SimulationData,
        config: SimConfig
    ) {
        this.simulationData = data;
        this.waveJSON.signal = data.map(({ name: name, values }): WaveGroup => {
            const regVar = config["requested vars"].find(
                ({ name: varName }) => varName === name
            );
            if (regVar && regVar.breakdown && values[0].length > 1) {
                let waveGroup: WaveGroup = [name, getWave("full", values)];
                for (let i = 0; i < values[0].length; i++) {
                    waveGroup.push(getWave(`[${i}]`, values, i));
                }
                return waveGroup;
            }
            return getWave(name, values);
        });
        this.displayWaveform();
    }
    static displayWaveform() {
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

        this.setHTML();
    }
}
