import * as vscode from "vscode";
import Extension from "./extension-components";
import { initialiseErrorChecking } from "./error-checking";
import { simulateFromModule, simulateFromConfig } from "./backend-api/simulate";

export function activate(context: vscode.ExtensionContext) {
    console.log("VIDE is now active");

    Extension.setContext(context);

    const diagnosticsCollection = vscode.languages.createDiagnosticCollection();
    context.subscriptions.push(diagnosticsCollection);
    Extension.initDiagnostics(diagnosticsCollection);

    const outputChannel = vscode.window.createOutputChannel("VIDE");
    context.subscriptions.push(outputChannel);
    Extension.initOutputChannel(outputChannel);

    initialiseErrorChecking(context);

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.simulate", () => {
            if (
                vscode.window.activeTextEditor &&
                vscode.window.activeTextEditor.document.languageId === "verilog"
            ) {
                simulateFromModule(vscode.window.activeTextEditor.document);
            } else if (
                vscode.window.activeTextEditor &&
                /.*\.simconfig\.json/.test(
                    vscode.window.activeTextEditor.document.fileName
                )
            ) {
                simulateFromConfig(vscode.window.activeTextEditor.document);
            } else {
                vscode.window.showErrorMessage(
                    "There is no active editor or open module to simulate"
                );
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.zoomInWave", () => {
            Extension.zoomInWave();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.zoomOutWave", () => {
            Extension.zoomOutWave();
        })
    );
}

export function deactivate() {}
