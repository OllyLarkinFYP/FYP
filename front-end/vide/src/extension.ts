import * as vscode from "vscode";
import Extension from "./extension-components";
import { initialiseErrorChecking } from "./error-checking";

export function activate(context: vscode.ExtensionContext) {
    console.log("VIDE is now active");

    const diagnosticsCollection = vscode.languages.createDiagnosticCollection();
    context.subscriptions.push(diagnosticsCollection);
    Extension.initDiagnostics(diagnosticsCollection);

    const outputChannel = vscode.window.createOutputChannel("VIDE");
    context.subscriptions.push(outputChannel);
    Extension.initOutputChannel(outputChannel);

    initialiseErrorChecking(context);

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.simulate", () => {
            vscode.window.showInformationMessage("Tried to simulate");
        })
    );
}

export function deactivate() {}
