import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
    console.log("VIDE is now active");

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.helloWorld", () => {
            vscode.window.showInformationMessage("Hello from VIDE!");
        })
    );
}

export function deactivate() {}
