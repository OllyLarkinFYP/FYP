import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
    console.log('Congratulations, your extension "vide" is now active!');

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.helloWorld", () => {
            vscode.window.showInformationMessage("Hello from VIDE!");
        })
    );
}

export function deactivate() {}
