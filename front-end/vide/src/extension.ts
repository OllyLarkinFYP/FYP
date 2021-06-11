import * as vscode from "vscode";
import { compile } from "./backend-api/compile";

export function activate(context: vscode.ExtensionContext) {
    console.log("VIDE is now active");

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.helloWorld", () => {
            vscode.window.showInformationMessage("Hello from VIDE!");
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("vide.compile", () => {
            compile();
        })
    );
}

export function deactivate() {}
