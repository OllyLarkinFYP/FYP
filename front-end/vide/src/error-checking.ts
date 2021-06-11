import * as vscode from "vscode";
import { compile } from "./backend-api/compile";
import Extension from "./extension-components";

export const initialiseErrorChecking = (context: vscode.ExtensionContext) => {
    if (
        vscode.window.activeTextEditor &&
        vscode.window.activeTextEditor.document.languageId === "verilog"
    ) {
        compile(vscode.window.activeTextEditor.document);
    }

    context.subscriptions.push(
        vscode.workspace.onDidSaveTextDocument((doc) => {
            if (doc.languageId === "verilog") {
                compile(doc);
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument((docChanges) => {
            if (docChanges.document.languageId === "verilog") {
                compile(docChanges.document);
            }
        })
    );

    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor((editor) => {
            if (editor?.document.languageId === "verilog") {
                compile(editor.document);
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidCloseTextDocument((doc) => {
            Extension.deleteFromDiagnostics(doc.uri);
        })
    );
};
