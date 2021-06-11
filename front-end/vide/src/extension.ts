import * as vscode from "vscode";
import Extension from "./extension-components";
import { compile } from "./backend-api/compile";

export function activate(context: vscode.ExtensionContext) {
    console.log("VIDE is now active");

    const diagnosticsCollection =
        vscode.languages.createDiagnosticCollection("VIDE");
    context.subscriptions.push(diagnosticsCollection);
    Extension.initDiagnostics(diagnosticsCollection);

    const outputChannel = vscode.window.createOutputChannel("VIDE");
    context.subscriptions.push(outputChannel);
    Extension.initOutputChannel(outputChannel);

    vscode.workspace.onDidSaveTextDocument((doc: vscode.TextDocument) => {
        if (doc.languageId === "verilog") {
            // TODO: compile with this doc as top level
            // TODO: ideally have a back-end API function that can compile an array of file contents, assuming the first is top level
            // TODO: get all .v files but remove the current doc
        }
    });
}

export function deactivate() {}
