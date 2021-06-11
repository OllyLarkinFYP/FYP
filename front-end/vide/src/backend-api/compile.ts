import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../execute-job";
import Extension from "../extension-components";
import { indentString } from "../utils/indent-string";

type ErrorMsg = {
    file: string;
    line: number;
    column: number;
    message: string;
};

type CompilerReturn = {
    status: string;
    errors: ErrorMsg[];
    warnings: ErrorMsg[];
};

const checkReturnType = (reply: CompilerReturn) => {
    return (
        reply.status !== undefined &&
        reply.errors !== undefined &&
        reply.warnings !== undefined
    );
};

const processErrors = (errors: ErrorMsg[], areWarnings: boolean = false) => {
    const diagMap: Map<string, vscode.Diagnostic[]> = new Map();
    const unlistedErrs: string[] = [];
    errors.forEach((error) => {
        if (error.file) {
            const filePath = "/" + error.file.replace(/\\/g, "/");
            // TODO: make it underline token
            const range = new vscode.Range(
                error.line - 1,
                0,
                error.line - 1,
                error.column
            );
            let diagnostics = diagMap.get(filePath);
            if (!diagnostics) {
                diagnostics = [];
            }
            diagnostics.push(
                new vscode.Diagnostic(
                    range,
                    error.message,
                    areWarnings
                        ? vscode.DiagnosticSeverity.Warning
                        : vscode.DiagnosticSeverity.Error
                )
            );
            diagMap.set(filePath, diagnostics);
        } else {
            unlistedErrs.push(error.message);
        }
    });
    Extension.setDiagnostics(
        Array.from(diagMap).map(([filePath, diags]) => [
            vscode.Uri.parse(filePath),
            diags,
        ])
    );
    if (unlistedErrs.length > 0) {
        const msg = indentString(
            unlistedErrs.reduce((prev, curr) => prev + "\n\n" + curr)
        );
        areWarnings
            ? console.warn(
                  `Warnings were generated when trying to build the project:\n\n${msg}`
              )
            : console.error(
                  `Errors were generated when trying to build the project:\n\n${msg}`
              );
    }
};

// API:
//      methodName: "compile"
//      parameters:
//          files:
//              array
//                  name: string
//                  contents: string
//          topLevel: string
//      returnType:
//          status: "success" | "failure" | "warnings" | "invalid_call"
//          errors:
//              array
//                  file: string
//                  line: int64
//                  column: int64
//                  message: string
//          warnings:
//              array
//                  file: string
//                  line: int64
//                  column: int64
//                  message: string
export const compile = async (topLevelDoc: vscode.TextDocument) => {
    const workspaceUris = await vscode.workspace.findFiles("**/*.v");
    const fileUris = workspaceUris.filter(
        (uri) => uri.toString() !== topLevelDoc.uri.toString()
    );

    const files = await Promise.all(
        fileUris.map(async (uri) => {
            let fileContArr = await vscode.workspace.fs.readFile(uri);
            let fileContStr = Buffer.from(fileContArr).toString();
            return {
                name: uri.fsPath,
                contents: fileContStr,
            };
        })
    );
    files.unshift({
        name: topLevelDoc.uri.fsPath,
        contents: topLevelDoc.getText(),
    });

    const job: OutgoingJob = {
        methodName: "compile",
        // topLevel parameter is blank so that it uses the first file
        parameters: [files, ""],
    };
    executeJob(job, (reply: CompilerReturn) => {
        if (checkReturnType(reply)) {
            switch (reply.status) {
                case "success":
                    Extension.clearDiagnostics();
                    break;
                case "warnings":
                    processErrors(reply.warnings, true);
                    break;
                case "failure":
                    processErrors(reply.errors);
                    break;
                default:
                    vscode.window.showErrorMessage(
                        "The call to the backend was invalid."
                    );
                    break;
            }
        } else {
            vscode.window.showErrorMessage(
                "Backend did not return the expected format for the 'compile' function."
            );
        }
    });
};
