import * as path from "path";
import * as vscode from "vscode";
import Extension from "../extension-components";
import { indentString } from "./indent-string";

export type ErrorPosition = {
    line: number;
    column: number;
};

export type ErrorMsg = {
    file: string;
    start: ErrorPosition;
    finish: ErrorPosition;
    message: string;
};

export const processErrors = (
    errors: ErrorMsg[],
    areWarnings: boolean = false
) => {
    const diagMap: Map<string, vscode.Diagnostic[]> = new Map();
    const unlistedErrs: string[] = [];
    errors.forEach((error) => {
        if (error.file) {
            const filePath = "file:" + path.normalize(error.file);
            // TODO: make it underline token
            const range = new vscode.Range(
                error.start.line - 1,
                error.start.column - 1,
                error.finish.line - 1,
                error.finish.column - 1
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
        if (areWarnings) {
            console.warn(
                `Warnings were generated when trying to build the project:${msg}`
            );
            vscode.window.showWarningMessage(
                `Warnings were generated when trying to build the project:${msg}`
            );
        } else {
            console.error(
                `Errors were generated when trying to build the project:${msg}`
            );
            vscode.window.showErrorMessage(
                `Errors were generated when trying to build the project:${msg}`
            );
        }
    }
};
