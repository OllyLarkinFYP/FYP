import * as path from "path";
import * as vscode from "vscode";
import Extension from "../extension-components";
import { indentString } from "./indent-string";

export type ErrorMsg = {
    file: string;
    line: number;
    column: number;
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
