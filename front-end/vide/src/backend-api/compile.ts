import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../execute-job";
import ExtensionComponents from "../extension-components";
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

const getErrorMessageString = (errorMsgs: ErrorMsg[]) => {
    return indentString(
        errorMsgs
            .map((errorMsg) => {
                return (
                    `\n${errorMsg.message}\n` +
                    `file: ${errorMsg.file}\n` +
                    `line: ${errorMsg.line}\n` +
                    `column: ${errorMsg.column}\n`
                );
            })
            .reduce((prev, curr) => {
                return prev + curr;
            })
    );
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
export const compile = async () => {
    const topLevel = await vscode.window.showInputBox({
        title: "Please enter the name of the top level module",
    });
    if (topLevel) {
        const fileUris = await vscode.workspace.findFiles("**/*.v");
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
        const job: OutgoingJob = {
            methodName: "compile",
            parameters: [files, topLevel],
        };
        console.time("compile");
        executeJob(job, (reply: CompilerReturn) => {
            if (checkReturnType(reply)) {
                switch (reply.status) {
                    case "success":
                        vscode.window.showInformationMessage(
                            "Compiled the project successfully!"
                        );
                        break;
                    case "warnings":
                        ExtensionComponents.sendWarningToOutputChannel(
                            "Compiled with warnings.",
                            `Compiled with warnings:\n${getErrorMessageString(
                                reply.warnings
                            )}`
                        );
                        break;
                    case "failure":
                        ExtensionComponents.sendErrorToOutputChannel(
                            "Compilation failed with errors.",
                            `Compilation failed with errors:\n${getErrorMessageString(
                                reply.errors
                            )}`
                        );
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
            console.timeEnd("compile");
        });
    }
};
