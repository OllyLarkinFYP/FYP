import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import Extension from "../extension-components";
import { ErrorMsg, processErrors } from "../utils/error-msg";
import { getFilesStartingWith } from "../utils/get-files";

type CompilerReturn = {
    status: string;
    errors: ErrorMsg[];
    warnings: ErrorMsg[];
};

const checkReturnType = (reply: CompilerReturn) => {
    return reply.status !== undefined && reply.errors && reply.warnings;
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
    const files = await getFilesStartingWith(topLevelDoc);
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
