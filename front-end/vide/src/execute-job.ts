import { exec } from "child_process";
import * as path from "path";
import * as vscode from "vscode";
import ExtensionComponents from "./extension-components";
import { indentString } from "./utils/indent-string";

const backendPath = path.join(__dirname, "../resources/back-end/CoreLogic.dll");

export type OutgoingJob = {
    methodName: string;
    parameters: any[];
};

type IncomingReply = {
    id?: number;
    reply?: any;
};

export const executeJob = (
    job: OutgoingJob,
    replyCallBack?: (reply: any) => void
) => {
    console.log("Executing job:", job);
    const jobString = JSON.stringify(JSON.stringify({ ...job, id: 0 }));
    exec(`dotnet ${backendPath} -j ${jobString}`, (err, stdout, stderr) => {
        if (!err) {
            if (stderr) {
                const indentedStdErr = indentString(stderr);
                ExtensionComponents.sendErrorToOutputChannel(
                    "Backend produced errors while trying to process the request",
                    `Backend produced errors while trying to process the request:\n${indentedStdErr}`
                );
            } else if (stdout) {
                let response: IncomingReply = {};
                try {
                    response = JSON.parse(stdout);
                } catch (jsonErr) {
                    const indentedStdOut = indentString(stdout);
                    ExtensionComponents.sendErrorToOutputChannel(
                        "Backend did not return the result in a valid JSON format.",
                        `Backend did not return the result in a valid JSON format. Returned string:\n${indentedStdOut}`
                    );
                }
                if (response.id !== undefined && response.reply !== undefined) {
                    console.log("Received reply:", response.reply);
                    if (replyCallBack) {
                        replyCallBack(response.reply);
                    }
                } else {
                    const indentedStdOut = indentString(stdout);
                    ExtensionComponents.sendErrorToOutputChannel(
                        "Backend did not return the result in a valid format.",
                        `Backend did not return the result in a valid format. Returned string:\n${indentedStdOut}`
                    );
                }
            } else {
                vscode.window.showErrorMessage(
                    "Backend did not produce any output."
                );
            }
        } else {
            ExtensionComponents.sendErrorToOutputChannel(
                "Backend threw an exception while trying to process the request",
                `Backend threw an exception while trying to process the request:\n${err.name}:\n${err.message}`
            );
        }
    });
};
