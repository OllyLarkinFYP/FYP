import { exec } from "child_process";
import * as path from "path";
import * as vscode from "vscode";
import ExtensionComponents from "./extension-components";

const backendPath = path.join(__dirname, "../resources/back-end/CoreLogic.dll");

export type OutgoingJob = {
    id: number;
    methodName: string;
    parameters: any[];
};

export type IncomingReply = {
    id?: number;
    reply?: any;
};

export const executeJob = (
    job: OutgoingJob,
    replyCallBack?: (reply: any) => void
) => {
    console.log("Executing job:", job);
    exec(
        `dotnet ${backendPath} -j ${JSON.stringify(JSON.stringify(job))}`,
        (err, stdout, stderr) => {
            if (!err) {
                if (stderr) {
                    ExtensionComponents.sendErrorToOutputChannel(
                        "Backend produced errors while trying to process the request",
                        `Backend produced errors while trying to process the request:\n${stderr}`
                    );
                } else if (stdout) {
                    let response: IncomingReply = {};
                    try {
                        response = JSON.parse(stdout);
                    } catch (jsonErr) {
                        ExtensionComponents.sendErrorToOutputChannel(
                            "Backend did not return the result in a valid JSON format.",
                            `Backend did not return the result in a valid JSON format. Returned string:\n${stdout}`
                        );
                    }
                    if (
                        response.id !== undefined &&
                        response.reply !== undefined
                    ) {
                        console.log("Received reply:", response.reply);
                        if (replyCallBack) {
                            replyCallBack(response.reply);
                        }
                    } else {
                        ExtensionComponents.sendErrorToOutputChannel(
                            "Backend did not return the result in a valid format.",
                            `Backend did not return the result in a valid format. Returned string:\n${stdout}`
                        );
                    }
                } else {
                    console.error("Execution of job produced no output");
                }
            } else {
                ExtensionComponents.sendErrorToOutputChannel(
                    "Backend threw an exception while trying to process the request",
                    `Backend threw an exception while trying to process the request:\n${err.name}:\n${err.message}`
                );
            }
        }
    );
};
