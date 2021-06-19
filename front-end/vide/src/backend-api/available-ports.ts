import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";

const validate = (variables: { name: string; input: boolean }[]) => {
    if (variables) {
        return variables.every(
            (elem) => elem.name !== undefined && elem.input !== undefined
        );
    } else {
        return false;
    }
};

export const getPorts = async (
    module: vscode.TextDocument,
    callBack: (variables: { name: string; input: boolean }[]) => void
) => {
    const job: OutgoingJob = {
        methodName: "getPortNames",
        parameters: [
            {
                name: module.fileName,
                contents: module.getText(),
            },
        ],
    };

    executeJob(job, (reply: { name: string; input: boolean }[]) => {
        if (validate(reply)) {
            callBack(reply);
        } else {
            console.error(
                "Could not get port information as backend did not return expected format."
            );
            vscode.window.showErrorMessage(
                "Could not get port information as backend did not return expected format."
            );
        }
    });
};
