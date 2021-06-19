import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import { getFilesStartingWith } from "../utils/get-files";

const validateVariableList = (variables: string[]) =>
    Array.isArray(variables) && variables.every((elem) => elem !== undefined);

export const getVariables = async (
    module: vscode.TextDocument,
    callBack: (variables: string[]) => void
) => {
    const files = await getFilesStartingWith(module);
    const job: OutgoingJob = {
        methodName: "getVariables",
        parameters: [files],
    };

    executeJob(job, (reply: string[]) => {
        if (validateVariableList(reply)) {
            callBack(reply);
        } else {
            console.error(
                "Could not get project variables as the backend did not return the correct format."
            );
            vscode.window.showErrorMessage(
                "Could not get project variables as the backend did not return the correct format."
            );
        }
    });
};
