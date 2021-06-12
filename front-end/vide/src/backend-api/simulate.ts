import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import Extension from "../extension-components";
import { getFilesStartingWith } from "../utils/get-files";

export const simulate = async (topLevelDoc: vscode.TextDocument) => {
    // TODO: check if input json already exists
    // TODO: if not create template, open template, notify user, and exit
    // TODO: if so, make a call to simulate
    vscode.window.showInformationMessage("Simualting");
    console.log(topLevelDoc);
};
