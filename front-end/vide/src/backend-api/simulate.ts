import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import Extension from "../extension-components";
import { getFilesStartingWith } from "../utils/get-files";
import * as path from "path";
import { generateConfig } from "./generate-simconfig";

const simulate = async (
    topLevelDoc: vscode.TextDocument,
    configDoc: vscode.TextDocument
) => {
    // TODO: check if input json already exists
    // TODO: if not create template, open template, notify user, and exit
    // TODO: if so, make a call to simulate
    console.log("simulate");
    console.log("module:", topLevelDoc.uri.toString());
    console.log("config:", configDoc.uri.toString());
};

export const simulateFromModule = (topLevelDoc: vscode.TextDocument) => {
    const configPath =
        topLevelDoc.uri.toString().slice(0, -2) + ".simconfig.json";
    vscode.workspace.openTextDocument(vscode.Uri.parse(configPath)).then(
        (configDoc) => simulate(topLevelDoc, configDoc),
        () => {
            console.warn(
                `No simulation config file for module ${topLevelDoc.fileName}`
            );
            vscode.window
                .showWarningMessage(
                    "Could not find simualtion configuration file for this module",
                    "Create"
                )
                .then((create) => {
                    if (create) {
                        generateConfig(topLevelDoc);
                    }
                });
        }
    );
};

export const simulateFromConfig = (configDoc: vscode.TextDocument) => {
    const modulePath = configDoc.uri.toString().slice(0, -15) + ".v";
    vscode.workspace.openTextDocument(vscode.Uri.parse(modulePath)).then(
        (topLevelDoc) => simulate(topLevelDoc, configDoc),
        () => {
            const relativeModulePath =
                vscode.workspace.asRelativePath(modulePath);
            Extension.sendErrorToOutputChannel(
                "Could not find verilog module matching config.",
                "Could not find verilog module matching config. \n" +
                    `Expected to find verilog module at: ${relativeModulePath}\n\n` +
                    "Make sure that the verilog module is located in the same directory as the configuration file and that they have the same name (without extension)\n\n" +
                    "e.g.\n" +
                    "\tfolder1/folder2/moduleName.v  -->  folder1/folder2/moduleName.simconfig.json"
            );
        }
    );
};
