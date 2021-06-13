import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import Extension, { SimulationData } from "../extension-components";
import { getFilesStartingWith } from "../utils/get-files";
import { generateConfig } from "./generate-simconfig";
import { SimConfig, validateSimConfig } from "../simconfig";
import { ErrorMsg, processErrors } from "../utils/error-msg";

type SimulatorReturn = {
    status: string;
    output: SimulationData;
    errors: ErrorMsg[];
    warnings: ErrorMsg[];
};

const validateSimulatorReturn = (simRet: SimulatorReturn) => {
    return (
        simRet.status !== undefined &&
        simRet.output &&
        simRet.output.every(({ name, values }) => name !== undefined && values)
    );
};

// API:
//      methodName: "simulate"
//      parameters:
//          files:
//              array
//                  name: string
//                  contents: string
//          topLevel: string
//          inputs:
//              array
//                  name: string
//                  repeating: boolean
//                  values: string[]
//          reqVars: array: string
//          cycles: uint
const simulate = async (
    topLevelDoc: vscode.TextDocument,
    configDoc: vscode.TextDocument
) => {
    let config: SimConfig;
    try {
        config = JSON.parse(configDoc.getText());
        if (!validateSimConfig(config)) {
            throw new Error();
        }
    } catch {
        console.error("Failed to read simconfig file: ", configDoc.fileName);
        vscode.window.showErrorMessage(
            `Failed to read simconfig file: ${configDoc.fileName}`
        );
        return;
    }

    const files = await getFilesStartingWith(topLevelDoc);
    const job: OutgoingJob = {
        methodName: "simulate",
        parameters: [
            files,
            "", // this should be blank so that it uses the first file
            config.inputs,
            config["requested vars"],
            config.cycles,
        ],
    };

    executeJob(job, (reply: SimulatorReturn) => {
        if (validateSimulatorReturn(reply)) {
            switch (reply.status) {
                case "success":
                    Extension.clearDiagnostics();
                    Extension.setSimulationDataAndDisplay(reply.output, config);
                    break;
                case "warnings":
                    processErrors(reply.warnings, true);
                    Extension.setSimulationDataAndDisplay(reply.output, config);
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
            console.error("Backend did not return the expected format.");
            vscode.window.showErrorMessage(
                "Backend simulation did not return the expected format."
            );
        }
    });
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
