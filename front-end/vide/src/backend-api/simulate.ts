import * as vscode from "vscode";
import { executeJob, OutgoingJob } from "../utils/execute-job";
import Extension, { SimulationData } from "../extension-components";
import { getFilesStartingWith } from "../utils/get-files";
import { generateConfig } from "../generate-simconfig";
import { SimConfig, validateSimConfig } from "../simconfig";
import { ErrorMsg, processErrors } from "../utils/error-msg";
import { getVariables } from "./available-variables";
import { indentString } from "../utils/indent-string";
import { getPorts } from "./available-ports";

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

const getInvalidReqs = (config: SimConfig, available: string[]): string[] => {
    let invalidVars: string[] = [];
    config["requested vars"].map(({ name }) => {
        if (!available.includes(name)) {
            invalidVars.push(name);
        }
    });
    return invalidVars;
};

const duplicatReqVars = (config: SimConfig): boolean => {
    const names = config["requested vars"].map(({ name }) => name);
    const duplicates = names.filter(
        (name, index) => names.indexOf(name) !== index
    );
    return duplicates.length > 0;
};

const duplicatInputs = (config: SimConfig): boolean => {
    const names = config.inputs.map(({ name }) => name);
    const duplicates = names.filter(
        (name, index) => names.indexOf(name) !== index
    );
    return duplicates.length > 0;
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

    getVariables(topLevelDoc, async (availableVars) => {
        const invalidReqs = getInvalidReqs(config, availableVars);
        if (invalidReqs.length > 0) {
            Extension.sendErrorToOutputChannel(
                "Some requested variables could not be found in the compiled netlist",
                "\nThe following requested variables were not found in the compiled netlist:\n" +
                    indentString(invalidReqs.join("\n")) +
                    "\n" +
                    "The available variables are:\n" +
                    indentString(availableVars.join("\n"))
            );
        } else if (duplicatReqVars(config)) {
            console.error("Duplicate requested vars");
            vscode.window.showErrorMessage(
                "Some variables have been requested multiple times. Duplicated requested variables are not supported."
            );
        } else if (duplicatInputs(config)) {
            console.error("Duplicate input");
            vscode.window.showErrorMessage(
                "Some inputs have been provided multiple times. Duplicated inputs are not supported."
            );
        } else {
            getPorts(
                topLevelDoc,
                (ports: { name: string; input: boolean }[]) => {
                    let invalidPorts: string[] = [];
                    config.inputs.forEach((inp) => {
                        if (
                            !ports.find(
                                ({ name, input }) => input && inp.name === name
                            )
                        ) {
                            invalidPorts.push(inp.name);
                        }
                    });
                    if (invalidPorts.length > 0) {
                        Extension.sendWarningToOutputChannel(
                            "Some provided inputs for simulation could not be found in the compiled netlist",
                            "These input variables could not be found in the netlist:\n" +
                                indentString(invalidPorts.join("\n")) +
                                "\n" +
                                "The available ports are:\n" +
                                indentString(
                                    ports
                                        .filter(({ input }) => input)
                                        .map(({ name }) => name)
                                        .join("\n")
                                )
                        );
                    }
                }
            );

            const files = await getFilesStartingWith(topLevelDoc);
            const job: OutgoingJob = {
                methodName: "simulate",
                parameters: [
                    files,
                    "", // this should be blank so that it uses the first file
                    config.inputs,
                    config["requested vars"].map(({ name }) => name),
                    config.cycles,
                ],
            };

            executeJob(job, (reply: SimulatorReturn) => {
                if (validateSimulatorReturn(reply)) {
                    switch (reply.status) {
                        case "success":
                            Extension.clearDiagnostics();
                            Extension.setSimulationDataAndDisplay(
                                reply.output,
                                config
                            );
                            break;
                        case "warnings":
                            processErrors(reply.warnings, true);
                            Extension.setSimulationDataAndDisplay(
                                reply.output,
                                config
                            );
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
                    console.error(
                        "Backend did not return the expected format."
                    );
                    vscode.window.showErrorMessage(
                        "Backend simulation did not return the expected format."
                    );
                }
            });
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
