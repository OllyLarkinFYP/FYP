import * as path from "path";
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

export const generateConfig = (module: vscode.TextDocument) => {
    const moduleName = path.basename(module.fileName, ".v");
    const modulePath = path.dirname(module.fileName);
    const configUri = vscode.Uri.parse(
        path.join("file:" + modulePath, `${moduleName}.simconfig.json`)
    );
    const job: OutgoingJob = {
        methodName: "getPortNames",
        parameters: [
            {
                name: module.fileName,
                contents: module.getText(),
            },
        ],
    };
    executeJob(job, (variables: { name: string; input: boolean }[]) => {
        if (validate(variables)) {
            if (variables.length === 0) {
                const errMsg = `The verilog file ${moduleName}.v does not define any ports and cannot be simulated.`;
                console.error(errMsg);
                vscode.window.showErrorMessage(errMsg);
            } else {
                const configObj = {
                    cycles: 10,
                    "requested vars": variables.map(({ name, input }) => {
                        return {
                            name: name,
                            breakdown: !input,
                        };
                    }),
                    inputs: variables
                        .filter(({ input }) => input)
                        .map(({ name }) => {
                            if (name.includes("clk") || name.includes("CLK")) {
                                return {
                                    name: name,
                                    repeating: true,
                                    values: ["0", "1"],
                                };
                            } else {
                                return {
                                    name: name,
                                    repeating: true,
                                    values: ["0", "1", "x"],
                                };
                            }
                        }),
                };
                const configText = JSON.stringify(configObj, null, 4);
                vscode.workspace.fs
                    .writeFile(configUri, Buffer.from(configText))
                    .then(
                        () =>
                            vscode.workspace
                                .openTextDocument(configUri)
                                .then((config) =>
                                    vscode.window.showTextDocument(config)
                                ),
                        () => {
                            console.error(
                                `Failed to create file ${configUri.toString()}`
                            );
                            vscode.window.showErrorMessage(
                                `Unable to automatically create simconfig file for ${moduleName}.v`
                            );
                        }
                    );
            }
        } else {
            vscode.window.showErrorMessage(
                `Unable to automatically create simconfig file for ${moduleName}.v`
            );
        }
    });
};
