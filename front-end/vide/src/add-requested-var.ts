import * as vscode from "vscode";
import { getVariables } from "./backend-api/available-variables";
import { SimConfig, validateSimConfig, Format } from "./simconfig";

export const addRequestedVar = (configDoc: vscode.TextDocument) => {
    let config: SimConfig;
    try {
        config = JSON.parse(configDoc.getText());
        if (!validateSimConfig(config)) {
            throw new Error();
        }
    } catch {
        console.error("Failed to read simconfig file: ", configDoc.fileName);
        vscode.window.showErrorMessage(
            "Could not add requested variable as failed to read simconfig file. Please make sure it is the correct format and retry."
        );
        return;
    }

    const modulePath = configDoc.uri.toString().slice(0, -15) + ".v";
    vscode.workspace.openTextDocument(vscode.Uri.parse(modulePath)).then(
        (module) =>
            getVariables(module, (variables) =>
                vscode.window
                    .showQuickPick(variables, {
                        canPickMany: false,
                        title: "Add new requested variable (1/3)",
                        placeHolder:
                            "Please choose a variable or press escape to cancel",
                    })
                    .then((variable) => {
                        if (variable) {
                            vscode.window
                                .showQuickPick(["true", "false"], {
                                    canPickMany: false,
                                    title: "Add new requested variable (2/3)",
                                    placeHolder:
                                        "Should it also break down into individual bits?",
                                })
                                .then((breakdown) => {
                                    if (breakdown) {
                                        vscode.window
                                            .showQuickPick(
                                                ["bin", "oct", "dec", "hex"],
                                                {
                                                    canPickMany: false,
                                                    title: "Add new requested variable (3/3)",
                                                    placeHolder:
                                                        "What base should the output be?",
                                                }
                                            )
                                            .then((base) => {
                                                if (base) {
                                                    config[
                                                        "requested vars"
                                                    ].push({
                                                        name: variable,
                                                        breakdown:
                                                            breakdown ===
                                                            "true",
                                                        format: base as Format,
                                                    });
                                                    vscode.workspace.fs.writeFile(
                                                        configDoc.uri,
                                                        Buffer.from(
                                                            JSON.stringify(
                                                                config,
                                                                null,
                                                                4
                                                            )
                                                        )
                                                    );
                                                }
                                            });
                                    }
                                });
                        }
                    })
            ),
        () => {
            console.error(
                "Could not find the module associated with this simconfig file."
            );
            vscode.window.showErrorMessage(
                "Could not find the module associated with this simconfig file."
            );
        }
    );
};
