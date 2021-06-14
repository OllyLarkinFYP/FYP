import { execSync } from "child_process";
import * as vscode from "vscode";

const minVer = 5;

export const checkDotnetVersion = () => {
    const message = "VIDE requires a .NET runtime version 5.0 or higher";
    const showError = (preMsg: string) => {
        vscode.window
            .showErrorMessage(`${preMsg}. ${message}`, "Install")
            .then((response) => {
                if (response) {
                    vscode.env.openExternal(
                        vscode.Uri.parse(
                            "https://dotnet.microsoft.com/download"
                        )
                    );
                }
            });
        console.log("Invalid .NET version");
    };

    let stdout;
    try {
        stdout = execSync("dotnet --version").toString();
    } catch {
        showError("Failed to get installed .NET version");
        return false;
    }

    const verNum = parseInt(stdout[0]);
    if (verNum < minVer) {
        showError("Current .NET version is not compatible");
        return false;
    }
    return true;
};
