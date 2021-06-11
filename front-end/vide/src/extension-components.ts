import * as vscode from "vscode";

export default class ExtensionComponents {
    private static outChannel?: vscode.OutputChannel = undefined;
    static sendErrorToOutputChannel(error: string, message: string) {
        if (!this.outChannel) {
            this.outChannel = vscode.window.createOutputChannel("VIDE");
        }
        this.outChannel.appendLine(message);
        this.outChannel.show();
        vscode.window.showErrorMessage(
            `${error}. Check 'VIDE' output log for more details.`
        );
        console.error(message);
    }
    static sendWarningToOutputChannel(warning: string, message: string) {
        if (!this.outChannel) {
            this.outChannel = vscode.window.createOutputChannel("VIDE");
        }
        this.outChannel.appendLine(message);
        this.outChannel.show();
        vscode.window.showWarningMessage(
            `${warning}. Check 'VIDE' output log for more details.`
        );
        console.warn(message);
    }
}
