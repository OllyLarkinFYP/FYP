import * as vscode from "vscode";

export default class Extension {
    private static outChannel?: vscode.OutputChannel = undefined;
    static initOutputChannel(outChannel: vscode.OutputChannel) {
        this.outChannel = outChannel;
        this.outChannel.hide();
    }
    static sendErrorToOutputChannel(error: string, message: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(message);
        this.outChannel?.show();
        vscode.window.showErrorMessage(
            `${error}. Check 'VIDE' output log for more details.`
        );
        console.error(message);
    }
    static sendWarningToOutputChannel(warning: string, message: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(message);
        this.outChannel?.show();
        vscode.window.showWarningMessage(
            `${warning}. Check 'VIDE' output log for more details.`
        );
        console.warn(message);
    }
    static sendInfoToOutputChannel(info: string) {
        this.outChannel?.clear();
        this.outChannel?.appendLine(info);
        this.outChannel?.show();
        console.log(info);
    }

    private static diagnosticsCollection?: vscode.DiagnosticCollection;
    static initDiagnostics(diagCollection: vscode.DiagnosticCollection) {
        this.diagnosticsCollection = diagCollection;
    }
    static setDiagnostics(diagnostics: [vscode.Uri, vscode.Diagnostic[]][]) {
        this.diagnosticsCollection?.clear();
        diagnostics.forEach(([uri, diags]) => {
            this.diagnosticsCollection?.set(uri, diags);
        });
    }
    static clearDiagnostics() {
        this.diagnosticsCollection?.clear();
    }
    static deleteFromDiagnostics(uri: vscode.Uri) {
        this.diagnosticsCollection?.delete(uri);
    }
}
