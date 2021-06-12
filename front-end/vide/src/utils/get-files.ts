import * as vscode from "vscode";

export const getFilesStartingWith = async (
    topLevelDoc: vscode.TextDocument
) => {
    const workspaceUris = await vscode.workspace.findFiles("**/*.v");
    const fileUris = workspaceUris.filter(
        (uri) => uri.toString() !== topLevelDoc.uri.toString()
    );

    const files = await Promise.all(
        fileUris.map(async (uri) => {
            let fileContArr = await vscode.workspace.fs.readFile(uri);
            let fileContStr = Buffer.from(fileContArr).toString();
            return {
                name: uri.fsPath,
                contents: fileContStr,
            };
        })
    );
    files.unshift({
        name: topLevelDoc.uri.fsPath,
        contents: topLevelDoc.getText(),
    });
    return files;
};
