import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

export async function lookupInPath(exec: string): Promise<string | null> {
    const paths = process.env.PATH ?? "";

    const candidates = paths.split(path.delimiter).flatMap((dirInPath) => {
        const candidate = path.join(dirInPath, exec);
        return os.type() === "Windows_NT" ? [candidate, `${candidate}.exe`] : [candidate];
    });

    for await (const file of candidates) {
        if (await pathExists(file)) {
            return file;
        }
    }
    return null;
}

async function pathExists(path: string): Promise<boolean> {
    try {
        const uri = vscode.Uri.file(path);
        return ((await vscode.workspace.fs.stat(uri)).type
            & (vscode.FileType.File | vscode.FileType.SymbolicLink)) !== 0;
    } catch {
        return false;
    }
}
