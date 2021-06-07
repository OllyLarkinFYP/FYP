import { exec, execSync } from "child_process";
import * as fs from "fs-extra";

const BUILD_BACK_END =
    "cd ../../back-end/CoreLogic && dotnet publish -c Release";
const FRONT_END_FOLDER = "resources/back-end";
const BACK_END_FOLDER = "../../back-end/CoreLogic/bin/Release/net5.0";
const EXE_PATH = "resources/back-end/CoreLogic.exe";

fs.rmdirSync(FRONT_END_FOLDER, { recursive: true });

execSync(BUILD_BACK_END);

fs.copySync(BACK_END_FOLDER, FRONT_END_FOLDER);

fs.unlinkSync(EXE_PATH);
