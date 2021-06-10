import { exec } from "child_process";
import * as path from "path";

const backendPath = path.join(__dirname, "../resources/back-end/CoreLogic.dll");

export type OutgoingJob = {
    id: number;
    methodName: string;
    parameters: any[];
};

export type IncomingReply = {
    id?: number;
    reply?: any;
};

export const executeJob = (
    job: OutgoingJob,
    replyCallBack: (reply: any) => void
) => {
    console.log("Executing job:", job);
    exec(
        `dotnet ${backendPath} -j ${JSON.stringify(JSON.stringify(job))}`,
        (err, stdout, stderr) => {
            if (!err) {
                if (stderr) {
                    console.error(
                        `Execution of job resulted in error: ${stderr}`
                    );
                } else if (stdout) {
                    let response: IncomingReply = {};
                    try {
                        response = JSON.parse(stdout);
                    } catch (jsonErr) {
                        console.error(
                            `Could not parse the method response as it is not valid JSON.`
                        );
                    }
                    if (
                        response.id !== undefined &&
                        response.reply !== undefined
                    ) {
                        console.log("Received reply:", response.reply);
                        replyCallBack(response.reply);
                    } else {
                        console.log(
                            "Could not parse response to IncomingReply type.",
                            response
                        );
                    }
                } else {
                    console.error("Execution of job produced no output");
                }
            } else {
                console.error(
                    `Execution of job resulted in exception: ${err.name}, ${err.message}`
                );
            }
        }
    );
};
