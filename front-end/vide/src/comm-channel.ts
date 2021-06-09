import { Writable, Readable } from "stream";
import { TypedEmitter } from "tiny-typed-emitter";
import NotImplementedError from "./utils/errors/not-implemented-error";

export class CommChannelClosedError extends Error {
    constructor(message?: string) {
        super(message);
        this.name = "CommChannelClosedError";
    }
}

export class CommChannelInvalidJobError extends Error {
    constructor(message?: string) {
        super(message);
        this.name = "CommChannelInvalidJobError";
    }
}

export type CommChannelCloseReason =
    | "read stream closed"
    | "write stream closed"
    | "protocol mismatch"
    | "could not write job"
    | "reply has invalid JSON format";

export type OutgoingJob = {
    id: number;
    methodName: string;
    parameters: any[];
};

export type IncomingReply = {
    id?: number;
    reply?: any;
};

interface CommChannelEvents {
    close: (reason?: CommChannelCloseReason, value?: any) => void;
}

const headerSize = 4;

export class CommChannel extends TypedEmitter<CommChannelEvents> {
    readStream: Readable;
    writeStream: Writable;

    replyCallBacks = new Map<number, (reply: any) => void>();

    private closed = false;

    constructor(reader: Readable, writer: Writable) {
        super();

        this.readStream = reader;
        this.writeStream = writer;

        this.readStream.on("close", () => {
            this.close("read stream closed");
        });
        this.writeStream.on("close", () => {
            this.close("write stream closed");
        });

        this.readStream.on("readable", () => {
            while (true) {
                const sizeBuff: Buffer | null =
                    this.readStream.read(headerSize);
                if (sizeBuff === null) {
                    break;
                }
                const messageSize = sizeBuff.readInt32LE();

                const messageBuff: Buffer | null =
                    this.readStream.read(messageSize);
                if (messageBuff === null) {
                    this.close("protocol mismatch", messageSize);
                    console.error(
                        `When trying to receive ${messageSize} bytes from the input stream, null was returned. This may indicate that the protocol is out of sync and the channel has been closed.`
                    );
                    return;
                }
                const messageStr = messageBuff.toString("utf-8");

                let response: IncomingReply = {};

                try {
                    response = JSON.parse(messageStr);
                } catch (err) {
                    console.error(
                        `Could not parse the returned message as it is not valid JSON: ${messageStr}`
                    );
                    this.close("reply has invalid JSON format");
                }

                if (response.id && this.replyCallBacks.has(response.id)) {
                    let callBack = this.replyCallBacks.get(response.id);
                    if (callBack) {
                        callBack(response.reply);
                    }
                    this.replyCallBacks.delete(response.id);
                }
            }
        });
    }

    processJob(job: OutgoingJob, replyCallBack: (reply: any) => void) {
        if (this.closed) {
            throw new CommChannelClosedError();
        }
        if (this.replyCallBacks.has(job.id)) {
            throw new CommChannelInvalidJobError(
                `A job with ID ${job.id} has already been registered.`
            );
        }

        this.replyCallBacks.set(job.id, replyCallBack);

        let sJob = JSON.stringify(job);

        const jobBuff = Buffer.from(sJob);
        const jobSize = jobBuff.byteLength;
        const sizeBuff = Buffer.alloc(headerSize).map((_unused, i) => {
            return (jobSize >> (i * 8)) & 255;
        });
        const messageBuff = Buffer.concat([sizeBuff, jobBuff]);

        if (!this.writeStream.write(messageBuff)) {
            this.close("could not write job", messageBuff);
        }
    }

    cancelJob(id: number) {
        this.replyCallBacks.delete(id);
    }

    close(reason?: CommChannelCloseReason, value?: any) {
        this.closed = true;
        this.readStream.removeAllListeners();
        this.writeStream.removeAllListeners();
        console.error(
            reason
                ? value
                    ? `CommChannel closed because ${reason} with value ${value}`
                    : `CommChannel closed because ${reason}`
                : "CommChannel closed"
        );
        this.emit("close", reason, value);
    }
}
