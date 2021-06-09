import { Writable, Readable } from "stream";
import { TypedEmitter } from "tiny-typed-emitter";
import NotImplementedError from "./utils/errors/not-implemented-error";

export class CommChannelClosedError extends Error {
    constructor(message?: string) {
        super(message);
        this.name = "CommChannelClosedError";
    }
}

export type CommChannelCloseReason =
    | "read stream closed"
    | "write stream closed"
    | "protocol mismatch";

interface CommChannelEvents {
    close: (reason?: CommChannelCloseReason) => void;
}

export default class CommChannel extends TypedEmitter<CommChannelEvents> {
    readStream: Readable;
    writeStream: Writable;

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
            const headerSize = 4;
            let sizeBuff: Buffer | null = this.readStream.read(headerSize);
            if (sizeBuff === null) {
                this.close("protocol mismatch");
                console.error(
                    "When trying to receive the size of the incoming message, null was returned. This may indicate that not enough information was sent. The communication channel has been closed as the protocol is now likely out of sync."
                );
                return;
            }
            let messageSize = sizeBuff.reduce((tot, curr, i) => {
                return tot + (curr << (i * 8));
            });

            let messageBuff: Buffer | null = this.readStream.read(messageSize);
            if (messageBuff === null) {
                this.close("protocol mismatch");
                console.error(
                    `When trying to receive ${messageSize} bytes from the input stream, null was returned. This may indicate that the protocol is out of sync and the channel has been closed.`
                );
                return;
            }
        });
    }

    processJob(job: { id: any }): Promise<{ id: any }> {
        if (this.closed) {
            throw new CommChannelClosedError();
        }

        throw new NotImplementedError(); // TODO: implement
    }

    close(reason?: CommChannelCloseReason) {
        this.closed = true;
        this.readStream.removeAllListeners();
        this.writeStream.removeAllListeners();
        this.emit("close", reason);
    }
}
