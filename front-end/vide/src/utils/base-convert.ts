import { Format } from "../simconfig";

const isUnknown = (str: string) => str.includes("x") || str.includes("X");

const revStr = (str: string) => str.split("").reverse().join("");

export const baseConvert = (binStr: string, format: Format): string => {
    if (format === "bin") {
        return `'b${binStr}`;
    }

    if (format === "dec") {
        if (isUnknown(binStr)) {
            return "x";
        }
        return `${parseInt(binStr, 2).toString()}`;
    } else {
        let base: number;
        let bitNum: number;
        let header: string;
        switch (format) {
            case "oct":
                base = 8;
                bitNum = 3;
                header = "'o";
                break;
            case "hex":
                base = 16;
                bitNum = 4;
                header = "'h";
                break;
        }

        const splitStr = revStr(binStr).match(
            RegExp(`.{1,${bitNum}}`, "g")
        ) || [binStr];

        return (
            header +
            splitStr
                .map((subNum) =>
                    isUnknown(subNum)
                        ? "x"
                        : parseInt(revStr(subNum), 2).toString(base)
                )
                .reverse()
                .join("")
        );
    }
};
