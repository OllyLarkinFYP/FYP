export const indentString = (str: string, linePrefix: string = "\t") => {
    return str
        .split(/[\r\n]+/)
        .map((s) => {
            return `${linePrefix}${s}\n`;
        })
        .reduce((prev, curr) => {
            return prev + curr;
        });
};
