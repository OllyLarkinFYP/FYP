export type Format = "bin" | "oct" | "hex" | "dec";

export type SimConfig = {
    cycles: number;
    "requested vars": {
        name: string;
        breakdown: boolean;
        format: Format;
    }[];
    inputs: {
        name: string;
        repeating: boolean;
        values: string[];
    }[];
};

export const validateSimConfig = (config: SimConfig) => {
    return (
        config.cycles &&
        config["requested vars"] &&
        config["requested vars"].every(
            ({ name, breakdown, format }) =>
                name !== undefined && breakdown !== undefined && format
        ) &&
        config.inputs &&
        config.inputs.every(
            ({ name, repeating, values }) =>
                name !== undefined && repeating !== undefined && values
        )
    );
};
