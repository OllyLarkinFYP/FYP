export type SimConfig = {
    cycles: number;
    "requested vars": {
        name: string;
        breakdown: boolean;
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
            ({ name, breakdown }) =>
                name !== undefined && breakdown !== undefined
        ) &&
        config.inputs &&
        config.inputs.every(
            ({ name, repeating, values }) =>
                name !== undefined && repeating !== undefined && values
        )
    );
};
