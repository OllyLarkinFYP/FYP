export type SimConfig = {
    cycles: number;
    "requested vars": string[];
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
        config.inputs &&
        config.inputs.every(
            ({ name, repeating, values }) =>
                name !== undefined && repeating !== undefined && values
        )
    );
};
