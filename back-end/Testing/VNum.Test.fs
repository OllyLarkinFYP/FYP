namespace Tests

open System
open CommonTypes
open Expecto
open Helper

module VNumTests =

    let utilTests =
        testList "Util Tests" [
            equalityTests "VNum" [
                "Equality ignores unknown bits",
                    VNum 5,
                        VNum(7, 3, [1u])

                "Equality ignores bits passed the size of the number",
                    VNum 1,
                        VNum(7, 1)

                "Bin produces correct number",
                    VNum 13,
                        VNum.bin "1101"

                "Oct produces correct number with unknowns",
                    VNum 95,
                        VNum.oct "137"

                "Hex produces correct number with unknowns",
                    VNum 41803,
                        VNum.hex "a34b"

                // This behaviour is required as an expression is typically 
                // bounded by that it is assigning too - not the args
                "Until trimmed the values are not bounded",
                    VNum 2,
                        (VNum(1,1) + VNum(1,1)) * VNum(1,2)
            ]

            equalityTests "bool" [
                "isUnknown correctly identifies unknown bits",
                    true,
                        VNum(1,1,[0u]).isUnknown

                "isUnknown correctly identifier no unknown bits",
                    false,
                        VNum(1,1,[]).isUnknown

                "toBool evaluates 0 correctly",
                    false,
                        VNum(0).toBool()

                "toBool evaluates non 0 correcly",
                    true,
                        VNum(1).toBool()

                "toBool returns false for unknown bits",
                    false,
                        VNum(7,3,[0u]).toBool()
            ]

            equalityTests "uint64" [
                "Trim masks over correct size",
                    7UL,
                        VNum(63,3).trim().value

                "Mask correctly removes specified bits",
                    5UL,
                        VNum(7,3).mask(Down,[1u]).value

                "Mask correctly removes internally specified bits",
                    5UL,
                        VNum(7,3,[1u]).mask().value
            ]

            equalityTests "int" [
                "toInt returns -1 for number with unknown bits",
                    -1,
                        VNum(5,2,[0u]).toInt()

                "toInt returns truncated value",
                    5,
                        VNum(13,3).toInt()
            ]

            equalityListTests "uint list" [
                "getBits correctly gets 0 bits",
                    [0u; 1u; 2u],
                        VNum(8,4).getBits 0

                "getBits correctly gets 1 bits",
                    [3u],
                        VNum(8,4).getBits 1

                "Bin correctly places unknown bits",
                    [0u; 3u],
                        VNum.bin("0x10x").unknownBits

                "Oct correctly places unknown bits",
                    [0u; 1u; 2u; 6u; 7u; 8u],
                        VNum.oct("2x4x").unknownBits

                "Hex correctly places unknown bits",
                    [4u; 5u; 6u; 7u; 12u; 13u; 14u; 15u],
                        VNum.hex("ax3x8").unknownBits
            ]
        ]

    let operatorTests =
        testList "Operator Tests" [
            equalityTests "VNum" [
                "Concat correctly truncates interior values",
                    VNum 15,
                        VNum.concat [VNum(7,2); VNum(7,2)]

                "Unary minus works as expected",
                    VNum.bin "1010",
                        - (VNum.bin "0110")

                "Unary minus returns x when num is partially unknown",
                    VNum.unknown 1u,
                        - (VNum.bin "0x10")

                "Unary bitwise negation gives correct output",
                    VNum.bin "10x",
                        ~~~ (VNum.bin "01x")

                "Plus gives correct output",
                    VNum 25,
                        VNum 15 + VNum 10

                "Plus returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum 15 + VNum.bin "0110x"

                "Minus gives correct output",
                    VNum 5,
                        VNum 15 - VNum 10

                "Minus returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum 15 - VNum.bin "0110x"

                "Multiply gives correct output",
                    VNum 150,
                        VNum 15 * VNum 10

                "Multiply returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum 15 * VNum.bin "0110x"

                "Divide gives correct output",
                    VNum 15,
                        VNum 150 / VNum 10

                "Divide returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum 15 / VNum.bin "0110x"

                "Divide returns x if second arg is 0",
                    VNum.unknown 1u,
                        VNum 15 / VNum 0

                "Modulus gives correct output",
                    VNum 5,
                        VNum 15 % VNum 10

                "Modulus returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum 15 % VNum.bin "0110x"

                "Left shift evaluates correctly",
                    VNum.bin "11001100",
                        VNum.(<<<) (VNum.bin "00110011", VNum 2)

                "Right shift evaluates correctly",
                    VNum.bin "00001100",
                        VNum.(>>>) (VNum.bin "00110011", VNum 2)

                "Left shift returns x if shift amount is unknown",
                    VNum.unknown 1u,
                        VNum.(<<<) (VNum.bin "00110011", VNum.bin "10x")

                "Right shift returns x if shift amount is unknown",
                    VNum.unknown 1u,
                        VNum.(>>>) (VNum.bin "00110011", VNum.bin "10x")

                "Power evaluates correctly",
                    VNum 64,
                        VNum 4 ^^ VNum 3

                "Power returns x if arg is unknown",
                    VNum.unknown 1u,
                        VNum.bin "1101x0" ^^ VNum 2

                "Bitwise AND evalutes correctly",
                    VNum.bin "00010x0x",
                        VNum.bin "001101xx" &&& VNum.bin "0101xx01"

                "Bitwise OR evaluates correctly",
                    VNum.bin "0111x1x1",
                        VNum.bin "001101xx" ||| VNum.bin "0101xx01"

                "Bitwise XOR evaluates correctly",
                    VNum.bin "0110xxxx",
                        VNum.bin "001101xx" ^^^ VNum.bin "0101xx01"
            ]

            equalityListTests "uint []" [
                "Concat correctly shifts the unknown bits",
                    [1u; 3u],
                        (VNum.concat [VNum(7,2,[1u]); VNum(7,2,[1u])]).unknownBits

                "Bitwise AND correct unknown bits",
                    [0u; 2u],
                        ((VNum.bin "001101xx") &&& (VNum.bin "0101xx01")).unknownBits

                "Bitwise OR correct unknown bits",
                    [1u; 3u],
                        ((VNum.bin "001101xx") ||| (VNum.bin "0101xx01")).unknownBits

                "Bitwise XOR correct unknown bits",
                    [0u; 1u; 2u; 3u],
                        ((VNum.bin "001101xx") ^^^ (VNum.bin "0101xx01")).unknownBits

                "Left shift correctly shifts unknown bits",
                    [3u; 4u],
                        (VNum.(<<<) (VNum.bin "000xx", VNum 3)).unknownBits

                "Right shift correctly shifts unknown bits",
                    [0u; 1u],
                        (VNum.(>>>) (VNum.bin "xx000", VNum 3)).unknownBits
            ]

            trueTests "bool" [
                "Left shift correctly shifts out unknown bits",
                    (VNum.(<<<) (VNum.bin "000x", VNum 4)).trim().isUnknown |> not

                "Right shift correctly shifts out unknown bits",
                    (VNum.(>>>) (VNum.bin "000x", VNum 1)).trim().isUnknown |> not
            ]

            equalityTests "uint" [
                "Concat correctly changes the size",
                    5u,
                        (VNum.concat [VNum(7,3); VNum(3,2)]).size
            ]
        ]

    let allTests =
        testList "All VNum Tests" [
            utilTests
            operatorTests
        ]
