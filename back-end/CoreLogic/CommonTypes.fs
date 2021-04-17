namespace CommonTypes

open System

/// A class used to simulate the behaviour of bit arrays in Verilog
/// Unchecked operators are explicitly used were required to get the desired overflow behaviour
type VNum(value: uint64, size: uint, unknownBits: uint list) =
    let width = 64

    member this.value = value
    member this.size = size
    member this.unknownBits = unknownBits

    new(v: uint64, s: uint) = VNum(v, s, [])
    new(v: int, s: int, ub: uint list) = VNum(uint64 v, uint s, ub)
    new(v: int, s: int) = VNum(uint64 v, uint s, [])
    new(c: char) = VNum(uint64 c, 4u, [])
    new(i: int) = VNum(uint64 i, 32u, [])
    new(b: bool) = if b then VNum(1UL,1u,[]) else VNum(0UL,1u,[])

    static member unknown size =
        let bits = [ 0u..size-1u ]
        VNum(0UL, size, bits)

    member this.mask unknownBits =
        let m = 
            (0UL, unknownBits)
            ||> List.fold (fun state elem ->
                let v = 1UL <<< (int elem)
                state + v) 
            |> (~~~)
        VNum(this.value &&& m, this.size, this.unknownBits)

    member this.mask () =
        this.mask this.unknownBits

    member this.isUnknown = not <| List.isEmpty this.unknownBits

    /// Masks out anything above the size of the number as well as unknown bits
    member this.trim () = 
        let m = 
            ((1UL <<< (int this.size)), 1UL)
            ||> FSharp.Core.Operators.(-)
        VNum(this.value &&& m, this.size, this.unknownBits).mask()

    member this.toBool () =
        if this.trim().value = 0UL || this.isUnknown
        then false
        else true

    member this.toInt () = 
        if this.isUnknown
        then -1
        else this.trim().value |> int

    override this.ToString() =
        this.size.ToString() + "'d" + this.value.ToString() + (sprintf " with unknown bits: %A" this.unknownBits)

    static member defaultSize = 32u

    // Allows use in the 'pown' function
    static member get_One () = VNum 1
    
    member this.reduce (operator: VNum -> VNum -> VNum) =
        let rec reduceRec (o: VNum -> VNum -> VNum) (i: VNum) (size: uint) =
            match size with
            | 1u -> VNum(i.value, 1u)
            | _ -> 
                let bit = VNum((i &&& VNum(1UL,1u)).value, 1u)
                reduceRec o (VNum.(>>>) (i, VNum(1))) (size - 1u)
                |> o bit
        if this.isUnknown
        then VNum.unknown 1u
        else reduceRec operator this this.size 

    static member concat (nums: VNum list) =
        let out = 
            nums
            |> List.reduce (fun a b -> 
                let newSize = a.size + b.size
                let newVal = (a.trim().value <<< int b.size) ||| b.trim().value
                let newUnknownBits =
                    a.unknownBits
                    |> List.map ((+) b.size)
                    |> (@) b.unknownBits
                VNum(newVal, newSize, newUnknownBits))
        out.trim()

    override this.Equals other =
        match other with
        | :? VNum as num -> 
            (this.trim().mask(num.unknownBits).value :> IEquatable<_>).Equals (num.trim().mask(this.unknownBits).value)
        | _ -> false

    override this.GetHashCode() = this.trim().value.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? VNum as num -> 
                (this.trim().mask(num.unknownBits).value :> IComparable<_>).CompareTo (num.trim().mask(this.unknownBits).value)
            | _ -> -1

    // *********** UNARY OPS ***********
    static member (~-) (num: VNum) = 
        ~~~num + VNum(1UL,num.size)

    static member (~~~) (num: VNum) = 
        VNum(FSharp.Core.Operators.(~~~) num.value, num.size, num.unknownBits)

    // *********** BINARY OPS ***********
    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (+) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else 
            let newVal =
                (num1.value, num2.value)
                ||> FSharp.Core.Operators.(+)
            VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (-) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else 
            let newVal =
                (num1.value, num2.value)
                ||> FSharp.Core.Operators.(-)
            VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (*) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else 
            let newVal =
                (num1.value, num2.value)
                ||> FSharp.Core.Operators.(*)
            VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (/) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown || num2 = VNum 0    // TODO: check if this is what happens
        then VNum.unknown newSize
        else 
            let newVal =
                (num1.value, num2.value)
                ||> FSharp.Core.Operators.(/)
            VNum(newVal, newSize)

    static member (%) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(num1.value % num2.value, newSize)

    static member (&&&) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(num1.value &&& num2.value, newSize)

    static member (<<<) (num: VNum, amount: VNum) =
        if amount.isUnknown
        then VNum.unknown num.size
        else 
            let newUnknownBits =
                num.unknownBits
                |> List.map ((+) (uint <| amount.toInt()))
            VNum(num.value <<< amount.toInt(), num.size, newUnknownBits)

    static member (>>>) (num: VNum, amount: VNum) =
        if amount.isUnknown
        then VNum.unknown num.size
        else 
            // This is required so that bit underflow does not occur
            // Assumes the input to amount is always positive
            let newUnknownBits =
                num.unknownBits
                |> List.choose (fun bit ->
                    if bit > (uint <| amount.toInt())
                    then Some <| bit - (uint <| amount.toInt())
                    else None)
            VNum(num.value >>> amount.toInt(), num.size, newUnknownBits)

    static member (^^^) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(num1.value ^^^ num2.value, newSize)

    static member (|||) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(num1.value ||| num2.value, newSize)

    static member (^^) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(pown (num1.trim().value) (int <| num2.trim().value), newSize)
