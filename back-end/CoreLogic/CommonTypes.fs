namespace CommonTypes

open System

/// A class used to simulate the behaviour of bit arrays in Verilog
/// Unchecked operators are explicitly used were required to get the desired overflow behaviour
type VNum(value: uint64, size: uint32) =
    let width = 64

    member this.value = value
    member this.size = size

    new(v: int, s: int) = VNum(uint64 v, uint32 s)
    new(c: char) = VNum(uint64 c, 4u)
    new(i: int) = VNum(uint64 i, 32u)
    new(b: bool) = if b then VNum(1UL,1u) else VNum(0UL,1u)

    member this.trim () = 
        let mask = 
            ((pown 2UL (int this.size)), 1UL)
            ||> FSharp.Core.Operators.(-)
        VNum(this.value &&& mask, this.size)

    member this.toBool () =
        if this.value = 0UL
        then false
        else true

    member this.toInt () = int this.value

    override this.ToString() =
        // "{ val: " + this.value.ToString() + ", size: " + this.size.ToString() + " }"
        this.size.ToString() + "'d" + this.value.ToString()

    static member defaultSize = 32u

    // Allows use in the 'pown' function
    static member get_One () = VNum 1
    
    member this.reduce (operator: VNum -> VNum -> VNum) =
        let rec reduceRec (o: VNum -> VNum -> VNum) (i: VNum) (size: uint32) =
            match size with
            | 1u -> VNum(i.value, 1u)
            | _ -> 
                let bit = VNum((i &&& VNum(1UL,1u)).value, 1u)
                reduceRec o (i >>> 1) (size - 1u)
                |> o bit
        reduceRec operator this this.size 

    static member concat (nums: VNum list) =
        let out = 
            nums
            |> List.reduce (fun a b -> 
                let newSize = a.size + b.size
                let newVal = (a.value <<< int b.size) ||| b.trim().value
                VNum(newVal, newSize))
        out.trim()

    override this.Equals other =
        match other with
        | :? VNum as num -> (this :> IEquatable<_>).Equals num
        | _ -> false

    override this.GetHashCode() = this.value.GetHashCode()

    interface IEquatable<VNum> with
        member this.Equals other = other.value.Equals this.value

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? VNum as num -> (this :> IComparable<_>).CompareTo num
            | _ -> -1

    interface IComparable<VNum> with
        member this.CompareTo other = other.value.CompareTo this.value

    // *********** UNARY OPS ***********
    static member (~-) (num: VNum) = 
        ~~~num + VNum(1UL,32u)

    static member (~~~) (num: VNum) = 
        VNum(FSharp.Core.Operators.(~~~) num.value, num.size)

    // *********** BINARY OPS ***********
    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (+) (num1: VNum, num2: VNum) =
        let newVal =
            (num1.value, num2.value)
            ||> FSharp.Core.Operators.(+)
        let newSize = max num1.size num2.size
        VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (-) (num1: VNum, num2: VNum) =
        let newVal =
            (num1.value, num2.value)
            ||> FSharp.Core.Operators.(-)
        let newSize = max num1.size num2.size
        VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (*) (num1: VNum, num2: VNum) =
        let newVal =
            (num1.value, num2.value)
            ||> FSharp.Core.Operators.(*)
        let newSize = max num1.size num2.size
        VNum(newVal, newSize)

    /// Unchecked used explicityly to make behaviour the same as Verilog numbers
    /// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
    /// https://stackoverflow.com/questions/51672820/how-do-i-explicitly-use-unchecked-arithmetic-operators-in-f
    static member (/) (num1: VNum, num2: VNum) =
        let newVal =
            (num1.value, num2.value)
            ||> FSharp.Core.Operators.(/)
        let newSize = max num1.size num2.size
        VNum(newVal, newSize)

    static member (%) (num1: VNum, num2: VNum) =
        VNum(num1.value % num2.value, max num1.size num2.size)

    static member (&&&) (num1: VNum, num2: VNum) =
        VNum(num1.value &&& num2.value, max num1.size num2.size)

    static member (<<<) (num: VNum, amount: int) =
        VNum(num.value <<< amount, num.size)

    static member (>>>) (num: VNum, amount: int) =
        VNum(num.value >>> amount, num.size)

    static member (^^^) (num1: VNum, num2: VNum) =
        VNum(num1.value ^^^ num2.value, max num1.size num2.size)

    static member (|||) (num1: VNum, num2: VNum) =
        VNum(num1.value ||| num2.value, max num1.size num2.size)
