namespace CommonTypes

open System

type Range = 
    | Single of uint32
    | Ranged of uint32 * uint32
    with
        static member max () = Ranged (63u,0u)
        static member defaultRange () = Ranged (31u,0u)
        static member overlap r1 r2 =
            match r1, r2 with
            | Single a, Single b -> a = b
            | Single a, Ranged (b,c) 
            | Ranged (b,c), Single a -> a <= b && a >= c
            | Ranged (a,b), Ranged (c,d) -> (a <= c && a >= d) || (b <= c && b >= d)
        member this.size =
            match this with
            | Single _ -> 1u
            | Ranged (msb, lsb) -> msb - lsb + 1u
        member this.offset diff =
            match this with
            | Single a -> Single <| a + diff
            | Ranged (a,b) -> Ranged (a+diff, b+diff)
        member this.offsetDown diff =
            match this with
            | Single a -> Single <| a - diff
            | Ranged (a,b) -> Ranged (a-diff, b-diff)
        member this.ground () =
            match this with
            | Single _ -> Single 0u
            | Ranged (a,b) -> Ranged (a-b,0u)
        member this.lower =
            match this with
            | Single a
            | Ranged (_,a) -> a
        member this.upper =
            match this with
            | Single a
            | Ranged (a,_) -> a
        member this.contains i =
            i <= this.upper && i >= this.lower
        override this.ToString() =
            match this with
            | Single a -> sprintf "[%d]" a
            | Ranged (a,b) -> sprintf "[%d..%d]" a b

type MaskDir =
    | Up
    | Down

module private Helpers =
    let intersect a b =
        let setA = Set.ofList a
        let setB = Set.ofList b
        Set.intersect setA setB
        |> Set.toList

    let digToDec b c  =
        let value = 
            match c with
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> int c - int '0'
                | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> int c - int 'a' + 10
                | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> int c - int 'A' + 10
                | _ -> raise <| ArgumentException()
        if value < b
        then value
        else raise <| ArgumentException()

/// A class used to simulate the behaviour of bit arrays in Verilog
/// Unchecked operators are explicitly used were required to get the desired overflow behaviour
type VNum(value: uint64, size: uint, unknownBits: uint list) =
    let width = 64u

    member this.value = value
    member this.size = size
    member this.unknownBits = unknownBits

    new(v: uint64, s: uint) = VNum(v, s, [])
    new(v: int, s: int, ub: uint list) = VNum(uint64 v, uint s, ub)
    new(v: int, s: int) = VNum(uint64 v, uint s, [])
    new(c: char) = VNum(uint64 c, 4u, [])
    new(i: int) = VNum(uint64 i, 32u, [])
    new(v: uint) = VNum(uint64 v, 32u, [])
    new(b: bool) = if b then VNum(1UL,1u,[]) else VNum(0UL,1u,[])

    /// Constructs a VNum from binary string 
    /// e.g.
    /// "10010x1" -> VNum(73UL, 7u, [1u]) or VNum(75UL, 7u, [1u])
    static member bin = VNum.fromBase 2

    static member oct = VNum.fromBase 8

    static member hex = VNum.fromBase 16

    static member fromBase b str =
        let shiftAmt =
            match b with
            | 2 -> 1
            | 8 -> 3
            | 16 -> 4
            | _ -> raise <| ArgumentException()
        let size = String.length str * shiftAmt |> uint
        (VNum(0UL,size), str)
        ||> Seq.fold (fun currNum dig ->
            let digVal =
                match dig with
                | 'x' | 'X' -> 0UL
                | _ -> Helpers.digToDec b dig |> uint64
            let unknown = dig = 'x' || dig = 'x'
            let newNumBase : VNum = VNum.(<<<) (currNum, VNum shiftAmt)
            if unknown
            then VNum(newNumBase.value + digVal, size, newNumBase.unknownBits @ [0u .. uint (shiftAmt-1)])
            else VNum(newNumBase.value + digVal, size, newNumBase.unknownBits))

    static member unknown size =
        let bits = [ 0u..size-1u ]
        VNum(0UL, size, bits)

    member this.isUnknown =
        this.unknownBits
        |> List.filter (fun i -> i < width) // Number can extend past their bounds
        |> List.isEmpty
        |> not

    member this.mask (down, unknownBits) =
        let m = 
            (0UL, unknownBits)
            ||> List.fold (fun state elem ->
                let v = 1UL <<< (int elem)
                state ||| v) 
        match down with
        | Down -> VNum(this.value &&& (~~~ m), this.size, this.unknownBits)
        | Up   -> VNum(this.value ||| m, this.size, this.unknownBits)
        
    member this.maskDown () =
        this.mask (Down, this.unknownBits)

    member this.maskUp () = 
        this.mask (Up, this.unknownBits)

    member this.setRange (range: Range) (value: VNum) =
        let shiftedVal: VNum = VNum.(<<<) (value.getRange <| range.ground(), VNum range.lower)
        let oldUnknowns = this.unknownBits |> List.filter (range.contains >> not)
        let newUnknowns =
            shiftedVal.unknownBits @ oldUnknowns
            |> List.distinct
        let allOnes = ~~~0UL
        let mask = (allOnes >>> int (width - range.upper - 1u)) <<< int range.lower
        let newVal = shiftedVal.value &&& mask
        let oldVal = this.value &&& (~~~mask)
        VNum(newVal ||| oldVal, this.size, newUnknowns).trim()

    member this.getRange (range: Range) =
        let unknowns = this.unknownBits |> List.map (fun bit -> bit - range.lower)
        let value = this.value >>> int range.lower
        VNum(value, range.size, unknownBits).trim()

    /// Masks out anything above the size of the number as well as unknown bits
    member this.trim () = 
        let m = 
            ((1UL <<< (int this.size)), 1UL)
            ||> FSharp.Core.Operators.(-)
        let ub =
            this.unknownBits
            |> List.filter (fun i -> i < this.size)
        VNum(this.value &&& m, this.size, ub).maskDown()

    member this.toBool () =
        if this.trim().value = 0UL || this.isUnknown
        then false
        else true

    member this.toInt () = 
        if this.isUnknown
        then -1
        else this.trim().value |> int

    override this.ToString() =
        this.size.ToString() + "'d" + this.trim().value.ToString() + (sprintf " with unknown bits: %A" this.unknownBits)

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
        nums
        |> List.reduce (fun a b -> 
            let newSize = a.size + b.size
            let newVal = (a.trim().value <<< int b.size) ||| b.trim().value
            let newUnknownBits =
                a.unknownBits
                |> List.map ((+) b.size)
                |> (@) b.unknownBits
            VNum(newVal, newSize, newUnknownBits))

    member this.getBits bit =
        let rec toBinLst size i =
            match size with
            | 0u -> []
            | _ -> [i%2UL] @ (toBinLst (size-1u) (i >>> 1))
        (([],0u), toBinLst this.size this.value)
        ||> List.fold (fun (retLst, i) dig ->
            if (uint64 bit) = dig
            then (retLst @ [i], i+1u)
            else (retLst, i+1u))
        |> fst

    override this.Equals other =
        match other with
        | :? VNum as num -> 
            (this.trim().mask(Down,num.unknownBits).value :> IEquatable<_>).Equals (num.trim().mask(Down,this.unknownBits).value)
        | _ -> false

    override this.GetHashCode() = this.trim().value.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? VNum as num -> 
                (this.trim().mask(Down,num.unknownBits).value :> IComparable<_>).CompareTo (num.trim().mask(Down,this.unknownBits).value)
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
                    if bit >= (uint <| amount.toInt())
                    then Some <| bit - (uint <| amount.toInt())
                    else None)
            VNum(num.value >>> amount.toInt(), num.size, newUnknownBits)

    static member (^^^) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        VNum(num1.value ^^^ num2.value, newSize, num1.unknownBits @ num2.unknownBits)

    static member (&&&) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        let aVal = num1.maskUp().value
        let bVal = num2.maskUp().value
        let newVal = aVal &&& bVal
        let ones = VNum(newVal, newSize).getBits 1
        let unknowns = num1.unknownBits @ num2.unknownBits
        VNum(newVal, newSize, Helpers.intersect ones unknowns)

    static member (|||) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        let aVal = num1.maskDown().value
        let bVal = num2.maskDown().value
        let newVal = aVal ||| bVal
        let ones = VNum(newVal, newSize).getBits 0
        let unknowns = num1.unknownBits @ num2.unknownBits
        VNum(newVal, newSize, Helpers.intersect ones unknowns)

    static member (^^) (num1: VNum, num2: VNum) =
        let newSize = max num1.size num2.size
        if num1.isUnknown || num2.isUnknown
        then VNum.unknown newSize
        else VNum(pown (num1.trim().value) (int <| num2.trim().value), newSize)

type PortType =
    | Wire
    | Reg

type PortDirAndType =
    | Input
    | Output of PortType

type MutMap<'TKey, 'TValue when 'TKey : comparison>(m) =
    let mutable map : Map<'TKey,'TValue> = m

    member _.Add keyValPair = map <- map.Add keyValPair
    member _.ContainsKey = map.ContainsKey
    member _.Item key = map.Item key
    member _.TryFind key = Map.tryFind key map
    member _.Map = map
