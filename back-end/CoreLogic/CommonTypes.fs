namespace CommonTypes

open System

type NumT = uint64
module NumT =
    let CastDouble (a: double) = uint64 a
    let CastInt (a: int) = uint64 a
    let CastChar (a: char) = uint64 a

type SizeT = uint32
module SizeT =
    let CastUInt64 (a: uint64) = uint32 a
