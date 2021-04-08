namespace Helpers

open System

/// Unchecked explicitly used incase compiled with checked
module UncheckedOperators = 
    let (!+) = FSharp.Core.Operators.(+)
    let (!-) = FSharp.Core.Operators.(-)
    let (!*) = FSharp.Core.Operators.(*)
    let (!/) = FSharp.Core.Operators.(/)
