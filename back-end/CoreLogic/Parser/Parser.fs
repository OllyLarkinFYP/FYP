namespace Parser

open System
open FParsec
open Parser

module private Helpers =
    let removeComments text =
        let pairedChars =
            text + " "
            |> Seq.pairwise
        (("", false, false, false), pairedChars)
        ||> Seq.fold (fun (retStr, slc, mlc, finishingmlc) (c1, c2) ->
            match slc, mlc, finishingmlc with
            | false, false, false ->
                if c1 = '/' && c2 = '/'
                then retStr + " ", true, false, false
                else if c1 = '/' && c2 = '*'
                then retStr + " ", false, true, false
                else retStr + string c1, false, false, false
            | _, _, true ->
                retStr + " ", slc, mlc, false
            | true, _, _ ->
                if c1 = '\n'
                then retStr + "\n", false, false, false
                else retStr + " ", true, false, false
            | _, true, _ ->
                if c1 = '*' && c2 = '/'
                then retStr + " ", false, false, true
                else
                    if c1 = '\n'
                    then retStr + "\n", false, true, false
                    else retStr + " ", false, true, false)
        |> function
        | s, _, _, _ -> s

module Parse =
    let sourceText source =
        source
        |> Helpers.removeComments
        |> run LangConstructs.pSourceText
