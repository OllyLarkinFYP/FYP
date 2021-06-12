namespace Parser

open System
open System.Text.RegularExpressions
open FParsec
open Parser
open CommonTypes

module private Helpers =
    // TODO: this really really needs to be made better
    // TODO: use an enum instead of 3 bools
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

    let (|RegMatch|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success 
        then Some (m.Groups) 
        else None

    let toWithPosResult (srcTxt: string) (file: string) (err: ParserError) (msg: string) : WithPos<string> =
        let tokenLength =
            match srcTxt.[int err.Position.Index..] with
            | RegMatch @"([a-zA-Z_][a-zA-Z_$0-9]*)" groups -> groups.[0].Length
            | RegMatch @"([0-9]*(?:'[bodh])?[_0-9]*)" groups -> groups.[0].Length
            | RegMatch @"(\|\||&&|~\^|\^~|===|!==|==|!=|<=|>=|<<<|>>>|<<|>>|\*\*|~&|~\||)" groups -> groups.[0].Length
            | _ -> 1
        { file = file
          start = err.Position
          finish = Position("", 0L, err.Position.Line, err.Position.Column + int64 tokenLength)
          value = msg }

module Parse =
    let sourceText file source =
        let srcTxt = 
            source
            |> Helpers.removeComments
        runParserOnString LangConstructs.pSourceText file "" srcTxt
        |> function
        | Success (res, _, _) -> Result.Ok res
        | Failure (msg, err, _) -> Result.Error <| Helpers.toWithPosResult srcTxt file err msg
