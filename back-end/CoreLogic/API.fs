module API

open System
open MethodDispatcher
open FParsec
open CommonHelpers
open CommonTypes
open Parser
open AST
open Compiler
open Compiler.Netlist
open Compiler.CompResult
open Simulator

[<ExposeType>]
type VerilogFile =
    { name: string
      contents: string }

[<ExposeType>]
type ErrorPosition =
    { line: int64
      column: int64 }

[<ExposeType>]
type ErrorMsg =
    { file: string
      start: ErrorPosition
      finish: ErrorPosition
      message: string }

[<ExposeType>]
type CompilerReturnType =
    { /// Should be one of: "success", "failure", "warnings", "invalid_call"
      status: string
      errors: ErrorMsg array
      warnings: ErrorMsg array }

let retSuccess = "success"
let retFailure = "failure"
let retWarnings = "warnings"
let retInvalid = "invalid_call"

let (|SUCCESS|FAILURE|WARNINGS|INVALID|) str =
    if str = retSuccess
    then SUCCESS
    else if str = retFailure
    then FAILURE
    else if str = retWarnings
    then WARNINGS
    else INVALID

let mutable lastASTs : (ASTT list) option = None
let mutable lastNetlist : Netlist option = None

let private compileFromASTs (asts: ASTT list) (topLevel: string) : CompilerReturnType =
    lastASTs <- Some asts
    Compile.project topLevel asts
    |> function
    | Succ net ->
        lastNetlist <- Some net
        { status = retSuccess
          errors = [||]
          warnings = [||] }
    | Warn (net, w) ->
        lastNetlist <- Some net
        let warnings =
            w
            |> List.map (fun warning -> 
                { file = warning.file
                  start =
                    { line = warning.start.Line
                      column = warning.start.Column }
                  finish =
                    { line = warning.finish.Line
                      column = warning.finish.Column }
                  message = warning.value })
            |> Array.ofList
        { status = retWarnings
          errors = [||]
          warnings = warnings }
    | Fail errs ->
        lastASTs <- None
        lastNetlist <- None
        let errors =
            errs
            |> List.map (fun err ->
                { file = err.file
                  start =
                    { line = err.start.Line
                      column = err.start.Column }
                  finish =
                    { line = err.finish.Line
                      column = err.finish.Column }
                  message = err.value })
            |> Array.ofList
        { status = retFailure
          errors = errors
          warnings = [||] }

[<ExposeMethod>]
let compile (files: VerilogFile array) (topLevel: string) : CompilerReturnType =
    files
    |> Array.resMap (fun file ->
        match Parse.sourceText file.name file.contents with
        | Result.Ok res -> Result.Ok res
        | Result.Error errorMsg -> 
            Result.Error
                { file = errorMsg.file
                  start =
                    { line = errorMsg.start.Line
                      column = errorMsg.start.Column }
                  finish =
                    { line = errorMsg.finish.Line
                      column = errorMsg.finish.Column }
                  message = errorMsg.value })
    |> function
    | Result.Error errs ->
        lastASTs <- None
        lastNetlist <- None
        { status = retFailure
          errors = errs
          warnings = [||] }
    | Result.Ok asts ->
        if topLevel = ""
        then 
            asts.[0].name
            |> compileFromASTs (List.ofArray asts)
        else
            compileFromASTs (List.ofArray asts) topLevel

[<ExposeType>]
type APISimInp =
    { name: string
      repeating: bool
      values: string array }

[<ExposeType>]
type SimulatorOutput =
    { name: string
      values: string array }

[<ExposeType>]
type SimulatorReturnType =
    { /// Should be one of: "success", "failure", "warnings", "invalid_call"
      status: string
      output: SimulatorOutput array
      errors: ErrorMsg array
      warnings: ErrorMsg array }

[<ExposeMethod>]
let simulateFromPrevious (inputs: APISimInp array) (reqVars: string array) (cycles: uint) : SimulatorReturnType =
    match lastNetlist with
    | None ->
        { status = retInvalid
          output = [||]
          errors = [||]
          warnings = [||] }
    | Some netlist ->
        let simInputs =
            inputs
            |> Array.map (fun inp ->
                let valLst =
                    inp.values
                    |> List.ofArray
                    |> List.map (fun str -> VNum.bin str)
                let simInput = 
                    if inp.repeating
                    then Repeating valLst
                    else Once valLst
                inp.name, simInput)
            |> Map.ofArray
        let validateReqVar reqVar =
            if netlist.varMap.ContainsKey reqVar
            then Result.Ok reqVar
            else 
                Result.Error
                    { file = ""
                      start =
                        { line = 0L
                          column = 0L }
                      finish =
                        { line = 0L
                          column = 0L }
                      message = sprintf "The requested variable '%s' could not be found in the netlist." reqVar }
        reqVars
        |> Array.resMap validateReqVar
        |> function
        | Result.Error errs ->
            { status = retFailure
              output = [||]
              errors = errs
              warnings = [||] }
        | Result.Ok simReqVars ->
            let simOut =
                (Map.empty, Simulate.runSimulation netlist simInputs (List.ofArray simReqVars) cycles)
                ||> List.fold (fun accMap cycleMap ->
                    (accMap, cycleMap)
                    ||> Map.fold (fun accMap' name value ->
                        if accMap'.ContainsKey name
                        then accMap'.Add(name, (value::accMap'.[name]))
                        else accMap'.Add(name, [ value ])))
                |> Map.toArray
                |> Array.map (fun (name, values) ->
                    let newVals =
                        values
                        |> Array.ofList
                        |> Array.map (fun vnum -> vnum.toBinString())
                    { name = name
                      values = newVals })
            { status = retSuccess
              output = simOut
              errors = [||]
              warnings = [||] }

[<ExposeMethod>]
let simulate (files: VerilogFile array) (topLevel: string) (inputs: APISimInp array) (reqVars: string array) (cycles: uint) : SimulatorReturnType =
    let compOut = compile files topLevel
    match compOut.status with
    | SUCCESS -> simulateFromPrevious inputs reqVars cycles
    | WARNINGS ->
        { simulateFromPrevious inputs reqVars cycles
            with 
                status = retWarnings
                warnings = compOut.warnings }
    | FAILURE ->
        { status = retFailure
          output = [||]
          errors = compOut.errors
          warnings = [||] }
    | INVALID ->
        { status = retInvalid
          output = [||]
          errors = [||]
          warnings = [||] }

[<ExposeType>]
type APIPort =
    { name: string
      input: bool }

[<ExposeMethod>]
let getPortNames (file: VerilogFile) : APIPort array =
    match Parse.sourceText file.name file.contents with
    | Result.Ok ast ->
        Compile.getASTPorts ast
        |> List.map (fun port ->
            { name = port.name
              input = port.pType = PortDirAndType.Input })
        |> Array.ofList
    | _ -> [||]

[<ExposeMethod>]
let getVariables (files: VerilogFile array) : string array =
    let compOut = compile files ""
    match compOut.status with
    | SUCCESS ->
        match lastNetlist with
        | None -> [||]
        | Some netlist ->
            netlist.varMap
            |> Map.toArray
            |> Array.map (fun (name, _) -> name)
    | WARNINGS ->
        [||]
    | FAILURE ->
        [||]
    | INVALID ->
        [||]
