open Flox
open Flox.ErrorTypes

let handleArgs argv =
    match argv |> Array.toList with
    | path :: [] ->
        Script.runFile path
    | _ :: _ ->
        ErrorSystem.runtimeError InvalidInput
    | _ ->
        Repl.runPrompt()

[<EntryPoint>]
let main argv =
    argv
    |> handleArgs
    |> ErrorSystem.handleResult
