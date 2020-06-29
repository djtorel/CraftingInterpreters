module Flox.ErrorSystem

open Flox.ErrorTypes

let runtimeError errorType =
    match errorType with
    | InvalidInput -> Error { Message = "Usage: flox [script]"; Code = 64 }
    | UnknownError -> Error { Message = "An unknown internal error occurred"; Code = 64 }

let handleResult result =
    match result with
    | Ok v -> v
    | Error (e : RuntimeError) ->
        e.Message |> printfn "%s"
        e.Code |> printfn "Code: %d"
        e.Code

let scanError error location c =
    { Location = location
      Message = sprintf "[line %i, col, %i] %s: %c" location.Line location.Column error c }

let handleScanResult result =
    match result with
    | Ok v -> v
    | Error e ->
        e.Message |> printfn "%s"
