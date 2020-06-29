module Flox.Repl

open System
open System.Collections.Generic
open Flox.ErrorTypes

let runPrompt() =
    let rec prompt buffer =
        printf "> "
        match Console.ReadLine() with
        | "quit;" | null -> []
        | line ->
            buffer @ [line]
            |> Scanner.run
            |> List.ofSeq
            |> prompt
    match prompt [] with
    | [] -> Ok 0
    | _ ->
        ErrorSystem.runtimeError UnknownError
