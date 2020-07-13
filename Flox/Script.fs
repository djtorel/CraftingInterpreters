module Flox.Script

open System.IO

let runFile path =
    path
    |> File.ReadAllText
    |> Scanner.run
    |> ignore
    Ok 0
