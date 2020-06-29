module Flox.Script

open System.IO

let runFile path =
    path
    |> File.ReadLines
    |> Scanner.run
    |> ignore
    Ok 0
