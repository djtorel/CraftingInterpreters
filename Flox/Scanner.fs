module Flox.Scanner

open Flox.Token.TokenTypes
open Flox.CommonLibrary
open Flox.Location

let addToken tokenType location tokens = Token.make tokenType location :: tokens

let mapTokenResult foundToken location tokens =
    Result.map (addToken foundToken location) tokens

let mapErrorResult error char location tokens =
    let err = Error [ErrorSystem.scanError error location char]
    combineResult err tokens cons List.append

let rec scanline location tokens currentChar =
    let mapResultAndMove = function
        | Ok t -> scanline (nextColumn location) (mapTokenResult t location tokens)
        | Error (e, c) -> scanline (nextColumn location) (mapErrorResult e c location tokens)

    match currentChar with
    | [] -> tokens |> Result.map List.rev |> Result.mapError List.rev
    | '(' :: tl -> mapResultAndMove (Ok LEFT_PAREN) tl
    | ')' :: tl -> mapResultAndMove (Ok RIGHT_PAREN) tl
    | '{' :: tl -> mapResultAndMove (Ok LEFT_BRACE) tl
    | '}' :: tl -> mapResultAndMove (Ok RIGHT_BRACE) tl
    | ',' :: tl -> mapResultAndMove (Ok COMMA) tl
    | '.' :: tl -> mapResultAndMove (Ok DOT) tl
    | '-' :: tl -> mapResultAndMove (Ok MINUS) tl
    | '+' :: tl -> mapResultAndMove (Ok PLUS) tl
    | ';' :: tl -> mapResultAndMove (Ok SEMICOLON) tl
    | '*' :: tl -> mapResultAndMove (Ok STAR) tl
    | '!' :: '=' :: tl -> mapResultAndMove (Ok BANG_EQUAL) tl
    | '!' :: tl -> mapResultAndMove (Ok BANG) tl
    | '=' :: '=' :: tl -> mapResultAndMove (Ok EQUAL_EQUAL) tl
    | '=' :: tl -> mapResultAndMove (Ok EQUAL) tl
    | '<' :: '=' :: tl -> mapResultAndMove (Ok LESS_EQUAL) tl
    | '<' :: tl -> mapResultAndMove (Ok LESS) tl
    | '>' :: '=' :: tl -> mapResultAndMove (Ok GREATER_EQUAL) tl
    | '>' :: tl -> mapResultAndMove (Ok GREATER) tl
    | c :: tl -> mapResultAndMove (Error ("Unknown character", c)) tl


(*
So we have an IEnumerable<string> and a string list
Each item in the seq or list is a line of source

We need to scan each line character by character
Can use Seq.mapi to keep track of line number and to iterate over lines
Use recursive function to scan each line...
*)
let run (source : string seq) =
    source
    |> Seq.mapi(fun iter str ->
        str
        |> List.ofSeq
        |> scanline ({ Line = iter + 1; Column = 1 }) (Ok []))
    |> Seq.iter(fun t ->
        match t with
        | Ok token -> printfn "Token: %A" token
        | Error e -> printfn "Error: %A" e)
    source
