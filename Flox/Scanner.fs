module Flox.Scanner

open Flox.Token.TokenTypes
open Flox.CommonLibrary

let addToken tokenType location tokens = Token.make tokenType location :: tokens

let mapTokenResult foundToken location tokens =
    Result.map (addToken foundToken location) tokens

let mapErrorResult error location char tokens =
    let err = Error [ErrorSystem.scanError error location char]
    combineResult err tokens cons List.append

let rec scanline location tokens = function
    | [] -> tokens |> Result.map List.rev |> Result.mapError List.rev
    | '(' :: tl -> scanline (Location.nextColumn location) (mapTokenResult LEFT_PAREN location tokens) tl
    | ')' :: tl -> scanline (Location.nextColumn location) (mapTokenResult RIGHT_PAREN location tokens) tl
    | '{' :: tl -> scanline (Location.nextColumn location) (mapTokenResult LEFT_BRACE location tokens) tl
    | '}' :: tl -> scanline (Location.nextColumn location) (mapTokenResult RIGHT_BRACE location tokens) tl
    | ',' :: tl -> scanline (Location.nextColumn location) (mapTokenResult COMMA location tokens) tl
    | '.' :: tl -> scanline (Location.nextColumn location) (mapTokenResult DOT location tokens) tl
    | '-' :: tl -> scanline (Location.nextColumn location) (mapTokenResult MINUS location tokens) tl
    | '+' :: tl -> scanline (Location.nextColumn location) (mapTokenResult PLUS location tokens) tl
    | ';' :: tl -> scanline (Location.nextColumn location) (mapTokenResult SEMICOLON location tokens) tl
    | '*' :: tl -> scanline (Location.nextColumn location) (mapTokenResult STAR location tokens) tl
    | c :: tl -> scanline (Location.nextColumn location) (mapErrorResult "Unknown input" location c tokens) tl

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
