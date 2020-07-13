module Flox.Scanner
open System

open Flox.Token.TokenTypes
open Flox.CommonLibrary
open Flox.Location

let addToken tokenType location tokens = Token.make tokenType location :: tokens

let mapTokenResult foundToken location tokens =
    Result.map (addToken foundToken location) tokens

let mapErrorResult error char location tokens =
    let err = Error [ErrorSystem.scanError error location char]
    combineResult err tokens cons List.append

let scanSource source =
    let rec loop location tokens chars =
        let mapResultAndMove = function
            | Ok t -> loop (nextColumn location) (mapTokenResult t location tokens)
            | Error (e, c) -> loop (nextColumn location) (mapErrorResult e c location tokens)

        let moveToNextLine () =
            loop (nextLine location) tokens

        let skipColumn tail =
            loop (nextColumn location) tokens tail

        let skipUntil (cmp : string) (loc : Location) (src: char list) =
            let rec skipToLocation l = function
                | [] -> l
                | '\n' :: tl  -> skipToLocation (nextLine l) tl
                | _ :: tl -> skipToLocation (nextColumn l) tl

            (String.Concat src).Split(cmp, 2)
            |> function
                | [|skipped; kept|]->
                    (List.ofSeq kept)
                    |> loop (skipToLocation (incrementColumnBy loc cmp.Length) (List.ofSeq skipped)) tokens
                | _ -> loop location tokens []

        match chars with
        | [] -> tokens |> mapTokenResult EOF location |> Result.map List.rev |> Result.mapError List.rev
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
        | '/' :: '/' :: tl -> tl |> skipUntil "\n" (nextLine location)
        | '/' :: '*' :: tl -> tl |> skipUntil "*/" (incrementColumnBy location 2)
        | '/' :: tl -> mapResultAndMove (Ok SLASH) tl
        | '\n' :: tl -> moveToNextLine () tl
        | c :: tl when c |> Char.IsWhiteSpace -> skipColumn tl
        | c :: tl -> loop (nextColumn location) (mapErrorResult "Unknown char" c location tokens) tl

    loop (start ()) (Ok []) (List.ofSeq source)

let run source =
    source
    |> scanSource
    |> function
        | Ok token -> printfn "Token: %A" token
        | Error e -> printfn "Error: %A" e
    source
