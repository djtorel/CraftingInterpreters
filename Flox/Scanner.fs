module Flox.Scanner

open Flox.Token.TokenTypes

let add_token tokenType location tokens = Token.make tokenType location :: tokens

let incr_col location =
    Location.next_column location 1

let map_token_result foundToken location tokens =
    Result.map (add_token foundToken location) tokens

let rec scanline location tokens = function
    | [] -> tokens |> Result.map List.rev |> Result.mapError List.rev
    | '(' :: tl -> scanline (incr_col location) (map_token_result LEFT_PAREN location tokens) tl
    | _ :: tl -> scanline (Location.next_column location 1) (Result.map (add_token UNKNOWN location) tokens) tl

(*
So we have an IEnumerable<string> and a string list
Each item in the seq or list is a line of source

We need to scan each line character by character
Can use Seq.mapi to keep track of line number and to iterate over lines
Use recursive function to scan each line...
*)
let run (source : string seq) =
    // Scan
    // Create list of tokens
    // Print tokens
    source
    |> Seq.mapi(fun iter str ->
        scanline ({ Line = iter + 1; Column = 0 }) (Ok []) (str |> List.ofSeq))
    |> Seq.iter(fun t ->
        match t with
        | Ok token -> printfn "Token: %A" token
        | Error e -> printfn "%A" e)
    source
