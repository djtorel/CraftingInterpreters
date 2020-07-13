module Flox.CommonLibrary

let cons x y = x :: y

let combineResult t1 t2 ok err =
    match t1, t2 with
    | Ok _, Error e | Error e, Ok _ -> Error e
    | Ok ok1, Ok ok2 -> Ok (ok ok1 ok2)
    | Error err1, Error err2 -> Error (err err1 err2)

let splitWhile f xs =
    let rec loop acc = function
        | hd :: tl when f hd -> loop (hd :: acc) tl
        | t -> List.rev acc, t
    loop [] xs
