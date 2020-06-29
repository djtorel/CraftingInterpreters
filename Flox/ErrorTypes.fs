namespace Flox.ErrorTypes

open Flox

type RuntimeError =
    { Message: string
      Code: int }

type ScanError =
    { Location : Location.Location
      Message: string }

type RuntimeErrorType =
    | InvalidInput
    | UnknownError
