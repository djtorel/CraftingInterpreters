namespace Flox.ErrorTypes

type RuntimeError =
    { Message: string
      Code: int }

type ScanError =
    { Line: int
      Where: string
      Message: string }

type RuntimeErrorType =
    | InvalidInput
    | UnknownError

type ScanErrorType =
    | Foo
