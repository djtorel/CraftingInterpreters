module Flox.Location

type Location =
    { Line : int
      Column : int }

let next_column location increment =
    { location with Column = location.Column + increment }

let next_line location =
    { Line = location.Line + 1
      Column = 0 }

let start () =
    { Line = 1
      Column = 0 }
