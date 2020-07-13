module Flox.Location

type Location =
    { Line : int
      Column : int }

let incrementColumnBy location increment =
    { location with Column = location.Column + increment }

let nextLine location =
    { Line = location.Line + 1
      Column = 1 }

let nextColumn location =
    { location with Column = location.Column + 1 }

let start () =
    { Line = 1
      Column = 1 }
