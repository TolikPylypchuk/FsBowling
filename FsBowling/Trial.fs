module Chessie.ErrorHandling.Trial

let rec traverse f list =

    let cons head tail = head :: tail

    match list with
    | [] -> [] |> ok
    | head :: tail -> cons <!> (f head) <*> (traverse f tail)

let sequence list = list |> traverse id
