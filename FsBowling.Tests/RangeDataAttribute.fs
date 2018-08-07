namespace FsBowling

open Xunit.Sdk

type RangeDataAttribute(start : int, end' : int) =
    inherit DataAttribute()

    override __.GetData _ : seq<obj[]> =
        Seq.init (end' - start) (fun index -> start + index)
        |> Seq.map (fun a -> [| a |])
