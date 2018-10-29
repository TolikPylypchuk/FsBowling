namespace FsBowling

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    let inline run state (State f) = f state

    let get = State (fun s -> s, s)

    let put newState = State (fun _ -> (), newState)

    let update f = State (fun s -> (), f s)

    let map f s = State (fun (state : 's) ->
        let x, state = run state s
        f x, state)

type StateBuilder() =

    member __.Zero () = State(fun s -> (), s)

    member __.Return x = State(fun s -> x, s)

    member inline __.ReturnFrom (x: State<'s, 'a>) = x

    member __.Bind (x, f) : State<'s, 'b> =
        State(fun state ->
            let result, state = State.run state x
            State.run state (f result))

    member __.Combine (x1: State<'s, 'a>, x2: State<'s, 'b>) =
        State(fun state ->
            let _, state = State.run state x1
            State.run state x2)
    member __.Delay f : State<'s, 'a> = f ()

    member this.For (seq, (f: 'a -> State<'s, 'b>)) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))

    member this.While (f, x) =
        if f ()
        then this.Combine (x, this.While (f, x))
        else this.Zero ()

[<AutoOpen>]
module StateBuilder =
    let state = new StateBuilder()
