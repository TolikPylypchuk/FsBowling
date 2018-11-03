module FsBowling.PlayerTests

open Xunit

[<Fact>]
let ``Creating a player should always be successful`` () =
    let name = PlayerName "test"

    let expected = {
        Name = name
        Frames = [ {
            State = NotStarted
            Number = 1
        } ]
    }

    Player.create name |> shouldBeSuccess expected

[<Fact>]
let ``A list of players should be valid when it's non-empty and when there are no duplications`` () =
    let players = [ "one"; "two"; "three" ]
    players |> PlayerName.createPlayerNames |> shouldBeSuccess (players |> List.map PlayerName)
    
[<Fact>]
let ``A list of players should not be valid when it's empty`` () =
    [] |> PlayerName.createPlayerNames |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``A list of players should not be valid when there are duplications`` () =
    let players = [ "one"; "one"; "two"; "three"; "three" ]
    players |> PlayerName.createPlayerNames |> shouldBeFailure [ [ "one"; "three" ] |> DuplicatePlayers ]
