module FsBowling.GameTests

open Xunit

[<Fact>]
let ``Creating a game should be successful when the list of players is non-empty and when there are no duplications`` () =
    let playerNames = [ "one"; "two"; "three" ] |> List.map PlayerName
    
    let expected = {
        Players = playerNames |> List.map (fun name ->
            {
                Name = name
                Frames = []
                CurrentFrame = {
                    State = NotStarted
                    Number = 1
                }
            })
    }
    
    playerNames |> Game.create |> shouldBeSuccess expected
    
[<Fact>]
let ``Creating a game should not be successful when the list of players is empty`` () =
    [] |> Game.create |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``Creating a game should not be successful when there are duplicate players`` () =
    let players = [ "one"; "one"; "two"; "three"; "three" ] |> List.map PlayerName
    players |> Game.create |> shouldBeFailure [ [ "one"; "three" ] |> List.map PlayerName |> DuplicatePlayers ]
