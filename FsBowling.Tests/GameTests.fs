module FsBowling.GameTests

open Xunit
open FsUnit.Xunit
open Chessie.ErrorHandling

open PlayerName

[<Fact>]
let ``Creating a game should be successful when the list of players is non-empty and when there are no duplications`` () =
    let playerNames = createPlayerNames [ "one"; "two"; "three" ]
    
    let expected = trial {
        let! names = playerNames
        return {
            Players = names |> List.map (fun name ->
                {
                    Name = name
                    Frames = []
                    CurrentFrame = {
                        State = NotStarted
                        Number = 1
                    }
                })
        }
    }
    
    playerNames >>= Game.create |> should equal expected

[<Fact>]
let ``Creating a game should not be successful when the list of players is empty`` () =
    createPlayerNames [] >>= Game.create |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``Creating a game should not be successful when there are duplicate players`` () =
    createPlayerNames [ "one"; "one"; "two"; "three"; "three" ]
    >>= Game.create
    |> shouldBeFailure [ [ "one"; "three" ] |> DuplicatePlayers ]
