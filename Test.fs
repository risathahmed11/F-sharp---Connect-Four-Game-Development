module Test

open NUnit.Framework
open FsUnit
open ConnectFourGame

[<Test>]
let ``CreatePlayer initializes with correct values`` () =
    let player = createPlayer "Max" 'X'
    player.Name |> should equal "Max"
    player.Symbol |> should equal 'X'
    player.Wins |> should equal 0
    player.Draws |> should equal 0
    player.Losses |> should equal 0

[<Test>]
let ``UpdateStats increments wins correctly`` () =
    let player = createPlayer "Max" 'X'
    let updatedPlayer = updateStats player "win"
    updatedPlayer.Wins |> should equal 1

[<Test>]
let ``InitBoard creates an empty board`` () =
    let board = initBoard ()
    board.Length |> should equal boardRows
    board.[0].Length |> should equal boardCols
    board |> Array.forall (fun row -> row |> Array.forall (fun cell -> cell = ' ')) |> should be True

[<Test>]
let ``IsValidMove returns true for valid move`` () =
    let board = initBoard ()
    isValidMove board 0 |> should be True

[<Test>]
let ``ApplyMove places symbol correctly`` () =
    let board = initBoard ()
    let newBoard, _ = applyMove board 0 'X' |> Option.get
    newBoard.[boardRows - 1].[0] |> should equal 'X'

[<Test>]
let ``CheckWinner returns true for a winning move`` () =
    let board = Array.init boardRows (fun i -> Array.create boardCols ' ')
    board.[boardRows - 1].[0] <- 'X'
    board.[boardRows - 2].[0] <- 'X'
    board.[boardRows - 3].[0] <- 'X'
    let newBoard, pos = applyMove board 0 'X' |> Option.get
    checkWinner newBoard pos |> should be True

[<Test>]
let ``IsBoardFull returns true for a full board`` () =
    let board = Array.init boardRows (fun _ -> Array.create boardCols 'X')
    isBoardFull board |> should be True

[<Test>]
let ``GetCpuMove returns a valid move`` () =
    let board = initBoard ()
    let move = getCpuMove board
    isValidMove board move |> should be True

[<Test>]
let ``SaveStatsToFile saves the stats`` () =
    let player1 = createPlayer "Max" 'X'
    let player2 = createPlayer "Lily" 'O'
    saveStatsToFile player1 player2
    let stats = System.IO.File.ReadAllText("player_stats.txt")
    stats |> should contain "Max: Wins - 0, Draws - 0, Losses - 0"
    stats |> should contain "Lily: Wins - 0, Draws - 0, Losses - 0"
