open System
open System.IO

type Player = {
    Name: string
    Symbol: char
    Wins: int
    Draws: int
    Losses: int
}

let createPlayer name symbol = {
    Name = name
    Symbol = symbol
    Wins = 0
    Draws = 0
    Losses = 0
}

let updateStats player result =
    match result with
    | "win" -> { player with Wins = player.Wins + 1 }
    | "draw" -> { player with Draws = player.Draws + 1 }
    | "loss" -> { player with Losses = player.Losses + 1 }
    | _ -> player

let boardCols = 7
let boardRows = 6

let initBoard () : char[][] = Array.init boardRows (fun _ -> Array.create boardCols ' ')

let printBoard (board: char[][]) =
    printfn "\n"
    for c in 1 .. boardCols do
        printf " (%d) " c
    printfn "\n"
    for row in board do
        printf "|"
        for cell in row do
            printf "  %c |" cell
        printfn "\n"
    printfn "%s\n" (String.replicate (boardCols * 4 + 1) "-")

let getPlayerSymbol playerName =
    let rec loop () =
        printf "%s, choose your Token (X or O): " playerName
        match Console.ReadLine().ToUpper() with
        | "X" -> 'X'
        | "O" -> 'O'
        | _ -> printf "Please enter the correct Tokens (X or O)\n"; loop ()
    loop ()

let isValidMove (board: char[][]) column =
    column >= 0 && column < boardCols && board.[0].[column] = ' '

let applyMove (board: char[][]) column symbol =
    let rec findRow r =
        if r >= 0 && board.[r].[column] = ' ' then
            let newRow = Array.copy board.[r]
            newRow.[column] <- symbol
            let newBoard = Array.copy board
            newBoard.[r] <- newRow
            Some (newBoard, (r, column))
        else if r >= 0 then
            findRow (r - 1)
        else
            None
    findRow (boardRows - 1)

let inBounds r c = r >= 0 && r < boardRows && c >= 0 && c < boardCols

let directions = [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (1, 1); (-1, 1); (1, -1) |]

let checkWinner (board: char[][]) (lastRow, lastCol) =
    let lastLetter = board.[lastRow].[lastCol]
    let searchDirection (dr, dc) =
        let rec countMatches r c count =
            if inBounds r c && board.[r].[c] = lastLetter then
                countMatches (r + dr) (c + dc) (count + 1)
            else
                count
        countMatches (lastRow + dr) (lastCol + dc) 0 + countMatches (lastRow - dr) (lastCol - dc) 0 - 1
    Array.exists (fun i -> 
        let (dr1, dc1) = directions.[i * 2]
        let (dr2, dc2) = directions.[i * 2 + 1]
        searchDirection (dr1, dc1) + searchDirection (dr2, dc2) - 1 >= 2
    ) [|0..3|]

let isBoardFull (board: char[][]) =
    board |> Array.forall (fun row -> row |> Array.forall (fun cell -> cell <> ' '))

let getCpuMove (board: char[][]) =
    let rnd = Random()
    let rec findValidMove () =
        let col = rnd.Next(boardCols)
        if isValidMove board col then col
        else findValidMove ()
    findValidMove ()

let saveStatsToFile player1 player2 =
    try
        let stats = sprintf "%s: Wins - %d, Draws - %d, Losses - %d\n%s: Wins - %d, Draws - %d, Losses - %d\n"
                             player1.Name player1.Wins player1.Draws player1.Losses
                             player2.Name player2.Wins player2.Draws player2.Losses
        File.AppendAllText("player_stats.txt", stats)
    with
    | ex -> printfn "An error occurred while saving stats: %s" (ex.Message)

let rec playGame board player1 player2 currentPlayer =
    printBoard board
    let validMove, newBoard, newPlayer1, newPlayer2, gameWon =
        if currentPlayer.Name = "CPU" then
            let cpuMove = getCpuMove board
            printfn "CPU chooses column %d" (cpuMove + 1)
            match applyMove board cpuMove currentPlayer.Symbol with
            | Some (newBoard, pos) when checkWinner newBoard pos ->
                let updatedPlayer = updateStats currentPlayer "win"
                let otherPlayer = updateStats (if currentPlayer = player1 then player2 else player1) "loss"
                printBoard newBoard
                printfn "Congratulations %s! You won!" currentPlayer.Name
                true, newBoard, (if currentPlayer = player1 then updatedPlayer else otherPlayer), (if currentPlayer = player2 then updatedPlayer else otherPlayer), true
            | Some (newBoard, _) when isBoardFull newBoard ->
                let updatedPlayer1 = updateStats player1 "draw"
                let updatedPlayer2 = updateStats player2 "draw"
                printBoard newBoard
                printfn "The game is a draw!"
                true, newBoard, updatedPlayer1, updatedPlayer2, true
            | Some (newBoard, _) ->
                true, newBoard, player1, player2, false
            | None -> false, board, player1, player2, false
        else
            printfn "%s's turn (Symbol: %c)" currentPlayer.Name currentPlayer.Symbol
            printf "Choose a column (1-%d): " boardCols
            match Int32.TryParse(Console.ReadLine()) with
            | true, column when isValidMove board (column - 1) ->
                match applyMove board (column - 1) currentPlayer.Symbol with
                | Some (newBoard, pos) when checkWinner newBoard pos ->
                    let updatedPlayer = updateStats currentPlayer "win"
                    let otherPlayer = updateStats (if currentPlayer = player1 then player2 else player1) "loss"
                    printBoard newBoard
                    printfn "Congratulations %s! You won!" currentPlayer.Name
                    true, newBoard, (if currentPlayer = player1 then updatedPlayer else otherPlayer), (if currentPlayer = player2 then updatedPlayer else otherPlayer), true
                | Some (newBoard, _) when isBoardFull newBoard ->
                    let updatedPlayer1 = updateStats player1 "draw"
                    let updatedPlayer2 = updateStats player2 "draw"
                    printBoard newBoard
                    printfn "The game is a draw!"
                    true, newBoard, updatedPlayer1, updatedPlayer2, true
                | Some (newBoard, _) ->
                    true, newBoard, player1, player2, false
                | None -> false, board, player1, player2, false
            | _ ->
                printfn "Column is full. Please pick another column."
                false, board, player1, player2, false

    if gameWon then
        newBoard, newPlayer1, newPlayer2, gameWon
    else
        playGame newBoard newPlayer1 newPlayer2 (if currentPlayer = player1 then player2 else player1)

let displayStats player1 player2 =
    printfn "\nGame Over!\n"
    printfn "------------"
    printfn "\nThe Record of Wins, losses, and Draw."
    printfn "---------------------------------------------"
    printfn "%s's stats: Wins - %d, Draws - %d, Losses - %d\n" player1.Name player1.Wins player1.Draws player1.Losses
    printfn "%s's stats: Wins - %d, Draws - %d, Losses - %d\n" player2.Name player2.Wins player2.Draws player2.Losses

let rec askForRematch player1 player2 =
    printfn "Would you like a rematch? (y/n)"
    match Console.ReadLine().ToLower() with
    | "y" ->
        let finalBoard, finalPlayer1, finalPlayer2, _ = playGame (initBoard ()) player1 player2 player1
        displayStats finalPlayer1 finalPlayer2
        saveStatsToFile finalPlayer1 finalPlayer2
        askForRematch finalPlayer1 finalPlayer2
    | "n" -> ()
    | _ ->
        printfn "Invalid input. Please enter 'y' or 'n'."
        askForRematch player1 player2

let rec mainMenu () =
    printfn "****************Welcome to Connect Four!****************\n"
    printfn "---------------------------------------------"
    printfn "Main Menu:"
    printfn "1. Start a Two Player Game"
    printfn "2. Start a Single Player Game"
    printfn "3. Exit Game"
    printfn "---------------------------------------------"
    match Console.ReadLine() with
    | "1" ->
        printf "Enter Player 1's name: "
        let player1Name = Console.ReadLine()
        let player1Symbol = getPlayerSymbol player1Name
        printf "Enter Player 2's name: "
        let player2Name = Console.ReadLine()
        let player2Symbol = if player1Symbol = 'X' then 'O' else 'X'
        let player1 = createPlayer player1Name player1Symbol
        let player2 = createPlayer player2Name player2Symbol
        let finalBoard, finalPlayer1, finalPlayer2, _ = playGame (initBoard ()) player1 player2 player1
        displayStats finalPlayer1 finalPlayer2
        saveStatsToFile finalPlayer1 finalPlayer2
        askForRematch finalPlayer1 finalPlayer2
        mainMenu ()
    | "2" ->
        printf "Enter your name: "
        let playerName = Console.ReadLine()
        let playerSymbol = getPlayerSymbol playerName
        let cpuSymbol = if playerSymbol = 'X' then 'O' else 'X'
        let player = createPlayer playerName playerSymbol
        let cpuPlayer = createPlayer "CPU" cpuSymbol
        let finalBoard, finalPlayer, finalCpuPlayer, _ = playGame (initBoard ()) player cpuPlayer player
        displayStats finalPlayer finalCpuPlayer
        saveStatsToFile finalPlayer finalCpuPlayer
        askForRematch finalPlayer finalCpuPlayer
        mainMenu ()
    | "3" -> printfn "\nThanks for playing!\n"
    | _ ->
        printfn "Invalid choice. Please enter a valid option."
        mainMenu ()

[<EntryPoint>]
let main argv =
    mainMenu ()
    0




