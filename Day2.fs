module Day2

open Prelude

type Hand = { red: int; green: int; blue: int }
type Game = { id: int; hands: Hand array }

let parseHand =
    String.split ", "
    >> Seq.fold
        (fun hand part ->
            let parts = part.Split " "
            let value = parts.[0] |> int

            match parts.[1] with
            | "red" -> { hand with red = value }
            | "green" -> { hand with green = value }
            | "blue" -> { hand with blue = value }
            | _ -> failwith "Invalid color")
        { red = 0; green = 0; blue = 0 }

let parseLine (input: string) : Game =
    let parts = input.Split ": "

    { id = parts[0] |> String.delete "Game " |> int
      hands = parts[1].Split "; " |> Array.map parseHand }

let matchesConstraint (game) =
    game.hands
    |> Array.forall (fun hand ->
        hand.red <= 12 && hand.green <= 13 && hand.blue <= 14)

let minHand (game: Game) =
    game.hands
    |> Array.fold
        (fun acc hand ->
            { red = max acc.red hand.red
              green = max acc.green hand.green
              blue = max acc.blue hand.blue })
        { red = 0; green = 0; blue = 0 }

let power (hand: Hand) = hand.red * hand.green * hand.blue

let part1 =
    String.lines
    >> Array.map parseLine
    >> Array.filter matchesConstraint
    >> Array.sumBy (fun game -> game.id)
    >> printfn "%d"

let part2 =
    String.lines
    >> Array.map parseLine
    >> Array.map minHand
    >> Array.sumBy power
    >> printfn "%d"
