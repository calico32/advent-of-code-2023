module Day3

open Prelude

let symbols = Set.ofSeq "@#$%&*/=+-"
let deltas = [ -1; 0; 1 ]

type Position = { row: int; col: int }
type Range = { first: int; last: int }
type Number = { value: int; pos: Position }

let findNumber (line: char array) (source: int) : (int * Range) =
    let mutable i = source
    let mutable j = source

    // search left
    while i > 0 && System.Char.IsDigit line.[i - 1] do
        i <- i - 1

    // search right
    while j < line.Length && System.Char.IsDigit line.[j] do
        j <- j + 1

    (line[i .. j - i] |> string |> int, { first = i; last = j })

let forAllSymbols (f: char -> Position -> 'a) (input: string) : list<'a> =
    let lines = input |> String.lines |> Array.map String.toCharArray

    let mutable results = []

    for row = 0 to lines.Length - 1 do
        for col = 0 to lines.[row].Length - 1 do
            let c = lines.[row].[col]

            if symbols.Contains c then

                results <- results @ [ f c { row = row; col = col } ]

    results


let numbersAround (pos: Position) (input: string) =
    let lines = input |> String.lines |> Array.map String.toCharArray
    let mutable numbers = Set.empty

    for r' in deltas do
        for c' in deltas do
            let row' = pos.row + r'
            let col' = pos.col + c'

            if
                row' >= 0
                && row' < lines.Length
                && col' >= 0
                && col' < lines.[row'].Length
                && System.Char.IsDigit lines.[row'].[col']
            then
                let number, range = findNumber lines.[row'] col'

                let pos = { row = row'; col = range.first }
                let value = { value = number; pos = pos }

                numbers <- numbers.Add value

    numbers |> Set.toSeq |> Seq.map (fun n -> n.value) |> Seq.toList


let part1 (input: string) =
    input
    |> forAllSymbols (fun _ pos -> numbersAround pos input |> Seq.sum)
    |> Seq.sum
    |> printfn "%d"

let part2 (input: string) =
    input
    |> forAllSymbols (fun symbol pos ->
        if symbol = '*' then
            let numbers = numbersAround pos input
            if numbers.Length = 2 then (Seq.reduce (*) numbers) else 0
        else
            0)
    |> Seq.sum
    |> printfn "%d"
