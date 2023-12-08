module Day4

open Prelude

type Card =
    { id: int
      winningNumbers: int Set
      hand: int list }

let parseInts = String.split " " >> Seq.filter String.isNotBlank >> Seq.map int

let parseCards =
    String.lines
    >> Seq.filter String.isNotBlank
    >> Seq.map (fun line ->
        let parts = line.Split ": "
        let sections = parts.[1].Split " | "

        { id = parts.[0] |> String.delete "Card " |> int
          winningNumbers = sections.[0] |> parseInts |> Set.ofSeq
          hand = sections.[1] |> parseInts |> Seq.toList })

let score (card: Card) =
    card.hand |> Seq.filter card.winningNumbers.Contains |> Seq.length

let power2 x = pown 2 (x - 1)

let count (cards: int list) : int =
    let rec count' (cards: int list) (multipliers: int list) : int =
        match cards with
        | [] -> 0
        | x :: xs ->
            let modified, rest = multipliers.Tail |> List.splitAt x

            multipliers.Head
            + count' xs (List.map ((+) multipliers.Head) modified @ rest)

    count' cards (List.replicate cards.Length 1)

let part1 = parseCards >> Seq.map (score >> power2) >> Seq.sum >> printfn "%d"

let part2 = parseCards >> Seq.toList >> List.map score >> count >> printfn "%d"
