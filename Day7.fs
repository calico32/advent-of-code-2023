module Day7

open Prelude

[<Struct>]
type CardGroup = { count: int; value: int }

[<Struct>]
type Hand =
    { cards: int array
      bid: int
      groups: CardGroup array
      handType: int }

let parseCard =
    function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | card -> int card - int '0'

let withoutJokers = id

let withJokers =
    function
    | 11 -> 1
    | card -> card

let highestCard a b =
    if a > b then -1
    elif a < b then 1
    else 0

let groupCards cards =
    let cards = cards |> Array.sortByDescending id

    let mutable map = Map.empty

    for card in cards do
        let v = Map.tryFind card map |> Option.defaultValue 0
        map <- map.Add(card, v + 1)

    let jokers = Map.tryFind 1 map |> Option.defaultValue 0

    map.Remove 1 // jokers are always sorted to the end
    |> Map.toArray
    |> Array.map (fun (k, v) -> { count = v; value = k })
    |> Array.sortByDescending (fun x -> x.count)
    |> (fun x -> Array.append x [| { count = jokers; value = 1 } |])

let byRegularClassification (groups: CardGroup array) =
    let groupCount = groups.Length
    let highestCardCount = groups.[0].count

    match (groupCount, highestCardCount) with
    | 1, 5 -> 6 // aaaaa | g=1 h=5
    | 2, 4 -> 5 // aaaab | g=2 h=4
    | 2, 3 -> 4 // aaabb | g=2 h=3
    | 3, 3 -> 3 // aaabc | g=3 h=3
    | 3, 2 -> 2 // aabbc | g=3 h=2
    | 4, 2 -> 1 // aabcd | g=4 h=2
    | _ -> 0 //    abcde | g=5 h=1

let byJokerClassification (groups: CardGroup array) =
    // transform joker into most common card
    if groups.Length = 1 then
        byRegularClassification groups
    else
        groups[0] <-
            { count = groups.[0].count + groups.[groups.Length - 1].count
              value = groups.[0].value }

        groups[groups.Length - 1] <- { count = 0; value = 0 }

        byRegularClassification groups[0 .. groups.Length - 2]

let parseHand cardTransformer classifier line =
    let parts = line |> String.split " "

    let cards =
        parts[0]
        |> String.toCharArray
        |> Array.map (parseCard >> cardTransformer)

    let bid = int parts[1]
    let groups = groupCards cards
    let handType = classifier groups

    { cards = cards
      bid = bid
      groups = groups
      handType = handType }

let compareHands a b =
    let handTypes = compare a.handType b.handType

    if handTypes <> 0 then
        handTypes
    else
        // compare by cards
        a.cards
        |> Array.zip b.cards
        |> Array.map (fun (a, b) -> highestCard a b)
        |> Array.find (fun x -> x <> 0)

let printHand hand =
    let cardNames = String.toArray "0X23456789TJQKA"

    cardNames.[1] <- "\x1b[31mX\x1b[0m"

    for card in hand.cards do
        printf "%s" cardNames.[card]

    printf " - "

    for group in hand.groups do
        String.replicate group.count cardNames.[group.value] |> printf "%s"

    printf " - %d: " hand.handType

    match hand.handType with
    | 6 -> printfn "5 of a kind"
    | 5 -> printfn "4 of a kind"
    | 4 -> printfn "Full house"
    | 3 -> printfn "3 of a kind"
    | 2 -> printfn "2 pair"
    | 1 -> printfn "1 pair"
    | _ -> printfn "High card"

    hand

let rec sortHands =
    function
    | [] -> []
    | x :: xs ->
        let smaller = List.filter (fun e -> compareHands e x < 0) >> sortHands
        let larger = List.filter (fun e -> compareHands e x >= 0) >> sortHands
        smaller xs @ [ x ] @ larger xs

let collectBids = List.mapi (fun i hand -> hand.bid * (i + 1)) >> List.sum

let solve cardTransformer classifier =
    String.lines
    >> Array.map (parseHand cardTransformer classifier)
    >> Array.toList
    >> sortHands
    >> List.map printHand
    >> collectBids
    >> printfn "%d"

let part1 = solve withoutJokers byRegularClassification
let part2 = solve withJokers byJokerClassification
