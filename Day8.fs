module Day8

open Prelude

type Instruction =
    | Left
    | Right

[<Struct>]
type Network =
    { commands: Instruction array
      nodes: Map<string, string * string>
      mutable locations: string array
      mutable step: int }

let parseCommand =
    function
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "invalid command"

let parseInput (input: string) =
    let sections = input |> String.split "\n\n"
    let commands = sections.[0] |> String.toCharArray |> Array.map parseCommand

    let nodes =
        sections.[1]
        |> String.split "\n"
        |> Array.map (fun line ->
            let parts = line |> String.split " = "
            let key = parts.[0]

            let leaves =
                parts.[1]
                |> String.delete "("
                |> String.delete ")"
                |> String.split ", "

            key, (leaves.[0], leaves.[1]))
        |> Map.ofArray

    { commands = commands
      nodes = nodes
      locations = [||]
      step = 0 }

let step (network: Network byref) =
    let command = network.commands.[network.step % network.commands.Length]
    let nodes = network.nodes

    network.locations <-
        network.locations
        |> Array.map (fun k ->
            let (left, right) = nodes.[k]

            match command with
            | Left -> left
            | Right -> right)

    network.step <- network.step + 1

let rec gcd a b = if b = 0L then abs a else gcd b (a % b)
let lcm a b = a * b / (gcd a b)

let period (network: Network) (location: string) =
    let mutable step = 0
    let mutable destination = location

    while not (destination.EndsWith "Z") do
        let command = network.commands.[step % network.commands.Length]

        let (left, right) = network.nodes.[destination]

        match command with
        | Left -> destination <- left
        | Right -> destination <- right

        step <- step + 1

    int64 step

let part1 (input: string) =
    let mutable network = parseInput input
    network.locations <- [| "AAA" |]

    while network.locations[0] <> "ZZZ" do
        step &network

    printfn "%d" network.step

let part2 (input: string) =
    let network = parseInput input

    network.nodes.Keys
    |> Seq.filter (String.endsWith "A")
    |> Seq.toArray
    |> Array.map (period network)
    |> Array.fold lcm 1L
    |> printfn "%d"
