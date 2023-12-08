module Day6

open Prelude

[<Struct>]
type Race =
    { time: int64
      distance: int64 }

    static member ofArray(arr: int64 array) =
        { time = arr[0]; distance = arr[1] }

    static member ofTuple(t: int64 * int64) = { time = fst t; distance = snd t }


let parseRaces =
    String.lines
    >> Array.map (
        String.delete "Time: "
        >> String.delete "Distance: "
        >> String.split " "
        >> Array.filter String.isNotBlank
        >> Array.map int64
    )
    >> Array.transpose
    >> Array.map Race.ofArray


let parseKernedRace =
    String.lines
    >> Array.map (
        String.delete "Time: "
        >> String.delete "Distance: "
        >> String.delete " "
        >> int64
    )
    >> Race.ofArray


let won (race: Race) (holdTime: int64) =
    let won = (race.time - holdTime) * holdTime > race.distance
    if won then [| 1 |] else [||]

let solve (race: Race) =
    [| 1L .. race.time - 1L |]
    |> Array.Parallel.collect (won race)
    |> Array.length


let part1 = parseRaces >> Array.map solve >> Array.product >> printfn "%d"

let part2 = parseKernedRace >> solve >> printfn "%d"
