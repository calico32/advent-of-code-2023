module AdventOfCode

open Prelude

open System.IO

let fileError (file: string) =
    $"input file {file} not found or inaccessible"

let readRegularInput (day: int) : Result<string, string> =
    let path = sprintf "./input/day%d.txt" day

    try
        File.ReadAllText path |> Ok
    with _ ->
        fileError path |> Error

let readExampleInput (day: int) : Result<string, string> =
    let path = sprintf "./input/example%d.txt" day

    try
        File.ReadAllText path |> Ok
    with _ ->
        fileError path |> Error

let readArbitraryInput (path: string) : Result<string, string> =
    try
        File.ReadAllText path |> Ok
    with _ ->
        fileError path |> Error

let readInput (day: int) (specifier: string option) : string =
    match specifier with
    | Some "e"
    | Some "example" -> readExampleInput day
    | _ -> readRegularInput day
    |> Result.unwrap



type Day
    private
    (part1: string -> unit, part2: string -> unit, day: int, variant: string) =
    static member Variant
        (variant: string)
        (day: int)
        (part1: string -> unit)
        (part2: string -> unit)
        =
        Day(part1, part2, day, variant)

    static member New
        (day: int)
        (part1: string -> unit)
        (part2: string -> unit)
        =
        Day(part1, part2, day, "")

    member this.part1 = part1
    member this.part2 = part2
    member this.day = day
    member this.variant = variant


let days =
    // add a variant with `Day.Variant "variant" day part1 part2`
    [| Day.New 1 Day1.part1 Day1.part2
       Day.New 2 Day2.part1 Day2.part2
       Day.New 3 Day3.part1 Day3.part2
       Day.New 4 Day4.part1 Day4.part2
       Day.New 5 Day5.part1 Day5.part2
       Day.New 6 Day6.part1 Day6.part2
       Day.New 7 Day7.part1 Day7.part2 |]
