open System.IO

let run (argv: string array) =
    if argv.Length < 2 then
        failwith "Usage: dotnet run <day> <part> [variant]"

    let day = int argv.[0]
    let part = int argv.[1]
    let variant = Array.tryItem 2 argv |> Option.defaultValue ""

    if day < 1 || day > AdventOfCode.days.Length then
        failwith $"Day {day} not implemented"

    let input = AdventOfCode.readInput day (Array.tryItem 2 argv)

    let day =
        AdventOfCode.days
        |> Array.tryFind (fun x ->
            x.day = day && x.variant = variant
            || ((variant = "e" || variant = "example") && x.variant = ""))

    match day with
    | Some day ->
        if part = 1 then day.part1 input
        elif part = 2 then day.part2 input
        else failwith $"Part {part} not implemented"
    | None -> failwith $"Day {day} with variant {variant} not implemented"

    0


[<EntryPoint>]
let main argv =
    try
        run argv
    with Failure message ->
        printfn "Error: %s" message
        1
