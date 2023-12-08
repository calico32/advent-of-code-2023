module Day5

open Prelude

#nowarn "25"

[<Struct>]
type Range =
    { source: int64
      target: int64
      length: int64 }

[<Struct>]
type Sieve =
    { source: string
      target: string
      ranges: Range array }

[<Struct>]
type Almanac =
    { seeds: int64 array
      sieves: Sieve list }


let parseSieve (input: string) : Sieve =
    let lines = input |> String.lines |> Array.toList

    if lines.Length = 0 then
        failwith "Invalid sieve"

    let header :: ranges = lines
    let header = header |> String.delete " map:" |> String.split "-to-"

    { source = header[0]
      target = header[1]
      ranges =
        ranges
        |> List.map (fun line ->
            let parts = line.Split " " |> Array.map int64

            { source = parts.[1]
              target = parts.[0]
              length = parts.[2] })
        |> List.toArray }

let sieve (source: int64) (sieve: Sieve) : int64 =
    let range =
        sieve.ranges
        |> Array.tryFind (fun range ->
            source >= range.source
            && source <= range.source + range.length - int64 1)

    match range with
    | None -> source
    | Some range ->
        let offset = source - range.source
        range.target + offset


let parseAlmanac (input: string) : Almanac =
    let sections = input.Split "\n\n" |> Array.toList

    let seeds =
        sections.Head
        |> String.delete "seeds: "
        |> String.split " "
        |> Seq.map int64
        |> Seq.toArray

    { seeds = seeds
      sieves = sections.Tail |> List.map parseSieve }

let parseAlmanacComplex (input: string) : Almanac seq * int =
    let sections = input |> String.split "\n\n" |> Array.toList

    let seedRanges =
        sections.Head
        |> String.delete "seeds: "
        |> String.split " "
        |> Array.map int64
        |> Array.chunkBySize 2

    let sieves = sections.Tail |> List.map parseSieve

    seq {
        for seedRange in seedRanges do
            let seeds =
                [| seedRange.[0] .. seedRange.[0] + seedRange[1] - int64 1 |]

            yield { seeds = seeds; sieves = sieves }
    },
    seedRanges.Length

let doSieve (almanac: Almanac) =
    almanac.seeds
    |> Array.Parallel.map (fun seed -> almanac.sieves |> List.fold sieve seed)

let part1 = parseAlmanac >> doSieve >> Array.min >> printfn "%d"

let part2 (input: string) =
    let almanacs, almanacCount = parseAlmanacComplex input

    let mins =
        almanacs
        |> Seq.mapi (fun i almanac ->
            printfn
                "almanac %d/%d: %d seeds"
                (i + 1)
                almanacCount
                almanac.seeds.Length

            let min = doSieve almanac |> Array.min

            printfn "almanac %d/%d: %d" (i + 1) almanacCount min

            min)
        |> Seq.toArray

    printfn "--- %d ---" (Array.min mins)
