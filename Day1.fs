module Day1

open Prelude
open System.Text.RegularExpressions

let numerals =
    [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

let digits = [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]

let numbers =
    Regex @"(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9)"

let stringToDigit (str: string) =
    max (Array.indexOf str numerals) (Array.indexOf str digits) + 1

let parseDigitsOnly (input: string) : int =
    let digits = input |> String.filter System.Char.IsDigit
    string digits.[0] + string digits.[digits.Length - 1] |> int

let parseDigitsAndNumerals (input: string) : int =
    let matches =
        input |> Regex.matchAll numbers |> Seq.cast<Match> |> Seq.toList

    stringToDigit matches[0].Value * 10
    + stringToDigit matches[matches.Length - 1].Value


let part1 =
    String.lines >> Array.map parseDigitsOnly >> Array.sum >> printfn "%d"

let part2 =
    String.lines
    >> Array.map parseDigitsAndNumerals
    >> Array.sum
    >> printfn "%d"
