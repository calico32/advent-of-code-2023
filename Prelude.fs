module Prelude

open System.Text.RegularExpressions

module List =
    /// Return the product of all elements in the list.
    let product (list: int list) = list |> List.fold (*) 1

    /// Return a list of indices representing the original list sorted in descending order.
    let gradeDown (list: 'T list) =
        list |> List.indexed |> List.sortByDescending snd |> List.map fst

    /// Return a list of indices representing the original list sorted in ascending order.
    let gradeUp (list: 'T list) =
        list |> List.indexed |> List.sortBy snd |> List.map fst

module Array =
    /// Return the product of all elements in the array.
    let product (array: int array) = array |> Array.fold (*) 1

    /// Return the index of the first element is equal to the given value, or -1 if not found.
    let indexOf (value: 'T) (array: 'T array) =
        array
        |> Array.tryFindIndex (fun x -> x = value)
        |> Option.defaultValue -1

module String =
    /// Split a string into lines. This is equivalent to `string.Split "\n"`.
    let lines (string: string) = string.Split "\n"
    /// Split a string given a separator. This is equivalent to `string.Split`.
    let split (separator: string) (str: string) = str.Split separator

    /// Replace all instances of a string with another string. This is
    /// equivalent to `string.Replace`.
    let replace (str: string) (replacement: string) (input: string) =
        input.Replace(str, replacement)

    /// Delete all instances of a string. This is equivalent to
    /// `string.Replace(str, "")`.
    let delete (str: string) (input: string) = input.Replace(str, "")

    /// Trim whitespace from both ends of a string. This is equivalent to
    /// `string.Trim()`.
    let trim (input: string) = input.Trim()

    /// Returns true if the string is empty or whitespace and false otherwise.
    /// This is equivalent to `string.Trim() = ""`.
    let isBlank (input: string) = input.Trim() = ""

    /// Returns true if the string is not empty or whitespace and false otherwise.
    /// This is equivalent to `string.Trim() <> ""`.
    let isNotBlank (input: string) = input.Trim() <> ""

    /// Returns an array of characters representing the string. This is
    /// equivalent to `string.ToCharArray()`.
    let toCharArray (input: string) = input.ToCharArray()

    /// Returns an array of single-character strings representing the string.
    /// This is equivalent to `Array.map string string.ToCharArray()`.
    let toArray (input: string) = input.ToCharArray() |> Array.map string

type Result<'T, 'TError> with

    /// Unwraps a Result<'T, 'TError> into a 'T, or throws an exception with the error message
    static member unwrap(result: Result<'T, 'TError>) =
        match result with
        | Ok x -> x
        | Error e -> failwith (e.ToString())

module Debug =
    let print (value: 'T) =
        printfn "%A" value
        value


module Tuple =
    let toArray (tuple: 'T * 'T) = [| fst tuple; snd tuple |]

    let toArray3 (tuple: 'T * 'T * 'T) =
        let a, b, c = tuple
        [| a; b; c |]



type Regex with

    static member matchAll (regex: Regex) (input: string) =
        let rec loop (matches: Match list) (input: string) =
            let m = regex.Match input

            if m.Success then
                loop (m :: matches) (input.Substring(m.Index + 1))
            else
                matches

        loop [] input |> List.rev
