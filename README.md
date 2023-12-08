# Advent of Code 2023

F# solutions to the [Advent of Code 2023](https://adventofcode.com/2023) puzzles.

## Input files

Input files are expected to be at `./input/dayX.txt`, where `X` is the day number.

The examples from the puzzles are included at `./input/exampleX.txt`.

## Running the solutions

To run the solutions, use the `dotnet run` command from the root of the repository.

```bash
dotnet run <day> <part> [variant]
```

The `variant` argument is optional and can be used to specify either the example input file (variant `e` or `example`) or a custom moddule (see `AdventOfCode.fs`).
