module OptionDojo

open Helper

// The standard F# option
// https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-optionmodule.html
let standardNoneOption: option<int> = None
let standardSomeOption: option<int> = Some 42

// `option<'a>` can also be written as `'a option`.
let standardNoneOption': int option = None
let standardSomeOption': int option = Some 42


// User-defined Option for learning purpose!
// Note that the F# standard type has the first character in lowercase.
type Option<'a> =
    | Some' of 'a
    | None'

// In F#, we prefer to use option rather than bool.
let tryParseInt' (s: string): Option<int> =
    // In F#, a function that uses the `out` parameter
    // automatically becomes a function that returns a tuple.
    let success, result = System.Int32.TryParse s

    // Prefer `option` to `bool`
    if success then
        Some' result
    else
        None'

// Create utilities to handle options.
let defaultValue' value opt =
    match opt with
    | Some' x -> x
    | None' -> value

let map' f opt =
    match opt with
    | Some' x -> Some' (f x)
    | None' -> None'

// Use them!
let createNumberMsg' ageStr =
    tryParseInt' ageStr
    |> map' (sprintf "Your favorite number: %d")
    |> defaultValue' "input must be an integer"

let validNumberMsg = createNumberMsg' "42" // "Your favorite number: 42"
let invalidNumberMsg = createNumberMsg' "not numeric" // "input must be an integer"


// Now let's go back to the standard `option`!
// Define a function as before that parses the string as an `int` and returns the standard `option`.
let tryParseInt (s: string): int option =
    match System.Int32.TryParse s with
    | true, result -> Some result
    | false, _ -> None

// It can be used in the same way as before.
let createAgeMsg ageStr =
    tryParseInt ageStr
    |> Option.map (sprintf "Your favorite number: %d")
    |> Option.defaultValue "input must be an integer"


// Define a custom type `Age`
//
// REQUIREMENT:
// Age should be an integer between 0 (inclusive) and 130 (inclusive).
type Age = Age of int16

// In F#, numbers of type `int16` can be defined by adding the `s` suffix after the literal.
let fourtyTwoYearsOld = Age 42s


// Exercises!

// tryParseInt16: string -> int16 option
let tryParseInt16 (s: string): int16 option = Hole?implement_tryParseInt16

// isValidAge: int16 -> boolean
let isValidAge n = Hole?implement_isValidAge

// showAge: Age -> string
let showAge age = Hole?implement_showAge

// Not all `int16` values represent a reasonable age.
// Therefore, the return value should be `option`, not just `Age`.
//
// mkAge: int16 -> Age option
let mkAge n = Hole?implement_mkAge

// tryParseAge: string -> Age option
let tryParseAge s = Hole?implement_tryParseAge

// Parses a string as an age and returns a message string.
// 
// REQUIREMENT:
// Return a string containing the word `"invalid"`
// if the age could not be parsed or is out of range.
//
// ageToMessage: string -> string
let ageToMessage s = Hole?implement_ageToMessage

