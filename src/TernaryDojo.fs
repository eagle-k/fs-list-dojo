module TernaryDojo

open Helper

// Let's develop a library of 3-valued logic with `NULL`.
// Three-valued logic is also used in SQL.
// See: https://en.wikipedia.org/wiki/Three-valued_logic

// The three values can be defined as nullable boolean,
// or simply as an enumerated type, as follows.
type Ternary =
    | False
    | Null
    | True

// Return the same result as boolean `not`,
// but if it is `Null`, it returns `Null`.
//
// tnot: Ternary -> Ternary
let tnot a = Hole?implement_tnot

// Return the same result as boolean `(&&)`,
// but returns `Null` if the result is not sure.
//
// tand: Ternary -> Ternary -> Ternary
let tand a b = Hole?implement_tand

// Return the same result as boolean `(||)`,
// but returns `Null` if the result is not sure.
//
// tor: Ternary -> Ternary -> Ternary
let tor a b = Hole?implement_tor

