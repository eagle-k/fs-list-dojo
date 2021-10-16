module TernaryTest

open Expecto
open TernaryDojo

// In F#, you can also define your own infix operators.
// Following the existing (&&) and (||),
// we define the operator in three-valued logic as follows for convenience.
let (&.&) = tand
let (|.|) = tor


[<Tests>]
let tests =
    testList "TernaryDojo" [
        ptestList "tnot properties" [
            testProperty "double negative elimination" <| fun a ->
                tnot (tnot a) = a
        ]

        ptestList "tand properties" [
            testProperty "idempotent" <| fun a ->
                (a &.& a) = a

            testProperty "identity" <| fun a ->
                (a &.& True) = a

            testProperty "associativity" <| fun a b c ->
                ((a &.& b) &.& c) = (a &.& (b &.& c))

            testProperty "commutativity" <| fun a b ->
                (a &.& b) = (b &.& a)
        ]

        ptestList "tor properties" [
            testProperty "idempotent" <| fun a ->
                (a |.| a) = a

            testProperty "identity" <| fun a ->
                (a |.| False) = a

            testProperty "associativity" <| fun a b c ->
                ((a |.| b) |.| c) = (a |.| (b |.| c))

            testProperty "commutativity" <| fun a b ->
                (a |.| b) = (b |.| a)
        ]

        ptestList "mixed properties" [
            testProperty "tor << tand absorption" <| fun a b ->
                (a |.| (a &.& b)) = a

            testProperty "tand << tor absorption" <| fun a b ->
                (a &.& (a |.| b)) = a

            testProperty "distribution" <| fun a b c ->
                (a &.& (b |.| c)) = ((a &.& b) |.| (a &.& c))

            testProperty "De Morgan" <| fun a b ->
                (tnot (a &.& b)) = ((tnot a) |.| (tnot b))
        ]
    ]

// Note: this 3-valued logic is a De Morgan algebra.
// The law of excluded middle and the law of non-contradiction can hold with booleans, but not in this case.

