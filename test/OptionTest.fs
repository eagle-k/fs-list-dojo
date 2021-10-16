module OptionTest

open Expecto
open FsCheck
open OptionDojo
open FSharp.Core

[<Tests>]
let tests =
    testList "OptionDojo" [
        ptestList "tryParseInt16" [
            testCase "tryParseInt16 cases" <| fun () ->
                Expect.equal (tryParseInt16 "") None "empty string"
                Expect.equal (tryParseInt16 "42") (Some 42s) "42"
                Expect.equal (tryParseInt16 "2e2") None "2e2"
                Expect.equal (tryParseInt16 "32767") (Some 32767s) "max"
                Expect.equal (tryParseInt16 "-32768") (Some -32768s) "min"
                Expect.equal (tryParseInt16 "32768") None "max + 1"
                Expect.equal (tryParseInt16 "-32769") None "min - 1"

            // fsharplint:disable-next-line CanBeReplacedWithComposition
            testProperty "tryParseInt16 property" <| fun (x: int16) ->
                Option.isSome (tryParseInt16 (string x))
        ]

        ptestList "isValidAge" [
            testCase "isValidAge cases" <| fun () ->
                Expect.isFalse (isValidAge -1s) "-1"
                Expect.isTrue (isValidAge 0s) "0"
                Expect.isTrue (isValidAge 130s) "130"
                Expect.isFalse (isValidAge 131s) "131"

            testProperty "isValidAge property" <| fun x ->
                (x <> -1s && x <> 130s) ==> (isValidAge x = isValidAge (x + 1s))
        ]

        ptestList "showAge" [
            testCase "showAge cases" <| fun () ->
                Expect.stringContains (showAge (Age 42s)) "42" "It should show the age `42` to the user."

            testProperty "showAge property" <| fun age ->
                (isValidAge age) ==> (TestHelper.isSubstring (string age) (showAge (Age age)))
        ]

        ptestList "tryParseAge" [
            testCase "tryParseAge cases" <| fun () ->
                Expect.equal (tryParseAge "42") (Some (Age 42s)) ""
                Expect.equal (tryParseAge "131") None ""
                Expect.equal (tryParseAge "abc") None ""
                Expect.equal (tryParseAge "2e2") None ""
               
            testProperty "tryParseAge proprety when valid" <| fun x ->
                (isValidAge x) ==> (tryParseAge (string x) = Some (Age x))

            testProperty "tryParseAge property when invalid" <| fun x ->
                (isValidAge x |> not) ==> Option.isNone (tryParseAge (string x))
        ]

        ptestList "ageToMessage" [
            testCase "ageToMessage cases" <| fun () ->
                Expect.stringContains (ageToMessage "42") "42" "42"
                Expect.stringContains (ageToMessage "131") "invalid" "131"
                Expect.stringContains (ageToMessage "abc") "invalid" "abc"
                Expect.stringContains (ageToMessage "2e2") "invalid" "2e2"

            testProperty "ageToMessage property when valid" <| fun x ->
                let s = string x
                let msg = ageToMessage s
                (isValidAge x) ==> (
                    (msg |> TestHelper.isSubstring s) &&
                    (msg |> TestHelper.isSubstring "invalid" |> not)
                )

            testProperty "ageToMessage property when invalid" <| fun x ->
                let s = string x
                (isValidAge x |> not) ==> (
                    ageToMessage s |> TestHelper.isSubstring "invalid"
                )
        ]
    ]

