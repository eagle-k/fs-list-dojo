module ListTest

open Expecto
open FsCheck
open ListDojo
open TestHelper

[<Tests>]
let tests =
    testList "ListDojo" [
        ptestList "isEmpty" [
            testCase "isEmpty cases" <| fun () ->
                Expect.equal (isEmpty Nil) true "empty list"
                Expect.equal (isEmpty (42 --> Nil)) false "singleton list"
                Expect.equal (isEmpty (Cons ("Hello", Cons ("World", Nil)))) false "two items"

            testProperty "isEmpty should return the same result as the standard function" <| fun (xs: int list) ->
                isEmpty (fromStandardList xs) = List.isEmpty xs
        ]

        ptestList "singleton" [
            testCase "singleton cases" <| fun () ->
                Expect.equal (singleton 42) (42 --> Nil) "int to int list"
                Expect.equal (singleton (singleton "Hello")) (("Hello" --> Nil) --> Nil) "string to string list"

            testProperty "singleton should return the same result as the standard function" <| fun (x: int) ->
                singleton x |> toStandardList = List.singleton x
        ]

        ptestList "length" [
            testCase "length cases" <| fun () ->
                Expect.equal (length Nil) 0 "empty list"
                Expect.equal (length (42 --> Nil)) 1 "singleton list"
                Expect.equal (length ((-42 --> (42 --> Nil)) --> Nil)) 1 "nested list"
                Expect.equal (length ("Hello" --> ("World" --> Nil))) 2 "two items"

            testProperty "length should return the same result as the standard function" <| fun (xs: int list) ->
                length (fromStandardList xs) = List.length xs
        ]

        ptestList "tryHead" [
            testCase "tryHead cases" <| fun () ->
                Expect.equal (tryHead Nil) None "empty list"
                Expect.equal (tryHead (42 --> Nil)) (Some 42) "singleton list"
                Expect.equal (tryHead (1 --> (2 --> (3 --> Nil)))) (Some 1) "longer list"
                Expect.equal (tryHead ((1 --> (2 --> Nil)) --> ((3 --> Nil) --> Nil))) (Some (1 --> (2 --> Nil))) "nested list"

            testProperty "tryHead should return the same result as the standard function" <| fun (xs: int list) ->
                tryHead (fromStandardList xs) = List.tryHead xs
        ]

        ptestList "tryLast" [
            testCase "tryLast cases" <| fun () ->
                Expect.equal (tryLast Nil) None "empty list"
                Expect.equal (tryLast (42 --> Nil)) (Some 42) "singleton list"
                Expect.equal (tryLast (1 --> (2 --> (3 --> Nil)))) (Some 3) "longer list"
                Expect.equal (tryLast ((1 --> Nil) --> ((2 --> (3 --> Nil)) --> Nil))) (Some (2 --> (3 --> Nil))) "nested list"

            testProperty "tryLast should return the same result as the standard function" <| fun (xs: int list) ->
                tryLast (fromStandardList xs) = List.tryLast xs
        ]

        ptestList "contains" [
            testCase "contains cases" <| fun () ->
                Expect.equal (contains 0 Nil) false "empty list"
                Expect.equal (contains 2 (1 --> (2 --> (3 --> Nil)))) true "contains"
                Expect.equal (contains 0 (1 --> (2 --> (3 --> Nil)))) false "not contain"
                
            testProperty "contains should return the same result as the standard function" <| fun value (xs: int list) ->
                contains value (fromStandardList xs) = List.contains value xs
        ]

        ptestList "exists" [
            testCase "exists cases" <| fun () ->
                Expect.equal (exists ((=) 0) Nil) false "empty list"
                Expect.equal (exists (fun n -> n > 2) (1 --> (2 --> (3 --> Nil)))) true "exists"
                Expect.equal (exists (fun n -> n < 0) (1 --> (2 --> (3 --> Nil)))) false "not exist"

            testProperty "exists should return the same result as the standard function" <| fun pred (xs: int list) ->
                exists pred (fromStandardList xs) = List.exists pred xs
        ]

        ptestList "forall" [
            testCase "forall cases" <| fun () ->
                Expect.equal (forall ((=) 0) Nil) true "empty list"
                Expect.equal (forall (fun n -> n > 0) (1 --> (2 --> (3 --> Nil)))) true "forall holds"
                Expect.equal (forall (fun n -> n > 2) (1 --> (2 --> (3 --> Nil)))) false "not all"

            testProperty "forall should return the same result as the standard function" <| fun pred (xs: int list) ->
                forall pred (fromStandardList xs) = List.forall pred xs
        ]

        ptestList "tryPick" [
            testCase "tryPick cases" <| fun () ->
                let doubleIfGreaterThanTwo n = if n > 2 then Some (2 * n) else None
                Expect.equal (tryPick doubleIfGreaterThanTwo Nil) None "empty list"
                Expect.equal (tryPick doubleIfGreaterThanTwo (1 --> (3 --> (5 --> Nil)))) (Some 6) "exists"
                Expect.equal (tryPick doubleIfGreaterThanTwo (0 --> (1 --> (2 --> Nil)))) None "not exist"

            testProperty "tryPick should return the same result as the standard function" <| fun (chooser: int -> string option) (xs: int list) ->
                tryPick chooser (fromStandardList xs) = List.tryPick chooser xs
        ]

        ptestList "replicate" [
            testCase "replicate cases" <| fun () ->
                Expect.equal (replicate 0 "xyz") Nil "0"
                Expect.equal (replicate 1 "xyz") ("xyz" --> Nil) "1"
                Expect.equal (replicate 3 "xyz") ("xyz" --> ("xyz" --> ("xyz" --> Nil))) "3"

            testProperty "replicate should return the same result as the standard function" <| fun count (initial: string) ->
                (count >= 0) ==> lazy (
                    replicate count initial |> toStandardList = List.replicate count initial)
        ]

        ptestList "tryItem" [
            testCase "tryItem cases" <| fun () ->
                Expect.equal (tryItem 0 Nil) None "empty list"
                Expect.equal (tryItem 0 (1 --> (2 --> (3 --> Nil)))) (Some 1) "head"
                Expect.equal (tryItem 1 (1 --> (2 --> (3 --> Nil)))) (Some 2) "mid"
                Expect.equal (tryItem 2 (1 --> (2 --> (3 --> Nil)))) (Some 3) "last"
                Expect.equal (tryItem 3 (1 --> (2 --> (3 --> Nil)))) None "out of range"
                Expect.equal (tryItem -1 (1 --> (2 --> (3 --> Nil)))) None "negative index"
                
            testProperty "tryItem should return the same result as the standard function" <| fun index (xs: int list) ->
                tryItem index (fromStandardList xs) = List.tryItem index xs
        ]

        ptestList "tryFind" [
            testCase "tryFind cases" <| fun () ->
                Expect.equal (tryFind (fun _ -> true) Nil) None "empty list"
                Expect.equal (tryFind (fun n -> n > 2) (1 --> (2 --> (3 --> Nil)))) (Some 3) "exists one"
                Expect.equal (tryFind (fun n -> n > 1) (1 --> (2 --> (3 --> Nil)))) (Some 2) "exists many"
                Expect.equal (tryFind (fun n -> n > 4) (1 --> (2 --> (3 --> Nil)))) None "not exist"

            testProperty "tryFind should return the same result as the standard function" <| fun pred (xs: int list) ->
                tryFind pred (fromStandardList xs) = List.tryFind pred xs
        ]

        ptestList "tryFindBack" [
            testCase "tryFindBack cases" <| fun () ->
                Expect.equal (tryFindBack (fun _ -> true) Nil) None "empty list"
                Expect.equal (tryFindBack (fun n -> n > 2) (1 --> (2 --> (3 --> Nil)))) (Some 3) "exists one"
                Expect.equal (tryFindBack (fun n -> n > 1) (1 --> (2 --> (3 --> Nil)))) (Some 3) "exists many"
                Expect.equal (tryFindBack (fun n -> n > 4) (1 --> (2 --> (3 --> Nil)))) None "not exist"

            testProperty "tryFindBack should return the same result as the standard function" <| fun pred (xs: int list) ->
                tryFindBack pred (fromStandardList xs) = List.tryFindBack pred xs
        ]

        ptestList "tryFindIndex" [
            testCase "tryFindIndex cases" <| fun () ->
                Expect.equal (tryFindIndex (fun _ -> true) Nil) None "empty list"
                Expect.equal (tryFindIndex (fun n -> n > 2) (1 --> (2 --> (3 --> Nil)))) (Some 2) "exists one"
                Expect.equal (tryFindIndex (fun n -> n > 1) (1 --> (2 --> (3 --> Nil)))) (Some 1) "exists many"
                Expect.equal (tryFindIndex (fun n -> n > 4) (1 --> (2 --> (3 --> Nil)))) None "not exist"

            testProperty "tryFindIndex should return the same result as the standard function" <| fun pred (xs: int list) ->
                tryFindIndex pred (fromStandardList xs) = List.tryFindIndex pred xs
        ]

        ptestList "rev" [
            testCase "rev cases" <| fun () ->
                Expect.equal (rev Nil) Nil "empty list"
                Expect.equal (rev (42 --> Nil)) (42 --> Nil) "singleton list"
                Expect.equal (rev (1 --> (2 --> (3 --> Nil)))) (3 --> (2 --> (1 --> Nil))) "longer list"

            testProperty "rev should return the same result as the standard function" <| fun (xs: int list) ->
                rev (fromStandardList xs) |> toStandardList = List.rev xs
        ]

        ptestList "filter" [
            testCase "filter cases" <| fun () ->
                Expect.equal (filter (fun _ -> true) Nil) Nil "empty list"
                Expect.equal (filter ((=) 2) (1 --> (2 --> (3 --> Nil)))) (2 --> Nil) "exists"
                Expect.equal (filter ((=) 42) (42 --> (-42 --> (42 --> Nil)))) (42 --> (42 --> Nil)) "exists many"
                Expect.equal (filter (fun n -> n < 0) (1 --> (2 --> (3 --> Nil)))) Nil "not exist"

            testProperty "filter should return the same result as the standard function" <| fun pred (xs: int list) ->
                filter pred (fromStandardList xs) |> toStandardList = List.filter pred xs
        ]

        ptestList "choose" [
            testCase "choose cases" <| fun () ->
                Expect.equal (choose (fun _ -> Some 42) Nil) Nil "empty list"
                Expect.equal (choose Some (1 --> (2 --> (3 --> Nil)))) (1 --> (2 --> (3 --> Nil))) "exists"
                Expect.equal (choose (fun n -> if n % 2 = 0 then Some (n / 2) else None) (2 --> (3 --> (4 --> Nil)))) (1 --> (2 --> Nil)) "exists many"
                Expect.equal (choose (fun _ -> None) (1 --> (2 --> (3 --> Nil)))) Nil "not exist"

            testProperty "choose should return the same result as the standard function" <| fun (chooser: int -> string option) (xs: int list) ->
                choose chooser (fromStandardList xs) |> toStandardList = List.choose chooser xs
        ]

        ptestList "partition" [
            testCase "partition cases" <| fun () ->
                Expect.equal (partition (fun _ -> true) Nil) (Nil, Nil) "empty list"
                Expect.equal (partition ((=) 2) (1 --> (2 --> (3 --> Nil)))) (2 --> Nil, 1 --> (3 --> Nil)) "exists"
                Expect.equal (partition ((=) 42) (42 --> (-42 --> (42 --> Nil)))) (42 --> (42 --> Nil), -42 --> Nil) "exists many"
                Expect.equal (partition (fun n -> n < 0) (1 --> (2 --> (3 --> Nil)))) (Nil, 1 --> (2 --> (3 --> Nil))) "not exist"

            testProperty "partition should return the same result as the standard function" <| fun pred (xs: int list) ->
                let t, f = partition pred (fromStandardList xs)
                (toStandardList t, toStandardList f) = List.partition pred xs
        ]

        ptestList "truncate" [
            testCase "truncate cases" <| fun () ->
                Expect.equal (truncate 1 Nil) Nil "empty list"
                Expect.equal (truncate 1 (10 --> (20 --> (30 --> Nil)))) (10 --> Nil) "one"
                Expect.equal (truncate 2 (10 --> (20 --> (30 --> Nil)))) (10 --> (20 --> Nil)) "two"
                Expect.equal (truncate 3 (10 --> (20 --> (30 --> Nil)))) (10 --> (20 --> (30 --> Nil))) "three"
                Expect.equal (truncate 5 (10 --> (20 --> (30 --> Nil)))) (10 --> (20 --> (30 --> Nil))) "out of range"
                Expect.equal (truncate 0 (10 --> (20 --> (30 --> Nil)))) Nil "zero"
                Expect.equal (truncate -1 (10 --> (20 --> (30 --> Nil)))) Nil "negative"

            testProperty "truncate should return the same result as the standard function" <| fun count (xs: int list) ->
                toStandardList (truncate count (fromStandardList xs)) = List.truncate count xs
        ]

        ptestList "takeWhile" [
            testCase "takeWhile cases" <| fun () ->
                Expect.equal (takeWhile (fun _ -> true) Nil) Nil "empty list"
                Expect.equal (takeWhile (fun n -> n < 3) (1 --> (2 --> (3 --> (1 --> Nil))))) (1 --> (2 --> Nil)) "exists"
                Expect.equal (takeWhile (fun n -> n > 0) (1 --> (2 --> (3 --> (1 --> Nil))))) (1 --> (2 --> (3 --> (1 --> Nil)))) "all hold"
                Expect.equal (takeWhile (fun n -> n > 1) (1 --> (2 --> (3 --> (1 --> Nil))))) Nil "not satisfy at first"

            testProperty "takeWhile should return the same result as the standard function" <| fun pred (xs: int list) ->
                toStandardList (takeWhile pred (fromStandardList xs)) = List.takeWhile pred xs
        ]

        ptestList "skipWhile" [
            testCase "skipWhile cases" <| fun () ->
                Expect.equal (skipWhile (fun _ -> false) Nil) Nil "empty list"
                Expect.equal (skipWhile (fun n -> n < 3) (1 --> (2 --> (3 --> (1 --> Nil))))) (3 --> (1 --> Nil)) "exists"
                Expect.equal (skipWhile (fun n -> n > 0) (1 --> (2 --> (3 --> (1 --> Nil))))) Nil "all hold"
                Expect.equal (skipWhile (fun n -> n > 1) (1 --> (2 --> (3 --> (1 --> Nil))))) (1 --> (2 --> (3 --> (1 --> Nil)))) "not satisfy at first"

            testProperty "skipWhile should return the same result as the standard function" <| fun pred (xs: int list) ->
                toStandardList (skipWhile pred (fromStandardList xs)) = List.skipWhile pred xs
        ]

        ptestList "fold" [
            testCase "fold cases" <| fun () ->
                Expect.equal (fold (fun state _ -> state + 1) 42 Nil) 42 "empty list"
                Expect.equal (fold (sprintf "%s,%d") "start" (1 --> (2 --> (3 --> Nil)))) "start,1,2,3" "string state"
                Expect.equal (fold (-) 100 (1 --> (2 --> (3 --> Nil)))) 94 "((100 - 1) - 2) - 3"

            testProperty "fold should return the same result as the standard function" <| fun folder (state: string) (xs: int list) ->
                fold folder state (fromStandardList xs) = List.fold folder state xs
        ]

        ptestList "foldBack" [
            testCase "foldBack cases" <| fun () ->
                Expect.equal (foldBack (fun state _ -> state + 1) Nil 42) 42 "empty list"
                Expect.equal (foldBack (sprintf "%d,%s") (1 --> (2 --> (3 --> Nil))) "start") "1,2,3,start" "string state"
                Expect.equal (foldBack (-) (1 --> (2 --> (3 --> Nil))) 100) -98 "1 - (2 - (3 - 100))"

            testProperty "foldBack should return the same result as the standard function" <| fun folder (xs: int list) (state: string) ->
                foldBack folder (fromStandardList xs) state = List.foldBack folder xs state
        ]

        ptestList "append" [
            testCase "append cases" <| fun () ->
                Expect.equal (append Nil Nil) Nil "empty list"
                Expect.equal (append (42 --> Nil) Nil) (42 --> Nil) "1 + 0"
                Expect.equal (append Nil (42 --> Nil)) (42 --> Nil) "0 + 1"
                Expect.equal (append (1 --> (2 --> (3 --> Nil))) (10 --> (20 --> Nil))) (1 --> (2 --> (3 --> (10 --> (20 --> Nil))))) "many"

            testProperty "append should return the same result as the standard function" <| fun (xs: int list) (ys: int list) ->
                append (fromStandardList xs) (fromStandardList ys) |> toStandardList = xs @ ys
        ]

        ptestList "concat" [
            testCase "concat cases" <| fun () ->
                Expect.equal (concat Nil) Nil "[]"
                Expect.equal (concat (Nil --> Nil)) Nil "[[]]"
                Expect.equal (concat (Nil --> (Nil --> (Nil --> Nil)))) Nil "[[]; []; []]"
                Expect.equal (concat ((1 --> Nil) --> Nil)) (1 --> Nil) "[[1]]"
                Expect.equal (concat ((1 --> Nil) --> ((2 --> (3 --> Nil)) --> (Nil --> ((4 --> Nil) --> Nil))))) (1 --> (2 --> (3 --> (4 --> Nil)))) "[[1]; [2; 3]; []; [4]]"

            testProperty "concat should return the same result as the standard function" <| fun (xss: int list list) ->
                let userList = xss |> List.map fromStandardList |> fromStandardList
                concat userList |> toStandardList = List.concat xss
        ]

        ptestList "map" [
            testCase "map cases" <| fun () ->
                Expect.equal (map (fun _ -> 42) Nil) Nil "empty list"
                Expect.equal (map ((*) 2) (1 --> (2 --> (3 --> Nil)))) (2 --> (4 --> (6 --> Nil))) "double"
                Expect.equal (map string (1 --> (2 --> (3 --> Nil)))) ("1" --> ("2" --> ("3" --> Nil))) "int to string"

            testProperty "map should return the same result as the standard function" <| fun (mapping: int -> string) (xs: int list) ->
                map mapping (fromStandardList xs) |> toStandardList = List.map mapping xs
        ]

        ptestList "collect" [
            testCase "collect cases" <| fun () ->
                Expect.equal (collect id Nil) Nil "[]"
                Expect.equal (collect id (Nil --> Nil)) Nil "[[]]"
                Expect.equal (collect id (Nil --> (Nil --> (Nil --> Nil)))) Nil "[[]; []; []]"
                Expect.equal (collect id ((1 --> Nil) --> Nil)) (1 --> Nil) "[[1]]"
                Expect.equal (collect (fun n -> (n --> ((2 * n) --> Nil))) (1 --> (3 --> (5 --> Nil)))) (1 --> (2 --> (3 --> (6 --> (5 --> (10 --> Nil)))))) "[[1; 2]; [3; 6]; [5; 10]]"

            testProperty "collect should return the same result as the standard function" <| fun (mapping: int -> string list) (xs: int list) ->
                collect (mapping >> fromStandardList) (fromStandardList xs) |> toStandardList = List.collect mapping xs
        ]

        ptestList "zip" [
            testCase "zip cases" <| fun () ->
                Expect.equal (zip Nil Nil) Nil "empty list"
                Expect.equal (zip (1 --> Nil) (2 --> Nil)) ((1, 2) --> Nil) "singleton list"
                Expect.equal (zip (1 --> (2 --> (3 --> Nil))) ("abc" --> ("def" --> ("ghi" --> Nil)))) ((1, "abc") --> ((2, "def") --> ((3, "ghi") --> Nil))) "longer list"

            testProperty "zip should return the same result as the standard function" <| fun (list: (int * string) list) ->
                let xs, ys = List.unzip list
                zip (fromStandardList xs) (fromStandardList ys) |> toStandardList = List.zip xs ys
        ]

        ptestList "unzip" [
            testCase "unzip cases" <| fun () ->
                Expect.equal (unzip Nil) (Nil, Nil) "empty list"
                Expect.equal (unzip ((1, 2) --> Nil)) ((1 --> Nil), (2 --> Nil)) "singleton list"
                Expect.equal (unzip ((1, "abc") --> ((2, "def") --> ((3, "ghi") --> Nil)))) ((1 --> (2 --> (3 --> Nil))), ("abc" --> ("def" --> ("ghi" --> Nil)))) "longer list"

            testProperty "unzip should return the same result as the standard function" <| fun (list: (int * string) list) ->
                let xs, ys = unzip (fromStandardList list)
                (toStandardList xs, toStandardList ys) = List.unzip list
        ]

        // Re-implement functions to be tail-recursive for the stack-safety.
        testList "stack safety" [
            ptestCase "length stack safe" <| fun () ->
                Expect.equal (length (fromStandardList [1..1000000])) 1000000 "length should not throw StackOverflowException"

            ptestCase "replicate stack safe" <| fun () ->
                Expect.equal (replicate 1000000 false |> toStandardList) (List.replicate 1000000 false) "replicate should not throw StackOverflowException"

            ptestCase "tryFindBack stack safe" <| fun () ->
                Expect.equal (tryFindBack ((=) 1) (fromStandardList [1..1000000]) ) (Some 1) "tryFindBack should not throw StackOverflowException"

            ptestCase "tryFindIndex stack safe" <| fun () ->
                Expect.equal (tryFindIndex ((=) 1) (fromStandardList [1..1000000]) ) (Some 0) "tryFindIndex should not throw StackOverflowException"

            ptestCase "rev stack safe" <| fun () ->
                Expect.equal (rev (fromStandardList [1..1000000]) |> toStandardList) [1000000..-1..1] "rev should not throw StackOverflowException"

            ptestCase "filter stack safe" <| fun () ->
                Expect.equal (filter (fun n -> n % 2 = 1) (fromStandardList [1..1000000]) |> toStandardList) [1..2..1000000] "rev should not throw StackOverflowException"

            ptestCase "choose stack safe" <| fun () ->
                Expect.equal (choose (fun n -> if n % 2 = 0 then Some (n / 2) else None) (fromStandardList [1..1000000]) |> toStandardList) [1..500000] "choose should not throw StackOverflowException"

            ptestCase "partition stack safe" <| fun () ->
                let xs, ys = partition (fun n -> n % 2 = 1) (fromStandardList [1..1000000])
                Expect.equal (toStandardList xs, toStandardList ys) ([1..2..1000000], [2..2..1000000]) "partition should not throw StackOverflowException"

            ptestCase "truncate stack safe" <| fun () ->
                Expect.equal (truncate 999999 (fromStandardList [1..1000000]) |> toStandardList) [1..999999] "truncate should not throw StackOverflowException"

            ptestCase "takeWhile stack safe" <| fun () ->
                Expect.equal (takeWhile ((>) 1000000) (fromStandardList [1..1000000]) |> toStandardList) [1..999999] "takeWhile should not throw StackOverflowException"

            ptestCase "skipWhile stack safe" <| fun () ->
                Expect.equal (skipWhile ((>) 1000000) (fromStandardList [1..1000000]) |> toStandardList) [1000000] "skipWhile should not throw StackOverflowException"

            ptestCase "fold stack safe" <| fun () ->
                Expect.equal (fold (+) 0L (fromStandardList [1L..1000000L])) 500000500000L "fold should not throw StackOverflowException"

            ptestCase "foldBack stack safe" <| fun () ->
                Expect.equal (foldBack (+) (fromStandardList [1L..1000000L]) 0L) 500000500000L "foldBack should not throw StackOverflowException"

            ptestCase "append stack safe" <| fun () ->
                Expect.equal (append (fromStandardList []) (fromStandardList [1..1000000]) |> toStandardList) [1..1000000] "append [] [..] should not throw StackOverflowException"
                Expect.equal (append (fromStandardList [1..1000000]) (fromStandardList []) |> toStandardList) [1..1000000] "append [..] [] should not throw StackOverflowException"

            ptestCase "concat stack safe" <| fun () ->
                Expect.equal (concat (fromStandardList [for i in 1..2..2000000 -> fromStandardList [i; i + 1]]) |> toStandardList) [1..2000000] "concat [..] should not throw StackOverflowException"
                Expect.equal (concat (fromStandardList [fromStandardList [1..1000000]; fromStandardList [1000001..2000000]]) |> toStandardList) [1..2000000] "concat [[..]; [..]] should not throw StackOverflowException"

            ptestCase "map stack safe" <| fun () ->
                Expect.equal (map ((*) 2) (fromStandardList [1..1000000]) |> toStandardList) [2..2..2000000] "map should not throw StackOverflowException"

            ptestCase "collect stack safe" <| fun () ->
                Expect.equal (collect (fun n -> [n; n + 1] |> fromStandardList) (fromStandardList [1..2..2000000]) |> toStandardList) [1..2000000] "collect should not throw StackOverflowException"

            ptestCase "zip stack safe" <| fun () ->
                let xs, ys = List.partition (fun n -> n % 2 = 1) [1..2000000]
                Expect.equal (zip (fromStandardList xs) (fromStandardList ys) |> toStandardList) (List.zip xs ys) "zip should not throw StackOverflowException"

            ptestCase "unzip stack safe" <| fun () ->
                let xs, ys = List.partition (fun n -> n % 2 = 1) [1..2000000]
                let list = List.zip xs ys
                let us, vs = unzip (fromStandardList list)
                Expect.equal (toStandardList us, toStandardList vs) (List.unzip list) "unzip should not throw StackOverflowException"
        ]
    ]

