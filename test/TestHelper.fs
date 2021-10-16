module TestHelper

let isSubstring (subString: string) (source: string) = source.Contains subString


let toStandardList (xs: 'a ListDojo.List): 'a list =
    let rec loop acc = function
        | ListDojo.Nil -> acc
        | ListDojo.Cons (head, tail) -> loop (head :: acc) tail
    xs |> loop [] |> List.rev

let fromStandardList (xs: 'a list): 'a ListDojo.List =
    let rec loop acc = function
        | [] -> acc
        | (x :: xs) -> loop (ListDojo.Cons (x, acc)) xs
    xs |> List.rev |> loop ListDojo.Nil

