module ListDojo

open Helper

// The standard F# list
// https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html
let standardEmptyList: int list = []
let standardSingletonList: int list = [42]

// Elements are separated by semicolons.
// Note that they are not separated by commas.
let standardOneToThree = [1; 2; 3]

// Add an element to the list using the `(::)` operator.
let standardOneToThree' = 1 :: (2 :: (3 :: [])) // fsharplint:disable-line Hints

// The `(::)` operator is right associative, so the parentheses can be omitted.
let standardOneToThree'' = 1 :: 2 :: 3 :: [] // fsharplint:disable-line Hints

// head        tail
// 1     ::    (2 :: 3 :: [])
//
// head: 'a
// tail: 'a list


// User-defined List for learning purpose!
// Note that the F# standard type has the first character in lowercase.
// `Nil` should be equivalent to `[]` in the standard F# list.
// `Cons` should be almost the same as `(::)` in the standard F# list.
// `(::)` is a binary operator, while `Cons` is a one-argument function that takes a tuple.
type List<'a> =
    | Nil
    | Cons of 'a * List<'a>

// `(-->)` is a user-defined operator that is almost the same to `(::)` in the standard F# list.
let (-->) head tail = Cons (head, tail)

// Create an empty list
// It should be equivalent to `[]` in the standard F# list.
let empty = Nil

// Create user-defined List that is equivalent to the standard F# list `[1; 2; 3]`.
// Be careful with the order.
let oneToThree = Cons (1, Cons (2, Cons (3, Nil)))

// Alternative definition using `(-->)` operator.
// `[1; 2; 3]` is just a sugar syntax for `1 :: (2 :: (3 :: []))`.
// Since `(::)` is a right associative operator, it can be written as `1 :: 2 :: 3 :: []` without parentheses.
// However, `(-->)` is a left associative operator, so you cannot omit the parentheses.
let oneToThree' = 1 --> (2 --> (3 --> Nil))



// Now let's move on to the exercises!
// Don't forget to use the `rec` keyword if you want to call the function itself again in the function definition.

// Return `true` if the list is empty, `false` otherwise.
let isEmpty (xs: List<'a>) = Hole?isEmpty

// Create a singleton list from a single element.
// In other words, it takes one argument and returns a list containing that value as the only element.
let singleton value = Hole?singleton

// Return the length of the list.
// In other words, it returns the number of elements in the list.
// Of course, `Nil` is excluded from the count.
let length xs = Hole?length

// Returns the first element of the list.
// When the list is empty, the head does not exist, so it should return the option of element.
let tryHead xs = Hole?tryHead

// Returns the last element of the list.
// It should return the option.
let tryLast xs = Hole?tryLast

let contains value xs = Hole?contains

let exists pred xs = Hole?exists

let forall pred xs = Hole?forall

let tryPick chooser xs = Hole?tryPick

let replicate count initial = Hole?replicate

let tryItem index xs = Hole?tryItem

let tryFind pred xs = Hole?tryFind

let tryFindBack pred xs = Hole?tryFindBack

let tryFindIndex pred xs = Hole?tryFindIndex

let rev xs = Hole?rev

let filter pred xs = Hole?filter
    
let choose chooser xs = Hole?choose

let partition pred xs = Hole?partition

let truncate count xs = Hole?truncate

let takeWhile pred xs = Hole?takeWhile
        
let skipWhile pred xs = Hole?skipWhile

let fold folder state xs = Hole?fold

let foldBack folder xs state = Hole?foldBack

let append xs ys = Hole?append

let concat xss = Hole?concat

let map mapping xs = Hole?map

let collect mapping xs = Hole?collect

let zip list1 list2 = Hole?zip

let unzip list = Hole?unzip

