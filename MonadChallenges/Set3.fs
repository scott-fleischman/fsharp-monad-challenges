module Set3

//allPairs :: [a] -> [b] -> [(a,b)]
let rec allPairs (xs : 'a List) (ys : 'b List) : (('a * 'b) List) =
    let rec applyAll p qs =
        match qs with
        | [] -> []
        | q :: qs' -> (p, q) :: applyAll p qs'

    match xs with
    | [] -> []
    | x :: xs' -> applyAll x ys @ allPairs xs' ys

type Card =
    { number : int
    ; suit : string
    }

//show (Card 2 "h") == "2h"
let show ({ number = number; suit = suit }) = sprintf "%i%s" number suit

//allCards :: [Int] -> [String] -> [Card]
let allCards ns ss = allPairs ns ss |> List.map (fun (n, s) -> { number = n; suit = s})

let showList cards = List.map show cards

//allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
let rec allCombs (f : 'a -> 'b -> 'c) (xs : 'a List) (ys : 'b List) : ('c List) =
    let rec applyAll p qs =
        match qs with
        | [] -> []
        | q :: qs' -> f p q :: applyAll p qs'

    match xs with
    | [] -> []
    | x :: xs' -> applyAll x ys @ allCombs f xs' ys
let allPairs' xs ys = allCombs (fun x y -> x, y) xs ys
let allCards' xs ys = allCombs (fun n s -> { number = n; suit = s }) xs ys

//allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
let allCombs3 (f : 'a -> 'b -> 'c -> 'd) (ays : 'a List) (bs : 'b List) (cs : 'c List) : ('d List) =
    let start = allCombs f ays bs
    allCombs id start cs

let allCombs4 (f : 'a -> 'b -> 'c -> 'd -> 'e) (ays : 'a List) (bs : 'b List) (cs : 'c List) (ds : 'd List) : ('e List) =
    let start = allCombs f ays bs
    let next = allCombs id start cs
    allCombs id next ds

//combStep
let rec combStep (fs : ('a -> 'b) List) (ays : 'a List) : ('b List) =
    let rec applyAll p qs =
        match qs with
        | [] -> []
        | q :: qs' -> p q :: applyAll p qs'

    match fs with
    | [] -> []
    | f :: fs' -> applyAll f ays @ combStep fs' ays

let allCombs' xs ys =
    let pair a b = a, b
    let start = [pair]
    let next = combStep start xs
    combStep next ys

let pure' x = [x]
let (<*>) fs ays = combStep fs ays

let allCombs'' xs ys = pure' (fun a b -> a, b) <*> xs <*> ys
let allCombs3' f xs ys zs = pure' f <*> xs <*> ys <*> zs
let allCombs4' f xs ys zs ws = pure' f <*> xs <*> ys <*> zs <*> ws
