module Set2

open MCPrelude

type 'a Maybe =
    | Some of 'a
    | None

let show (m : 'a Maybe) : string =
    match m with
    | Some a -> sprintf "Some %A" a
    | None -> "None"

//headMay :: [a] -> Maybe a
let headMay (list : 'a List) : 'a Maybe =
    match list with
    | [] -> None
    | head :: _ -> Some head

//tailMay :: [a] -> Maybe [a]
let tailMay (list : 'a List) : 'a List Maybe =
    match list with
    | [] -> None
    | _ :: tail -> Some tail

//lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
let rec lookupMay (key : 'a) (list : ('a * 'b) List) : 'b Maybe =
    match list with
    | [] -> None
    | (key', value) :: _ when key' = key -> Some value
    | _ :: tail -> lookupMay key tail

//divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
let divMay numerator denominator = if denominator = 0I then None else Some (double numerator / double denominator)
let (/?) = divMay

//maximumMay :: Ord a => [a] -> Maybe a
let rec findMay f (list : 'a List) : 'a Maybe =
    match list with
    | [] -> None
    | head :: tail ->
        let result =
            match findMay f tail with
            | None -> head
            | Some maxTail -> f head maxTail
        Some result
let maximumMay x = findMay max x
let minimumMay x = findMay min x

//minimumMay :: Ord a => [a] -> Maybe a
let findMay' f (list : 'a List) : 'a Maybe =
    let rec aux m xs =
        match xs with
        | [] -> m
        | x' :: xs' -> aux (f m x') xs'

    match list with
    | [] -> None
    | head :: tail -> Some (aux head tail)
let minimumMay' x = findMay' min x

//queryGreek :: GreekData -> String -> Maybe Double
let queryGreek (data : GreekData) (key : string) : double Maybe =
    match lookupMay key data with
    | None -> None
    | Some xs ->
        match headMay xs, tailMay xs with
        | Some head, Some tail ->
            match maximumMay tail with
            | None -> None
            | Some max -> divMay max head
        | _, _ -> None

//link :: Maybe a -> (a -> Maybe b) -> Maybe b
let link (first : 'a Maybe) (f : 'a -> 'b Maybe) : ('b Maybe) =
    match first with
    | None -> None
    | Some a -> f a
let chain x y = link y x

let joinMaybe (f : 'a -> 'b -> 'c Maybe) (m1 : 'a Maybe) (m2 : 'b Maybe) : 'c Maybe =
    match m1, m2 with
    | Some x, Some y -> f x y
    | _, _ -> None

//link :: Maybe a -> (a -> Maybe b) -> Maybe b
let queryGreek2 (data : GreekData) (key : string) : double Maybe =
    let xsMaybe = lookupMay key data
    let tailMaybe = link xsMaybe tailMay
    let maxMaybe = link tailMaybe maximumMay
    let headMaybe = link xsMaybe headMay
    joinMaybe divMay maxMaybe headMaybe

let makeSome f x = Some (f x)
//mkMaybe :: a -> Maybe a
let mkMaybe (x : 'a) : 'a Maybe = Some x

//addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
let addSalaries (salaries : (string * bigint) list) (n1 : string) (n2 : string) : bigint Maybe =
    let s1 = lookupMay n1 salaries
    let s2 = lookupMay n2 salaries
    let f g x y = mkMaybe (g x y)
    joinMaybe (f (+)) s1 s2

// yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
let join (f : 'a -> 'b -> 'c) (m1 : 'a Maybe) (m2 : 'b Maybe) : 'c Maybe =
    match m1, m2 with
    | Some x, Some y -> mkMaybe (f x y)
    | _, _ -> None

let addSalaries2 (salaries : (string * bigint) list) (n1 : string) (n2 : string) : bigint Maybe =
    let s1 = lookupMay n1 salaries
    let s2 = lookupMay n2 salaries
    join (+) s1 s2

//tailProd :: Num a => [a] -> Maybe a
let tailOp f d (xs : bigint list) : bigint Maybe =
    let tailMaybe = tailMay xs
    let folder = List.fold (fun a x -> f a x) d
    match tailMaybe with
    | Some tail -> Some (folder tail)
    | None -> None

//link :: Maybe a -> (a -> Maybe b) -> Maybe b
//transMaybe :: (a -> b) -> Maybe a -> Maybe b
let transMaybe (x : 'a Maybe) (f : 'a -> 'b) : 'b Maybe =
    match x with
    | Some x' -> Some (f x')
    | None -> None

let tailOp' f d xs =
    let tailMaybe = tailMay xs
    let folder = List.fold (fun a x -> f a x) d
    transMaybe tailMaybe folder

let tailProd = tailOp' (*) 1I
let tailSum = tailOp' (+) 0I

let combine (x : 'a Maybe Maybe) : 'a Maybe =
    match x with
    | Some x' -> x'
    | None -> None

let tailListOp op xs =
    let tailMaybe = tailMay xs
    combine (transMaybe tailMaybe op)
let tailMax x = tailListOp maximumMay x
let tailMin x = tailListOp minimumMay' x
