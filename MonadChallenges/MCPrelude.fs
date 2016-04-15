module MCPrelude

type Seed = Seed of bigint

let mkSeed n = Seed n

let rand (Seed s) =
    let m = bigint 0x7FFFFFFF
    let s' = (s * 16807I) % m
    (s', Seed s')

let toLetter n =
    let n' = n % 26I
    'a' + char (int n')

type GreekData = (string * bigint list) list

let greekDataA =
    [ ("alpha", [5I; 10I])
    ; ("beta", [0I; 8I])
    ; ("gamma", [18I; 47I; 60I])
    ; ("delta", [42I])
    ]

let greekDataB =
    [ ("phi", [53I; 13I])
    ; ("chi", [21I; 8I; 191I])
    ; ("psi", [])
    ; ("omega", [6I; 82I; 144I])
    ]

let salaries =
    [ ("alice", 105000I)
    ; ("bob", 90000I)
    ; ("carol", 85000I)
    ]

let firstNames = ["alice"; "bob"; "carol"; "dave"]

let lastNames = ["doe"; "jones"; "smith"]

let cardRanks = [2; 3; 4; 5]

let cardSuits = ["H"; "D"; "C"; "S"]
