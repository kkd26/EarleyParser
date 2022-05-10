type production = Head.t * BodyElement.t list list

let s : production = (S, [ [ NT NP; NT VP ] ])
let pp : production = (PP, [ [ NT P; NT NP ] ])
let np : production = (NP, [ [ NT N; NT PP ]; [ NT N ] ])

let vp : production =
  (VP, [ [ NT VP; NT PP ]; [ NT V; NT VP ]; [ NT V; NT NP ]; [ NT V ] ])

let nouns : NonTerminal.t * string list =
  (N, [ "can"; "fish"; "rivers"; "they" ])

let propositions : NonTerminal.t * string list = (P, [ "in" ])
let verbs : NonTerminal.t * string list = (V, [ "can"; "fish" ])

let makeDict (head, wordList) : Dictionary.store =
  List.fold_right
    (fun word env -> Dictionary.update env word head)
    wordList Dictionary.empty

let makeProd ((head, bodyList) : production) : Rule.store =
  List.map (fun body -> (head, Body.init body)) bodyList

let init : Rule.store = makeProd s @ makeProd pp @ makeProd np @ makeProd vp
let dict = makeDict nouns @ makeDict propositions @ makeDict verbs
