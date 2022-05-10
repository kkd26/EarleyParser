exception IncorrectElement of string

type t = Dot | T of Terminal.t | NT of NonTerminal.t

let toString = function
  | Dot -> "Dot"
  | T t -> "T(" ^ t ^ ")"
  | NT nt -> "NT(" ^ NonTerminal.toString nt ^ ")"

let getNonTerminal (element : t) =
  match element with
  | NT nt -> nt
  | _ -> raise (IncorrectElement "not a nonterminal")

let map f (element : t list) = List.map f element
