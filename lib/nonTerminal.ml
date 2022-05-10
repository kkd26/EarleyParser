type t = S | NP | VP | PP | N | V | P

let toString = function
  | S -> "S"
  | NP -> "NP"
  | VP -> "VP"
  | PP -> "PP"
  | N -> "N"
  | V -> "V"
  | P -> "P"
