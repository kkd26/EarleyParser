type t = int * int

let init : t = (0, 0)
let getRight (r : t) = snd r
let getLeft (r : t) = fst r

let toString ((a, b) : t) : string =
  "[" ^ string_of_int a ^ "," ^ string_of_int b ^ "]"
