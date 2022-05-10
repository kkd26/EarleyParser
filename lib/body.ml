type t = BodyElement.t list * Range.t

exception ElementDoesntExist

let getRange (body : t) : Range.t = snd body
let getElements (body : t) : BodyElement.t list = fst body

let makeBody (elements : BodyElement.t list) (range : Range.t) : t =
  (elements, range)

let addElement (body : t) (element : BodyElement.t) =
  let range = getRange body in
  let elements = getElements body in
  makeBody (element :: elements) range

let changeRange (body : t) (range : Range.t) =
  let elements = getElements body in
  makeBody elements range

let rec getElementAfterDot (elements : BodyElement.t list) =
  match elements with
  | [ _ ] | [] -> raise ElementDoesntExist
  | BodyElement.Dot :: e :: _ -> e
  | _ :: xs -> getElementAfterDot xs

let map f (body : t list) = List.map f body
let init (elements : BodyElement.t list) = makeBody elements Range.init

let toString (list : t) =
  (fun (b, r) -> Utils.listToString BodyElement.toString b ^ Range.toString r)
    list
