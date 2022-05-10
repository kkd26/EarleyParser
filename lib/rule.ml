module Rule = Store.Make (struct
  type head = Head.t
  type body = Body.t

  let toString = NonTerminal.toString
end)

include Rule

let toString (list : store) =
  String.concat "\n"
    (List.map
       (fun (head, body) -> Head.toString head ^ " -> " ^ Body.toString body)
       list)

let hasNonTerminalAfterDot (rule : t) : bool =
  let body = getValue rule in
  let elements = Body.getElements body in
  try
    let element = Body.getElementAfterDot elements in
    match element with BodyElement.NT _ -> true | _ -> false
  with Body.ElementDoesntExist -> false

let hesDotAtTheEnd (rule : t) : bool =
  let body = getValue rule in
  let elements = Body.getElements body in
  try
    let _ = Body.getElementAfterDot elements in
    false
  with Body.ElementDoesntExist -> true

let getNonTerminalAfterADot (rule : t) : NonTerminal.t =
  BodyElement.getNonTerminal @@ Body.getElementAfterDot @@ Body.getElements
  @@ getValue @@ rule

let moveDotAfterNonTerminal (rule : t) (newRange : Range.t) : t =
  let _ = hasNonTerminalAfterDot rule in
  let elements = Body.getElements @@ getValue @@ rule in
  let rec swap = function
    | ([ _ ] | []) as l -> l
    | BodyElement.Dot :: x :: xs -> x :: Dot :: xs
    | x :: xs -> x :: swap xs
  in
  let newElements = swap elements in
  let newBody = Body.makeBody newElements newRange in
  (Rule.getKey rule, newBody)
