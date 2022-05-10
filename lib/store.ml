module Make (S : sig
  type head
  type body

  val toString : head -> string
end) =
struct
  exception EmptyStore of string

  type t = S.head * S.body
  type store = t list

  let getAll (env : store) (head : S.head) : store =
    let t = List.filter (fun (h, _) -> h = head) env in
    match t with [] -> raise (EmptyStore (S.toString head)) | t -> t

  let update (env : store) (head : S.head) (body : S.body) : store =
    let newElement = (head, body) in
    if List.mem newElement env then env else newElement :: env

  let empty : store = []
  let getFirst (rules : store) = List.hd rules
  let map f (rules : store) = List.map f rules
  let filter f (rules : store) : store = List.filter f rules
  let getKey (rules : t) = fst rules
  let getValue (rules : t) = snd rules
end
