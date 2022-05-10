module Dict = Store.Make (struct
  type head = Terminal.t
  type body = NonTerminal.t

  let toString x = x
end)

include Dict

let toString (list : store) =
  String.concat "\n"
    (List.map
       (fun (head, body) -> head ^ " -> " ^ NonTerminal.toString body)
       list)
