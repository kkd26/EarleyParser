open Monad

module Chart = State (struct
  type t = Rule.store * Rule.store
end)

include Chart

let ( >>> ) m x = Chart.( >>= ) m (fun _ -> x)
let getProductions = get >>= fun x -> return @@ snd @@ x
let getCharts = get >>= fun x -> return @@ fst @@ x
let putToCharts chart = getProductions >>= fun prod -> put (chart, prod)

let pushToChart (head : Head.t) (body : Body.t) =
  getCharts >>= fun env ->
  let newEnv = Rule.update env head body in
  putToCharts newEnv

let pushAllToChartWihtSameHead (head : Head.t) (bodyList : Body.t list) =
  List.fold_right
    (fun body m -> pushToChart head body >>> m)
    bodyList (return ())

let pushAllToChart (rules : Rule.store) =
  List.fold_right
    (fun (head, body) m -> pushToChart head body >>> m)
    rules (return ())

let emptyChart = putToCharts Rule.empty

let getProductionBody (head : Head.t) =
  getProductions >>= fun env ->
  let body = try Rule.getAll env head with Rule.EmptyStore _ -> [] in
  return body

let getChartBody (head : Head.t) =
  getCharts >>= fun env ->
  let body = try Rule.getAll env head with Rule.EmptyStore _ -> [] in
  return body

let runChart chart initProduction =
  let stateAndValue = runState ~init:(Rule.empty, initProduction) chart in
  snd stateAndValue

let printString s = return (Printf.printf "%s" s)
