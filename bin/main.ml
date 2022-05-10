open EarleyParser
open Chart
open Parser

let () =
  let chart =
    fromString "they can fish" >>> getChartBody S >>= fun x ->
    return @@ Rule.toString @@ x >>= printString
  in
  runChart chart Production.init
