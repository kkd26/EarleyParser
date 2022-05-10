open Chart

let getStartRule = getProductionBody S

let initStep =
  getStartRule >>= fun rules ->
  let firstRule = Rule.getFirst rules in
  let body = Rule.getValue firstRule in
  let newBody = Body.addElement body Dot in
  pushToChart S newBody

let predictStepOneRule (nt, (_, j)) =
  getProductionBody nt >>= fun rules ->
  let r =
    Rule.map
      (fun rule ->
        let body = Rule.getValue rule in
        let bodyWithDot = Body.addElement body Dot in
        (nt, Body.changeRange bodyWithDot (j, j)))
      rules
  in
  pushAllToChart r

let getUniqueNonTerminalsAfterDot compare =
  getCharts >>= fun rules ->
  let filtered = Rule.filter Rule.hasNonTerminalAfterDot rules in
  let elements =
    Rule.map
      (fun rule ->
        (Rule.getNonTerminalAfterADot rule, Body.getRange @@ Rule.getValue rule))
      filtered
  in
  let uniqueElements = Utils.listUniqueComparator compare elements in
  return uniqueElements

let predictStep =
  let compare (x : NonTerminal.t * Range.t) (y : NonTerminal.t * Range.t) =
    fst x = fst y && Range.getRight (snd x) = Range.getRight (snd y)
  in
  getUniqueNonTerminalsAfterDot compare >>= fun nonTerminals ->
  List.fold_right
    (fun e m -> predictStepOneRule e >>> m)
    nonTerminals (return ())

let scanStep word =
  let wordAndNonTerminal = Dictionary.getAll Production.dict word in
  let nonTerminalsInDict =
    Dictionary.map Dictionary.getValue wordAndNonTerminal
  in
  let compare (x : NonTerminal.t * Range.t) (y : NonTerminal.t * Range.t) =
    x = y
  in
  getUniqueNonTerminalsAfterDot compare >>= fun nonTerminals ->
  let filtered =
    List.filter (fun (nt, _) -> List.mem nt nonTerminalsInDict) nonTerminals
  in
  let elements : BodyElement.t list = [ T word; Dot ] in
  let ruleHeadAndBody =
    List.map
      (fun (nt, range) ->
        let newRange = (Range.getLeft range, Range.getRight range + 1) in
        let body = Body.makeBody elements newRange in
        (nt, body))
      filtered
  in
  pushAllToChart ruleHeadAndBody

let completeStep =
  getCharts >>= fun rules ->
  let filtered = Rule.filter Rule.hasNonTerminalAfterDot rules in
  List.fold_right
    (fun rule m ->
      let nonTerminal = Rule.getNonTerminalAfterADot rule in
      let i = Range.getLeft @@ Body.getRange @@ Rule.getValue @@ rule in
      let k = Range.getRight @@ Body.getRange @@ Rule.getValue @@ rule in
      getChartBody nonTerminal >>= fun rules ->
      let f =
        Rule.filter
          (fun rule ->
            Rule.hesDotAtTheEnd rule
            && Range.getLeft @@ Body.getRange @@ Rule.getValue @@ rule = k)
          rules
      in
      let t =
        List.map
          (fun rule1 ->
            let j = Range.getRight @@ Body.getRange @@ Rule.getValue rule1 in
            Rule.moveDotAfterNonTerminal rule (i, j))
          f
      in
      pushAllToChart t >>> m)
    filtered (return ())
