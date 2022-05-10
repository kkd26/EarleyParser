let listToString toStringFunction list =
  String.concat " " (List.map toStringFunction list)

let listUnique list =
  List.fold_left
    (fun prev e -> if List.mem e prev then prev else e :: prev)
    [] list

let listUniqueComparator f list =
  let rec member f x = function
    | [] -> false
    | y :: ys -> f x y || member f x ys
  in
  List.fold_left
    (fun prev e -> if member f e prev then prev else e :: prev)
    [] list

let splitString = Str.split (Str.regexp "[ \t]+")