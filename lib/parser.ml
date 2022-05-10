open Chart
open Steps

let fromString s =
  let words = Utils.splitString s in
  let rec fromArrayOfWords = function
    | [] -> return ()
    | x :: xs ->
        predictStep >>> scanStep x >>> completeStep >>> fromArrayOfWords xs
  in
  initStep >>> fromArrayOfWords words
