open Measurement
open Base


let measHash = Hashtbl.create 10

let parseRecipeLine measHash s =
  try
    let a = Pcre.extract ~full_match:false ~pat:"([0-9\\/\\.]*)\\s*(\\S*)\\s+(.*)" s in
    if stringEq a.(0) "" then
      (Unquant, a.(1)^" "^a.(2))
    else if memMeas measHash a.(1) then
      (makeMeas measHash a.(1) (parseFraction a.(0)), a.(2))
    else
      (Generic (parseFraction a.(0)), a.(1)^" "^a.(2))
  with
  | Not_found -> (Unquant, s) (* couldn't parse with numbers *)

let parseRecipe measHash l =
  if List.length l < 2 then
    if List.length l = 0 then failwith("incomplete recipe")
    else failwith("incomplete recipe: "^(List.hd l))
  else (firstWord (List.hd l), List.map (parseRecipeLine measHash) (List.tl l))

let scaleIngred quant = function
  (meas, (info: String.t)) -> (scaleMeas quant meas, info^"")

let scaleRecipe quant = function
  (name, ingredList) ->
    (name^(Printf.sprintf " scaled by %g" quant),
    List.map (scaleIngred quant) ingredList)

let getRecipeName = function
  name, _ -> name

let writeRecipeNames infname outfname =
  let recipes = List.map (parseRecipe measHash) (tidyFileToChunks infname) in
  stringListToFile outfname (List.map getRecipeName recipes)

let writeRecipe ch = function
  name, ingreds ->
    Printf.fprintf ch "%s\n" name;
    List.iter (
      fun (meas,ingred) ->
        Printf.fprintf ch "%s %s\n" (measToString meas) ingred
    ) ingreds

let writeRecipeList ch =
  list_iteri (
    fun i l ->
      Printf.fprintf ch "\n# day %d\n" (i+1);
      List.iter (
        fun r ->
          writeRecipe ch r;
          Printf.fprintf ch "\n"
      ) l
  )
