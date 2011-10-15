open Base
open Recipe
open Measurement

let calorieHashOfFile measHash fname =
  let calorieHash = Hashtbl.create 10 in
  List.iter (
    fun line ->
      let a = Array.of_list ( Pcre.split ~pat:"\\s" line ) in
      assert(Array.length a = 4);
      (*print_endline ("found "^a.(2));*)
      Hashtbl.add calorieHash a.(2) (* key is the name *)
         ( (float_of_string a.(3)), (* the number of calories *)
         makeMeas measHash a.(1) (float_of_string a.(0)) );
  ) (fileToList fname);
  calorieHash

  (* we find the first word in the itemStr which is bound in calorieHash *)
let findCaloric calorieHash itemStr =
  (* aux finds the first string that exists in calorieHash *)
  let rec aux = function
    | item::l ->
        let dplItem = deplural (String.lowercase item) in
        if Hashtbl.mem calorieHash dplItem then
          Some dplItem
        else aux l
    | [] ->
        None
  in
  aux (Pcre.split ~pat:"\\s" itemStr)

  (* total calories of recipe *)
let calorifyRecipeGen verb ch calorieHash = function
  (name, ingredList) ->
    (name,
      List.fold_left (
        fun subtot (meas, item) ->
          match findCaloric calorieHash item with
          | Some(calStr) -> (
              try
                let (cals, calsMeas) = Hashtbl.find calorieHash calStr in
                if verb then (
                  Printf.fprintf ch "found as '%s': '%s' with %4.1f calories\n" calStr item cals;
                );
                subtot +. cals *. (measRatio meas calsMeas)
              with
              | Incompatible_classes ->
                  failwith (calStr^" and "^item^" are not compatible in "^name);
          )
          | None ->
              if verb then Printf.fprintf ch "noncaloric: '%s'\n" item;
              subtot +. 0.
      ) 0. ingredList )

let calorifyRecipe = calorifyRecipeGen false stdout
let calorifyRecipeLog = calorifyRecipeGen true
let calorifyRecipeVerb = calorifyRecipeGen true

let calorieScaleRecipe calorieHash desiredCalories recipe =
  scaleRecipe
     (desiredCalories /. (snd (calorifyRecipe calorieHash recipe)))
     recipe

let calorieHashToNames calorieHash =
  List.map (
    Pcre.replace ~pat:"-" ~templ:" "
  ) (Hashtbl.fold (fun k _ l -> k::l) calorieHash [])

let writeRecipeListCal ch calorieHash =
  list_iteri (
    fun i l ->
      let calorie_list =
        List.map
          (fun r -> snd (calorifyRecipe calorieHash r))
          l
      in
      Printf.fprintf ch "\n# day %d: %g calories\n"
                        (i+1)
                        (List.fold_left ( +. ) 0. calorie_list);
      List.iter2 (
        fun r c ->
          writeRecipe ch r;
          Printf.fprintf ch "%5.1f calories\n\n" c;
          ) l calorie_list
  )
