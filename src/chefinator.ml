open Menu
open Measurement
open Calories
open Base
open Shop
open Recipe

(*open Measurement;
open ParseRecipe;*)
(*let recipeFiles = ["test.txt"]*)
let recipeFiles = ["data/simpleRecipes.txt"; "data/esther.txt";
"data/bryan.txt"; "data/erick.txt"]
let calorieFile = "data/calorieCount.txt"
let baseIngredFile = "data/baseIngreds.txt"
(*let menuPlanFile = "testPlan.txt"*)
let menuPlanFile = "data/menuPlan.txt"

let init () =
  let measHash = initializeMeasHash () in
  parseMeasurements measHash measNames;
  let calorieHash = calorieHashOfFile measHash calorieFile in
  let catBaseIngreds = tidyFileToChunks baseIngredFile in
  let baseIngreds =
    Dasg.dasgOfStringList (
      List.filter (fun x -> not (stringEq x "")) (
        List.flatten catBaseIngreds ) )
  in
  (*dasgPrint baseIngreds;*)
  let recipes =
    List.map
      (Recipe.parseRecipe measHash)
      (List.flatten (List.map tidyFileToChunks recipeFiles)) in
  let recipeHash = pairsToHash recipes in
  measHash, calorieHash, baseIngreds, recipeHash

let guessScale calorieHash needed recipe =
  let cals = snd (calorifyRecipe calorieHash recipe) in
  let scale = needed /. cals in
  (*Printf.printf "%s: %f calories, if only %f\n" (fst recipe) cals scale;*)
  Printf.printf "%d %s\n" (round scale) (fst recipe);
  ()

let checkRecipeLog ch calorieHash baseIngreds recipe =
  Printf.fprintf ch "'%s'\n" (fst recipe);
  let cals = snd (calorifyRecipeLog ch calorieHash recipe) in
  (* the below actually does make sense, as eventually we want to get hashes for
   * all recipes *)
  let _ = classifyAllRecipesLog ch baseIngreds [recipe] in
  Printf.fprintf ch "calories: %f\n\n" cals;
  ()

let checkRecipe = checkRecipeLog stdout

let checkRecipeFile measHash calorieHash baseIngreds fname =
  let out = open_out (fname^".log") in
  List.iter (checkRecipeLog out calorieHash baseIngreds)
    ( List.map
      (parseRecipe measHash)
    (tidyFileToChunks fname) );
    close_out out;
    ()

let checkMeal calorieHash recipeHash s =
  let currCals =
    list_floatSum (
      List.map (
        fun item ->
          try
            let a =
              Pcre.extract ~full_match:false ~pat:"([\\d\\.]*)\\s*(.*)"
                (tidyString item)
            in
            let recipeName = tidyString a.(1) in
            if Hashtbl.mem recipeHash recipeName then
              let recipe = Hashtbl.find recipeHash recipeName in
              guessScale calorieHash 12000. recipe;
              let cals =
                snd (
                  calorifyRecipe
                    calorieHash
                    (scaleRecipe (float_of_string a.(0)) recipe) )
              in
              Printf.printf "currently %s gives us %f\n" a.(0) cals;
              cals
            else
              failwith ("unknown recipe: "^recipeName)
          with
          | Not_found -> failwith ("checkMeal couldn't make sense of "^item)
      ) (Pcre.split ~pat:"\\s*:\\s*" s) )
  in
  Printf.printf "total: %f\n\n" currCals;
  ()

let check () =
  let measHash, calorieHash, baseIngreds, _ = init () in
  List.iter
    (checkRecipeFile measHash calorieHash baseIngreds)
    recipeFiles

let rock () =
  let _, calorieHash, baseIngreds, recipeHash = init () in
  let (ingredToBase, _) =
    classifyAllRecipes
      baseIngreds
      ( hashtbl_getVals recipeHash )
  in
  let scaledRecipes =
    scaleByMenuPlan recipeHash (menuPlanOfFile menuPlanFile) in
  let ch = open_out "chefinator.log" in
  let shopList =
    makeShopList ch ingredToBase (List.flatten scaledRecipes) in
  close_out ch;
  (* printShopList shopList; *)
  let ch = open_out "shoppingList.txt" in
  writeShopList ch shopList;
  close_out ch;
  let ch = open_out "dayMenu.txt" in
  writeRecipeListCal ch calorieHash scaledRecipes;
  close_out ch;
  (* now calc total calories *)
  calorifyRecipe calorieHash (
    "total calories: ",
    List.flatten (List.map snd (List.flatten scaledRecipes))
  )

let _ = rock ()
