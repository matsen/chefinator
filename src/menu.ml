#use "shop.ml";;

let menuPlanOfFile mpFile = 
  let ch = open_out (mpFile^".log") in
  let mp = 
    List.map (
      List.map (
        fun mealStr ->
          let a = Pcre.extract ~full_match:false ~pat:"([\\d\\.]*)\\s+(.*)" mealStr in
          let quant = float_of_string a.(0) in
          Printf.fprintf ch "found %g of '%s'\n" quant a.(1);
          (float_of_string a.(0), a.(1))
      )
    ) (
      List.map 
      (Pcre.split ~pat:"\\s*:\\s*")  
      (tidyFileToList mpFile)
    )
  in
  close_out ch;
  mp

let scaleByMenuPlan recipeHash menuPlan = 
  List.map (
    List.map (
      fun (scale, recipeName) ->
        let tidy = tidyString recipeName in
        if Hashtbl.mem recipeHash tidy then
          scaleRecipe 
          scale
          (Hashtbl.find recipeHash tidy)
        else
          failwith ("unknown recipe: "^tidy)
          ) 
  ) menuPlan
