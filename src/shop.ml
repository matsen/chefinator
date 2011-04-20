#use "calories.ml";;
#use "dasg.ml";;

let ingredToWords s = 
  Pcre.split ~pat:"\\s+" (
    Pcre.replace ~pat:"\\(.*\\)" ~templ:"" s )

let killParen = Pcre.replace ~pat:"\\(.*\\)" ~templ:""

let classifyIngred baseIngreds ingred = 
  (* kill parenthetical statements *)
  let ingredEdit = killParen ingred in
  (* aux tries to match any of the keys of d to ingred, then recurs if found *)
  let rec aux biStr d =
    try
      dasgIter (
        fun s dp -> 
          (* match the beginning of words only or things in dashed *)
          if Pcre.pmatch ~pat:("\\s"^s) ingredEdit || 
             Pcre.pmatch ~pat:("^"^s) ingredEdit ||
             Pcre.pmatch ~pat:("-"^s) ingredEdit 
             then
            (* found something *)
            let spaced = 
              biStr^(
                if String.length biStr <> 0 then " "
                else ""
              ) in
            raise (Done (aux (spaced^s) dp));
      ) d;
      (* nothing found *)
      biStr
    with
    | Done(doneStr) -> doneStr
  in 
  aux "" baseIngreds

let getIngredList recipeList = 
  List.flatten ( List.map ( fun (name, ingredList) -> ingredList ) recipeList )

let classifyAllRecipesGen verb ch baseIngreds recipeList = 
  let ingredToBase = Hashtbl.create (5*(List.length recipeList)) in
  let baseToIngred = Hashtbl.create (5*(List.length recipeList)) in
  List.iter (
    fun (meas, ingred) ->
      let base = classifyIngred baseIngreds (String.lowercase ingred) in
      if stringEq base "" then (
        if verb then 
          Printf.fprintf ch "** unclassified: '%s'\n" ingred;
      )
      else (
        if verb then (
          Printf.fprintf ch "'%s': '%s'\n" base ingred;
        );
        Hashtbl.add ingredToBase ingred base;
        Hashtbl.add baseToIngred base ingred;
        )
  ) (getIngredList recipeList);
  (ingredToBase, baseToIngred)

let classifyAllRecipesLog = classifyAllRecipesGen true
let classifyAllRecipesVerb = classifyAllRecipesLog stdout
let classifyAllRecipes = classifyAllRecipesGen false stdout

let makeShopList ch ingredToBase recipeList = 
  let shopList = Hashtbl.create (2 * (List.length recipeList)) in
  List.iter (
    fun (meas, ingred) ->
      try 
        if Hashtbl.mem ingredToBase ingred then (
          (* this is a classified ingredient *)
          let baseIngred = Hashtbl.find ingredToBase ingred in
          if Hashtbl.mem shopList baseIngred then (
            let subtot, measList = Hashtbl.find shopList baseIngred in
            let newSubtot = measSum subtot meas in
            Printf.fprintf ch 
              "found '%s', increased quantity by %s to %s\n"
              baseIngred
              (measToString meas)
              (measToString newSubtot);
            Hashtbl.replace shopList baseIngred (newSubtot, measList @ [meas])
          )
          else (
            Printf.fprintf ch 
              "found '%s' which was new, now have %s\n"
              baseIngred
              (measToString meas);
              Hashtbl.add shopList baseIngred (meas, [meas])
              )
        )
        else (
          if not (stringEq ingred "") then
            Printf.fprintf ch "unclassified: '%s'\n" ingred;
        )
      with
      | Incompatible_classes -> 
          flush ch;
          print_endline ("incompatibility: "^ingred)
  ) (getIngredList recipeList);
  shopList
 
let writeShopList ch shopList = 
  Hashtbl.iter (
    fun item (meas, measList) ->
      Printf.fprintf ch "%s  %s  (%s)\n" 
      (measToString meas)
      item
      (String.concat ", " (List.map measToStringNoSpace measList))
  ) shopList

let printShopList = writeShopList stdout

