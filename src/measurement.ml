#use "base.ml";;

exception Incompatible_classes

type measurement =      Tsp of float | Tbsp of float | Cup of float | Quart of float | Gal of float | 
                        Oz of float | Lb of float |
                        Generic of float | Unquant

type measClass =        EngVolume | EngWeight | Other

let tsp x = Tsp x
let tbsp x = Tbsp x
let cup x = Cup x
let quart x = Quart x
let gal x = Gal x
let oz x = Oz x
let lb x = Lb x
let generic x = Generic x
let unquant x = Unquant 

let initializeMeasHash () = (
  let measHash = Hashtbl.create 10 in
  Hashtbl.add measHash "Tsp" tsp;
  Hashtbl.add measHash "Tbsp" tbsp;
  Hashtbl.add measHash "Cup" cup;
  Hashtbl.add measHash "Quart" quart;
  Hashtbl.add measHash "Gal" gal;
  Hashtbl.add measHash "Oz" oz;
  Hashtbl.add measHash "Lb" lb;
  Hashtbl.add measHash "Generic" generic;
  Hashtbl.add measHash "Unquant" unquant;
  measHash
)

let measToPair = function 
  | Tsp(x)     -> "Tsp", x
  | Tbsp(x)    -> "Tbsp", x
  | Cup(x)     -> "Cup", x
  | Quart(x)   -> "Quart", x
  | Gal(x)     -> "Gal", x
  | Oz(x)      -> "Oz", x
  | Lb(x)     -> "Lb", x
  | Generic(x) -> "Generic", x
  | Unquant    -> "Unquant", 0. (* inelegant! *)

let measNames = 
  [
  "Tsp t tsp teaspoon";
  "Tbsp T tbsp tb tablespoon";
  "Cup C c cup";
  "Quart qt";
  "Gal";
  "Oz oz";
  "Lb lb";
  "Generic generic";
  ]

let measToString meas = 
  let s, x =
    match meas with 
    | Tsp(x)     -> "Tsp", x
    | Tbsp(x)    -> "Tbs", x
    | Cup(x)     -> "Cup", x
    | Quart(x)   -> "Qt ", x
    | Gal(x)     -> "Gal", x
    | Oz(x)      -> "Oz ", x
    | Lb(x)     -> "Lb ", x
    | Generic(x) -> "   ", x
    | Unquant    -> "   ", 0. (* inelegant! *)
  in
  if x <> 0. then
    Printf.sprintf "%4.1f %s" x s
  else "        "
  
let measToStringNoSpace meas = 
  tidyString (measToString meas)

  (* remove trailing s and period *)
let procMeasString s = Pcre.replace ~pat:"s?[\\.]?$" ~templ:"" s

let memMeas measHash measString =
  Hashtbl.mem measHash (procMeasString measString)

let findMeas measHash measString =
  Hashtbl.find measHash (procMeasString measString)

let makeMeas measHash measString x = 
  if memMeas measHash measString then
      (findMeas measHash measString) x
  else
    failwith (measString^" is not a known measurement.")

let mapMeas measHash f meas = 
  let (measString, x) = measToPair meas in
  makeMeas measHash measString (f x)

let parseMeasurements measHash measNames =
  List.iter (
    fun s ->
      let l = Pcre.split ~pat:"\\s+" s in
      let base = List.hd l in
      if memMeas measHash base then (
        try
          let constructor = findMeas measHash base in
          List.iter (
            fun s -> Hashtbl.add measHash s constructor;
            (* print_endline (s^" is "^base); *)
          ) (List.tl l);
        with
        | Not_found -> failwith ("breakdown in parseMeasurements: "^base)
      )
      else (
        failwith (base^" not known in parseMeasurements!");
      )
  ) measNames;
  ()

  (* this function scales everything to the smallest unit. *)
let measToBase = function 
  | Tsp(x)     -> EngVolume, 1. *. x
  | Tbsp(x)    -> EngVolume, 3. *. x
  | Cup(x)     -> EngVolume, 48. *. x
  | Quart(x)   -> EngVolume, 192. *. x
  | Gal(x)     -> EngVolume, 768. *. x
  | Oz(x)      -> EngWeight, 1. *. x
  | Lb(x)      -> EngWeight, 16. *. x
  | Generic(x) -> Other, 1. *. x
  | Unquant    -> Other, 0.

let baseToMeas measClass x = 
  let rec measFold (x, currConstruct) = function
    | (convAmt, constructor)::l ->
        let newX = x /. convAmt in
        if newX >= 1. then
          measFold ( newX, constructor) l
        else 
          currConstruct x
    | [] -> 
        currConstruct x
  in
  match measClass with
  | EngVolume -> 
      measFold 
        (x, tsp)
        [ 3., tbsp; 16., cup; 4., quart; 4., gal ]
  | EngWeight -> 
      measFold 
        (x, oz)
        [ 16., lb; ]
  | Other ->
      if x <> 0. then
        generic (x)
      else
        Unquant

let scaleMeas factor meas = 
  let (mClass, quant) = measToBase meas in
  baseToMeas mClass (quant *. factor)

let measSum m1 m2 = 
  let mClass1, x1 = measToBase m1 in
  let mClass2, x2 = measToBase m2 in
  if mClass1 = mClass2 then
    baseToMeas mClass1 (x1 +. x2)
  else
    raise Incompatible_classes

let measRatio num denom = 
  let numClass, numX = measToBase num in
  let denomClass, denomX = measToBase denom in
  if numClass = denomClass then
    numX /. denomX
  else
    raise Incompatible_classes
    (*failwith "incompatible classes in measRatio"*)
