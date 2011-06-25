let addToRefList l x = l := x::!l

let list_floatSum = List.fold_left ( +. ) 0.

let tidyString s =
  Pcre.replace ~pat:"^\\s*" ~templ:"" (
    Pcre.replace ~pat:"\\s*$" ~templ:"" s
  )

let stringEq a b = ((String.compare a b) = 0)

let round x =
  if (x -. (floor x)) > 0.5 then
    1 + int_of_float (floor x)
  else
    int_of_float (floor x)

let stringListToFile fname l =
  let ch = open_out fname in
  List.iter (
    fun s -> Printf.fprintf ch "%s\n" s
  ) l;
  close_out ch;
  ()

let list_findUnique l =
  let h = Hashtbl.create (List.length l) in
  List.iter (
    fun x ->
      if not (Hashtbl.mem h x) then
        Hashtbl.add h x true
  ) l;
  Hashtbl.fold (fun a b l -> a::l) h []

  (* actually cuts out punctuation and plural *)
let deplural = Pcre.replace ~pat:"s?[\\W,.:]*$" ~templ:""

let firstWord s =
  let l = Pcre.split ~pat:"\\s" s in
  if List.length l <> 0 then List.hd l
  else ""

  (* skips comments *)
let fileToList fname =
  let lines = ref [] in
  let chIn = open_in fname in
  try
    while true do
      let line = input_line chIn in
      if not (Pcre.pmatch ~pat:"^#" line) then
        addToRefList lines line
    done;
    List.rev !lines
  with
  End_of_file -> (List.rev !lines)

let tidyFileToList fname =
  List.map tidyString (fileToList fname)

  (* skips comments *)
let fileToChunks fname =
  let chunks = ref [] in
  let currChunk = ref [] in
  let chIn = open_in fname in
  let hop () =
    addToRefList chunks (List.rev !currChunk);
    currChunk := [];
    ()
  in
  try
    while true do
      let line = input_line chIn in
      if Pcre.pmatch ~pat:"^\\s*$" line then
        hop ()
      else if not (Pcre.pmatch ~pat:"^#" line) then
        addToRefList currChunk line;
    done;
    List.rev !chunks
  with
  End_of_file -> hop (); (List.rev !chunks)

let tidyFileToChunks fname =
  List.map (List.map tidyString) (fileToChunks fname)

let parseFraction s =
  try (
    if Pcre.pmatch ~pat:"\\/" s then (
      let l = Pcre.split ~pat:"\\/" s in
      if List.length l <> 2 then raise Exit;
      (float_of_string (List.nth l 0)) /. (float_of_string (List.nth l 1))
    )
    else (
      float_of_string s
    )
  )
  with Exit -> failwith (s^" is not a well-formed fraction.")

  (* note binds the pair itself *)
let pairsToHash l =
  let h = Hashtbl.create (List.length l) in
  List.iter (
    fun (a,b) -> Hashtbl.add h a (a,b)
  ) l;
  h

let hashtbl_getKeys h = Hashtbl.fold (fun k v l -> k::l) h []
let hashtbl_getVals h = Hashtbl.fold (fun k v l -> v::l) h []

let list_iteri f myList =
  let rec aux i = function
    | x::l ->
      f i x;
      aux (i+1) l
    | [] -> ()
  in
  aux 0 myList
