exception Done of string

type dasg = Dasg of ((string, dasg) Hashtbl.t)

let hashOfDasg = function
  | Dasg (h) -> h

let dasgify f d = f (hashOfDasg d)
let dasgMem = dasgify Hashtbl.mem
let dasgFind = dasgify Hashtbl.find
let dasgAdd = dasgify Hashtbl.add
let dasgIter f d = Hashtbl.iter f (hashOfDasg d)
let dasgCreate n = Dasg ( Hashtbl.create n )
let dasgFold f d x = Hashtbl.fold f (hashOfDasg d) x

let rec extendDasg d = function
  | s::l -> (
      if dasgMem d s then
        extendDasg (dasgFind d s) l
      else
        let newD = dasgCreate 4 in
        dasgAdd d s newD;
        extendDasg newD l
  )
  | [] -> ()

let printSpace n =
  for i=1 to n do print_string " " done

let dasgPrint d =
  let rec aux tabLevel = function
    | Dasg (h) ->
        List.iter (
          fun s ->
            let d = Hashtbl.find h s in
            printSpace tabLevel;
            print_string (s^"\n");
            aux (tabLevel+2) d;
        ) (
          List.sort compare (
            Hashtbl.fold (fun k _ l -> k::l) h [] ) )
  in
  aux 0 d

let dasgOfStringList sList =
  let l = List.map (Pcre.split ~pat:"\\s+") sList in
  let d = dasgCreate (List.length l) in
  List.iter (extendDasg d) l;
  d
