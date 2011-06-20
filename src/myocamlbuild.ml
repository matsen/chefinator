open Ocamlbuild_plugin;;
open Command;;


(* the following two functions are a hack to properly include the library
 * depenencies until GODI upgrades to ocamlbuild 3.12, which works nicely with
 * ocamlfind. *)

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
    while true do Buffer.add_channel buf ic 1 done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  let contents = (Buffer.contents buf) in
  if contents = "" then ""
  else Filename.chop_suffix contents "\n"
in

let ocamlfind_query pkg =
  syscall (Printf.sprintf "ocamlfind query %s" (Filename.quote pkg))
in

dispatch begin function

  | After_rules ->
      (* automatically include <lib> when the use_<lib> tag is given in _tags *)
      ocaml_lib ~extern:true ~dir:(ocamlfind_query "pcre") "pcre";

  | _ -> ()
end;;

