open Jlite_structs

(* helper methods *)

exception DuplicateName;;
exception InvalidVar;;

let check_unique_names names =
  let unique_names = List.sort_uniq compare names in
  let num_unique_names = List.length unique_names in
  let num_names = (List.length names) in
  if num_unique_names != num_names
  then
    raise DuplicateName

let get_var_name
    ((var_type, var_id): var_decl) =
  match var_id with
  | SimpleVarId x -> x
  | _ -> raise InvalidVar

let get_md_name md_decl =
  match md_decl.jliteid with
  | SimpleVarId x -> x
  | _ -> raise InvalidVar


let check_method_decl md =
  let param_names = List.map get_var_name md.params in
  let localvar_names = List.map get_var_name md.localvars in
  let all_var_names = param_names @ localvar_names in
  check_unique_names all_var_names;

  md

let check_class_main
    ((c, md): class_main) =
  let checked_method_decl = check_method_decl md in
  (c, checked_method_decl)

let check_class_decl
    ((class_name, var_decls, md_decls): class_decl) =
  let var_names = List.map get_var_name var_decls in
  let md_names = List.map get_md_name md_decls in
  let field_names = var_names @ md_names in
  check_unique_names field_names;

  let checked_md_decls = List.map check_method_decl md_decls in
  (class_name, var_decls, checked_md_decls)

let check_program
	((mainclass, classes):jlite_program) : jlite_program =

  (* check for unique class names *)
  let main_class_name = fst mainclass in
  let other_class_names = List.map (fun (x, _, _) -> x) classes in
  let class_names = main_class_name :: other_class_names in
  check_unique_names class_names;

  let checked_mainclass = check_class_main mainclass in
  let checked_classes = List.map check_class_decl classes in
  (checked_mainclass, checked_classes)

let type_check_jlite_program prog =
  check_program prog
