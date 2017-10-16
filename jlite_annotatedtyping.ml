open Jlite_structs

(* helper methods *)

(* distinct name checking *)
exception DuplicateClass;;
exception DuplicateClassMember;;
exception DuplicateMethodMember;;

exception UnknownClass;;
exception UnknownClassMember;;
exception UnknownReference;;
exception FieldNotFoundInEnv;;
exception FieldNotFoundInClass;;
exception InvalidPrintArgument;;
exception InvalidTypesForBinaryExp;;
exception InvalidTypesForFieldAccess;;
exception InvalidTypesForMdCall;;
exception InvalidArgumentsForMdCall;;

exception InvalidVar;;
exception InvalidProg;;

let is_unique_names names =
  let unique_names = List.sort_uniq compare names in
  let num_unique_names = List.length unique_names in
  let num_names = (List.length names) in
  num_unique_names == num_names

let get_var_name
    ((var_type, var_id): var_decl) =
  match var_id with
  | SimpleVarId x -> x
  | _ -> raise InvalidVar

let get_md_name md_decl =
  match md_decl.jliteid with
  | SimpleVarId x -> x
  | _ -> raise InvalidVar

let every xs =
  List.fold_left (fun x y -> x && y) true xs

let any xs =
  List.fold_left (fun x y -> x || y) false xs

let check_method_decl md =
  let param_names = List.map get_var_name md.params in
  let localvar_names = List.map get_var_name md.localvars in
  let all_var_names = param_names @ localvar_names in
  if is_unique_names all_var_names
  then true
  else raise DuplicateMethodMember

let check_class_main
    ((c, md): class_main) =
  check_method_decl md

let check_class_decl
    ((class_name, var_decls, md_decls): class_decl) =
  let var_names = List.map get_var_name var_decls in
  let md_names = List.map get_md_name md_decls in
  let field_names = var_names @ md_names in
  if not (is_unique_names field_names)
  then
    raise DuplicateClassMember
  else
    let checked_md_decls = List.map check_method_decl md_decls in
    every checked_md_decls

let check_program
	((mainclass, classes):jlite_program) =

  (* check for unique class names *)
  let main_class_name = fst mainclass in
  let other_class_names = List.map (fun (x, _, _) -> x) classes in
  let class_names = main_class_name :: other_class_names in
  let value = is_unique_names class_names in
  if not value
  then
    raise DuplicateClass;

  let checked_mainclass = check_class_main mainclass in
  let checked_classes = every (List.map check_class_decl classes) in
  checked_mainclass && checked_classes

type class_members_desc = ((string * jlite_type) list)
type class_desc = class_name * class_members_desc

type env = (string * jlite_type) list

let get_main_class_type
    ((class_name, _): class_main) : jlite_type =
  ObjectT(class_name)

let get_class_type
    ((class_name, _, _): class_decl) : jlite_type =
  ObjectT(class_name)

let get_var_type
    ((var_type, _): var_decl) : jlite_type =
  var_type

let get_var_name_from_id vid : string =
  match vid with
  | SimpleVarId x -> x
  | _ -> raise InvalidVar

let get_md_type md : jlite_type =
  let params_type = List.map fst md.params in
  Method (params_type, md.rettype)

let get_md_name md : string =
  get_var_name_from_id md.jliteid

let get_main_class_desc
    available_classes ((cname, md): class_main) : class_desc =

  let cd_prop = ("main", get_md_type md) in
  (cname, [cd_prop])

let get_class_desc
    available_classes ((cname, vds, mds): class_decl) : class_desc =

  let vd_names = List.map get_var_name vds in
  let vd_types = List.map get_var_type vds in
  let vd_props = List.combine vd_names vd_types in
  let md_names = List.map get_md_name mds in
  let md_types = List.map get_md_type mds in
  let md_props = List.combine md_names md_types in
  let cd_props = vd_props @ md_props in
  (cname, cd_props)

let initialize
    ((mc, cs): jlite_program) =
  let mc_type = get_main_class_type mc in
  let cs_types = List.map get_class_type cs in
  let classes = mc_type :: cs_types in

  let mc_desc = get_main_class_desc classes mc in
  let cs_descs = List.map (get_class_desc classes) cs in
  mc_desc :: cs_descs

let get_md_env md =
  let all_vars = md.params @ md.localvars in
  let names = List.map get_var_name all_vars in
  let types = List.map get_var_type all_vars in
  List.combine names types

let get_class_env c vds mds =
  let vd_env = List.map (fun vd -> (get_var_name vd, get_var_type vd)) vds in
  let md_env = List.map (fun md -> (get_md_name md, get_md_type md)) mds in
  ("this", ObjectT c) :: vd_env @ md_env

let rec get_type_in_env
    (env: env) (value: string) : jlite_type =
  match env with
  | (v, t)::env_rem ->
    if (v = value)
    then t
    else get_type_in_env env_rem value
  | [] -> raise UnknownReference

let rec get_type_in_cdr
    (cdr: class_members_desc) (value: string) : jlite_type =
  match cdr with
  | (v, t)::cdr_rem ->
    if (v = value)
    then t
    else get_type_in_cdr cdr_rem value
  | [] -> raise UnknownClassMember

let rec get_cdr_in_cdrs
    (cdrs: class_desc list) (cname: class_name) : class_members_desc =
  match cdrs with
  | (cdr_name, cdr)::cdrs_rem ->
    if (cdr_name = cname)
    then cdr
    else get_cdr_in_cdrs cdrs_rem cname
  | [] -> raise UnknownClass

let get_type_in_cdrs
    (cdrs: class_desc list) (cname: class_name) (value: string)  =
  let cdr = get_cdr_in_cdrs cdrs cname in
  get_type_in_cdr cdr value

let rec get_exp_type
    (cdrs: class_desc list) (env: env) exp : jlite_type =
  match exp with
  | UnaryExp (_, x) -> get_exp_type cdrs env x
  | BinaryExp (op, x, y) ->
    begin
      let t_x = get_exp_type cdrs env x in
      let t_y = get_exp_type cdrs env y in
      match (op, t_x, t_y) with
      | (BooleanOp _, BoolT, BoolT) -> BoolT
      | (RelationalOp _, IntT, IntT) -> BoolT
      | (AritmeticOp _, IntT, IntT) -> IntT
      | _ -> raise InvalidTypesForBinaryExp
    end
  | FieldAccess (e, id) ->
    begin
      match (get_exp_type cdrs env e, get_var_name_from_id id) with
      | (ObjectT o, id_name) -> get_type_in_cdrs cdrs o id_name
      | _ -> raise InvalidTypesForFieldAccess
    end
  | ObjectCreate x ->
    begin
      match get_cdr_in_cdrs cdrs x with
      | _ -> ObjectT x
    end
  | MdCall (e, es) ->
    begin
      let t_e = get_exp_type cdrs env e in
      let t_es = List.map (get_exp_type cdrs env) es in
      match (t_e, t_es) with
      | (Method (params, ret), args) ->
        if (params = args)
        then ret
        else raise InvalidArgumentsForMdCall
      | _ -> raise InvalidTypesForMdCall
    end
  | BoolLiteral _ -> BoolT
  | IntLiteral _ -> IntT
  | StringLiteral _ -> StringT
  | ThisWord -> get_type_in_env env "this"
  | NullWord -> Null
  | Var x -> get_type_in_env env (get_var_name_from_id x)
  | TypedExp (_, t) -> t

let type_check_exp cdrs env exp =
  let t_exp = get_exp_type cdrs env exp in
  TypedExp (exp, t_exp)

let type_check_stmt
    (cdrs: class_desc list) (env: env) stmt : jlite_stmt =
  match stmt with
  (* | IfStmt (c, t, e) -> *)
  (*   begin *)
  (*     match get_exp_type md_env c with *)
  (*     | BT(BoolT) -> *)
  (*     | _ -> raise InvalidIfArgument *)
  (*   end *)
  (* | WhileStmt (e, es) ->  *)
  | ReadStmt _ -> stmt
  | PrintStmt e -> PrintStmt (type_check_exp cdrs env e)
  | AssignStmt (id, e) -> AssignStmt (id, type_check_exp cdrs env e)
  | AssignFieldStmt (e1, e2) ->
    begin
      let t_e1 = type_check_exp cdrs env e1 in
      let t_e2 = type_check_exp cdrs env e2 in
      AssignFieldStmt (t_e1, t_e2)
    end
  | MdCallStmt e -> MdCallStmt (type_check_exp cdrs env e)
  | ReturnStmt e -> ReturnStmt (type_check_exp cdrs env e)
  | ReturnVoidStmt -> stmt

let type_check_md_decl
    (cdrs:class_desc list) (class_env:env) md : md_decl =
  let env = get_md_env md @ class_env in
  let t_stmts = List.map (type_check_stmt cdrs env) md.stmts in
  {md with stmts = t_stmts}

let type_check_main_class
    cdrs ((c, md): class_main) : class_main =
  let env = get_class_env c [] [md] in
  let t_md = type_check_md_decl cdrs env md in
  (c, t_md)

let type_check_class
    cdrs ((c, vds, mds): class_decl) : class_decl =
  let env = get_class_env c vds mds in
  let t_mds = List.map (type_check_md_decl cdrs env) mds in
  (c, vds, t_mds)

let type_check_prog
    ((mainclass, classes): jlite_program) : jlite_program =
  let cdrs = initialize (mainclass, classes) in
  let t_mainclass = type_check_main_class cdrs mainclass in
  let t_classes = List.map (type_check_class cdrs) classes in
  (t_mainclass, t_classes)

let type_check_jlite_program prog =
  if check_program prog
  then type_check_prog prog
  else raise InvalidProg
