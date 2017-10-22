open Jlite_structs

(* helper methods *)

(* distinct name checking *)
exception DuplicateClass;;
exception DuplicateClassMember;;
exception DuplicateMethodMember;;

exception UnknownClass;;
exception UnknownClassMember;;
exception UnknownReference;;
exception UntypedExpression;;

exception FieldNotFoundInEnv;;
exception FieldNotFoundInClass;;
exception InvalidPrintArgument;;
exception InvalidTypesForBinaryExp;;
exception InvalidTypesForFieldAccess;;
exception InvalidTypesForMdCall;;
exception InvalidTypesForAssignStmt;;
exception InvalidTypesForAssignFieldStmt;;
exception InvalidTypesForReadStmt;;
exception InvalidTypesForPrintStmt;;
exception InvalidTypeReturnedInMethod;;
exception InvalidConditionTypeInIfStmt;;
exception InvalidStmtTypesInIfStmt;;
exception InvalidConditionTypeInWhileStmt;;
exception InvalidArgumentsForMdCall;;

exception InvalidVar;;
exception InvalidProg;;

let is_unique_names names =
  let unique_names = List.sort_uniq compare names in
  let num_unique_names = List.length unique_names in
  let num_names = (List.length names) in
  num_unique_names = num_names

let get_var_name
    ((var_type, var_id): var_decl) =
  match var_id with
  | SimpleVarId x -> x
  | TypedVarId (name, _, _) -> name

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
type class_mds_map = (string * string) list
type class_desc = class_name * class_members_desc * class_mds_map

type env = var_decl list

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
  | TypedVarId (name, _, _) -> name

let get_md_type md cname : jlite_type =
  let params_type = List.map fst md.params in
  Method (params_type, md.rettype, cname)

let get_md_name md : string =
  get_var_name_from_id md.jliteid

let get_md_ir3_name cname count : string =
  cname ^ "_" ^ string_of_int count

let get_main_class_desc
    available_classes ((cname, md): class_main) : class_desc =

  let cd_prop = ("main", get_md_type md cname) in
  (cname, [cd_prop], [("main", "main")])

let rec range n =
  if n < 1
  then []
  else range (n - 1) @ [n - 1]


let get_class_desc
    available_classes ((cname, vds, mds): class_decl) : class_desc =

  let vd_names = List.map get_var_name vds in
  let vd_types = List.map get_var_type vds in
  let vd_props = List.combine vd_names vd_types in
  let md_names = List.map get_md_name mds in
  let md_ir3_names = List.map (get_md_ir3_name cname) (range (List.length mds)) in
  let md_types = List.map (fun x -> get_md_type x cname) mds in
  let md_props = List.combine md_names md_types in
  let md_ir3_mapping = List.combine md_names md_ir3_names in
  let cd_props = vd_props @ md_props in
  (cname, cd_props, md_ir3_mapping)

let initialize
    ((mc, cs): jlite_program) =
  let mc_type = get_main_class_type mc in
  let cs_types = List.map get_class_type cs in
  let classes = mc_type :: cs_types in

  let mc_desc = get_main_class_desc classes mc in
  let cs_descs = List.map (get_class_desc classes) cs in
  mc_desc :: cs_descs

let get_typed_var_decl ((t, v): var_decl) scope =
  let name = get_var_name_from_id v in
  t, TypedVarId (name, t, scope)

let get_md_env md =
  md.params @ md.localvars

let get_class_env c vds mds : env =
  let md_env = List.map (fun md -> (get_md_type md c, SimpleVarId (get_md_name md))) mds in
  (ObjectT c, SimpleVarId "this") :: vds @ md_env

let are_equal_vars v1 v2 =
  match v1, v2 with
  | SimpleVarId x, SimpleVarId y -> x = y
  | SimpleVarId x, TypedVarId (y_str, _, _) -> x = y_str
  | TypedVarId (x_str, _, _), SimpleVarId y -> x_str = y
  | TypedVarId (x_str, _, _), TypedVarId (y_str, _, _) -> x_str = y_str

let rec get_var_decl_in_env
    (env: env) (var: var_id) : var_decl =
  match env with
  | [] -> raise UnknownReference
  | (t, v)::env_rem ->
    if are_equal_vars v var
    then (t, v)
    else get_var_decl_in_env env_rem var

let rec get_type_in_env
    (env: env) (var: var_id) : jlite_type =
  let (var_type, _) = get_var_decl_in_env env var in
  var_type

let rec get_var_in_env env var =
  let (_, id) = get_var_decl_in_env env var in
  id

let get_cname_from_env_var_name env var_name =
  let var = SimpleVarId var_name in
  let t_var = get_type_in_env env var in
  match t_var with
  | ObjectT x -> x
  | _ -> raise UnknownClass

let rec get_type_in_cdr
    (cdr: class_members_desc) (value: string) : jlite_type =
  match cdr with
  | (v, t)::cdr_rem ->
    if (v = value)
    then t
    else get_type_in_cdr cdr_rem value
  | [] -> raise UnknownClassMember

let rec get_cdr_in_cdrs
    (cdrs: class_desc list) (cname: class_name) : class_members_desc * class_mds_map =
  match cdrs with
  | (cdr_name, cdr, cmds_map)::cdrs_rem ->
    if (cdr_name = cname)
    then (cdr, cmds_map)
    else get_cdr_in_cdrs cdrs_rem cname
  | [] -> raise UnknownClass

let get_type_in_cdrs
    (cdrs: class_desc list) (cname: class_name) (value: string)  =
  let (cdr, _) = get_cdr_in_cdrs cdrs cname in
  get_type_in_cdr cdr value

let get_ir3_name_from_jlite
    (cdrs: class_desc list) (cname: class_name) var_name : string =
  let (_, ir3_map) = get_cdr_in_cdrs cdrs cname in
  match List.find_opt (fun (x, _) -> x = var_name) ir3_map with
  | None -> raise UnknownReference
  | Some (_, y) -> y

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
      | (Method (params, ret, _), args) ->
        if (params = args)
        then ret
        else raise InvalidArgumentsForMdCall
      | _ -> raise InvalidTypesForMdCall
    end
  | BoolLiteral _ -> BoolT
  | IntLiteral _ -> IntT
  | StringLiteral _ -> StringT
  | ThisWord -> get_type_in_env env (SimpleVarId "this")
  | NullWord -> Null
  | Var x -> get_type_in_env env x
  | TypedExp (_, t) -> t

let rec type_check_exp cdrs env exp : jlite_exp =
  let rec helper exp =
    match exp with
    | UnaryExp (u, e) -> UnaryExp (u, type_check_exp cdrs env e)
    | BinaryExp (b, e1, e2) ->
      begin
        let t_e1 = type_check_exp cdrs env e1 in
        let t_e2 = type_check_exp cdrs env e2 in
        BinaryExp (b, t_e1, t_e2)
      end
    | FieldAccess (e, id) -> FieldAccess (type_check_exp cdrs env e, id)
    | ObjectCreate _ -> exp
    | MdCall (e, es) ->
      let t_e = type_check_exp cdrs env e in
      let t_es = List.map (type_check_exp cdrs env) es in
      let renamed_t_e =
        begin
          match t_e with
          | TypedExp(te, t) ->
            begin
              match te with
              | Var (SimpleVarId v) ->
                begin
                  let cname = get_cname_from_env_var_name env "this" in
                  let ir3_name = get_ir3_name_from_jlite cdrs cname v in
                  TypedExp(Var (SimpleVarId ir3_name), t)
                end
              | FieldAccess (TypedExp(Var (SimpleVarId v), tv), SimpleVarId m) ->
                begin
                  let cname = get_cname_from_env_var_name env v in
                  let ir3_name = get_ir3_name_from_jlite cdrs cname m in
                  TypedExp(FieldAccess (TypedExp(Var (SimpleVarId v), tv), SimpleVarId ir3_name), t)
                end
              | FieldAccess (TypedExp(Var (TypedVarId (vi, tvi, s)), tv), SimpleVarId m) ->
                begin
                  let cname = get_cname_from_env_var_name env vi in
                  let ir3_name = get_ir3_name_from_jlite cdrs cname m in
                  TypedExp(FieldAccess (TypedExp(Var (TypedVarId (vi, tvi, s)), tv), SimpleVarId ir3_name), t)
                end
            end
          | _ -> raise UntypedExpression
        end
      in
      MdCall (renamed_t_e, t_es)
    | BoolLiteral _ -> exp
    | IntLiteral _ -> exp
    | StringLiteral _ -> exp
    | ThisWord -> exp
    | NullWord -> exp
    | Var e -> Var (get_var_in_env env e)
    | TypedExp (e, t) -> e
  in
  TypedExp (helper exp, get_exp_type cdrs env exp)

let rec get_stmts_type cdrs env stmts : jlite_type =
  match stmts with
  | [] -> VoidT
  | [stmt] -> get_stmt_type cdrs env stmt
  | _::rem_stmts -> get_stmts_type cdrs env rem_stmts

and get_stmt_type cdrs env stmt : jlite_type =
  match stmt with
  | IfStmt (_, _, f) -> get_stmts_type cdrs env f
  | WhileStmt (_, ws) -> get_stmts_type cdrs env ws
  | ReadStmt _ -> VoidT
  | PrintStmt _ -> VoidT
  | AssignStmt _ -> VoidT
  | AssignFieldStmt _ -> VoidT
  | MdCallStmt e -> get_exp_type cdrs env e
  | ReturnStmt e -> get_exp_type cdrs env e
  | ReturnVoidStmt -> VoidT

let rec type_check_stmt
    (cdrs: class_desc list) (env: env) stmt : jlite_stmt =
  match stmt with
  | IfStmt (cond, t_stmts, f_stmts) ->
    begin
      let typed_cond = type_check_exp cdrs env cond in
      let t_cond = get_exp_type cdrs env typed_cond in
      if t_cond = BoolT
      then
        begin
          let typed_t_stmts = type_check_stmts cdrs env t_stmts in
          let typed_f_stmts = type_check_stmts cdrs env f_stmts in
          let type_t_stmt = get_stmts_type cdrs env typed_t_stmts in
          let type_f_stmt = get_stmts_type cdrs env typed_f_stmts in
          if type_t_stmt = type_f_stmt
          then IfStmt(typed_cond, typed_t_stmts, typed_f_stmts)
          else raise InvalidStmtTypesInIfStmt
        end
      else raise InvalidConditionTypeInIfStmt
    end
  | WhileStmt (cond, w_stmts) ->
    begin
      let typed_cond = type_check_exp cdrs env cond in
      let t_cond = get_exp_type cdrs env typed_cond in
      if t_cond = BoolT
      then
        begin
          let typed_w_stmts = type_check_stmts cdrs env w_stmts in
          WhileStmt (typed_cond, typed_w_stmts)
        end
      else raise InvalidConditionTypeInWhileStmt
    end
  | ReadStmt id ->
    begin
      let envid = get_var_in_env env id in
      let t_id = get_type_in_env env envid in
      match t_id with
      | IntT | BoolT | StringT -> ReadStmt envid
      | _ -> raise InvalidTypesForReadStmt
    end
  | PrintStmt exp ->
    begin
      let typed_exp = type_check_exp cdrs env exp in
      let t_exp = get_exp_type cdrs env typed_exp in
      match t_exp with
      | IntT | BoolT | StringT -> PrintStmt typed_exp
      | _ -> raise InvalidTypesForPrintStmt
    end
  | AssignStmt (id, e) ->
    begin
      let envid = get_var_in_env env id in
      let t_id = get_type_in_env env envid in
      let typed_exp = type_check_exp cdrs env e in
      let t_exp = get_exp_type cdrs env typed_exp in
      match (t_id, t_exp) with
      | ObjectT _, Null -> AssignStmt (envid, typed_exp)
      | x, y ->
        begin
          if (t_id = t_exp)
          then AssignStmt (envid, typed_exp)
          else raise InvalidTypesForAssignStmt
        end
    end
  | AssignFieldStmt (e1, e2) ->
    begin
      let typed_e1 = type_check_exp cdrs env e1 in
      let typed_e2 = type_check_exp cdrs env e2 in
      let t_e1 = get_exp_type cdrs env typed_e1 in
      let t_e2 = get_exp_type cdrs env typed_e2 in
      match (t_e1, t_e2) with
      | ObjectT _, Null -> AssignFieldStmt (typed_e1, typed_e2)
      | x, y ->
        begin
          if (t_e1 = t_e2)
          then AssignFieldStmt (typed_e1, typed_e2)
          else raise InvalidTypesForAssignFieldStmt
        end
    end
  | MdCallStmt e ->
    begin
      let typed_e = type_check_exp cdrs env e in
      MdCallStmt typed_e
    end
  | ReturnStmt e ->
    begin
      let typed_e = type_check_exp cdrs env e in
      ReturnStmt typed_e
    end
  | ReturnVoidStmt -> stmt

and type_check_stmts cdrs env stmts =
  match stmts with
  | [] -> []
  | ReturnVoidStmt::_ -> [ReturnVoidStmt]
  | (ReturnStmt rexp)::_ -> [ReturnStmt (type_check_exp cdrs env rexp)]
  | stmt::rem_stmts -> type_check_stmt cdrs env stmt :: type_check_stmts cdrs env rem_stmts

let type_check_md_decl
    (cname: class_name) (cdrs:class_desc list) (class_env:env) md (md_count: int) : md_decl =
  let env = get_md_env md @ class_env in
  let typed_stmts = type_check_stmts cdrs env md.stmts in
  let type_stmts = get_stmts_type cdrs env typed_stmts in
  let is_correct_ret_type t_s t_md =
    match (t_s, t_md) with
    | Null, ObjectT _ -> true
    | x, y -> x = y
  in
  let new_md_ir3id =
    if md_count > -1
    then SimpleVarId (cname ^ "_" ^ string_of_int md_count)
    else md.jliteid
  in
  if is_correct_ret_type type_stmts md.rettype
  then
    {
      md with stmts = typed_stmts;
              ir3id = new_md_ir3id;
    }
  else raise InvalidTypeReturnedInMethod

let type_check_main_class
    cdrs ((c, md): class_main) : class_main =
  let env = get_class_env c [] [md] in
  let t_md = type_check_md_decl c cdrs env md (-1) in
  (c, t_md)

let type_check_class
    cdrs ((c, vds, mds): class_decl) : class_decl =

  let t_vds = List.map (fun v -> get_typed_var_decl v 1) vds in
  let env = get_class_env c t_vds mds in
  let rec helper mds count =
    match mds with
    | [] -> []
    | md::rem_mds ->
      begin
        let t_md = type_check_md_decl c cdrs env md count in
        let rem_t_mds = helper rem_mds (count + 1) in
        t_md :: rem_t_mds
      end
  in
  let t_mds = helper mds 0 in
  (c, t_vds, t_mds)

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
