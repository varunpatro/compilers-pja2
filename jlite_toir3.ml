open Jlite_structs
open Ir3_structs
open Jlite_annotatedtyping

exception InvalidExprForId3;;
exception InvalidExprForIdc3;;

let rec unzip3 lst =
  match lst with
  | [] -> ([], [], [])
  | (x, y, z)::rem_lst ->
    begin
      let (xs, ys, zs) = unzip3 rem_lst in
      x :: xs, y :: ys, z :: zs
    end

let labelcount = ref 0
let fresh_label () =
  (labelcount:=!labelcount+1; !labelcount)

let varcount = ref 0
let fresh_var () =
  (varcount:=!varcount+1; (string_of_int !varcount))


let get_var_decl3_from_var_decl
    (t_id, id: var_decl) : var_decl3 =
  let id3 = get_var_name_from_id id in
  t_id, id3


let ir3expr_to_id3 expr jt vars stmts toidc3 =
  if toidc3 = false
  then (expr, vars, stmts)
  else
    begin
      let new_varname = "_t" ^ fresh_var() in
      let new_vardecl = (jt, new_varname) in
      let new_stmt = AssignStmt3 (new_varname, expr) in
      (Idc3Expr (Var3 new_varname),
       vars @ [new_vardecl],
       stmts @ [new_stmt])
    end

let varid_to_ir3exp
    cname v toid3 : (ir3_exp * var_decl3 list * ir3_stmt list) =
  match v with
  | SimpleVarId id -> (Idc3Expr (Var3 id), [], [])
  | TypedVarId (id, t, s) ->
    if s = 1
    then let thisExpr =
           FieldAccess3 ("this", id) in
      (ir3expr_to_id3 thisExpr t [] [] toid3)
    else let newExpr = Idc3Expr (Var3 id) in
      (newExpr, [], [])

let ir3expr_get_idc3
    (expr: ir3_exp) : idc3 =
  match expr with
  | Idc3Expr e -> e
  | _ -> raise InvalidExprForIdc3

let ir3expr_get_id3
    (expr: ir3_exp) : id3 =
  let idc3_val = ir3expr_get_idc3 expr in
  match idc3_val with
  | Var3 id -> id
  | _ -> raise InvalidExprForId3

let expr_to_expr3
    cname expr toidc3 toid3 : (ir3_exp * var_decl3 list * ir3_stmt list) =
  let rec helper je toidc3 toid3 =
    match je with
    | TypedExp (te, t) ->
      begin
        match te with
        | UnaryExp (op, e) ->
          begin
            let (argir3, vars, stmts) = helper e true false in
            let argidc3 = ir3expr_get_idc3 argir3 in
            let newExpr = UnaryExp3 (op, argidc3) in
            (ir3expr_to_id3 newExpr t vars stmts toidc3)
          end
        | BinaryExp (op, arg1, arg2) ->
          begin
            let (arg1ir3, vars1, stmts1) = helper arg1 true false in
            let (arg2ir3, vars2, stmts2) = helper arg2 true false in
            let arg1idc3 = ir3expr_get_idc3 arg1ir3 in
            let arg2idc3 = ir3expr_get_idc3 arg2ir3 in
            let newExpr = BinaryExp3 (op, arg1idc3, arg2idc3) in
            (ir3expr_to_id3 newExpr t
               (vars1 @ vars2) (stmts1 @ stmts2) toidc3)
          end
        | FieldAccess (e, id) ->
          begin
            let (argir3, vars, stmts) = helper e true false in
            let argid3 = ir3expr_get_id3 argir3 in
            let newExpr = FieldAccess3 (argid3, get_var_name_from_id id) in
            (ir3expr_to_id3 newExpr t vars stmts toidc3)
          end
        | ObjectCreate name ->
          begin
            let newExpr = ObjectCreate3 name in
            (ir3expr_to_id3 newExpr t [] [] toidc3)
          end
        | MdCall (e, es) ->
          begin
            let (callee, caller, evars, estmts) =
              match e with
              | TypedExp (te, t) ->
                begin
                  match te with
                  | Var v -> (v, "this", [], [])
                  | FieldAccess (e, id) ->
                    begin
                      let (eir3, vars, stmts) = helper e true false in
                      (id, ir3expr_get_id3 eir3, vars, stmts)
                    end
                  | _ -> raise InvalidTypesForMdCall
                  end
              | _ -> raise UntypedExpression
            in
            let args = List.map (fun x -> helper x true false) es in
            let (args_ir3, args_vars, args_stmts) = unzip3 args in
            let args_idc3 = Var3 caller :: List.map ir3expr_get_idc3 args_ir3 in
            let newExpr = MdCall3 (get_var_name_from_id callee, args_idc3) in
            (ir3expr_to_id3 newExpr t
               (evars @ List.flatten args_vars) (estmts @ List.flatten args_stmts) toidc3)
          end
        | BoolLiteral b ->
          begin
            let newExpr = Idc3Expr (BoolLiteral3 b) in
            ir3expr_to_id3 newExpr t [] [] toid3
          end
        | IntLiteral i ->
          begin
            let newExpr = Idc3Expr (IntLiteral3 i) in
            ir3expr_to_id3 newExpr t [] [] toid3
          end
        | StringLiteral s ->
          begin
            let newExpr = Idc3Expr (StringLiteral3 s) in
            ir3expr_to_id3 newExpr t [] [] toid3
          end
        | ThisWord -> (Idc3Expr (Var3 "this"), [], [])
        | NullWord ->
          begin
            let newExpr = (Idc3Expr (IntLiteral3 0)) in
			(ir3expr_to_id3 newExpr t [] [] toid3)
          end
        | Var id -> varid_to_ir3exp cname id true
        | TypedExp (te, t) -> helper te toidc3 toid3
      end
    | _ -> raise UntypedExpression
  in

  helper expr toidc3 toid3



let rec stmts_to_IR3stmts cname md stmts =
  match stmts with
  | [] -> [], []
  | s::lst ->
    begin
      let rec helper s : (var_decl3 list * ir3_stmt list) =
        match s with
        | IfStmt (e, stmts1, stmts2) ->
          begin
            let true_label = fresh_label() in
            let false_label = fresh_label() in
            let finish_label = fresh_label() in
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e false false in
            let (tvars, tstmts) = stmts_to_IR3stmts cname md stmts1 in
            let (fvars, fstmts) = stmts_to_IR3stmts cname md stmts2 in
            let vars = exprvars @ tvars @ fvars in
            let stmts =
              begin
                let finish = [Label3 finish_label] in
                let check = IfStmt3 (expr3, true_label) :: [GoTo3 false_label] in
                let correct = Label3 true_label :: tstmts @ [GoTo3 finish_label] in
                let incorrect = Label3 false_label :: fstmts @ [GoTo3 finish_label] in
                check @ correct @ incorrect @ finish
              end
            in
            (vars, stmts)
          end
        | WhileStmt (e, stmts) ->
          begin
            let start_label = fresh_label() in
            let true_label = fresh_label() in
            let false_label = fresh_label() in
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e false false in
            let (wvars, wstmts) = stmts_to_IR3stmts cname md stmts in
            let vars = exprvars @ wvars in
            let stmts =
              begin
                let start = Label3 start_label :: exprstmts in
                let check = IfStmt3 (expr3, true_label) :: [GoTo3 false_label] in
                let body_part = Label3 true_label :: wstmts @ [GoTo3 start_label] in
                let finish = [Label3 false_label] in
                start @ check @ body_part @ finish
              end
            in
            (vars, stmts)
          end
        | ReadStmt id ->
          begin
            let (idir3, idvars, idstmts) = varid_to_ir3exp cname id true in
            let readlnIR3 = ReadStmt3 (ir3expr_get_id3 idir3) in
            let vars = idvars in
            let stmts = idstmts @ [readlnIR3] in
            (vars, stmts)
          end
        | PrintStmt e ->
          begin
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e false false in
            let printIR3 = PrintStmt3 (ir3expr_get_idc3 expr3) in
            (exprvars, exprstmts @ [printIR3])
          end
        | AssignStmt (id, e) ->
          begin
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e false false in
            let assgIR3 =
              match id with
              | TypedVarId (x, _, 1) ->
                AssignFieldStmt3 (FieldAccess3 ("this", x), expr3)
              | SimpleVarId x | TypedVarId (x, _, _) -> AssignStmt3(x, expr3)
            in
            (exprvars, exprstmts @ [assgIR3])
          end
        | AssignFieldStmt (e1, e2) ->
          begin
            let (arg1ir3, arg1vars, arg1stmts) = expr_to_expr3 cname e1 false false in
            let (arg2ir3, arg2vars, arg2stmts) = expr_to_expr3 cname e1 false false in
            let assgIR3 = AssignFieldStmt3 (arg1ir3, arg2ir3) in
            (arg1vars @ arg2vars, arg1stmts @ arg2stmts @ [assgIR3])
          end
        | MdCallStmt e ->
          begin
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e true true in
            let retIR3 = (MdCallStmt3 expr3) in
            (exprvars, exprstmts @ [retIR3])
          end
        | ReturnStmt e ->
          begin
            let (expr3, exprvars, exprstmts) = expr_to_expr3 cname e true true in
            let retIR3 = (ReturnStmt3 (ir3expr_get_id3 expr3)) in
            (exprvars, exprstmts @ [retIR3])
          end
        | ReturnVoidStmt -> ([], [ReturnVoidStmt3])
      in
      let (vs, ss) = helper s in
      let (tailvars, tailstmts) = stmts_to_IR3stmts cname md lst in
      (vs @ tailvars, ss @ tailstmts)
    end

let get_md_decl3_from_md_decl
    (cname: class_name) (md: md_decl) : md_decl3 =
  let (nvars, nstmts)  = stmts_to_IR3stmts cname md md.stmts in
  {
    id3 = get_var_name_from_id md.ir3id;
    rettype3 = md.rettype;
    params3 =
      begin
        let this_var =  (ObjectT cname, "this") in
        let param_vars = List.map get_var_decl3_from_var_decl md.params in
        this_var :: param_vars
      end
    ;
    localvars3 =
      begin
        let local_vars = List.map get_var_decl3_from_var_decl md.localvars in
        local_vars @ nvars
      end
    ;
    ir3stmts = nstmts;
  }

let get_cdata3_from_class
    (cname, vars, _ : class_decl) : cdata3 =
  cname, List.map get_var_decl3_from_var_decl vars

let jlite_program_to_IR3
    (((mc_name, mc_md), classes): jlite_program) : ir3_program =

  let mainclass_cdata3 = (mc_name, []) in
  let mainclass_md_decl3 = get_md_decl3_from_md_decl mc_name mc_md in
  let classes_cdata3 = List.map get_cdata3_from_class classes in
  let class_md_decl3 =
    begin
      let get_md3s_from_class (c, _, mds) = List.map (get_md_decl3_from_md_decl c) mds in
      let class_md3s = List.map get_md3s_from_class classes in
      List.flatten class_md3s
    end
  in
  let cdata3_list = mainclass_cdata3::classes_cdata3 in
  cdata3_list, mainclass_md_decl3, class_md_decl3
