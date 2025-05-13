open Core

(***************************************************************************)
(* 1) AST Types                                                            *)
(***************************************************************************)

type pattern =
  | PWildcard
  | PVar of string
  | PInt of int
  | PBool of bool
  | PFloat of float
  | PString of string
  | PVariant of string * pattern list
  | PRecord of (string * pattern) list * bool (* true = open row with _ *)

type expr =
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EString of string
  | EVar of string
  | ELambda of string list * expr
  | EApp of expr * expr list
  | EIf of expr * expr * expr
  | EWhile of expr * expr
  | ELetIn of string * expr * expr
  | ELetRec of (string * expr) list * expr
  | EMatch of expr * (pattern * expr) list
  | ERecord of (string * expr) list
  | EFieldGet of expr * string
  | EFieldSet of expr * string * expr
  | EVariant of string * expr list
  | EArray of expr list
  | EArrayGet of expr * expr
  | EArraySet of expr * expr * expr
  | ERef of expr
  | ESetRef of expr * expr
  | ESequence of expr * expr (* e1 ; e2 *)
  | EDeref of expr
  | ERecordExtend of expr * (string * expr) list

type stmt =
  | SLet of string * expr
  | SLetRec of (string * expr) list
  | SModule of string * stmt list
  | SOpen of string
  | SExpr of expr

type program = stmt list

(***************************************************************************)
(* 2) Runtime Value Types                                                  *)
(***************************************************************************)

type value =
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string
  | VVariant of string * value list
  | VRecord of (string, value) Hashtbl.t
  | VArray of value array
  | VRef of value ref
  | VClosure of clos
  | VModule of env
  | VUnit
  | VBuiltin of (value list -> value)

and clos =
  { params : string list
  ; body : expr
  ; env : env
  }

and env = (string, value) Hashtbl.t

(***************************************************************************)
(* 3) Environment Helpers                                                  *)
(***************************************************************************)

let create_env () : env = Hashtbl.create (module String)

let copy_env (parent : env) : env =
  let child = Hashtbl.create (module String) in
  Hashtbl.iteri parent ~f:(fun ~key ~data -> Hashtbl.set child ~key ~data);
  child
;;

let find_var (e : env) (x : string) : value option = Hashtbl.find e x
let set_var (e : env) (x : string) (v : value) : unit = Hashtbl.set e ~key:x ~data:v

let a =
  fun x y z ->
  print_endline x;
  print_endline y;
  print_endline z;
  while
    print_endline "d";
    true
  do
    print_endline "Hello"
  done
;;

(***************************************************************************)
(* 4) Pattern Matching                                                      *)
(***************************************************************************)

let rec match_pattern (v : value) (p : pattern) : (string * value) list option =
  match p, v with
  | PWildcard, _ -> Some []
  | PVar x, _ -> Some [ x, v ]
  | PInt i, VInt j when i = j -> Some []
  | PBool b, VBool c when Bool.equal b c -> Some []
  | PFloat f, VFloat g when Float.equal f g -> Some []
  | PString s, VString t when String.equal s t -> Some []
  | PVariant (tag, ps), VVariant (tagv, vs)
    when String.equal tag tagv && List.length ps = List.length vs ->
    let rec combine subpats subvals accum =
      match subpats, subvals, accum with
      | [], [], Some acc -> Some acc
      | p_hd :: p_tl, v_hd :: v_tl, Some acc ->
        (match match_pattern v_hd p_hd with
         | None -> None
         | Some binds -> combine p_tl v_tl (Some (acc @ binds)))
      | _ -> None
    in
    combine ps vs (Some [])
  | PRecord (fields, is_open), VRecord tbl ->
    (* Ensure specified fields match; if closed pattern, ensure no extra fields exist. *)
    let rec match_fields fl acc =
      match fl with
      | [] -> Some acc
      | (lbl, pat) :: tl -> (
          match Hashtbl.find tbl lbl with
          | None -> None
          | Some v_f -> (
              match match_pattern v_f pat with
              | None -> None
              | Some binds -> match_fields tl (acc @ binds)))
    in
    (match match_fields fields [] with
     | None -> None
     | Some binds ->
       if is_open
       then Some binds
       else (
         (* closed: record must not have extra fields *)
         if Hashtbl.length tbl = List.length fields
         then Some binds
         else None))
  | _ -> None
;;

(***************************************************************************)
(* 5) Trampoline for Tail Calls                                            *)
(***************************************************************************)

type eval_result =
  | Value of value
  | TailCall of clos * value list

(* finish_eval: resolves any TailCall until we get a concrete Value. *)
let rec finish_eval (r : eval_result) : value =
  match r with
  | Value v -> v
  | TailCall (cl, args) ->
    let child_env = copy_env cl.env in
    List.iter2_exn cl.params args ~f:(fun p a -> set_var child_env p a);
    finish_eval (eval_expr child_env cl.body)

(***************************************************************************)
(* 6) Expression Evaluation                                                *)
(***************************************************************************)

and eval_expr (env : env) (e : expr) : eval_result =
  match e with
  | EInt i -> Value (VInt i)
  | EBool b -> Value (VBool b)
  | EFloat f -> Value (VFloat f)
  | EString s -> Value (VString s)
  | EVar x ->
    (match find_var env x with
     | Some v -> Value v
     | None -> failwith (Printf.sprintf "Unbound variable %s" x))
  | ELambda (params, body) -> Value (VClosure { params; body; env })
  | EApp (fn_expr, arg_exprs) ->
    (* Evaluate fn first (fully, so no nested tailcall escapes). *)
    let fn_val = finish_eval (eval_expr env fn_expr) in
    (* Evaluate each argument fully. *)
    let arg_vals = List.map arg_exprs ~f:(fun a -> finish_eval (eval_expr env a)) in
    (match fn_val with
     | VClosure cl ->
       (* Produce a tail call in case we are in tail position of the caller. *)
       TailCall (cl, arg_vals)
     | VBuiltin bf -> Value (bf arg_vals)
     | _ -> failwith "Trying to call a non-function value")
  | EIf (cond_expr, then_expr, else_expr) ->
    let cond_val = finish_eval (eval_expr env cond_expr) in
    (match cond_val with
     | VBool true -> eval_expr env then_expr
     | VBool false -> eval_expr env else_expr
     | _ -> failwith "If condition must be bool")
  | EWhile (cond_expr, body_expr) ->
    let rec loop () =
      let cval = finish_eval (eval_expr env cond_expr) in
      match cval with
      | VBool true ->
        ignore (finish_eval (eval_expr env body_expr));
        loop ()
      | VBool false -> Value VUnit
      | _ -> failwith "While condition must be bool"
    in
    loop ()
  | ELetIn (x, e1, e2) ->
    let v1 = finish_eval (eval_expr env e1) in
    let child = copy_env env in
    set_var child x v1;
    eval_expr child e2
  | ELetRec (bindings, body) ->
    let child = copy_env env in
    (* Step 1: Initialize each name to VUnit. *)
    List.iter bindings ~f:(fun (x, _) -> set_var child x VUnit);
    (* Step 2: Evaluate the RHS of each binding, referencing child. *)
    List.iter bindings ~f:(fun (x, rhs_expr) ->
      let v = finish_eval (eval_expr child rhs_expr) in
      set_var child x v);
    (* Step 3: Evaluate the body in child. *)
    eval_expr child body
  | EMatch (scrut_expr, cases) ->
    let sv = finish_eval (eval_expr env scrut_expr) in
    match_eval env sv cases
  | ERecord fields ->
    let tbl = Hashtbl.create (module String) in
    List.iter fields ~f:(fun (fld, fe) ->
      let fv = finish_eval (eval_expr env fe) in
      Hashtbl.set tbl ~key:fld ~data:fv);
    Value (VRecord tbl)
  | EFieldGet (rec_expr, field) ->
    let rec_val = finish_eval (eval_expr env rec_expr) in
    (match rec_val with
     | VRecord tbl ->
       (match Hashtbl.find tbl field with
        | Some v -> Value v
        | None -> failwith (Printf.sprintf "No field '%s' in record" field))
     | VModule menv ->
       (match find_var menv field with
        | Some v -> Value v
        | None -> failwith (Printf.sprintf "No field '%s' in module" field))
     | _ -> failwith "Field access on non-record/non-module")
  | EFieldSet (rec_expr, field, new_expr) ->
    let rec_val = finish_eval (eval_expr env rec_expr) in
    let new_val = finish_eval (eval_expr env new_expr) in
    (match rec_val with
     | VRecord tbl ->
       Hashtbl.set tbl ~key:field ~data:new_val;
       Value VUnit
     | _ -> failwith "Field set on non-record")
  | EVariant (tag, exprs) ->
    let vals = List.map exprs ~f:(fun ex -> finish_eval (eval_expr env ex)) in
    Value (VVariant (tag, vals))
  | EArray elts ->
    let arr_vals = List.map elts ~f:(fun e -> finish_eval (eval_expr env e)) in
    Value (VArray (Array.of_list arr_vals))
  | EArrayGet (arr_expr, idx_expr) ->
    let arr_val = finish_eval (eval_expr env arr_expr) in
    let idx_val = finish_eval (eval_expr env idx_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then failwith "Array index out of bounds"
       else Value arr.(i)
     | _ -> failwith "Invalid array access")
  | EArraySet (arr_expr, idx_expr, v_expr) ->
    let arr_val = finish_eval (eval_expr env arr_expr) in
    let idx_val = finish_eval (eval_expr env idx_expr) in
    let new_val = finish_eval (eval_expr env v_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then failwith "Array index out of bounds"
       else (
         arr.(i) <- new_val;
         Value VUnit)
     | _ -> failwith "Invalid array set")
  | ERef e1 ->
    let v1 = finish_eval (eval_expr env e1) in
    Value (VRef (ref v1))
  | ESetRef (ref_expr, new_expr) ->
    let r = finish_eval (eval_expr env ref_expr) in
    let nv = finish_eval (eval_expr env new_expr) in
    (match r with
     | VRef cell ->
       cell := nv;
       Value VUnit
     | _ -> failwith "Attempting to set a non-ref value")
  | EDeref e1 ->
    let rv = finish_eval (eval_expr env e1) in
    (match rv with
     | VRef cell -> Value !cell
     | _ -> failwith "Deref on non-ref value")
  | ESequence (e1, e2) ->
    let _ = eval_expr env e1 in
    (* evaluate e1, discard its result *)
    eval_expr env e2 (* then evaluate and return e2 *)
  | ERecordExtend (base_expr, fields) ->
    (* Evaluate the base record *)
    let base_val = finish_eval (eval_expr env base_expr) in
    let base_tbl =
      match base_val with
      | VRecord tbl -> tbl
      | _ -> failwith "Record extension base is not a record"
    in
    (* Copy existing fields to a new table *)
    let new_tbl = Hashtbl.copy base_tbl in
    (* Evaluate new field expressions and replace/insert. *)
    List.iter fields ~f:(fun (fld, fe) ->
        let fv = finish_eval (eval_expr env fe) in
        Hashtbl.set new_tbl ~key:fld ~data:fv);
    Value (VRecord new_tbl)

and match_eval (env : env) (v : value) (cases : (pattern * expr) list) : eval_result =
  match cases with
  | [] -> failwith "Non-exhaustive pattern match"
  | (pat, rhs) :: tl ->
    (match match_pattern v pat with
     | None -> match_eval env v tl
     | Some binds ->
       let child = copy_env env in
       List.iter binds ~f:(fun (nm, vl) -> set_var child nm vl);
       eval_expr child rhs)

(***************************************************************************)
(* 7) Statement Evaluation                                                 *)
(***************************************************************************)

and eval_stmt (env : env) (s : stmt) : unit =
  match s with
  | SLet (x, e1) ->
    let v1 = finish_eval (eval_expr env e1) in
    set_var env x v1
  | SLetRec bindings ->
    (* Step 1: Initialize each name to VUnit in the top-level env. *)
    List.iter bindings ~f:(fun (nm, _) -> set_var env nm VUnit);
    (* Step 2: Evaluate each binding in env. *)
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let v = finish_eval (eval_expr env rhs_expr) in
      set_var env nm v)
  | SModule (mname, stmts) ->
    let menv = create_env () in
    Hashtbl.iteri env ~f:(fun ~key ~data -> set_var menv key data);
    List.iter stmts ~f:(eval_stmt menv);
    set_var env mname (VModule menv)
  | SOpen mname ->
    (match find_var env mname with
     | Some (VModule menv) ->
       Hashtbl.iteri menv ~f:(fun ~key ~data -> set_var env key data)
     | _ -> failwith (Printf.sprintf "Cannot open non-module '%s'" mname))
  | SExpr e -> ignore (finish_eval (eval_expr env e))
;;

(***************************************************************************)
(* 8) Program Evaluation                                                   *)
(***************************************************************************)

let eval_program (env : env) (prog : program) : unit = List.iter prog ~f:(eval_stmt env)

module Chatml_alpha = struct
  (***************************************************************************)
  (* 1) Fresh name generator                                                *)
  (***************************************************************************)

  let counter = ref 0

  let fresh_name base =
    incr counter;
    Printf.sprintf "%s_%d" base !counter
  ;;

  (***************************************************************************)
  (* 2) Patterns                                                             *)
  (***************************************************************************)

  (* alpha_convert_pattern env pat saved_bindings
		     - env: (string -> string) table for name rewriting
		     - pat: the pattern to rename
		     - saved_bindings: used to remember old environment bindings, so we can restore them
		       once we exit the pattern's scope
  *)
  let rec alpha_convert_pattern
            (env : (string, string) Hashtbl.t)
            (pat : pattern)
            (saved_bindings : (string * string option) list ref)
    : pattern
    =
    match pat with
    | PWildcard | PInt _ | PBool _ | PFloat _ | PString _ -> pat
    | PVar x ->
      let new_x = fresh_name x in
      let old = Hashtbl.find env x in
      (* Save old info so we can restore it later. *)
      saved_bindings := (x, old) :: !saved_bindings;
      Hashtbl.set env ~key:x ~data:new_x;
      PVar new_x
    | PVariant (tag, subpats) ->
      let subpats' =
        List.map subpats ~f:(fun sp -> alpha_convert_pattern env sp saved_bindings)
      in
      PVariant (tag, subpats')
    | PRecord (fields, opn) ->
      let fields' =
        List.map fields ~f:(fun (lbl, pat) ->
            lbl, alpha_convert_pattern env pat saved_bindings)
      in
      PRecord (fields', opn)
  ;;

  (***************************************************************************)
  (* 3) Expressions                                                          *)
  (***************************************************************************)

  let rec alpha_convert_expr (env : (string, string) Hashtbl.t) (e : expr) : expr =
    match e with
    (* Simple literals & constants: no renaming needed. *)
    | EInt _ | EBool _ | EFloat _ | EString _ -> e
    (* Variable reference: EVar x -> EVar (env[x]) if present. *)
    | EVar x ->
      (match Hashtbl.find env x with
       | Some new_x -> EVar new_x
       | None -> EVar x (* If missing, treat as free var. *))
    (* Function definition: create fresh names for each parameter. *)
    | ELambda (params, body) ->
      let saved_bindings = ref [] in
      (* Generate fresh names for each parameter. *)
      let new_params =
        List.map params ~f:(fun p ->
          let new_p = fresh_name p in
          let old = Hashtbl.find env p in
          saved_bindings := (p, old) :: !saved_bindings;
          Hashtbl.set env ~key:p ~data:new_p;
          new_p)
      in
      (* Recurse on the body using the updated env. *)
      let new_body = alpha_convert_expr env body in
      (* Restore environment. *)
      List.iter !saved_bindings ~f:(fun (old_p, old_opt) ->
        match old_opt with
        | None -> Hashtbl.remove env old_p
        | Some prev -> Hashtbl.set env ~key:old_p ~data:prev);
      ELambda (new_params, new_body)
    (* Function application: just rename subexpressions. *)
    | EApp (fn, args) ->
      let fn' = alpha_convert_expr env fn in
      let args' = List.map args ~f:(alpha_convert_expr env) in
      EApp (fn', args')
    (* If-then-else. *)
    | EIf (cond, t, f) ->
      EIf (alpha_convert_expr env cond, alpha_convert_expr env t, alpha_convert_expr env f)
    (* While & do. *)
    | EWhile (cond, body) ->
      EWhile (alpha_convert_expr env cond, alpha_convert_expr env body)
    (* Sequence e1; e2 *)
    | ESequence (e1, e2) ->
      ESequence (alpha_convert_expr env e1, alpha_convert_expr env e2)
    (* Let-binding: rename the bound variable, rename rhs & body. *)
    | ELetIn (x, rhs, body) ->
      let rhs' = alpha_convert_expr env rhs in
      (* rename x to new_x, update env. *)
      let saved = Hashtbl.find env x in
      let new_x = fresh_name x in
      Hashtbl.set env ~key:x ~data:new_x;
      let body' = alpha_convert_expr env body in
      (* restore environment *)
      (match saved with
       | None -> Hashtbl.remove env x
       | Some old -> Hashtbl.set env ~key:x ~data:old);
      ELetIn (new_x, rhs', body')
    (* Let-rec: rename each bound variable, alpha-convert each RHS. *)
    | ELetRec (bindings, body) ->
      (* 1. Reserve fresh names for each binding so they can refer to each other. *)
      let binding_names =
        List.map bindings ~f:(fun (nm, _) ->
          (* produce new name for nm *)
          let new_nm = fresh_name nm in
          nm, new_nm)
      in
      (* 2. Save old environment for each. *)
      let saved = ref [] in
      List.iter binding_names ~f:(fun (old_nm, new_nm) ->
        let old_val = Hashtbl.find env old_nm in
        saved := (old_nm, old_val) :: !saved;
        Hashtbl.set env ~key:old_nm ~data:new_nm);
      (* 3. Now alpha-convert each RHS. *)
      let new_bindings =
        List.map2_exn bindings binding_names ~f:(fun (_, rhs_expr) (_old_nm, new_nm) ->
          let rhs' = alpha_convert_expr env rhs_expr in
          new_nm, rhs')
      in
      (* 4. alpha-convert the body. *)
      let body' = alpha_convert_expr env body in
      (* 5. restore the environment. *)
      List.iter !saved ~f:(fun (old_nm, old_opt) ->
        match old_opt with
        | None -> Hashtbl.remove env old_nm
        | Some oldv -> Hashtbl.set env ~key:old_nm ~data:oldv);
      ELetRec (new_bindings, body')
    (* Match/cases: rename scrut, then each case pattern & RHS. *)
    | EMatch (scrut, cases) ->
      let scrut' = alpha_convert_expr env scrut in
      let cases' =
        List.map cases ~f:(fun (pat, rhs) ->
          let saved_bindings = ref [] in
          let pat' = alpha_convert_pattern env pat saved_bindings in
          let rhs' = alpha_convert_expr env rhs in
          (* restore environment after each pattern+rhs *)
          List.iter !saved_bindings ~f:(fun (old_nm, old_opt) ->
            match old_opt with
            | None -> Hashtbl.remove env old_nm
            | Some oldv -> Hashtbl.set env ~key:old_nm ~data:oldv);
          pat', rhs')
      in
      EMatch (scrut', cases')
    (* Record creation, field get/set, arrays, etc. - rename subexpressions. *)
    | ERecord fields ->
      let fields' =
        List.map fields ~f:(fun (fld, fe) -> fld, alpha_convert_expr env fe)
      in
      ERecord fields'
    | EFieldGet (obj, field) -> EFieldGet (alpha_convert_expr env obj, field)
    | EFieldSet (obj, field, new_val) ->
      EFieldSet (alpha_convert_expr env obj, field, alpha_convert_expr env new_val)
    (* Polymorphic variant, rename subexprs. *)
    | EVariant (tag, vs) ->
      let vs' = List.map vs ~f:(alpha_convert_expr env) in
      EVariant (tag, vs')
    (* Arrays & indexing. *)
    | EArray elts -> EArray (List.map elts ~f:(alpha_convert_expr env))
    | EArrayGet (arr, idx) ->
      EArrayGet (alpha_convert_expr env arr, alpha_convert_expr env idx)
    | EArraySet (arr, idx, v) ->
      EArraySet
        (alpha_convert_expr env arr, alpha_convert_expr env idx, alpha_convert_expr env v)
    (* References, setref, deref just rename subexpressions. *)
    | ERef e1 -> ERef (alpha_convert_expr env e1)
    | ESetRef (r, v) -> ESetRef (alpha_convert_expr env r, alpha_convert_expr env v)
    | EDeref e1 -> EDeref (alpha_convert_expr env e1)
    | ERecordExtend (base, fields) ->
      let base' = alpha_convert_expr env base in
      let fields' = List.map fields ~f:(fun (lbl, ex) -> lbl, alpha_convert_expr env ex) in
      ERecordExtend (base', fields')
  ;;

  (***************************************************************************)
  (* 4) Statements                                                           *)
  (***************************************************************************)

  let rec alpha_convert_stmt (env : (string, string) Hashtbl.t) (s : stmt) : stmt =
    match s with
    | SLet (x, rhs) ->
      (* no restore for top-level let *)
      let rhs' = alpha_convert_expr env rhs in
      let new_x = fresh_name x in
      Hashtbl.set env ~key:x ~data:new_x;
      SLet (new_x, rhs')
    | SLetRec bindings ->
      (* 1) First, create fresh names for each binding. *)
      let binding_names =
        List.map bindings ~f:(fun (nm, _rhs) ->
          let new_nm = fresh_name nm in
          nm, new_nm)
      in
      (* 2) Insert these new names into the environment so that
		     (a) they see each other (for mutually recursive definitions)
		     (b) subsequent statements can reference them. *)
      List.iter binding_names ~f:(fun (old_nm, new_nm) ->
        Hashtbl.set env ~key:old_nm ~data:new_nm);
      (* 3) Now alpha-convert each binding’s RHS with the updated env. *)
      let new_binds =
        List.map2_exn bindings binding_names ~f:(fun (_, rhs_expr) (_old_nm, new_nm) ->
          let rhs' = alpha_convert_expr env rhs_expr in
          new_nm, rhs')
      in
      (* 4) Do NOT restore the old environment here, because it’s top-level.
		     We want “f -> f_1” (for example) to stay in the env so that later
		     statements can see it. *)
      SLetRec new_binds
    | SModule (mname, stmts) ->
      (* rename mname, but do NOT restore it at top level *)
      let new_m = fresh_name mname in
      Hashtbl.set env ~key:mname ~data:new_m;
      let stmts' = List.map stmts ~f:(alpha_convert_stmt env) in
      SModule (new_m, stmts')
    | SOpen mname ->
      (* If you want to rename modules too, do so. Otherwise handle as you prefer. *)
      (match Hashtbl.find env mname with
       | Some new_name -> SOpen new_name
       | None -> SOpen mname)
    | SExpr e ->
      let e' = alpha_convert_expr env e in
      SExpr e'
  ;;

  (***************************************************************************)
  (* 5) Entire program                                                      *)
  (***************************************************************************)

  let alpha_convert_program (prog : program) : program =
    let env = Hashtbl.create (module String) in
    List.map prog ~f:(fun stmt ->
      let result = alpha_convert_stmt env stmt in
      print_endline @@ Printf.sprintf "After stmt: environment =\n";
      Hashtbl.iteri env ~f:(fun ~key ~data ->
        print_endline @@ Printf.sprintf "  %s -> %s\n" key data);
      result)
  ;;
end
