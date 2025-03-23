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
