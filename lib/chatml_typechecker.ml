open Core
open Chatml.Chatml_lang

(***************************************************************************)
(* A significantly simplified Hindley-Milner style type-checker for        *)
(* the ChatML language.                                                    *)
(*                                                                         *)
(* The implementation is heavily inspired by the Nox type-checker that was *)
(* provided in the task description.  Most of the algorithmic structure—   *)
(* unification, generalisation / instantiation, row-polymorphic records—   *)
(* is taken from there but adapted to the simpler ChatML AST (which has no *)
(* source locations) and to the small subset of the language that is used  *)
(* by the demo `dsl_script` program.                                        *)
(*                                                                         *)
(* The goal is not to provide a production-quality checker (there are many *)
(* features still missing) but to catch the kinds of mistakes shown in the *)
(* script – most importantly, accessing or mutating a record field that is *)
(* not present in the record literal that created the value.               *)
(***************************************************************************)

(** --------------------------------------------------------------------- *)

(** 1. Utility environment (string maps).                                  *)

(** --------------------------------------------------------------------- *)

module Env = struct
  type 'a t = 'a String.Map.t

  let empty : 'a t = String.Map.empty
  let add k v m : 'a t = Map.set m ~key:k ~data:v
  let find m k = Map.find m k
  let find_exn m k = Map.find_exn m k
  let of_list l = List.fold l ~init:empty ~f:(fun acc (k, v) -> add k v acc)
  let merge m1 m2 = Map.merge_skewed m1 m2 ~combine:(fun ~key:_ v_left _v_right -> v_left)
  let is_empty = Map.is_empty
  let bindings = Map.to_alist
  let map m ~f = Map.map m ~f
  let iter m ~f = Map.iteri m ~f:(fun ~key ~data -> f key data)

  let choose m =
    match Map.min_elt m with
    | None -> raise_notrace (Invalid_argument "empty map")
    | Some (k, v) -> k, v
  ;;

  let exists m f = Map.existsi m ~f:(fun ~key ~data -> f key data)
  let fold m ~init ~f = Map.fold m ~init ~f:(fun ~key ~data acc -> f ~key ~data acc)
  let singleton = String.Map.singleton
end

(** --------------------------------------------------------------------- *)

(** 2. Types and type variables                                            *)

(** --------------------------------------------------------------------- *)

type name = string

type typ =
  | Fun of typ list * typ
  | Generic of name
  | Var of tv ref
  | Ref of typ
  | Record of typ (* row *)
  | Variant of typ (* not implemented – kept for parity *)
  | Tuple of typ list
  | Array of typ
  | Number
  | Boolean
  | String
  | Row of typ Env.t * typ (* fields * tail *)
  | Empty_row
  | Unit

and tv =
  | Bound of typ
  | Free of name * int (* name, level *)

(** --------------------------------------------------------------------- *)

(** 3. Global state for the HM algorithm                                   *)

(** --------------------------------------------------------------------- *)

let previous_id = ref 0
let current_id = ref 0
let current_lvl = ref 0

let reset_levels () =
  previous_id := 0;
  current_id := 0;
  current_lvl := 0
;;

let enter_level () =
  previous_id := !current_id;
  incr current_lvl
;;

let exit_level () =
  current_id := !previous_id;
  decr current_lvl
;;

let gensym () : string =
  let id = !current_id in
  incr current_id;
  let letter = Char.of_int_exn ((id mod 26) + Char.to_int 'a') |> Char.to_string in
  if id >= 26 then letter ^ Int.to_string (id / 26) else letter
;;

let new_var level = Var (ref (Free (gensym (), level)))

(** --------------------------------------------------------------------- *)

(** 4. Generalisation / instantiation                                      *)

(** --------------------------------------------------------------------- *)

let instantiate ty =
  let table = Hashtbl.create (module String) in
  let rec inst = function
    | Fun (ps, r) -> Fun (List.map ps ~f:inst, inst r)
    | Generic x ->
      (match Hashtbl.find table x with
       | Some t -> t
       | None ->
         let v = new_var !current_lvl in
         Hashtbl.add_exn table ~key:x ~data:v;
         v)
    | Var { contents = Bound ty } -> inst ty
    | Ref t -> Ref (inst t)
    | Record r -> Record (inst r)
    | Variant r -> Variant (inst r)
    | Tuple ts -> Tuple (List.map ts ~f:inst)
    | Row (fields, t) -> Row (Env.map fields ~f:inst, inst t)
    | Array t -> Array (inst t)
    | t -> t
  in
  inst ty
;;

(** 
  Generalisation                                                                                                                                                                          
  We now adopt the same strategy that the Nox checker uses: all free type variables that were  
  introduced at a deeper level than the current one are turned into           
  universally-quantified (Generic) type variables.                            
*)
let generalise ty =
  let rec gen = function
    | Fun (params, ret) -> Fun (List.map params ~f:gen, gen ret)
    | Var { contents = Bound t } -> gen t
    | Var { contents = Free (name, lvl) } when lvl > !current_lvl -> Generic name
    | Ref t -> Ref (gen t)
    | Record row -> Record (gen row)
    | Variant row -> Variant (gen row)
    | Tuple ts -> Tuple (List.map ts ~f:gen)
    | Row (fields, tail) -> Row (Env.map fields ~f:gen, gen tail)
    | Array t -> Array (gen t)
    | t -> t
  in
  gen ty
;;

(** --------------------------------------------------------------------- *)

(** 5. Row utilities                                                       *)

(** --------------------------------------------------------------------- *)

let rec merge_fields = function
  | Var { contents = Bound ty } -> merge_fields ty
  | Var _ as var -> Env.empty, var
  | Record r -> merge_fields r
  | Variant r -> merge_fields r
  | Row (fs, rest) ->
    (match merge_fields rest with
     | fs', rest' when Env.is_empty fs' -> fs, rest'
     | fs', rest' -> Env.merge fs fs', rest')
  | Empty_row -> Env.empty, Empty_row
  | _ -> failwith "merge_fields: expect a row type"
;;

(** --------------------------------------------------------------------- *)

(** 6. Occurs check                                                        *)

(** --------------------------------------------------------------------- *)

let rec occurs tv = function
  | Fun (ps, r) -> List.exists ps ~f:(occurs tv) || occurs tv r
  | Var { contents = Bound ty } -> occurs tv ty
  | Var tv' when phys_equal tv tv' -> true
  | Var ({ contents = Free (name, lvl) } as tv') ->
    let new_lvl =
      match !tv with
      | Free (_, lvl_tv) -> Int.min lvl lvl_tv
      | _ -> lvl
    in
    tv' := Free (name, new_lvl);
    false
  | Ref t | Record t | Variant t | Array t -> occurs tv t
  | Tuple ts -> List.exists ts ~f:(occurs tv)
  | Row (fs, rest) -> Env.exists fs (fun _ t -> occurs tv t) || occurs tv rest
  | _ -> false
;;

(** --------------------------------------------------------------------- *)

(** 7. Unification                                                         *)

(** --------------------------------------------------------------------- *)

exception Type_error of string
exception Type_error_with_loc of string * Source.span

let rec unify lhs rhs =
  if phys_equal lhs rhs
  then ()
  else (
    match lhs, rhs with
    | Fun (ps1, r1), Fun (ps2, r2) ->
      if List.length ps1 <> List.length ps2
      then raise (Type_error "Function arity mismatch")
      else (
        List.iter2_exn ps1 ps2 ~f:unify;
        unify r1 r2)
    | Var { contents = Bound t1 }, t2 | t1, Var { contents = Bound t2 } -> unify t1 t2
    | Var ({ contents = Free _ } as tv), t | t, Var ({ contents = Free _ } as tv) ->
      if occurs tv t then raise (Type_error "Recursive types") else tv := Bound t
    | Ref t1, Ref t2 | Array t1, Array t2 -> unify t1 t2
    | Record r1, Record r2 | Variant r1, Variant r2 -> unify r1 r2
    | Tuple ts1, Tuple ts2 ->
      if List.length ts1 <> List.length ts2
      then raise (Type_error "Tuple arity mismatch")
      else List.iter2_exn ts1 ts2 ~f:unify
    | (Row _ as row1), (Row _ as row2) -> unify_rows row1 row2
    | Row (fs, _), Empty_row | Empty_row, Row (fs, _) ->
      let lbl, _ = Env.choose fs in
      raise (Type_error (Printf.sprintf "Row does not contain label '%s'" lbl))
    | Number, Number
    | Boolean, Boolean
    | String, String
    | Empty_row, Empty_row
    | Unit, Unit -> ()
    | _ ->
      raise
        (Type_error
           (Printf.sprintf "Cannot unify %s with %s" (show_type lhs) (show_type rhs))))

and unify_rows lhs rhs =
  (* Convert both rows to a field-map plus a tail row var *)
  let map_l, tail_l = merge_fields lhs in
  let map_r, tail_r = merge_fields rhs in
  (* Unify common labels, collect missing ones *)
  let rec collect l r missing_l missing_r =
    match l, r with
    | (lbl_l, ty_l) :: tl, (lbl_r, ty_r) :: tr ->
      (match String.compare lbl_l lbl_r with
       | 0 ->
         unify ty_l ty_r;
         collect tl tr missing_l missing_r
       | c when c < 0 -> collect tl r missing_l (Env.add lbl_l ty_l missing_r)
       | _ -> collect l tr (Env.add lbl_r ty_r missing_l) missing_r)
    | [], [] -> missing_l, missing_r
    | [], rest -> Env.of_list rest |> Env.merge missing_l, missing_r
    | rest, [] -> missing_l, Env.of_list rest |> Env.merge missing_r
  in
  let missing_l, missing_r =
    collect (Env.bindings map_l) (Env.bindings map_r) Env.empty Env.empty
  in
  match Env.is_empty missing_l, Env.is_empty missing_r with
  | true, true -> unify tail_l tail_r
  | true, false -> unify tail_r (Row (missing_r, tail_l))
  | false, true -> unify tail_l (Row (missing_l, tail_r))
  | false, false ->
    (match tail_l with
     | Var ({ contents = Free _ } as tv) ->
       let row_var = new_var !current_lvl in
       unify tail_r (Row (missing_r, row_var));
       (* Ensure tv is still free, then bind. *)
       (match !tv with
        | Bound _ -> raise (Type_error "Recursive row types")
        | _ -> ());
       tv := Bound (Row (missing_l, row_var))
     | Empty_row -> unify tail_l (Row (missing_l, new_var 0))
     | _ -> assert false)

(** --------------------------------------------------------------------- *)
(** 8. Pretty printer for types (used in error messages)                   *)
(** --------------------------------------------------------------------- *)

and show_type = function
  | Number -> "number"
  | Boolean -> "bool"
  | String -> "string"
  | Unit -> "unit"
  | Array t -> Printf.sprintf "[%s] array" (show_type t)
  | Ref t -> Printf.sprintf "ref(%s)" (show_type t)
  | Fun (ps, r) ->
    let params = ps |> List.map ~f:show_type |> String.concat ~sep:", " in
    Printf.sprintf "(%s -> %s)" params (show_type r)
  | Record row -> Printf.sprintf "{%s}" (show_row row)
  | Tuple ts ->
    ts |> List.map ~f:show_type |> String.concat ~sep:" * " |> Printf.sprintf "(%s)"
  | Variant _ -> "<variant>" (* not fully implemented *)
  | Row _ | Empty_row -> "<row>" (* only appears nested *)
  | Generic n -> n
  | Var { contents = Free (n, _) } -> Printf.sprintf "'%s" n
  | Var { contents = Bound t } -> show_type t

and show_row row =
  let rec aux = function
    | Empty_row -> ""
    | Row (fs, rest) ->
      let fields_str =
        Env.bindings fs
        |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s: %s" k (show_type v))
        |> String.concat ~sep:"; "
      in
      let rest_str =
        match rest with
        | Empty_row -> ""
        | _ -> "; ..."
      in
      fields_str ^ rest_str
    | Var { contents = Free (n, _) } -> Printf.sprintf "'%s" n
    | Var { contents = Bound t } -> aux t
    | _ -> ""
  in
  aux row
;;

(** --------------------------------------------------------------------- *)

(** 9. Typing environment                                                  *)

(** --------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* 9.1  Free variables / generalisation / instantiation                       *)
(* -------------------------------------------------------------------------- *)

type scheme = typ
type tenv = scheme Env.t ref

let init_env () : tenv =
  let base =
    Env.of_list
      [ "print", Fun ([ Generic "any" ], Unit)
      ; "num2str", Fun ([ Number ], String)
      ; "bool2str", Fun ([ Boolean ], String)
      ; "+", Fun ([ Number; Number ], Number)
      ; "-", Fun ([ Number; Number ], Number)
      ; "*", Fun ([ Number; Number ], Number)
      ; "/", Fun ([ Number; Number ], Number)
      ]
  in
  ref base
;;

let lookup env x =
  match Env.find !env x with
  | Some sc -> instantiate sc
  | None -> raise (Type_error (Printf.sprintf "Unknown variable '%s'" x))
;;

let add env x ty = env := Env.add x (generalise ty) !env

(** --------------------------------------------------------------------- *)

(** 10. Inference – expressions                                            *)

(** --------------------------------------------------------------------- *)

let rec infer_expr env expr =
  try
    match expr.value with
    | EInt _ | EFloat _ -> Number
    | EBool _ -> Boolean
    | EString _ -> String
    | EVar x -> lookup env x
    | ELambda (params, body) ->
      enter_level ();
      let param_tys =
        List.map params ~f:(fun p ->
          let t = new_var !current_lvl in
          add env p t;
          t)
      in
      let body_ty = infer_expr env body in
      exit_level ();
      Fun (param_tys, body_ty)
    | EApp (fn_expr, arg_exprs) ->
      let fn_ty = infer_expr env fn_expr in
      let arg_tys = List.map arg_exprs ~f:(infer_expr env) in
      let ret_ty = new_var !current_lvl in
      unify fn_ty (Fun (arg_tys, ret_ty));
      ret_ty
    | EIf (c, t, e) ->
      let c_ty = infer_expr env c in
      unify c_ty Boolean;
      let t_ty = infer_expr env t in
      let e_ty = infer_expr env e in
      unify t_ty e_ty;
      t_ty
    | EWhile (cond, body) ->
      unify (infer_expr env cond) Boolean;
      ignore (infer_expr env body);
      Unit
    | ESequence (e1, e2) ->
      ignore (infer_expr env e1);
      infer_expr env e2
    | ELetIn (x, rhs, body) ->
      enter_level ();
      let rhs_ty = infer_expr env rhs in
      exit_level ();
      add env x rhs_ty;
      infer_expr env body
    | ELetRec (bindings, body) ->
      (* Add placeholders first *)
      List.iter bindings ~f:(fun (nm, _) -> add env nm (new_var !current_lvl));
      (* Now infer and unify *)
      List.iter bindings ~f:(fun (nm, rhs) -> unify (lookup env nm) (infer_expr env rhs));
      infer_expr env body
    | ERecord fields ->
      let row =
        List.fold_right fields ~init:Empty_row ~f:(fun (lbl, expr) acc ->
          let ty = infer_expr env expr in
          Row (Env.singleton lbl ty, acc))
      in
      Record row
    | EFieldGet (obj, lbl) ->
      let obj_ty = infer_expr env obj in
      let field_ty = new_var !current_lvl in
      let tail_row = new_var !current_lvl in
      unify obj_ty (Record (Row (Env.singleton lbl field_ty, tail_row)));
      field_ty
    | EFieldSet (obj, lbl, rhs) ->
      let obj_ty = infer_expr env obj in
      let rhs_ty = infer_expr env rhs in
      let tail_row = new_var !current_lvl in
      unify obj_ty (Record (Row (Env.singleton lbl rhs_ty, tail_row)));
      Unit
    | EArray elts ->
      let elt_ty = new_var !current_lvl in
      List.iter elts ~f:(fun e -> unify (infer_expr env e) elt_ty);
      Array elt_ty
    | EArrayGet (arr, idx) ->
      unify (infer_expr env idx) Number;
      let elt_ty = new_var !current_lvl in
      unify (infer_expr env arr) (Array elt_ty);
      elt_ty
    | EArraySet (arr, idx, v) ->
      unify (infer_expr env idx) Number;
      let v_ty = infer_expr env v in
      unify (infer_expr env arr) (Array v_ty);
      Unit
    | ERef e -> Ref (infer_expr env e) (* simplified *)
    | ERecordExtend (base_expr, fields) ->
      (* Infer type of the base record. *)
      let base_ty = infer_expr env base_expr in
      (* Infer types for the overriding / extending fields. *)
      let fields_env =
        List.fold fields ~init:Env.empty ~f:(fun acc (lbl, expr) ->
          let ty = infer_expr env expr in
          Env.add lbl ty acc)
      in
      let new_row = Row (fields_env, base_ty) in
      (* The base record must at least contain the fields we are overriding/extending.
    unify base_ty (Record new_row); *)
      let fields, tail = merge_fields (Record new_row) in
      (* The new row must be a superset of the base record. *)
      Record (Row (fields, tail))
    | EDeref e ->
      let ty = new_var !current_lvl in
      unify (Ref ty) (infer_expr env e);
      ty
    | ESetRef (lhs, rhs) ->
      let lhs_ty = infer_expr env lhs in
      let rhs_ty = infer_expr env rhs in
      unify (Ref (new_var !current_lvl)) lhs_ty;
      unify lhs_ty (Ref rhs_ty);
      Unit
    | EVariant (tag, exprs) ->
      (* Infer types for each argument of the variant constructor. *)
      let arg_tys = List.map exprs ~f:(infer_expr env) in
      let case_ty =
        match arg_tys with
        | [] -> Unit
        | [ t ] -> t
        | ts -> Tuple ts
      in
      let row_var = new_var !current_lvl in
      Variant (Row (Env.singleton tag case_ty, row_var))
    | EMatch (scrut, cases) ->
      (* Type of the scrutinee expression. *)
      let scrut_ty = infer_expr env scrut in
      (* Type of the overall match expression. *)
      let result_ty = new_var !current_lvl in
      (* Helper: infer pattern, returns env extended with bindings. *)
      let rec infer_pattern env pat expected_ty : tenv =
        match pat with
        | PWildcard -> env
        | PVar x ->
          env := Env.add x expected_ty !env;
          env
        | PInt _ ->
          unify expected_ty Number;
          env
        | PBool _ ->
          unify expected_ty Boolean;
          env
        | PFloat _ ->
          unify expected_ty Number;
          env
        | PString _ ->
          unify expected_ty String;
          env
        | PRecord (fields, is_open) ->
          (* Create fresh type variables for each subpattern and build row. *)
          let env_after, fields_acc =
            List.fold fields ~init:(env, []) ~f:(fun (env_acc, fields_acc) (lbl, pat) ->
              let ty = new_var !current_lvl in
              let env_acc' = infer_pattern env_acc pat ty in
              env_acc', (lbl, ty) :: fields_acc)
          in
          let fields_env = Env.of_list (List.rev fields_acc) in
          let tail_row = if is_open then new_var !current_lvl else Empty_row in
          let record_ty = Record (Row (fields_env, tail_row)) in
          unify expected_ty record_ty;
          env_after
        | PVariant (tag, subpats) ->
          (* Create fresh types for each subpattern, bind them. *)
          (* We need to process subpatterns left-to-right, threading the env. *)
          let env_after, sub_tys_rev =
            List.fold subpats ~init:(env, []) ~f:(fun (env_acc, tys) sp ->
              let ty = new_var !current_lvl in
              let env_acc' = infer_pattern env_acc sp ty in
              env_acc', ty :: tys)
          in
          let sub_tys = List.rev sub_tys_rev in
          let case_ty =
            match sub_tys with
            | [] -> Unit
            | [ t ] -> t
            | ts -> Tuple ts
          in
          let row_var = new_var !current_lvl in
          let variant_ty = Variant (Row (Env.singleton tag case_ty, row_var)) in
          unify expected_ty variant_ty;
          env_after
      in
      (* Iterate over each case. *)
      List.iter cases ~f:(fun (pat, rhs) ->
        (* Work with a fresh copy of the environment for each arm so that
         bindings are not shared across arms. *)
        let env_arm = ref !env in
        let _env_after_pat = infer_pattern env_arm pat scrut_ty in
        let rhs_ty = infer_expr env_arm rhs in
        unify rhs_ty result_ty);
      result_ty
  with
  | Type_error msg -> raise (Type_error_with_loc (msg, expr.span))
;;

(** --------------------------------------------------------------------- *)

(** 11. Inference – statements                                             *)

(** --------------------------------------------------------------------- *)

let rec infer_stmt env = function
  | SLet (x, e) ->
    enter_level ();
    let ty = infer_expr env e in
    exit_level ();
    add env x ty
  | SLetRec bindings ->
    (* Recursive bindings introduce a new level so that generalisation treats
       any type variables appearing only in the right-hand side of the bindings
       as generic. *)
    enter_level ();
    List.iter bindings ~f:(fun (nm, _) -> add env nm (new_var !current_lvl));
    List.iter bindings ~f:(fun (nm, rhs) -> unify (lookup env nm) (infer_expr env rhs));
    exit_level ()
  | SExpr e -> ignore (infer_expr env e)
  | SModule (mname, stmts) ->
    (* Create a fresh typing environment for the module that initially shares all
       the bindings from the outer scope (so the module can refer to them),
       but whose subsequent mutations are local to the module. *)
    let outer_snapshot = !env in
    let mod_env = ref outer_snapshot in
    (* A placeholder type for the module itself, so that the module body can
       refer to [mname] recursively (e.g.  when defining mutually-recursive
       modules or values that reference the toplevel module).  We allocate it
       at the current level so that it may later be generalised. *)
    let module_placeholder = new_var !current_lvl in
    (* Register the placeholder both in the outer env and in the module env
       before we start type-checking the module’s statements. *)
    env := Env.add mname module_placeholder !env;
    mod_env := Env.add mname module_placeholder !mod_env;
    (* Type-check all the statements inside the module using [mod_env]. *)
    List.iter stmts ~f:(fun stmt -> infer_stmt mod_env stmt.value);
    (* Collect all bindings that were introduced **inside** the module.  We do so
       by comparing the environment after the module body with the snapshot we
       took before entering the module.  Any key that is either new or has
       been rebound should be exported. *)
    let exports_list =
      Env.bindings !mod_env
      |> List.filter ~f:(fun (k, data) ->
        if String.equal k mname
        then false (* never export the module binding itself *)
        else (
          match Env.find outer_snapshot k with
          | None -> true (* brand-new binding *)
          | Some old -> not (phys_equal old data)))
    in
    let exports_map = Env.of_list exports_list in
    (* Turn the [exports_map] into a row type.  We *always* close the row when
       building a module type – modules expose a fixed set of fields. *)
    let row = Row (exports_map, Empty_row) in
    let module_typ = Record row in
    (* Unify the placeholder with the actual module type and update the
       binding in the outer environment with a *generalised* scheme. *)
    unify module_placeholder module_typ;
    (* Replace the (now unified) placeholder with the generalised module type *)
    env := Env.add mname (generalise module_typ) !env
  | SOpen mname ->
    (* Fetch the type of the module, check that it is indeed a record, then
       bring each of its fields into the current scope. *)
    (match lookup env mname with
     | Record row ->
       let fields, tail = merge_fields row in
       (match tail with
        | Empty_row | Var { contents = Free _ } -> () (* ok *)
        | _ -> ());
       Env.iter fields ~f:(fun field_name ty -> add env field_name ty)
     | _ -> raise (Type_error (Printf.sprintf "Cannot open non-module '%s'" mname)))
;;

(** --------------------------------------------------------------------- *)

(** 12. Entry point                                                        *)

(** --------------------------------------------------------------------- *)

let infer_program (prog : program) : unit =
  reset_levels ();
  let env = init_env () in
  (* Each element of [prog] now carries its own source span.  The current
     type-checker, however, does not yet make use of that information.  We
     therefore simply discard the annotation for the time being. *)
  try
    List.iter (fst prog) ~f:(fun stmt_node -> infer_stmt env stmt_node.value);
    (* If we reach here no exception was raised. *)
    printf "Type checking succeeded!\n%!"
  with
  | Type_error msg -> printf "Type error: %s\n%!" msg
  | Type_error_with_loc (msg, span) ->
    let source = Source.read (Source.make @@ snd prog) span in
    printf "line %i, characters %i-%i:" span.left.line span.left.column span.right.column;
    printf "\n%i|    %s" span.left.line source;
    printf "%s\n" (String.make (span.left.column + 3) ' ');
    printf "      %s\n\n" (String.make (span.right.column - span.left.column) '^');
    printf "Type error: %s" msg
;;
