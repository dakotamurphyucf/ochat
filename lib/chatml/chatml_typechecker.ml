(** Hindley–Milner type-checker for the ChatML language.

    {1 Overview}

    This module provides a self-contained Hindley–Milner (HM) type-checker
    for the {!module:Chatml.Chatml_lang} abstract syntax tree.  It performs
    full type inference with:

    • `let`-polymorphism with a value restriction,
    • row-polymorphic records and variants,
    • first-class functions, arrays and references,
    • built-in support for a small prelude of primitives (see {!init_env}).

    The checker is used in two modes:

    • a {e strict} mode via {!check_program}, which either returns a
      snapshot of the inferred principal types or a structured diagnostic;
    • a small compatibility/expect-test helper via {!infer_program}, which
      formats the same result for humans.

    A few implementation details are worth calling out because they shape
    the current ChatML semantics:

    • monomorphic bindings are {e not} instantiated on lookup, so helper
      functions preserve sharing between a parameter's projected fields and
      the value they eventually return;
    • lambda parameters whose type has been constrained by record field
      accesses are reopened to an open row before generalisation, allowing
      helpers such as state-transition functions to accept larger records
      than the subset of fields they touch;
    • record copy-update (`{ r with field = v }`) treats missing labels on
      an open base row as overrides of fields in the open tail instead of
      recursively extending the row, which keeps array-update patterns
      sound and ergonomic.

    The public surface is intentionally tiny:

    {ul
    {- {!check_program} validates a parsed program and returns either a
       checked snapshot or a diagnostic.}
    {- {!checked_lookup_span_type} queries that snapshot for the principal
       type inferred at a given {!Source.span}.}
    {- {!format_diagnostic} renders strict-checking failures in the human
       readable form used by tests and command-line tools.}
    {- {!infer_program} and {!type_lookup_for_program} remain as small
       convenience wrappers for tests/tooling.}}

    Everything else in this file exists to implement the inference engine
    itself and is **not** part of the stable API.
*)

open Core
open Chatml.Chatml_lang
module Builtin_spec = Chatml.Chatml_builtin_spec

(***************************************************************************)
(* A Hindley-Milner style type-checker for the ChatML language.            *)
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
  | TInt
  | TFloat
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

(** 3. Inference state and span tracking for the HM algorithm              *)

(** --------------------------------------------------------------------- *)

type span_key = int * int [@@deriving compare, sexp, hash]

let span_to_key (sp : Source.span) : span_key = sp.left.offset, sp.right.offset

module SpanTbl = Hashtbl.Make (struct
    type t = span_key [@@deriving compare, hash, sexp]
  end)

type infer_state =
  { mutable current_id : int
  ; mutable current_lvl : int
  ; mutable id_checkpoints : int list
  ; span_types : typ SpanTbl.t
  }

let create_state () : infer_state =
  { current_id = 0; current_lvl = 0; id_checkpoints = []; span_types = SpanTbl.create () }
;;

let enter_level (state : infer_state) =
  state.id_checkpoints <- state.current_id :: state.id_checkpoints;
  state.current_lvl <- state.current_lvl + 1
;;

let exit_level (state : infer_state) =
  match state.id_checkpoints with
  | previous_id :: tl ->
    state.current_id <- previous_id;
    state.current_lvl <- state.current_lvl - 1;
    state.id_checkpoints <- tl
  | [] -> failwith "Typechecker level stack underflow"
;;

let gensym (state : infer_state) : string =
  let id = state.current_id in
  state.current_id <- state.current_id + 1;
  let letter = Char.of_int_exn ((id mod 26) + Char.to_int 'a') |> Char.to_string in
  if id >= 26 then letter ^ Int.to_string (id / 26) else letter
;;

let record_span_type (state : infer_state) (span : Source.span) (ty : typ) : unit =
  Hashtbl.set state.span_types ~key:(span_to_key span) ~data:ty
;;

type diagnostic =
  { message : string
  ; span : Source.span option
  }

type checked_program = { span_types : typ SpanTbl.t }

let checked_lookup_span_type (checked : checked_program) (span : Source.span) : typ option
  =
  Hashtbl.find checked.span_types (span_to_key span)
;;

let format_diagnostic (source_text : string) (diagnostic : diagnostic) : string =
  match diagnostic.span with
  | None -> Printf.sprintf "Type error: %s" diagnostic.message
  | Some span ->
    let source = Source.read (Source.make source_text) span in
    let caret_count = Int.max 1 (span.right.column - span.left.column) in
    Printf.sprintf
      "line %i, characters %i-%i:\n%i|    %s%s\n      %s\n\nType error: %s"
      span.left.line
      span.left.column
      span.right.column
      span.left.line
      source
      (String.make (span.left.column + 3) ' ')
      (String.make caret_count '^')
      diagnostic.message
;;

let new_var (state : infer_state) level = Var (ref (Free (gensym state, level)))

(** --------------------------------------------------------------------- *)

(** 4. Generalisation / instantiation                                      *)

(** --------------------------------------------------------------------- *)

let instantiate (state : infer_state) ty =
  let table = Hashtbl.create (module String) in
  let rec inst = function
    | Fun (ps, r) -> Fun (List.map ps ~f:inst, inst r)
    | Generic x ->
      (match Hashtbl.find table x with
       | Some t -> t
       | None ->
         let v = new_var state state.current_lvl in
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
let generalise (state : infer_state) ty =
  let rec gen = function
    | Fun (params, ret) -> Fun (List.map params ~f:gen, gen ret)
    | Var { contents = Bound t } -> gen t
    | Var { contents = Free (name, lvl) } when lvl > state.current_lvl -> Generic name
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

let rec unify (state : infer_state) lhs rhs =
  if phys_equal lhs rhs
  then ()
  else (
    match lhs, rhs with
    | Fun (ps1, r1), Fun (ps2, r2) ->
      if List.length ps1 <> List.length ps2
      then raise (Type_error "Function arity mismatch")
      else (
        List.iter2_exn ps1 ps2 ~f:(unify state);
        unify state r1 r2)
    | Var { contents = Bound t1 }, t2 | t1, Var { contents = Bound t2 } ->
      unify state t1 t2
    | Var ({ contents = Free _ } as tv), t | t, Var ({ contents = Free _ } as tv) ->
      if occurs tv t then raise (Type_error "Recursive types") else tv := Bound t
    | Ref t1, Ref t2 | Array t1, Array t2 -> unify state t1 t2
    | Record r1, Record r2 | Variant r1, Variant r2 -> unify state r1 r2
    | Tuple ts1, Tuple ts2 ->
      if List.length ts1 <> List.length ts2
      then raise (Type_error "Tuple arity mismatch")
      else List.iter2_exn ts1 ts2 ~f:(unify state)
    | (Row _ as row1), (Row _ as row2) -> unify_rows state row1 row2
    | Row (fs, _), Empty_row | Empty_row, Row (fs, _) ->
      let lbl, _ = Env.choose fs in
      raise (Type_error (Printf.sprintf "Row does not contain label '%s'" lbl))
    | Number, Number
    | Number, TInt
    | TInt, Number
    | Number, TFloat
    | TFloat, Number
    | TInt, TInt
    | TFloat, TFloat
    | Boolean, Boolean
    | String, String
    | Empty_row, Empty_row
    | Unit, Unit -> ()
    | _ ->
      raise
        (Type_error
           (Printf.sprintf "Cannot unify %s with %s" (show_type lhs) (show_type rhs))))

and unify_rows (state : infer_state) lhs rhs =
  (* Convert both rows to a field-map plus a tail row var *)
  let map_l, tail_l = merge_fields lhs in
  let map_r, tail_r = merge_fields rhs in
  (* Unify common labels, collect missing ones *)
  let rec collect l r missing_l missing_r =
    match l, r with
    | (lbl_l, ty_l) :: tl, (lbl_r, ty_r) :: tr ->
      (match String.compare lbl_l lbl_r with
       | 0 ->
         unify state ty_l ty_r;
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
  | true, true -> unify state tail_l tail_r
  | true, false -> unify state tail_r (Row (missing_r, tail_l))
  | false, true -> unify state tail_l (Row (missing_l, tail_r))
  | false, false ->
    (match tail_l with
     | Var ({ contents = Free _ } as tv) ->
       let row_var = new_var state state.current_lvl in
       unify state tail_r (Row (missing_r, row_var));
       (* Ensure tv is still free, then bind. *)
       (match !tv with
        | Bound _ -> raise (Type_error "Recursive row types")
        | _ -> ());
       tv := Bound (Row (missing_l, row_var))
     | Empty_row -> unify state tail_l (Row (missing_l, new_var state 0))
     | _ -> assert false)

(** --------------------------------------------------------------------- *)
(** 8. Pretty printer for types (used in error messages)                   *)
(** --------------------------------------------------------------------- *)

(* Print human-readable type names.
   We purposefully differentiate the concrete machine types [int] and
   [float] from the polymorphic super-type [number] so that error
   messages such as “Cannot unify int with float” become actionable.
   Previously both [TInt] and [TFloat] were printed as "number",
   resulting in misleading diagnostics like “Cannot unify number with
   number”. *)

and show_type = function
  | TInt -> "int"
  | TFloat -> "float"
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
type tenv = scheme Env.t

(* Unlike a classical HM environment that stores only polymorphic schemes,
   ChatML's typing environment stores both:
   - generalized schemes (types containing [Generic] nodes), and
   - monomorphic shared types for lambda parameters / weak bindings.

   Preserving the latter by reference is important for record-heavy helper
   functions: if we instantiated every lookup we would lose the sharing that
   connects [st.tasks], [st.task_index] and the final [st] returned by the
   helper, causing accidental row narrowing. *)
let rec contains_generic (ty : typ) : bool =
  match ty with
  | Generic _ -> true
  | Fun (params, ret) -> List.exists params ~f:contains_generic || contains_generic ret
  | Var { contents = Bound t } -> contains_generic t
  | Var { contents = Free _ } -> false
  | Ref t | Record t | Variant t | Array t -> contains_generic t
  | Tuple ts -> List.exists ts ~f:contains_generic
  | Row (fields, tail) ->
    Env.exists fields (fun _field ty' -> contains_generic ty') || contains_generic tail
  | TInt | TFloat | Number | Boolean | String | Empty_row | Unit -> false
;;

let rec typ_of_builtin_ty (ty : Builtin_spec.ty) : typ =
  match ty with
  | Builtin_spec.TVar name -> Generic name
  | Builtin_spec.TInt -> TInt
  | Builtin_spec.TFloat -> TFloat
  | Builtin_spec.TNumber -> Number
  | Builtin_spec.TBool -> Boolean
  | Builtin_spec.TString -> String
  | Builtin_spec.TUnit -> Unit
  | Builtin_spec.TArray inner -> Array (typ_of_builtin_ty inner)
  | Builtin_spec.TFun (params, ret) ->
    Fun (List.map params ~f:typ_of_builtin_ty, typ_of_builtin_ty ret)
;;

let init_env () : tenv =
  Builtin_spec.builtins
  |> List.map ~f:(fun builtin -> builtin.name, typ_of_builtin_ty builtin.scheme)
  |> Env.of_list
;;

(* Instantiate only genuinely polymorphic schemes.  Monomorphic bindings are
   returned as-is so that all uses share the same mutable inference variables. *)
let lookup (state : infer_state) (env : tenv) x =
  match Env.find env x with
  | Some sc -> if contains_generic sc then instantiate state sc else sc
  | None -> raise (Type_error (Printf.sprintf "Unknown variable '%s'" x))
;;

let add_mono (env : tenv) x ty : tenv = Env.add x ty env

let add_generalized (state : infer_state) (env : tenv) x ty : tenv =
  Env.add x (generalise state ty) env
;;

let with_new_level (state : infer_state) ~(f : unit -> 'a) : 'a =
  enter_level state;
  Exn.protect ~f ~finally:(fun () -> exit_level state)
;;

let rec is_non_expansive (expr : expr) : bool =
  match expr with
  | EUnit
  | EInt _
  | EFloat _
  | EBool _
  | EString _
  | EVar _
  | EVarLoc _
  | ELambda _
  | ELambdaSlots _ -> true
  | ERecord fields ->
    List.for_all fields ~f:(fun (_lbl, expr_node) -> is_non_expansive expr_node.value)
  | EVariant (_tag, exprs) ->
    List.for_all exprs ~f:(fun expr_node -> is_non_expansive expr_node.value)
  | EApp _
  | EIf _
  | EWhile _
  | ELetIn _
  | ELetRec _
  | ELetBlock _
  | ELetBlockSlots _
  | ELetRecSlots _
  | EMatch _
  | EMatchSlots _
  | EFieldGet _
  | EArray _
  | EArrayGet _
  | EArraySet _
  | ERef _
  | ESetRef _
  | ESequence _
  | EDeref _
  | ERecordExtend _ -> false
;;

let rec restrict_free_vars_to_level (max_level : int) (ty : typ) : unit =
  match ty with
  | Fun (params, ret) ->
    List.iter params ~f:(restrict_free_vars_to_level max_level);
    restrict_free_vars_to_level max_level ret
  | Generic _ | TInt | TFloat | Number | Boolean | String | Empty_row | Unit -> ()
  | Var { contents = Bound t } -> restrict_free_vars_to_level max_level t
  | Var ({ contents = Free (name, lvl) } as tv) ->
    if lvl > max_level then tv := Free (name, max_level)
  | Ref t | Record t | Variant t | Array t -> restrict_free_vars_to_level max_level t
  | Tuple ts -> List.iter ts ~f:(restrict_free_vars_to_level max_level)
  | Row (fields, tail) ->
    Env.iter fields ~f:(fun _field ty' -> restrict_free_vars_to_level max_level ty');
    restrict_free_vars_to_level max_level tail
;;

let rec infer_nonrecursive_binding
          (state : infer_state)
          (env : tenv)
          (name : string)
          (rhs : expr node)
  : tenv
  =
  let rhs_ty = with_new_level state ~f:(fun () -> infer_expr state env rhs) in
  if is_non_expansive rhs.value
  then add_generalized state env name rhs_ty
  else (
    restrict_free_vars_to_level state.current_lvl rhs_ty;
    add_mono env name rhs_ty)

and infer_recursive_bindings
      (state : infer_state)
      (env : tenv)
      (bindings : (string * expr node) list)
  : tenv
  =
  let binding_types =
    with_new_level state ~f:(fun () ->
      let env_with_placeholders =
        List.fold bindings ~init:env ~f:(fun env_acc (nm, _) ->
          add_mono env_acc nm (new_var state state.current_lvl))
      in
      List.iter bindings ~f:(fun (nm, rhs) ->
        let placeholder_ty = Env.find_exn env_with_placeholders nm in
        let rhs_ty = infer_expr state env_with_placeholders rhs in
        unify state placeholder_ty rhs_ty);
      List.map bindings ~f:(fun (nm, _) -> nm, Env.find_exn env_with_placeholders nm))
  in
  List.fold binding_types ~init:env ~f:(fun env_acc (nm, ty) ->
    add_generalized state env_acc nm ty)

and infer_record_extend
      (state : infer_state)
      (env : tenv)
      (base_expr : expr node)
      (fields : (string * expr node) list)
  : typ
  =
  let base_ty = infer_expr state env base_expr in
  let base_row = new_var state state.current_lvl in
  unify state base_ty (Record base_row);
  let base_fields, base_tail = merge_fields base_row in
  let override_fields =
    List.fold fields ~init:Env.empty ~f:(fun acc (lbl, expr) ->
      let ty = infer_expr state env expr in
      Env.add lbl ty acc)
  in
  let missing_override_labels =
    Env.fold override_fields ~init:[] ~f:(fun ~key ~data:_ acc ->
      match Env.find base_fields key with
      | Some _ -> acc
      | None -> key :: acc)
  in
  let result_tail =
    match List.rev missing_override_labels, base_tail with
    | [], _ -> base_tail
    | _missing, Empty_row ->
      (* Closed records may genuinely gain new fields via copy-update. *)
      Empty_row
    | missing, _ ->
      (* For an open base row, interpret updates on labels not yet
         materialised in [base_fields] as overriding fields living in
         the open tail rather than recursively extending the row.  This
         avoids recursive equations when the updated record later flows
         back into the original type, e.g. when writing an updated
         array element back into the same array. *)
      let old_field_types =
        List.fold missing ~init:Env.empty ~f:(fun acc lbl ->
          Env.add lbl (new_var state state.current_lvl) acc)
      in
      let fresh_tail = new_var state state.current_lvl in
      unify state base_tail (Row (old_field_types, fresh_tail));
      fresh_tail
  in
  let result_fields =
    Env.fold override_fields ~init:base_fields ~f:(fun ~key ~data acc ->
      Env.add key data acc)
  in
  Record (Row (result_fields, result_tail))

and validate_pattern_binders (pat : pattern) : unit =
  let seen = Hash_set.create (module String) in
  let duplicate =
    collect_pattern_vars pat
    |> List.find ~f:(fun name ->
      if Hash_set.mem seen name
      then true
      else (
        Hash_set.add seen name;
        false))
  in
  match duplicate with
  | None -> ()
  | Some name -> raise (Type_error (Printf.sprintf "Duplicate pattern binder '%s'" name))

and is_catch_all_pattern (pat : pattern) : bool =
  match pat with
  | PWildcard | PVar _ -> true
  | _ -> false

and record_field_type (row_ty : typ) (label : string) : typ option =
  match resolve_bound_type row_ty with
  | Record row -> record_field_type row label
  | Row (fields, tail) ->
    (match Env.find fields label with
     | Some ty -> Some ty
     | None -> record_field_type tail label)
  | _ -> None

and is_closed_record_row (row_ty : typ) : bool =
  match resolve_bound_type row_ty with
  | Record row -> is_closed_record_row row
  | Row (_fields, tail) -> is_closed_record_row tail
  | Empty_row -> true
  | _ -> false

and payload_component_types (payload_ty : typ) : typ list =
  match resolve_bound_type payload_ty with
  | Unit -> []
  | Tuple ts -> ts
  | ty -> [ ty ]

and show_missing_variant_case (tag : string) (payload_ty : typ) : string =
  match payload_component_types payload_ty with
  | [] -> Printf.sprintf "`%s" tag
  | components ->
    let placeholders = List.map components ~f:(fun _ -> "_") |> String.concat ~sep:", " in
    Printf.sprintf "`%s(%s)" tag placeholders

and pattern_totally_covers_type (pat : pattern) (expected_ty : typ) : bool =
  match pat with
  | PWildcard | PVar _ -> true
  | PRecord (fields, is_open) ->
    let subpatterns_cover_all =
      List.for_all fields ~f:(fun (label, subpat) ->
        match record_field_type expected_ty label with
        | Some field_ty -> pattern_totally_covers_type subpat field_ty
        | None -> false)
    in
    if not subpatterns_cover_all
    then false
    else if is_open
    then true
    else (
      match resolve_bound_type expected_ty with
      | Record row ->
        let row_fields, _tail = merge_fields row in
        let field_names = List.map fields ~f:fst |> String.Set.of_list in
        Set.equal field_names (Env.bindings row_fields |> List.map ~f:fst |> String.Set.of_list)
        && is_closed_record_row row
      | _ -> false)
  | _ -> false

and variant_constructor_info
      (variant_ty : typ)
  : ((string * typ) list * typ) option
  =
  match resolve_bound_type variant_ty with
  | Variant row ->
    let fields, tail = merge_fields row in
    Some (Env.bindings fields, tail)
  | _ -> None

and pattern_fully_covers_variant_case (pat : pattern) ~(tag : string) ~(payload_ty : typ) : bool =
  match pat with
  | PVariant (tag', subpats) when String.equal tag tag' ->
    let payload_tys = payload_component_types payload_ty in
    List.length subpats = List.length payload_tys
    && List.for_all2_exn subpats payload_tys ~f:pattern_totally_covers_type
  | _ -> false

and is_closed_variant_tail (tail_ty : typ) : bool =
  match resolve_bound_type tail_ty with
  | Empty_row -> true
  | _ -> false

and show_pattern_brief (pat : pattern) : string =
  match pat with
  | PWildcard -> "_"
  | PVar x -> x
  | PInt i -> Int.to_string i
  | PBool b -> Bool.to_string b
  | PFloat f -> Float.to_string f
  | PString s -> Printf.sprintf "%S" s
  | PVariant (tag, []) -> Printf.sprintf "`%s" tag
  | PVariant (tag, _subpats) -> Printf.sprintf "`%s(...)" tag
  | PRecord _ -> "{...}"

and simple_pattern_key_of_pattern (pat : pattern) : (string * string) option =
  match pat with
  | PInt i -> Some ("int:" ^ Int.to_string i, Int.to_string i)
  | PBool b -> Some ("bool:" ^ Bool.to_string b, Bool.to_string b)
  | PFloat f ->
    let rendered = Float.to_string f in
    Some ("float:" ^ rendered, rendered)
  | PString s -> Some ("string:" ^ s, Printf.sprintf "%S" s)
  | PVariant (tag, []) -> Some ("variant:" ^ tag, Printf.sprintf "`%s" tag)
  | _ -> None

and validate_match_case_shapes (patterns : pattern list) : unit =
  let seen_simple_patterns = Hash_set.create (module String) in
  let rec loop catch_all_pat pats =
    match pats with
    | [] -> ()
    | pat :: tl ->
      (match catch_all_pat with
       | Some prev_pat ->
         raise
           (Type_error
              (Printf.sprintf
                 "Redundant match arm '%s': previous catch-all pattern '%s' already \
                  matches all cases"
                 (show_pattern_brief pat)
                 (show_pattern_brief prev_pat)))
       | None -> ());
      (match simple_pattern_key_of_pattern pat with
       | Some (key, display) ->
         if Hash_set.mem seen_simple_patterns key
         then
           raise
             (Type_error
                (Printf.sprintf "Duplicate match arm for pattern '%s'" display))
         else Hash_set.add seen_simple_patterns key
       | None -> ());
      let next_catch_all_pat = if is_catch_all_pattern pat then Some pat else None in
      loop next_catch_all_pat tl
  in
  loop None patterns

and validate_boolean_match_exhaustiveness (scrut_ty : typ) (patterns : pattern list) : unit =
  match resolve_bound_type scrut_ty with
  | Boolean ->
    let has_catch_all = List.exists patterns ~f:is_catch_all_pattern in
    if not has_catch_all
    then (
      let seen_true = List.exists patterns ~f:(function PBool true -> true | _ -> false) in
      let seen_false = List.exists patterns ~f:(function PBool false -> true | _ -> false) in
      (match seen_true, seen_false with
       | true, true -> ()
       | true, false ->
         raise (Type_error "Non-exhaustive boolean match: missing case 'false'")
       | false, true ->
         raise (Type_error "Non-exhaustive boolean match: missing case 'true'")
       | false, false ->
         raise
           (Type_error
              "Non-exhaustive boolean match: missing cases 'true' and 'false'")))
  | _ -> ()

and validate_typed_match_redundancy (scrut_ty : typ) (patterns : pattern list) : unit =
  match resolve_bound_type scrut_ty with
  | Boolean ->
    let seen_true = ref false in
    let seen_false = ref false in
    List.iter patterns ~f:(fun pat ->
      if is_catch_all_pattern pat && !seen_true && !seen_false
      then
        raise
          (Type_error
             (Printf.sprintf
                "Redundant match arm '%s': previous arms already cover boolean cases 'true' \
                 and 'false'"
                (show_pattern_brief pat)));
      (match pat with
       | PBool true -> seen_true := true
       | PBool false -> seen_false := true
       | _ -> ()))
  | Variant _ ->
    (match variant_constructor_info scrut_ty with
     | None -> ()
     | Some (constructors, tail) ->
       let covered_tags = Hash_set.create (module String) in
       let payload_by_tag =
         String.Map.of_alist_exn constructors
       in
       let all_known_tags_covered () =
         List.for_all constructors ~f:(fun (tag, _payload_ty) -> Hash_set.mem covered_tags tag)
       in
       let closed_tail = is_closed_variant_tail tail in
       List.iter patterns ~f:(fun pat ->
         (match pat with
          | PVariant (tag, _subpats) when Hash_set.mem covered_tags tag ->
            let covered_payload = Map.find_exn payload_by_tag tag in
            raise
              (Type_error
                 (Printf.sprintf
                    "Redundant match arm '%s': previous arms already cover variant case '%s'"
                    (show_pattern_brief pat)
                    (show_missing_variant_case tag covered_payload)))
          | _ when is_catch_all_pattern pat && closed_tail && all_known_tags_covered () ->
            raise
              (Type_error
                 (Printf.sprintf
                    "Redundant match arm '%s': previous arms already cover all variant constructors"
                    (show_pattern_brief pat)))
          | _ -> ());
         List.iter constructors ~f:(fun (tag, payload_ty) ->
           if pattern_fully_covers_variant_case pat ~tag ~payload_ty
           then Hash_set.add covered_tags tag)))
  | _ -> ()

and validate_match_exhaustiveness (state : infer_state) (scrut_ty : typ) (patterns : pattern list)
  : unit
  =
  if List.exists patterns ~f:is_catch_all_pattern
  then ()
  else (
    match resolve_bound_type scrut_ty with
    | Boolean -> validate_boolean_match_exhaustiveness scrut_ty patterns
    | Variant _ ->
      (match variant_constructor_info scrut_ty with
       | None -> ()
       | Some (constructors, tail) ->
         let missing_constructor =
           List.find constructors ~f:(fun (tag, payload_ty) ->
             not
               (List.exists patterns ~f:(fun pat ->
                  pattern_fully_covers_variant_case pat ~tag ~payload_ty)))
         in
         (match missing_constructor with
          | Some (tag, payload_ty) ->
            raise
              (Type_error
                 (Printf.sprintf
                    "Non-exhaustive variant match: missing case '%s'"
                    (show_missing_variant_case tag payload_ty)))
          | None -> unify state tail Empty_row))
    | TInt -> raise (Type_error "Non-exhaustive match on int: add '_' arm")
    | TFloat -> raise (Type_error "Non-exhaustive match on float: add '_' arm")
    | Number -> raise (Type_error "Non-exhaustive match on number: add '_' arm")
    | String -> raise (Type_error "Non-exhaustive match on string: add '_' arm")
    | Record _ ->
      if List.exists patterns ~f:(fun pat -> pattern_totally_covers_type pat scrut_ty)
      then ()
      else raise (Type_error "Non-exhaustive match on record: add '_' arm")
    | _ -> raise (Type_error "Non-exhaustive match: add '_' arm"))

and infer_pattern (state : infer_state) (env : tenv) (pat : pattern) (expected_ty : typ)
  : tenv
  =
  match pat with
  | PWildcard -> env
  | PVar x -> add_mono env x expected_ty
  | PInt _ ->
    unify state expected_ty TInt;
    env
  | PBool _ ->
    unify state expected_ty Boolean;
    env
  | PFloat _ ->
    unify state expected_ty TFloat;
    env
  | PString _ ->
    unify state expected_ty String;
    env
  | PRecord (fields, is_open) ->
    let env_after, fields_acc =
      List.fold fields ~init:(env, []) ~f:(fun (env_acc, fields_acc) (lbl, pat) ->
        let ty = new_var state state.current_lvl in
        let env_acc' = infer_pattern state env_acc pat ty in
        env_acc', (lbl, ty) :: fields_acc)
    in
    let fields_env = Env.of_list (List.rev fields_acc) in
    let tail_row = if is_open then new_var state state.current_lvl else Empty_row in
    let record_ty = Record (Row (fields_env, tail_row)) in
    unify state expected_ty record_ty;
    env_after
  | PVariant (tag, subpats) ->
    let env_after, sub_tys_rev =
      List.fold subpats ~init:(env, []) ~f:(fun (env_acc, tys) sp ->
        let ty = new_var state state.current_lvl in
        let env_acc' = infer_pattern state env_acc sp ty in
        env_acc', ty :: tys)
    in
    let sub_tys = List.rev sub_tys_rev in
    let case_ty =
      match sub_tys with
      | [] -> Unit
      | [ t ] -> t
      | ts -> Tuple ts
    in
    let row_var = new_var state state.current_lvl in
    let variant_ty = Variant (Row (Env.singleton tag case_ty, row_var)) in
    unify state expected_ty variant_ty;
    env_after

and reopen_lambda_param_if_closed_record (state : infer_state) (ty : typ) : typ =
  match ty with
  | Var ({ contents = Bound bound_ty } as tv) ->
    let reopened = reopen_lambda_param_if_closed_record state bound_ty in
    tv := Bound reopened;
    ty
  | Record row -> Record (reopen_lambda_row state row)
  | _ -> ty

and resolve_bound_type (ty : typ) : typ =
  match ty with
  | Var { contents = Bound bound_ty } -> resolve_bound_type bound_ty
  | _ -> ty

and reopen_lambda_row (state : infer_state) (row : typ) : typ =
  match resolve_bound_type row with
  | Row _ as full_row ->
    let merged_fields, _merged_tail = merge_fields full_row in
    (* Lambda parameters should be row-polymorphic by default: if a helper
       touches only a subset of record fields, callers should be able to pass
       larger records.  We therefore rebuild the parameter row with a fresh
       open tail after inference has discovered the required fields.  This is
       intentionally biased toward the "state-machine helper" use-case that
       ChatML scripts rely on heavily. *)
    Row (merged_fields, new_var state state.current_lvl)
  | other -> other

(** --------------------------------------------------------------------- *)
(** 10. Inference – expressions                                            *)
(** --------------------------------------------------------------------- *)

and infer_expr (state : infer_state) (env : tenv) expr =
  (* We first perform the usual inference work, then — if it succeeds — we
     record the resulting type in [state.span_types].  The resolver consults this
     table to choose an appropriate slot descriptor. *)
  let result_ty =
    try
      match expr.value with
      | EUnit -> Unit
      | EInt _ -> TInt
      | EFloat _ -> TFloat
      | EBool _ -> Boolean
      | EString _ -> String
      | EVar x -> lookup state env x
      | EVarLoc _ -> failwith "EVarLoc should not appear before resolver"
      | ELambda (params, body) ->
        with_new_level state ~f:(fun () ->
          let env_with_params, param_tys_rev =
            List.fold params ~init:(env, []) ~f:(fun (env_acc, tys_rev) p ->
              let t = new_var state state.current_lvl in
              add_mono env_acc p t, t :: tys_rev)
          in
          let body_ty = infer_expr state env_with_params body in
          let param_tys =
            List.rev param_tys_rev
            |> List.map ~f:(reopen_lambda_param_if_closed_record state)
          in
          Fun (param_tys, body_ty))
      | ELambdaSlots (params, _slots, body) ->
        with_new_level state ~f:(fun () ->
          let env_with_params, param_tys_rev =
            List.fold params ~init:(env, []) ~f:(fun (env_acc, tys_rev) p ->
              let t = new_var state state.current_lvl in
              add_mono env_acc p t, t :: tys_rev)
          in
          let body_ty = infer_expr state env_with_params body in
          let param_tys =
            List.rev param_tys_rev
            |> List.map ~f:(reopen_lambda_param_if_closed_record state)
          in
          Fun (param_tys, body_ty))
      | EApp (fn_expr, arg_exprs) ->
        let fn_ty = infer_expr state env fn_expr in
        let arg_tys = List.map arg_exprs ~f:(fun arg -> infer_expr state env arg) in
        let ret_ty = new_var state state.current_lvl in
        unify state fn_ty (Fun (arg_tys, ret_ty));
        ret_ty
      | EIf (c, t, e) ->
        let c_ty = infer_expr state env c in
        unify state c_ty Boolean;
        let t_ty = infer_expr state env t in
        let e_ty = infer_expr state env e in
        unify state t_ty e_ty;
        t_ty
      | EWhile (cond, body) ->
        unify state (infer_expr state env cond) Boolean;
        ignore (infer_expr state env body);
        Unit
      | ESequence (e1, e2) ->
        ignore (infer_expr state env e1);
        infer_expr state env e2
      | ELetIn (x, rhs, body) ->
        let env' = infer_nonrecursive_binding state env x rhs in
        infer_expr state env' body
      | ELetBlock (bindings, body) ->
        let block_env =
          List.fold bindings ~init:env ~f:(fun env_acc (nm, rhs) ->
            infer_nonrecursive_binding state env_acc nm rhs)
        in
        infer_expr state block_env body
      | ELetBlockSlots (bindings, _slots, body) ->
        let block_env =
          List.fold bindings ~init:env ~f:(fun env_acc (nm, rhs) ->
            infer_nonrecursive_binding state env_acc nm rhs)
        in
        infer_expr state block_env body
      | ELetRec (bindings, body) ->
        let env_with_bindings = infer_recursive_bindings state env bindings in
        infer_expr state env_with_bindings body
      | ELetRecSlots (bindings, _slots, body) ->
        let env_with_bindings = infer_recursive_bindings state env bindings in
        infer_expr state env_with_bindings body
      | ERecord fields ->
        let row =
          List.fold_right fields ~init:Empty_row ~f:(fun (lbl, expr) acc ->
            let ty = infer_expr state env expr in
            Row (Env.singleton lbl ty, acc))
        in
        Record row
      | EFieldGet (obj, lbl) ->
        let obj_ty = infer_expr state env obj in
        let field_ty = new_var state state.current_lvl in
        let tail_row = new_var state state.current_lvl in
        unify state obj_ty (Record (Row (Env.singleton lbl field_ty, tail_row)));
        field_ty
      | EArray elts ->
        let elt_ty = new_var state state.current_lvl in
        List.iter elts ~f:(fun e -> unify state (infer_expr state env e) elt_ty);
        Array elt_ty
      | EArrayGet (arr, idx) ->
        unify state (infer_expr state env idx) TInt;
        let elt_ty = new_var state state.current_lvl in
        unify state (infer_expr state env arr) (Array elt_ty);
        elt_ty
      | EArraySet (arr, idx, v) ->
        unify state (infer_expr state env idx) TInt;
        let v_ty = infer_expr state env v in
        unify state (infer_expr state env arr) (Array v_ty);
        Unit
      | ERef e -> Ref (infer_expr state env e) (* simplified *)
      | ERecordExtend (base_expr, fields) ->
        infer_record_extend state env base_expr fields
      | EDeref e ->
        let ty = new_var state state.current_lvl in
        unify state (Ref ty) (infer_expr state env e);
        ty
      | ESetRef (lhs, rhs) ->
        let lhs_ty = infer_expr state env lhs in
        let rhs_ty = infer_expr state env rhs in
        unify state (Ref (new_var state state.current_lvl)) lhs_ty;
        unify state lhs_ty (Ref rhs_ty);
        Unit
      | EVariant (tag, exprs) ->
        let arg_tys = List.map exprs ~f:(fun ex -> infer_expr state env ex) in
        let case_ty =
          match arg_tys with
          | [] -> Unit
          | [ t ] -> t
          | ts -> Tuple ts
        in
        let row_var = new_var state state.current_lvl in
        Variant (Row (Env.singleton tag case_ty, row_var))
      | EMatch (scrut, cases) ->
        let scrut_ty = infer_expr state env scrut in
        let result_ty = new_var state state.current_lvl in
        validate_match_case_shapes (List.map cases ~f:fst);
        List.iter cases ~f:(fun (pat, rhs) ->
          validate_pattern_binders pat;
          let env_arm = infer_pattern state env pat scrut_ty in
          let rhs_ty = infer_expr state env_arm rhs in
          unify state rhs_ty result_ty);
        validate_match_exhaustiveness state scrut_ty (List.map cases ~f:fst);
        validate_typed_match_redundancy scrut_ty (List.map cases ~f:fst);
        result_ty
      | EMatchSlots (scrut, cases) ->
        let scrut_ty = infer_expr state env scrut in
        let result_ty = new_var state state.current_lvl in
        validate_match_case_shapes (List.map cases ~f:(fun (pat, _slots, _rhs) -> pat));
        List.iter cases ~f:(fun (pat, _slots, rhs) ->
          validate_pattern_binders pat;
          let env_arm = infer_pattern state env pat scrut_ty in
          let rhs_ty = infer_expr state env_arm rhs in
          unify state rhs_ty result_ty);
        validate_match_exhaustiveness
          state
          scrut_ty
          (List.map cases ~f:(fun (pat, _slots, _rhs) -> pat));
        validate_typed_match_redundancy
          scrut_ty
          (List.map cases ~f:(fun (pat, _slots, _rhs) -> pat));
        result_ty
    with
    | Type_error msg -> raise (Type_error_with_loc (msg, expr.span))
  in
  record_span_type state expr.span result_ty;
  result_ty
;;

(** --------------------------------------------------------------------- *)

(** 11. Inference – statements                                             *)

(** --------------------------------------------------------------------- *)

(* [infer_stmt_with_exports] mirrors the runtime's module semantics:
   statements still mutate the local typing environment in-order, but only
   explicit definitions contribute to a module's exported surface.
   In particular, [open M] imports names for subsequent statements yet does
   not re-export them from the enclosing module. *)
let rec infer_stmt_with_exports (state : infer_state) (env : tenv) (stmt : stmt)
  : tenv * string list
  =
  match stmt with
  | SLet (x, e) -> infer_nonrecursive_binding state env x e, [ x ]
  | SLetRec bindings ->
    infer_recursive_bindings state env bindings, List.map bindings ~f:fst
  | SExpr e ->
    ignore (infer_expr state env e);
    env, []
  | SModule (mname, stmts) ->
    let module_placeholder = new_var state state.current_lvl in
    let mod_env_start = add_mono env mname module_placeholder in
    let mod_env_final, exported_names_rev =
      List.fold stmts ~init:(mod_env_start, []) ~f:(fun (env_acc, names_rev) stmt_node ->
        let env_next, stmt_exports =
          infer_stmt_with_exports state env_acc stmt_node.value
        in
        env_next, List.rev_append stmt_exports names_rev)
    in
    let exported_names = List.rev exported_names_rev in
    let seen = Hash_set.create (module String) in
    let exports_map =
      List.fold exported_names ~init:Env.empty ~f:(fun acc name ->
        if String.equal name mname || Hash_set.mem seen name
        then acc
        else (
          Hash_set.add seen name;
          match Env.find mod_env_final name with
          | Some ty -> Env.add name ty acc
          | None -> acc))
    in
    let module_typ = Record (Row (exports_map, Empty_row)) in
    unify state module_placeholder module_typ;
    add_generalized state env mname module_typ, [ mname ]
  | SOpen mname ->
    (match lookup state env mname with
     | Record row ->
       let fields, tail = merge_fields row in
       (match tail with
        | Empty_row | Var { contents = Free _ } -> ()
        | _ -> ());
       ( Env.fold fields ~init:env ~f:(fun ~key ~data acc ->
           add_generalized state acc key data)
       , [] )
     | _ -> raise (Type_error (Printf.sprintf "Cannot open non-module '%s'" mname)))
;;

let infer_stmt (state : infer_state) (env : tenv) (stmt : stmt) : tenv =
  fst (infer_stmt_with_exports state env stmt)
;;

(** --------------------------------------------------------------------- *)

(** 12. Entry point                                                        *)

(** --------------------------------------------------------------------- *)

(** [check_program prog] runs strict HM inference on [prog].

    The function allocates a fresh inference state, infers every top-level
    statement in order, and returns either:

    - [Ok checked], where [checked] contains an immutable snapshot of the
      principal types recorded for each source span; or
    - [Error diagnostic], where [diagnostic] identifies the first typing
      error and never permits evaluation to proceed.

    The snapshot produced on success is the only source of type information
    consulted by the resolver in production. *)
let check_program (prog : program) : (checked_program, diagnostic) result =
  let state = create_state () in
  let env = init_env () in
  (* Each element of [prog] now carries its own source span.  The current
     type-checker, however, does not yet make use of that information.  We
     therefore simply discard the annotation for the time being. *)
  try
    let _final_env =
      List.fold (fst prog) ~init:env ~f:(fun env_acc stmt_node ->
        infer_stmt state env_acc stmt_node.value)
    in
    Ok { span_types = Hashtbl.copy state.span_types }
  with
  | Type_error msg -> Error { message = msg; span = None }
  | Type_error_with_loc (msg, span) -> Error { message = msg; span = Some span }
;;

(* Compatibility helper used by expect tests and ad-hoc debugging.  Strict
   callers should prefer [check_program]. *)
let infer_program (prog : program) : unit =
  match check_program prog with
  | Ok _ -> printf "Type checking succeeded!\n%!"
  | Error diagnostic -> printf "%s\n%!" (format_diagnostic (snd prog) diagnostic)
;;

(*************************************************************************** *)
(*  Public helper – produce a lookup function for a whole program          *)
(*************************************************************************** *)

(** [type_lookup_for_program prog] is a small convenience wrapper around
    {!check_program}.  On success it returns a span lookup closure backed by
    the checked snapshot; on failure it returns a closure that always yields
    [None].  This is suitable for tooling/tests that want best-effort access
    to inferred types without running the program. *)
let type_lookup_for_program (prog : program) : Source.span -> typ option =
  match check_program prog with
  | Ok checked -> checked_lookup_span_type checked
  | Error _ -> fun _span -> None
;;
