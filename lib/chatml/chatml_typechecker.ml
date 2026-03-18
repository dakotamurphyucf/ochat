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
    • record copy-update (`{ r with field = v }`) preserves the base row's
      open tail and layers updated fields in front, so helpers can both
      override existing fields and add new ones while still accepting
      larger records.

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
  | Mu of name * typ
  | Rec_var of name
  | Ref of typ
  | Record of typ (* row *)
  | Variant of typ (* not implemented – kept for parity *)
  | Tuple of typ list
  | Array of typ
  | TInt
  | TFloat
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

exception Type_error of string
exception Type_error_with_loc of string * Source.span

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

let tv_ref_mem (needle : tv ref) (haystack : tv ref list) : bool =
  List.exists haystack ~f:(fun candidate -> phys_equal needle candidate)
;;

let rec_name_mem (needle : name) (haystack : name list) : bool =
  List.exists haystack ~f:(String.equal needle)
;;

let rec substitute_rec_var ~(target : name) ~(replacement : typ) (ty : typ) : typ =
  match ty with
  | Fun (params, ret) ->
    Fun
      ( List.map params ~f:(substitute_rec_var ~target ~replacement)
      , substitute_rec_var ~target ~replacement ret )
  | Generic _ | Var _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> ty
  | Mu (binder, body) when String.equal binder target -> Mu (binder, body)
  | Mu (binder, body) -> Mu (binder, substitute_rec_var ~target ~replacement body)
  | Rec_var binder when String.equal binder target -> replacement
  | Rec_var _ -> ty
  | Ref t -> Ref (substitute_rec_var ~target ~replacement t)
  | Record row -> Record (substitute_rec_var ~target ~replacement row)
  | Variant row -> Variant (substitute_rec_var ~target ~replacement row)
  | Tuple ts -> Tuple (List.map ts ~f:(substitute_rec_var ~target ~replacement))
  | Array t -> Array (substitute_rec_var ~target ~replacement t)
  | Row (fields, tail) ->
    Row
      ( Env.map fields ~f:(substitute_rec_var ~target ~replacement)
      , substitute_rec_var ~target ~replacement tail )
;;

let unfold_mu (ty : typ) : typ =
  match ty with
  | Mu (binder, body) as mu -> substitute_rec_var ~target:binder ~replacement:mu body
  | other -> other
;;

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
    | Mu (binder, body) -> Mu (binder, inst body)
    | Rec_var _ as rec_var -> rec_var
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
    | Mu (binder, body) -> Mu (binder, gen body)
    | Rec_var _ as rec_var -> rec_var
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

let has_recursive_type (ty : typ) : bool =
  let rec aux (seen : tv ref list) = function
    | Fun (params, ret) -> List.exists params ~f:(aux seen) || aux seen ret
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then true else aux (tv :: seen) t
    | Mu (_binder, _body) -> true
    | Rec_var _ -> true
    | Var { contents = Free _ }
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> false
    | Ref t | Record t | Variant t | Array t -> aux seen t
    | Tuple ts -> List.exists ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.exists fields (fun _field ty' -> aux seen ty') || aux seen tail
  in
  aux [] ty
;;

let check_contractive (ty : typ) : (unit, string) result =
  let rec check_list
            (seen : tv ref list)
            ~(target : name option)
            ~(guarded : bool)
            ~(scope : name list)
            (tys : typ list)
    : (unit, string) result
    =
    match tys with
    | [] -> Ok ()
    | hd :: tl ->
      (match check_ty seen ~target ~guarded ~scope hd with
       | Ok () -> check_list seen ~target ~guarded ~scope tl
       | Error _ as err -> err)
  and check_fields
        (seen : tv ref list)
        ~(target : name option)
        ~(guarded : bool)
        ~(scope : name list)
        (fields : typ Env.t)
    : (unit, string) result
    =
    fields |> Env.bindings |> List.map ~f:snd |> check_list seen ~target ~guarded ~scope
  and check_ty
        (seen : tv ref list)
        ~(target : name option)
        ~(guarded : bool)
        ~(scope : name list)
        (ty : typ)
    : (unit, string) result
    =
    match ty with
    | Fun (params, ret) ->
      (match check_list seen ~target ~guarded:true ~scope params with
       | Ok () -> check_ty seen ~target ~guarded:true ~scope ret
       | Error _ as err -> err)
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen
      then Ok ()
      else check_ty (tv :: seen) ~target ~guarded ~scope bound_ty
    | Var { contents = Free _ }
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> Ok ()
    | Mu (binder, body) ->
      let scope' = binder :: scope in
      let outer_target =
        match target with
        | Some target_name when String.equal target_name binder -> None
        | _ -> target
      in
      (match check_ty seen ~target:(Some binder) ~guarded:false ~scope:scope' body with
       | Error _ as err -> err
       | Ok () -> check_ty seen ~target:outer_target ~guarded ~scope:scope' body)
    | Rec_var binder ->
      if not (rec_name_mem binder scope)
      then Error (Printf.sprintf "Unbound recursive type variable '%s'" binder)
      else (
        match target with
        | Some target_binder when String.equal target_binder binder && not guarded ->
          Error (Printf.sprintf "Unguarded recursive type variable '%s'" binder)
        | _ -> Ok ())
    | Ref inner | Record inner | Variant inner | Array inner ->
      check_ty seen ~target ~guarded:true ~scope inner
    | Tuple tys -> check_list seen ~target ~guarded:true ~scope tys
    | Row (fields, tail) ->
      (match check_fields seen ~target ~guarded ~scope fields with
       | Ok () -> check_ty seen ~target ~guarded ~scope tail
       | Error _ as err -> err)
  in
  check_ty [] ~target:None ~guarded:false ~scope:[] ty
;;

(** --------------------------------------------------------------------- *)

(** 5. Row utilities                                                       *)

(** --------------------------------------------------------------------- *)

let merge_fields row =
  let rec aux (seen : tv ref list) (seen_rec : name list) = function
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen then Env.empty, Var tv else aux (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else aux seen (binder :: seen_rec) (unfold_mu mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Var _ as var -> Env.empty, var
    | Record r -> aux seen seen_rec r
    | Variant r -> aux seen seen_rec r
    | Row (fs, rest) ->
      (match aux seen seen_rec rest with
       | fs', rest' when Env.is_empty fs' -> fs, rest'
       | fs', rest' -> Env.merge fs fs', rest')
    | Empty_row -> Env.empty, Empty_row
    | _ -> failwith "merge_fields: expect a row type"
  in
  aux [] [] row
;;

(** --------------------------------------------------------------------- *)

(** 6. Occurs check                                                        *)

(** --------------------------------------------------------------------- *)

let occurs tv ty =
  let rec aux (seen : tv ref list) = function
    | Fun (ps, r) -> List.exists ps ~f:(aux seen) || aux seen r
    | Var ({ contents = Bound ty } as tv') ->
      if tv_ref_mem tv' seen then false else aux (tv' :: seen) ty
    | Var tv' when phys_equal tv tv' -> true
    | Var ({ contents = Free (name, lvl) } as tv') ->
      let new_lvl =
        match !tv with
        | Free (_, lvl_tv) -> Int.min lvl lvl_tv
        | _ -> lvl
      in
      tv' := Free (name, new_lvl);
      false
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> false
    | Ref t | Record t | Variant t | Array t -> aux seen t
    | Tuple ts -> List.exists ts ~f:(aux seen)
    | Row (fs, rest) -> Env.exists fs (fun _ t -> aux seen t) || aux seen rest
    | _ -> false
  in
  aux [] ty
;;

(** --------------------------------------------------------------------- *)

(** 7. Unification                                                         *)

(** --------------------------------------------------------------------- *)

let ensure_contractive_type (ty : typ) : unit =
  match check_contractive ty with
  | Ok () -> ()
  | Error msg -> raise (Type_error msg)
;;

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
    | (Mu (b1, body1) as mu1), (Mu (b2, body2) as mu2) ->
      (* General equi-recursive unification: treat mu-binders as alpha-equivalent.
         Rename both binders to a fresh name and unify bodies without unfolding. *)
      ensure_contractive_type mu1;
      ensure_contractive_type mu2;
      let fresh = "__unify_mu_" ^ gensym state in
      let body1' = substitute_rec_var ~target:b1 ~replacement:(Rec_var fresh) body1 in
      let body2' = substitute_rec_var ~target:b2 ~replacement:(Rec_var fresh) body2 in
      unify state body1' body2'
    | (Mu (_binder, _body) as mu), t | t, (Mu (_binder, _body) as mu) ->
      ensure_contractive_type mu;
      unify state (unfold_mu mu) t
    | Rec_var lhs_name, Rec_var rhs_name when String.equal lhs_name rhs_name -> ()
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

(* Print human-readable type names used in diagnostics. *)

and show_type ty =
  let rec show_type_with_seen (seen : tv ref list) (seen_rec : name list) = function
    | TInt -> "int"
    | TFloat -> "float"
    | Boolean -> "bool"
    | String -> "string"
    | Unit -> "unit"
    | Array t -> Printf.sprintf "[%s] array" (show_type_with_seen seen seen_rec t)
    | Ref t -> Printf.sprintf "ref(%s)" (show_type_with_seen seen seen_rec t)
    | Fun (ps, r) ->
      let params =
        ps |> List.map ~f:(show_type_with_seen seen seen_rec) |> String.concat ~sep:", "
      in
      Printf.sprintf "(%s -> %s)" params (show_type_with_seen seen seen_rec r)
    | Mu (binder, body) ->
      if rec_name_mem binder seen_rec
      then binder
      else
        Printf.sprintf
          "mu %s. %s"
          binder
          (show_type_with_seen seen (binder :: seen_rec) body)
    | Rec_var binder -> binder
    | Record row -> Printf.sprintf "{%s}" (show_row_with_seen seen seen_rec row)
    | Tuple ts ->
      ts
      |> List.map ~f:(show_type_with_seen seen seen_rec)
      |> String.concat ~sep:" * "
      |> Printf.sprintf "(%s)"
    | Variant row -> Printf.sprintf "[%s]" (show_variant_row_with_seen seen seen_rec row)
    | Row _ as row -> show_row_with_seen seen seen_rec row
    | Empty_row -> ""
    | Generic n -> n
    | Var { contents = Free (n, _) } -> Printf.sprintf "'%s" n
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then "'rec" else show_type_with_seen (tv :: seen) seen_rec t
  and row_fields_and_tail_with_seen (seen : tv ref list) (seen_rec : name list) row =
    match row with
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen
      then Env.empty, Var tv
      else row_fields_and_tail_with_seen (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else row_fields_and_tail_with_seen seen (binder :: seen_rec) (unfold_mu mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Record r -> row_fields_and_tail_with_seen seen seen_rec r
    | Variant r -> row_fields_and_tail_with_seen seen seen_rec r
    | Row (fs, rest) ->
      let rest_fields, tail = row_fields_and_tail_with_seen seen seen_rec rest in
      Env.merge fs rest_fields, tail
    | Empty_row -> Env.empty, Empty_row
    | Var _ as var -> Env.empty, var
    | Generic _ as generic -> Env.empty, generic
    | other -> Env.empty, other
  and show_row_with_seen (seen : tv ref list) (seen_rec : name list) row =
    let fields, tail = row_fields_and_tail_with_seen seen seen_rec row in
    let fields_str =
      Env.bindings fields
      |> List.map ~f:(fun (k, v) ->
        Printf.sprintf "%s: %s" k (show_type_with_seen seen seen_rec v))
      |> String.concat ~sep:"; "
    in
    let tail_str =
      match tail with
      | Empty_row -> ""
      | _ when String.is_empty fields_str -> "..."
      | _ -> "; ..."
    in
    fields_str ^ tail_str
  and resolve_bound_type_for_display_with_seen (seen : tv ref list) ty =
    match ty with
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen
      then Var tv
      else resolve_bound_type_for_display_with_seen (tv :: seen) bound_ty
    | _ -> ty
  and variant_payload_components_with_seen
        (seen : tv ref list)
        (_seen_rec : name list)
        payload_ty
    =
    match resolve_bound_type_for_display_with_seen seen payload_ty with
    | Unit -> []
    | Tuple ts -> ts
    | ty -> [ ty ]
  and show_variant_payload_with_seen (seen : tv ref list) (seen_rec : name list) ty =
    match variant_payload_components_with_seen seen seen_rec ty with
    | [] -> ""
    | [ single ] -> Printf.sprintf "(%s)" (show_type_with_seen seen seen_rec single)
    | many ->
      let inside =
        many |> List.map ~f:(show_type_with_seen seen seen_rec) |> String.concat ~sep:", "
      in
      Printf.sprintf "(%s)" inside
  and show_variant_row_with_seen (seen : tv ref list) (seen_rec : name list) row =
    let fields, tail = row_fields_and_tail_with_seen seen seen_rec row in
    let fields_str =
      Env.bindings fields
      |> List.map ~f:(fun (tag, payload_ty) ->
        Printf.sprintf
          "`%s%s"
          tag
          (show_variant_payload_with_seen seen seen_rec payload_ty))
      |> String.concat ~sep:" | "
    in
    let tail_str =
      match tail with
      | Empty_row -> ""
      | _ when String.is_empty fields_str -> "..."
      | _ -> " | ..."
    in
    fields_str ^ tail_str
  in
  show_type_with_seen [] [] ty

and row_fields_and_tail row =
  let rec aux (seen : tv ref list) (seen_rec : name list) row =
    match row with
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen then Env.empty, Var tv else aux (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else aux seen (binder :: seen_rec) (unfold_mu mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Record r -> aux seen seen_rec r
    | Variant r -> aux seen seen_rec r
    | Row (fs, rest) ->
      let rest_fields, tail = aux seen seen_rec rest in
      Env.merge fs rest_fields, tail
    | Empty_row -> Env.empty, Empty_row
    | Var _ as var -> Env.empty, var
    | Generic _ as generic -> Env.empty, generic
    | other -> Env.empty, other
  in
  aux [] [] row

and show_row row =
  let fields, tail = row_fields_and_tail row in
  let fields_str =
    Env.bindings fields
    |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s: %s" k (show_type v))
    |> String.concat ~sep:"; "
  in
  let tail_str =
    match tail with
    | Empty_row -> ""
    | _ when String.is_empty fields_str -> "..."
    | _ -> "; ..."
  in
  fields_str ^ tail_str

and resolve_bound_type_for_display ty =
  let rec aux (seen : tv ref list) = function
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen then Var tv else aux (tv :: seen) bound_ty
    | other -> other
  in
  aux [] ty

and variant_payload_components payload_ty =
  match resolve_bound_type_for_display payload_ty with
  | Unit -> []
  | Tuple ts -> ts
  | ty -> [ ty ]

and show_variant_payload ty =
  match variant_payload_components ty with
  | [] -> ""
  | [ single ] -> Printf.sprintf "(%s)" (show_type single)
  | many ->
    let inside = many |> List.map ~f:show_type |> String.concat ~sep:", " in
    Printf.sprintf "(%s)" inside

and show_variant_row row =
  let fields, tail = row_fields_and_tail row in
  let fields_str =
    Env.bindings fields
    |> List.map ~f:(fun (tag, payload_ty) ->
      Printf.sprintf "`%s%s" tag (show_variant_payload payload_ty))
    |> String.concat ~sep:" | "
  in
  let tail_str =
    match tail with
    | Empty_row -> ""
    | _ when String.is_empty fields_str -> "..."
    | _ -> " | ..."
  in
  fields_str ^ tail_str

and ensure_equality_type ty =
  let rec ensure_equality_type_with_seen (seen : tv ref list) (seen_rec : name list) ty =
    match resolve_bound_type_for_display_with_seen seen ty with
    | TInt | TFloat | Boolean | String | Unit | Generic _ -> ()
    | Var { contents = Free _ } -> ()
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen
      then ()
      else ensure_equality_type_with_seen (tv :: seen) seen_rec t
    | Mu (binder, body) ->
      if rec_name_mem binder seen_rec
      then ()
      else ensure_equality_type_with_seen seen (binder :: seen_rec) body
    | Rec_var binder when rec_name_mem binder seen_rec -> ()
    | Rec_var _ -> ()
    | Tuple ts -> List.iter ts ~f:(ensure_equality_type_with_seen seen seen_rec)
    | Record row | Variant row -> ensure_equality_row_with_seen seen seen_rec row
    | Array _ -> raise (Type_error "Equality is not supported for arrays")
    | Ref _ -> raise (Type_error "Equality is not supported for refs")
    | Fun _ -> raise (Type_error "Equality is not supported for functions")
    | Row _ | Empty_row -> ensure_equality_row_with_seen seen seen_rec ty
  and ensure_equality_row_with_seen (seen : tv ref list) (seen_rec : name list) row =
    let fields, tail = row_fields_and_tail_with_seen seen seen_rec row in
    Env.iter fields ~f:(fun _field ty -> ensure_equality_type_with_seen seen seen_rec ty);
    match tail with
    | Empty_row | Var { contents = Free _ } | Generic _ | Rec_var _ -> ()
    | Mu (binder, body) ->
      if rec_name_mem binder seen_rec
      then ()
      else ensure_equality_row_with_seen seen (binder :: seen_rec) body
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen
      then ()
      else ensure_equality_row_with_seen (tv :: seen) seen_rec t
    | _ -> ()
  and resolve_bound_type_for_display_with_seen (seen : tv ref list) ty =
    match ty with
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen
      then Var tv
      else resolve_bound_type_for_display_with_seen (tv :: seen) bound_ty
    | _ -> ty
  and row_fields_and_tail_with_seen (seen : tv ref list) (seen_rec : name list) row =
    match row with
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen
      then Env.empty, Var tv
      else row_fields_and_tail_with_seen (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else row_fields_and_tail_with_seen seen (binder :: seen_rec) (unfold_mu mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Record r -> row_fields_and_tail_with_seen seen seen_rec r
    | Variant r -> row_fields_and_tail_with_seen seen seen_rec r
    | Row (fs, rest) ->
      let rest_fields, tail = row_fields_and_tail_with_seen seen seen_rec rest in
      Env.merge fs rest_fields, tail
    | Empty_row -> Env.empty, Empty_row
    | Var _ as var -> Env.empty, var
    | Generic _ as generic -> Env.empty, generic
    | other -> Env.empty, other
  in
  ensure_equality_type_with_seen [] [] ty

and ensure_equality_row row =
  let fields, tail = row_fields_and_tail row in
  Env.iter fields ~f:(fun _field ty -> ensure_equality_type ty);
  match tail with
  | Empty_row | Var { contents = Free _ } | Generic _ -> ()
  | Var { contents = Bound t } -> ensure_equality_row t
  | _ -> ()
;;

(** --------------------------------------------------------------------- *)

(** 9. Typing environment                                                  *)

(** --------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* 9.1  Free variables / generalisation / instantiation                       *)
(* -------------------------------------------------------------------------- *)

type scheme = typ
type tenv = scheme Env.t
type type_env = typ Env.t

(* Unlike a classical HM environment that stores only polymorphic schemes,
   ChatML's typing environment stores both:
   - generalized schemes (types containing [Generic] nodes), and
   - monomorphic shared types for lambda parameters / weak bindings.

   Preserving the latter by reference is important for record-heavy helper
   functions: if we instantiated every lookup we would lose the sharing that
   connects [st.tasks], [st.task_index] and the final [st] returned by the
   helper, causing accidental row narrowing. *)
let contains_generic (ty : typ) : bool =
  let rec aux (seen : tv ref list) = function
    | Generic _ -> true
    | Fun (params, ret) -> List.exists params ~f:(aux seen) || aux seen ret
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then false else aux (tv :: seen) t
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> false
    | Var { contents = Free _ } -> false
    | Ref t | Record t | Variant t | Array t -> aux seen t
    | Tuple ts -> List.exists ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.exists fields (fun _field ty' -> aux seen ty') || aux seen tail
    | TInt | TFloat | Boolean | String | Empty_row | Unit -> false
  in
  aux [] ty
;;

let primitive_type_of_name (name : string) : typ option =
  match name with
  | "int" -> Some TInt
  | "float" -> Some TFloat
  | "bool" -> Some Boolean
  | "string" -> Some String
  | "unit" -> Some Unit
  | _ -> None
;;

let contains_rec_var_name (target : name) (ty : typ) : bool =
  let rec aux (seen : tv ref list) = function
    | Rec_var name when String.equal name target -> true
    | Rec_var _ -> false
    | Fun (params, ret) -> List.exists params ~f:(aux seen) || aux seen ret
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then false else aux (tv :: seen) t
    | Var { contents = Free _ }
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> false
    | Mu (binder, body) -> if String.equal binder target then false else aux seen body
    | Ref t | Record t | Variant t | Array t -> aux seen t
    | Tuple ts -> List.exists ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.exists fields (fun _field ty' -> aux seen ty') || aux seen tail
  in
  aux [] ty
;;

let ensure_unique_type_labels ~(what : string) (labels : string list) : unit =
  let seen = Hash_set.create (module String) in
  match
    List.find labels ~f:(fun label ->
      if Hash_set.mem seen label
      then true
      else (
        Hash_set.add seen label;
        false))
  with
  | None -> ()
  | Some label -> raise (Type_error (Printf.sprintf "Duplicate %s label '%s'" what label))
;;

let rec typ_of_type_expr ?self_name (types : type_env) (expr : type_expr) : typ =
  match expr with
  | TEName name ->
    (match primitive_type_of_name name with
     | Some ty -> ty
     | None ->
       (match self_name with
        | Some self when String.equal self name -> Rec_var name
        | _ ->
          (match Env.find types name with
           | Some ty -> ty
           | None -> raise (Type_error (Printf.sprintf "Unknown type '%s'" name)))))
  | TEArrow (lhs, rhs) ->
    let lhs_ty = typ_of_type_expr ?self_name types lhs in
    (match typ_of_type_expr ?self_name types rhs with
     | Fun (params, ret) ->
       if Poly.equal lhs_ty Unit then Fun (params, ret) else Fun (lhs_ty :: params, ret)
     | rhs_ty ->
       if Poly.equal lhs_ty Unit then Fun ([], rhs_ty) else Fun ([ lhs_ty ], rhs_ty))
  | TEArray inner -> Array (typ_of_type_expr ?self_name types inner)
  | TERecord fields ->
    ensure_unique_type_labels ~what:"type record field" (List.map fields ~f:fst);
    Record
      (Row
         ( Env.of_list
             (List.map fields ~f:(fun (label, ty_expr) ->
                label, typ_of_type_expr ?self_name types ty_expr))
         , Empty_row ))
  | TEVariant cases ->
    ensure_unique_type_labels ~what:"type variant constructor" (List.map cases ~f:fst);
    let payload_ty payloads =
      match List.map payloads ~f:(typ_of_type_expr ?self_name types) with
      | [] -> Unit
      | [ ty ] -> ty
      | tys -> Tuple tys
    in
    Variant
      (Row
         ( Env.of_list
             (List.map cases ~f:(fun (tag, payloads) -> tag, payload_ty payloads))
         , Empty_row ))
;;

let infer_type_decl (types : type_env) (name : string) (body : type_expr) : type_env =
  if Option.is_some (primitive_type_of_name name)
  then raise (Type_error (Printf.sprintf "Cannot redefine primitive type '%s'" name))
  else (
    match Env.find types name with
    | Some _ -> raise (Type_error (Printf.sprintf "Duplicate type declaration '%s'" name))
    | None ->
      let body_ty = typ_of_type_expr ~self_name:name types body in
      let declared_ty =
        if contains_rec_var_name name body_ty then Mu (name, body_ty) else body_ty
      in
      if has_recursive_type declared_ty then ensure_contractive_type declared_ty;
      Env.add name declared_ty types)
;;

let builtin_row_var_name (name : string) = "__builtin_row_" ^ name

let rec typ_of_builtin_ty (ty : Builtin_spec.ty) : typ =
  match ty with
  | Builtin_spec.TVar name -> Generic name
  | Builtin_spec.TInt -> TInt
  | Builtin_spec.TFloat -> TFloat
  | Builtin_spec.TBool -> Boolean
  | Builtin_spec.TString -> String
  | Builtin_spec.TUnit -> Unit
  | Builtin_spec.TArray inner -> Array (typ_of_builtin_ty inner)
  | Builtin_spec.TRef inner -> Ref (typ_of_builtin_ty inner)
  | Builtin_spec.TTuple tys -> Tuple (List.map tys ~f:typ_of_builtin_ty)
  | Builtin_spec.TRecord row -> Record (typ_of_builtin_row row)
  | Builtin_spec.TVariant row -> Variant (typ_of_builtin_row row)
  | Builtin_spec.TFun (params, ret) ->
    Fun (List.map params ~f:typ_of_builtin_ty, typ_of_builtin_ty ret)
  | Builtin_spec.TMu (binder, body) -> Mu (binder, typ_of_builtin_ty body)
  | Builtin_spec.TRec_var name -> Rec_var name

and typ_of_builtin_row (row : Builtin_spec.row) : typ =
  match row with
  | Builtin_spec.TRow_empty -> Empty_row
  | Builtin_spec.TRow_var name -> Generic (builtin_row_var_name name)
  | Builtin_spec.TRow_extend (fields, tail) ->
    Row
      ( Env.of_list (List.map fields ~f:(fun (label, ty) -> label, typ_of_builtin_ty ty))
      , typ_of_builtin_row tail )
;;

let init_env () : tenv =
  let globals =
    Builtin_spec.builtins |> List.map ~f:(fun b -> b.name, typ_of_builtin_ty b.scheme)
  in
  let modules =
    Builtin_spec.modules
    |> List.map ~f:(fun m -> m.name, typ_of_builtin_ty (Builtin_spec.module_scheme m))
  in
  Env.of_list (globals @ modules)
;;

(* Instantiate only genuinely polymorphic schemes.  Monomorphic bindings are
   returned as-is so that all uses share the same mutable inference variables. *)
let lookup (state : infer_state) (env : tenv) x =
  match Env.find env x with
  | Some sc -> if contains_generic sc then instantiate state sc else sc
  | None -> raise (Type_error (Printf.sprintf "Unknown variable '%s'" x))
;;

let add_mono (env : tenv) x ty : tenv =
  if has_recursive_type ty then ensure_contractive_type ty;
  Env.add x ty env
;;

let should_generalize_binding (ty : typ) : bool = not (has_recursive_type ty)

let add_generalized (state : infer_state) (env : tenv) x ty : tenv =
  (* Phase 5 rule: bindings whose type contains an explicit recursive type
     are kept monomorphic.  We therefore skip HM generalization entirely for
     such bindings and store the checked type as-is. *)
  if not (should_generalize_binding ty)
  then (
    ensure_contractive_type ty;
    Env.add x ty env)
  else Env.add x (generalise state ty) env
;;

let add_open_binding
      (state : infer_state)
      (env : tenv)
      ~(module_name : string)
      (name : string)
      (ty : typ)
  : tenv
  =
  match Env.find env name with
  | Some _ ->
    raise
      (Type_error
         (Printf.sprintf "open %s would shadow existing binding '%s'" module_name name))
  | None -> add_generalized state env name ty
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
  | EAnnot (expr_node, _) -> is_non_expansive expr_node.value
  | EPrim1 _
  | EPrim2 _
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

let rec is_function_like_expr (expr : expr) : bool =
  match expr with
  | ELambda _ | ELambdaSlots _ -> true
  | EAnnot (expr_node, _) -> is_function_like_expr expr_node.value
  | _ -> false
;;

let restrict_free_vars_to_level (max_level : int) (ty : typ) : unit =
  let rec aux (seen : tv ref list) ty =
    match ty with
    | Fun (params, ret) ->
      List.iter params ~f:(aux seen);
      aux seen ret
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> ()
    | Var ({ contents = Bound t } as tv) ->
      if not (tv_ref_mem tv seen) then aux (tv :: seen) t
    | Var ({ contents = Free (name, lvl) } as tv) ->
      if lvl > max_level then tv := Free (name, max_level)
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> ()
    | Ref t | Record t | Variant t | Array t -> aux seen t
    | Tuple ts -> List.iter ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.iter fields ~f:(fun _field ty' -> aux seen ty');
      aux seen tail
  in
  aux [] ty
;;

let rec infer_nonrecursive_binding
          (state : infer_state)
          (env : tenv)
          (types : type_env)
          (name : string)
          (rhs : expr node)
  : tenv
  =
  let rhs_ty = with_new_level state ~f:(fun () -> infer_expr state env types rhs) in
  if is_non_expansive rhs.value
  then add_generalized state env name rhs_ty
  else (
    restrict_free_vars_to_level state.current_lvl rhs_ty;
    add_mono env name rhs_ty)

and infer_recursive_bindings
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (bindings : (string * expr node) list)
  : tenv
  =
  List.iter bindings ~f:(fun (nm, rhs) ->
    if not (is_function_like_expr rhs.value)
    then
      raise
        (Type_error_with_loc
           (Printf.sprintf "Recursive binding '%s' must be a function" nm, rhs.span)));
  let binding_types =
    with_new_level state ~f:(fun () ->
      let env_with_placeholders =
        List.fold bindings ~init:env ~f:(fun env_acc (nm, _) ->
          add_mono env_acc nm (new_var state state.current_lvl))
      in
      List.iter bindings ~f:(fun (nm, rhs) ->
        let placeholder_ty = Env.find_exn env_with_placeholders nm in
        let rhs_ty = infer_expr state env_with_placeholders types rhs in
        unify state placeholder_ty rhs_ty);
      List.map bindings ~f:(fun (nm, _) -> nm, Env.find_exn env_with_placeholders nm))
  in
  List.fold binding_types ~init:env ~f:(fun env_acc (nm, ty) ->
    add_generalized state env_acc nm ty)

and infer_record_extend
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (base_expr : expr node)
      (fields : (string * expr node) list)
  : typ
  =
  let base_ty = infer_expr state env types base_expr in
  let base_row = new_var state state.current_lvl in
  unify state base_ty (Record base_row);
  let base_fields, base_tail = merge_fields base_row in
  let override_fields =
    List.fold fields ~init:Env.empty ~f:(fun acc (lbl, expr) ->
      let ty = infer_expr state env types expr in
      Env.add lbl ty acc)
  in
  let result_fields =
    Env.fold override_fields ~init:base_fields ~f:(fun ~key ~data acc ->
      Env.add key data acc)
  in
  (* Copy-update should support both:
     - overriding a field already known on the base row, and
     - adding a genuinely new field to an open-row parameter.

     We therefore keep the original open tail unchanged and simply layer the
     overriding fields in front of the base row.  If the tail later expands
     to contain the same labels, the row machinery's left-biased field merge
     ensures the updated field types win.  This gives helpers such as
     [let with_timeout cfg ms = { cfg with timeout_ms = ms }] the expected
     type [{ ...r } -> int -> { timeout_ms : int; ...r }]. *)
  Record (Row (result_fields, base_tail))

and validate_unique_labels
      ~(what : string)
      ~(span : Source.span option)
      (labels : string list)
  : unit
  =
  let seen = Hash_set.create (module String) in
  match
    List.find labels ~f:(fun lbl ->
      if Hash_set.mem seen lbl
      then true
      else (
        Hash_set.add seen lbl;
        false))
  with
  | None -> ()
  | Some lbl ->
    let msg = Printf.sprintf "Duplicate %s label '%s'" what lbl in
    (match span with
     | Some sp -> raise (Type_error_with_loc (msg, sp))
     | None -> raise (Type_error msg))

and validate_unique_record_fields
      (span : Source.span)
      (fields : (string * expr node) list)
  : unit
  =
  validate_unique_labels ~what:"record field" ~span:(Some span) (List.map fields ~f:fst)

and validate_unique_record_update_fields
      (span : Source.span)
      (fields : (string * expr node) list)
  : unit
  =
  validate_unique_labels ~what:"record update" ~span:(Some span) (List.map fields ~f:fst)

and validate_unique_record_labels_in_pattern (pat : pattern) (span : Source.span option)
  : unit
  =
  let rec loop = function
    | PUnit | PWildcard | PVar _ | PInt _ | PBool _ | PFloat _ | PString _ -> ()
    | PVariant (_tag, subpats) -> List.iter subpats ~f:loop
    | PRecord (fields, _is_open) ->
      validate_unique_labels ~what:"record pattern" ~span (List.map fields ~f:fst);
      List.iter fields ~f:(fun (_lbl, subpat) -> loop subpat)
  in
  loop pat

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
  | PUnit ->
    (match resolve_bound_type expected_ty with
     | Unit -> true
     | _ -> false)
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
        Set.equal
          field_names
          (Env.bindings row_fields |> List.map ~f:fst |> String.Set.of_list)
        && is_closed_record_row row
      | _ -> false)
  | _ -> false

and variant_constructor_info (variant_ty : typ) : ((string * typ) list * typ) option =
  match resolve_bound_type variant_ty with
  | Variant row ->
    let fields, tail = merge_fields row in
    Some (Env.bindings fields, tail)
  | _ -> None

and pattern_fully_covers_variant_case (pat : pattern) ~(tag : string) ~(payload_ty : typ)
  : bool
  =
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
  | PUnit -> "()"
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
  | PUnit -> Some ("unit:()", "()")
  | PInt i -> Some ("int:" ^ Int.to_string i, Int.to_string i)
  | PBool b -> Some ("bool:" ^ Bool.to_string b, Bool.to_string b)
  | PFloat f ->
    let rendered = Float.to_string f in
    Some ("float:" ^ rendered, rendered)
  | PString s -> Some ("string:" ^ s, Printf.sprintf "%S" s)
  | PVariant (tag, []) -> Some ("variant:" ^ tag, Printf.sprintf "`%s" tag)
  | _ -> None

and validate_match_case_shapes (cases : (pattern * Source.span) list) : unit =
  let seen_simple_patterns = Hash_set.create (module String) in
  let rec loop catch_all_case remaining_cases =
    match remaining_cases with
    | [] -> ()
    | (pat, pat_span) :: tl ->
      (match catch_all_case with
       | Some (prev_pat, _prev_span) ->
         raise
           (Type_error_with_loc
              ( Printf.sprintf
                  "Redundant match arm '%s': previous catch-all pattern '%s' already \
                   matches all cases"
                  (show_pattern_brief pat)
                  (show_pattern_brief prev_pat)
              , pat_span ))
       | None -> ());
      (match simple_pattern_key_of_pattern pat with
       | Some (key, display) ->
         if Hash_set.mem seen_simple_patterns key
         then
           raise
             (Type_error_with_loc
                (Printf.sprintf "Duplicate match arm for pattern '%s'" display, pat_span))
         else Hash_set.add seen_simple_patterns key
       | None -> ());
      let next_catch_all_case =
        if is_catch_all_pattern pat then Some (pat, pat_span) else catch_all_case
      in
      loop next_catch_all_case tl
  in
  loop None cases

and validate_boolean_match_exhaustiveness (scrut_ty : typ) (patterns : pattern list)
  : unit
  =
  match resolve_bound_type scrut_ty with
  | Boolean ->
    let has_catch_all = List.exists patterns ~f:is_catch_all_pattern in
    if not has_catch_all
    then (
      let seen_true =
        List.exists patterns ~f:(function
          | PBool true -> true
          | _ -> false)
      in
      let seen_false =
        List.exists patterns ~f:(function
          | PBool false -> true
          | _ -> false)
      in
      match seen_true, seen_false with
      | true, true -> ()
      | true, false ->
        raise (Type_error "Non-exhaustive boolean match: missing case 'false'")
      | false, true ->
        raise (Type_error "Non-exhaustive boolean match: missing case 'true'")
      | false, false ->
        raise
          (Type_error "Non-exhaustive boolean match: missing cases 'true' and 'false'"))
  | _ -> ()

and validate_typed_match_redundancy
      (scrut_ty : typ)
      (cases : (pattern * Source.span) list)
  : unit
  =
  match resolve_bound_type scrut_ty with
  | Unit ->
    let seen_unit = ref false in
    List.iter cases ~f:(fun (pat, pat_span) ->
      if is_catch_all_pattern pat && !seen_unit
      then
        raise
          (Type_error_with_loc
             ( Printf.sprintf
                 "Redundant match arm '%s': previous arms already cover unit case '()'"
                 (show_pattern_brief pat)
             , pat_span ));
      match pat with
      | PUnit -> seen_unit := true
      | _ -> ())
  | Boolean ->
    let seen_true = ref false in
    let seen_false = ref false in
    List.iter cases ~f:(fun (pat, pat_span) ->
      if is_catch_all_pattern pat && !seen_true && !seen_false
      then
        raise
          (Type_error_with_loc
             ( Printf.sprintf
                 "Redundant match arm '%s': previous arms already cover boolean cases \
                  'true' and 'false'"
                 (show_pattern_brief pat)
             , pat_span ));
      match pat with
      | PBool true -> seen_true := true
      | PBool false -> seen_false := true
      | _ -> ())
  | Variant _ ->
    (match variant_constructor_info scrut_ty with
     | None -> ()
     | Some (constructors, tail) ->
       let covered_tags = Hash_set.create (module String) in
       let payload_by_tag = String.Map.of_alist_exn constructors in
       let all_known_tags_covered () =
         List.for_all constructors ~f:(fun (tag, _payload_ty) ->
           Hash_set.mem covered_tags tag)
       in
       let closed_tail = is_closed_variant_tail tail in
       List.iter cases ~f:(fun (pat, pat_span) ->
         (match pat with
          | PVariant (tag, _subpats) when Hash_set.mem covered_tags tag ->
            let covered_payload = Map.find_exn payload_by_tag tag in
            raise
              (Type_error_with_loc
                 ( Printf.sprintf
                     "Redundant match arm '%s': previous arms already cover variant case \
                      '%s'"
                     (show_pattern_brief pat)
                     (show_missing_variant_case tag covered_payload)
                 , pat_span ))
          | _ when is_catch_all_pattern pat && closed_tail && all_known_tags_covered () ->
            raise
              (Type_error_with_loc
                 ( Printf.sprintf
                     "Redundant match arm '%s': previous arms already cover all variant \
                      constructors"
                     (show_pattern_brief pat)
                 , pat_span ))
          | _ -> ());
         List.iter constructors ~f:(fun (tag, payload_ty) ->
           if pattern_fully_covers_variant_case pat ~tag ~payload_ty
           then Hash_set.add covered_tags tag)))
  | _ -> ()

and validate_match_exhaustiveness
      (state : infer_state)
      (scrut_ty : typ)
      (patterns : pattern list)
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
    | Unit ->
      if
        List.exists patterns ~f:(function
          | PUnit -> true
          | _ -> false)
      then ()
      else raise (Type_error "Non-exhaustive match on unit: missing case '()'")
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
  | PUnit ->
    unify state expected_ty Unit;
    env
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

and reopen_lambda_param_type (state : infer_state) (ty : typ) : typ =
  match resolve_bound_type ty with
  | Record row -> Record (reopen_lambda_row state row)
  | Array elt_ty -> Array (reopen_lambda_param_type state elt_ty)
  | Ref inner_ty -> Ref (reopen_lambda_param_type state inner_ty)
  | Tuple tys -> Tuple (List.map tys ~f:(reopen_lambda_param_type state))
  | other -> other

and reopen_lambda_param_if_closed_record (state : infer_state) (ty : typ) : typ =
  match ty with
  | Var ({ contents = Bound bound_ty } as tv) ->
    let reopened = reopen_lambda_param_type state bound_ty in
    tv := Bound reopened;
    ty
  | _ -> reopen_lambda_param_type state ty

and resolve_bound_type (ty : typ) : typ =
  let rec aux (seen : tv ref list) (seen_rec : name list) = function
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen then Var tv else aux (tv :: seen) seen_rec bound_ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then mu
      else aux seen (binder :: seen_rec) (unfold_mu mu)
    | other -> other
  in
  aux [] [] ty

and reopen_lambda_row (state : infer_state) (row : typ) : typ =
  match resolve_bound_type row with
  | Row _ as full_row ->
    let merged_fields, _merged_tail = merge_fields full_row in
    let reopened_fields = Env.map merged_fields ~f:(reopen_lambda_param_type state) in
    (* Lambda parameters should be row-polymorphic by default: if a helper
       touches only a subset of record fields, callers should be able to pass
       larger records.  We therefore rebuild the parameter row with a fresh
       open tail after inference has discovered the required fields.  This is
       intentionally biased toward the "state-machine helper" use-case that
       ChatML scripts rely on heavily. *)
    Row (reopened_fields, new_var state state.current_lvl)
  | other -> other

(** --------------------------------------------------------------------- *)
(** 10. Inference – expressions                                            *)
(** --------------------------------------------------------------------- *)

and infer_unary_prim
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (prim : unary_prim)
      (arg : expr node)
  : typ
  =
  let arg_ty = infer_expr state env types arg in
  match prim with
  | UNegInt ->
    unify state arg_ty TInt;
    TInt
  | UNegFloat ->
    unify state arg_ty TFloat;
    TFloat

and infer_binary_prim
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (prim : binary_prim)
      (lhs : expr node)
      (rhs : expr node)
  : typ
  =
  let lhs_ty = infer_expr state env types lhs in
  let rhs_ty = infer_expr state env types rhs in
  match prim with
  | BIntAdd | BIntSub | BIntMul | BIntDiv ->
    unify state lhs_ty TInt;
    unify state rhs_ty TInt;
    TInt
  | BFloatAdd | BFloatSub | BFloatMul | BFloatDiv ->
    unify state lhs_ty TFloat;
    unify state rhs_ty TFloat;
    TFloat
  | BStringConcat ->
    unify state lhs_ty String;
    unify state rhs_ty String;
    String
  | BIntLt | BIntGt | BIntLe | BIntGe ->
    unify state lhs_ty TInt;
    unify state rhs_ty TInt;
    Boolean
  | BFloatLt | BFloatGt | BFloatLe | BFloatGe ->
    unify state lhs_ty TFloat;
    unify state rhs_ty TFloat;
    Boolean
  | BEq | BNeq ->
    unify state lhs_ty rhs_ty;
    ensure_equality_type lhs_ty;
    Boolean

and row_tail_equivalent (lhs : typ) (rhs : typ) : bool =
  let lhs' = resolve_bound_type lhs in
  let rhs' = resolve_bound_type rhs in
  (* Phase 6 rule: explicit recursive types stay separate from row tails.
     Control-flow joins must not preserve a row tail merely because both
     sides happen to mention the same recursive binder structure. *)
  phys_equal lhs' rhs'
  ||
  match lhs', rhs' with
  | Empty_row, Empty_row -> true
  | Var tv_l, Var tv_r -> phys_equal tv_l tv_r
  | Generic name_l, Generic name_r -> String.equal name_l name_r
  | Mu _, _ | _, Mu _ | Rec_var _, _ | _, Rec_var _ -> false
  | _ -> false

and build_row_type (fields : typ Env.t) (tail : typ) : typ =
  if Env.is_empty fields then tail else Row (fields, tail)

and row_known_fields_subset ~(subset : typ) ~(superset : typ) : bool =
  let subset_fields, _subset_tail = merge_fields subset in
  let superset_fields, _superset_tail = merge_fields superset in
  Env.fold subset_fields ~init:true ~f:(fun ~key ~data:_ acc ->
    acc
    &&
    match Env.find superset_fields key with
    | Some _ -> true
    | None -> false)

and join_record_rows (state : infer_state) (lhs : typ) (rhs : typ) : typ =
  let map_l, tail_l = merge_fields lhs in
  let map_r, tail_r = merge_fields rhs in
  let common_fields =
    Env.fold map_l ~init:Env.empty ~f:(fun ~key ~data:lhs_field_ty acc ->
      match Env.find map_r key with
      | None -> acc
      | Some rhs_field_ty -> Env.add key (join_type state lhs_field_ty rhs_field_ty) acc)
  in
  let shared_tail =
    if row_tail_equivalent tail_l tail_r then resolve_bound_type tail_l else Empty_row
  in
  build_row_type common_fields shared_tail

and join_type (state : infer_state) (lhs : typ) (rhs : typ) : typ =
  match resolve_bound_type lhs, resolve_bound_type rhs with
  | Record row_l, Record row_r -> Record (join_record_rows state row_l row_r)
  | lhs', rhs' ->
    unify state lhs' rhs';
    lhs

and infer_expr_against_expected
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (expr : expr node)
      (expected_ty : typ)
  : typ
  =
  match expr.value, resolve_bound_type expected_ty with
  | ELambda (params, body), Fun (expected_params, expected_ret)
    when List.length params = List.length expected_params ->
    with_new_level state ~f:(fun () ->
      let env_with_params =
        List.fold2_exn params expected_params ~init:env ~f:(fun env_acc param param_ty ->
          add_mono env_acc param param_ty)
      in
      let body_ty = infer_expr state env_with_params types body in
      unify state body_ty expected_ret;
      expected_ty)
  | ELambdaSlots (params, _slots, body), Fun (expected_params, expected_ret)
    when List.length params = List.length expected_params ->
    with_new_level state ~f:(fun () ->
      let env_with_params =
        List.fold2_exn params expected_params ~init:env ~f:(fun env_acc param param_ty ->
          add_mono env_acc param param_ty)
      in
      let body_ty = infer_expr state env_with_params types body in
      unify state body_ty expected_ret;
      expected_ty)
  | ( (ELambda (params, _) | ELambdaSlots (params, _, _))
    , Fun (expected_params, _expected_ret) ) ->
    raise
      (Type_error
         (Printf.sprintf
            "Annotated function expects %d parameter(s), but lambda has %d"
            (List.length expected_params)
            (List.length params)))
  | _ ->
    let inferred_ty = infer_expr state env types expr in
    unify state inferred_ty expected_ty;
    expected_ty

and infer_expr (state : infer_state) (env : tenv) (types : type_env) expr =
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
      | EPrim1 (prim, arg) -> infer_unary_prim state env types prim arg
      | EPrim2 (prim, lhs, rhs) -> infer_binary_prim state env types prim lhs rhs
      | ELambda (params, body) ->
        with_new_level state ~f:(fun () ->
          let env_with_params, param_tys_rev =
            List.fold params ~init:(env, []) ~f:(fun (env_acc, tys_rev) p ->
              let t = new_var state state.current_lvl in
              add_mono env_acc p t, t :: tys_rev)
          in
          let body_ty = infer_expr state env_with_params types body in
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
          let body_ty = infer_expr state env_with_params types body in
          let param_tys =
            List.rev param_tys_rev
            |> List.map ~f:(reopen_lambda_param_if_closed_record state)
          in
          Fun (param_tys, body_ty))
      | EApp (fn_expr, arg_exprs) ->
        let fn_ty = infer_expr state env types fn_expr in
        let arg_tys = List.map arg_exprs ~f:(fun arg -> infer_expr state env types arg) in
        let ret_ty = new_var state state.current_lvl in
        unify state fn_ty (Fun (arg_tys, ret_ty));
        ret_ty
      | EIf (c, t, e) ->
        let c_ty = infer_expr state env types c in
        unify state c_ty Boolean;
        let t_ty = infer_expr state env types t in
        let e_ty = infer_expr state env types e in
        join_type state t_ty e_ty
      | EWhile (cond, body) ->
        unify state (infer_expr state env types cond) Boolean;
        ignore (infer_expr state env types body);
        Unit
      | ESequence (e1, e2) ->
        ignore (infer_expr state env types e1);
        infer_expr state env types e2
      | ELetIn (x, rhs, body) ->
        let env' = infer_nonrecursive_binding state env types x rhs in
        infer_expr state env' types body
      | ELetBlock (bindings, body) ->
        let block_env =
          List.fold bindings ~init:env ~f:(fun env_acc (nm, rhs) ->
            infer_nonrecursive_binding state env_acc types nm rhs)
        in
        infer_expr state block_env types body
      | ELetBlockSlots (bindings, _slots, body) ->
        let block_env =
          List.fold bindings ~init:env ~f:(fun env_acc (nm, rhs) ->
            infer_nonrecursive_binding state env_acc types nm rhs)
        in
        infer_expr state block_env types body
      | ELetRec (bindings, body) ->
        let env_with_bindings = infer_recursive_bindings state env types bindings in
        infer_expr state env_with_bindings types body
      | ELetRecSlots (bindings, _slots, body) ->
        let env_with_bindings = infer_recursive_bindings state env types bindings in
        infer_expr state env_with_bindings types body
      | ERecord fields ->
        validate_unique_record_fields expr.span fields;
        let row =
          List.fold_right fields ~init:Empty_row ~f:(fun (lbl, expr) acc ->
            let ty = infer_expr state env types expr in
            Row (Env.singleton lbl ty, acc))
        in
        Record row
      | EFieldGet (obj, lbl) ->
        let obj_ty = infer_expr state env types obj in
        let field_ty = new_var state state.current_lvl in
        let tail_row = new_var state state.current_lvl in
        unify state obj_ty (Record (Row (Env.singleton lbl field_ty, tail_row)));
        field_ty
      | EArray elts ->
        let elt_ty = new_var state state.current_lvl in
        List.iter elts ~f:(fun e -> unify state (infer_expr state env types e) elt_ty);
        Array elt_ty
      | EArrayGet (arr, idx) ->
        unify state (infer_expr state env types idx) TInt;
        let elt_ty = new_var state state.current_lvl in
        unify state (infer_expr state env types arr) (Array elt_ty);
        elt_ty
      | EArraySet (arr, idx, v) ->
        unify state (infer_expr state env types idx) TInt;
        let v_ty = infer_expr state env types v in
        unify state (infer_expr state env types arr) (Array v_ty);
        Unit
      | ERef e -> Ref (infer_expr state env types e) (* simplified *)
      | ERecordExtend (base_expr, fields) ->
        validate_unique_record_update_fields expr.span fields;
        infer_record_extend state env types base_expr fields
      | EDeref e ->
        let ty = new_var state state.current_lvl in
        unify state (Ref ty) (infer_expr state env types e);
        ty
      | ESetRef (lhs, rhs) ->
        let lhs_ty = infer_expr state env types lhs in
        let rhs_ty = infer_expr state env types rhs in
        unify state (Ref (new_var state state.current_lvl)) lhs_ty;
        unify state lhs_ty (Ref rhs_ty);
        Unit
      | EVariant (tag, exprs) ->
        let arg_tys = List.map exprs ~f:(fun ex -> infer_expr state env types ex) in
        let case_ty =
          match arg_tys with
          | [] -> Unit
          | [ t ] -> t
          | ts -> Tuple ts
        in
        let row_var = new_var state state.current_lvl in
        Variant (Row (Env.singleton tag case_ty, row_var))
      | EAnnot (rhs, type_expr) ->
        let annotated_ty = typ_of_type_expr types type_expr in
        infer_expr_against_expected state env types rhs annotated_ty
      | EMatch (scrut, cases) ->
        let scrut_ty = infer_expr state env types scrut in
        let case_spans = List.map cases ~f:(fun case -> case.pat, case.pat_span) in
        validate_match_case_shapes case_spans;
        let result_ty =
          List.fold cases ~init:None ~f:(fun acc case ->
            try
              validate_unique_record_labels_in_pattern case.pat (Some case.pat_span);
              validate_pattern_binders case.pat;
              let env_arm = infer_pattern state env case.pat scrut_ty in
              let rhs_ty = infer_expr state env_arm types case.rhs in
              Some
                (match acc with
                 | None -> rhs_ty
                 | Some prior_ty -> join_type state prior_ty rhs_ty)
            with
            | Type_error msg -> raise (Type_error_with_loc (msg, case.pat_span))
            | Type_error_with_loc _ as exn -> raise exn)
        in
        validate_match_exhaustiveness state scrut_ty (List.map case_spans ~f:fst);
        validate_typed_match_redundancy scrut_ty case_spans;
        (match result_ty with
         | Some ty -> ty
         | None -> new_var state state.current_lvl)
      | EMatchSlots (scrut, cases) ->
        let scrut_ty = infer_expr state env types scrut in
        let case_spans = List.map cases ~f:(fun case -> case.pat, case.pat_span) in
        validate_match_case_shapes case_spans;
        let result_ty =
          List.fold cases ~init:None ~f:(fun acc case ->
            try
              validate_unique_record_labels_in_pattern case.pat (Some case.pat_span);
              validate_pattern_binders case.pat;
              let env_arm = infer_pattern state env case.pat scrut_ty in
              let rhs_ty = infer_expr state env_arm types case.rhs in
              Some
                (match acc with
                 | None -> rhs_ty
                 | Some prior_ty -> join_type state prior_ty rhs_ty)
            with
            | Type_error msg -> raise (Type_error_with_loc (msg, case.pat_span))
            | Type_error_with_loc _ as exn -> raise exn)
        in
        validate_match_exhaustiveness state scrut_ty (List.map case_spans ~f:fst);
        validate_typed_match_redundancy scrut_ty case_spans;
        (match result_ty with
         | Some ty -> ty
         | None -> new_var state state.current_lvl)
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
let rec infer_stmt_with_exports
          (state : infer_state)
          (env : tenv)
          (stmt : stmt)
          (types : type_env)
  : tenv * type_env * string list
  =
  match stmt with
  | SLet (x, e) -> infer_nonrecursive_binding state env types x e, types, [ x ]
  | SLetRec bindings ->
    infer_recursive_bindings state env types bindings, types, List.map bindings ~f:fst
  | SType (name, body) -> env, infer_type_decl types name body, []
  | SExpr e ->
    ignore (infer_expr state env types e);
    env, types, []
  | SModule (mname, stmts) ->
    let module_placeholder = new_var state state.current_lvl in
    let mod_env_start = add_mono env mname module_placeholder in
    let mod_env_final, _mod_types_final, exported_names_rev =
      List.fold
        stmts
        ~init:(mod_env_start, types, [])
        ~f:(fun (env_acc, types_acc, names_rev) stmt_node ->
          let env_next, types_next, stmt_exports =
            infer_stmt_with_exports state env_acc stmt_node.value types_acc
          in
          env_next, types_next, List.rev_append stmt_exports names_rev)
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
    add_generalized state env mname module_typ, types, [ mname ]
  | SOpen mname ->
    (match lookup state env mname with
     | Record row ->
       let fields, tail = merge_fields row in
       (match tail with
        | Empty_row | Var { contents = Free _ } -> ()
        | _ -> ());
       ( Env.fold fields ~init:env ~f:(fun ~key ~data acc ->
           add_open_binding state acc ~module_name:mname key data)
       , types
       , [] )
     | _ -> raise (Type_error (Printf.sprintf "Cannot open non-module '%s'" mname)))
;;

let infer_stmt (state : infer_state) (env : tenv) (types : type_env) (stmt : stmt)
  : tenv * type_env
  =
  let env', types', _exports = infer_stmt_with_exports state env stmt types in
  env', types'
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
  let types : type_env = Env.empty in
  (* Each element of [prog] now carries its own source span.  The current
     type-checker, however, does not yet make use of that information.  We
     therefore simply discard the annotation for the time being. *)
  try
    let _final_env, _final_types =
      List.fold prog.stmts ~init:(env, types) ~f:(fun (env_acc, types_acc) stmt_node ->
        infer_stmt state env_acc types_acc stmt_node.value)
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
  | Error diagnostic -> printf "%s\n%!" (format_diagnostic prog.source_text diagnostic)
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
