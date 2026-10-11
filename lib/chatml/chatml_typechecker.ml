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
    • field projection and open record patterns infer open rows directly,
      allowing helpers to accept larger records than the fields they touch;
      exact patterns and annotations retain their closed-row requirements;
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
module Builtin_surface = Chatml.Chatml_builtin_surface
module Type_alias = Chatml_type_alias
module Type_probe = Chatml_type_probe
module Signature_check = Chatml_signature_check

(***************************************************************************)
(* A Hindley-Milner style type-checker for the ChatML language.            *)
(***************************************************************************)

(** --------------------------------------------------------------------- *)

(** 1. Utility environment (string maps).                                  *)

(** --------------------------------------------------------------------- *)

include Chatml_inferred_type

(** --------------------------------------------------------------------- *)

(** 3. Inference state and span tracking for the HM algorithm              *)

(** --------------------------------------------------------------------- *)

type span_key = int * int [@@deriving compare, sexp, hash]

let span_to_key (sp : Source.span) : span_key = sp.left.offset, sp.right.offset

module SpanTbl = Hashtbl.Make (struct
    type t = span_key [@@deriving compare, hash, sexp]
  end)

(* Inference identities and weak cells belong to one compilation. Annotation
   ownership belongs to one unit and never changes when another unit is checked.
   The immutable invocation callback is outside this machine: compiler helpers
   can protect its exception origin without copying counters or swapping state. *)
type inference_machine =
  { mutable current_id : int
  ; mutable current_lvl : int
  ; mutable mu_id : int
  ; mutable until_checkpoint : int
  }

type infer_state =
  { shared : inference_machine
  ; checkpoint : unit -> unit
  ; source : Chatml_source.t option
  ; span_types : typ SpanTbl.t
  ; module_visibility : Chatml.Chatml_module_visibility.t SpanTbl.t
  }

let create_state ?(checkpoint = fun () -> ()) ?source () : infer_state =
  { shared = { current_id = 0; current_lvl = 0; mu_id = 0; until_checkpoint = 0 }
  ; checkpoint
  ; source
  ; span_types = SpanTbl.create ()
  ; module_visibility = SpanTbl.create ()
  }
;;

(* New unit annotations, same inference graph. No fresh gensym or type cells. *)
let for_source state source =
  { shared = state.shared
  ; checkpoint = state.checkpoint
  ; source = Some source
  ; span_types = SpanTbl.create ()
  ; module_visibility = SpanTbl.create ()
  }
;;

let checkpoint state =
  state.shared.until_checkpoint <- state.shared.until_checkpoint - 1;
  if state.shared.until_checkpoint <= 0
  then (
    state.shared.until_checkpoint <- 64;
    state.checkpoint ())
;;

(* Levels govern generalization, not variable identity. An inferred variable can
   escape an inner level through an outer parameter or captured mutable value.
   Reusing its name would conflate distinct binders when schemes instantiate. *)
let enter_level (state : infer_state) =
  state.shared.current_lvl <- state.shared.current_lvl + 1
;;

let exit_level (state : infer_state) =
  if state.shared.current_lvl <= 0
  then failwith "Typechecker level stack underflow"
  else state.shared.current_lvl <- state.shared.current_lvl - 1
;;

let gensym (state : infer_state) : string =
  let id = state.shared.current_id in
  state.shared.current_id <- state.shared.current_id + 1;
  let letter = Char.of_int_exn ((id mod 26) + Char.to_int 'a') |> Char.to_string in
  if id >= 26 then letter ^ Int.to_string (id / 26) else letter
;;

let gensym_mu (state : infer_state) : string =
  let id = state.shared.mu_id in
  state.shared.mu_id <- state.shared.mu_id + 1;
  Int.to_string id
;;

let record_span_type (state : infer_state) (span : Source.span) (ty : typ) : unit =
  Hashtbl.set state.span_types ~key:(span_to_key span) ~data:ty
;;

type diagnostic = Chatml_diagnostic.t

exception Type_error of string
exception Type_error_with_loc of string * Source.span
exception Type_diagnostic of diagnostic

type checked_program =
  { source : Chatml_source.t option
  ; span_types : typ SpanTbl.t
  ; module_visibility : Chatml.Chatml_module_visibility.t SpanTbl.t
  }

let checked_module_visibility (checked : checked_program) span =
  Hashtbl.find checked.module_visibility (span_to_key span)
;;

(* Raw-span lookup is a single-unit compatibility view. *)
let checked_lookup_span_type (checked : checked_program) (span : Source.span) : typ option
  =
  Hashtbl.find checked.span_types (span_to_key span)
;;

(* Location admission already validated coordinates; source and revision must
   match before identical offset pairs can query this unit's table. *)
let checked_owns_location (checked : checked_program) location =
  match checked.source with
  | None -> false
  | Some source ->
    Chatml_source.Id.equal
      (Chatml_source.id source)
      (Chatml_source.Location.source location)
    && Chatml_source.Digest.equal
         (Chatml_source.digest source)
         (Chatml_source.Location.source_digest location)
;;

let checked_lookup_location_type checked location =
  if checked_owns_location checked location
  then checked_lookup_span_type checked (Chatml_source.Location.span location)
  else None
;;

let checked_lookup_location_visibility checked location =
  if checked_owns_location checked location
  then checked_module_visibility checked (Chatml_source.Location.span location)
  else None
;;

let format_diagnostic source_text diagnostic =
  Chatml_diagnostic.render_legacy
    diagnostic
    ~source:source_text
    ~label:"Type error"
    ~checkpoint:(fun () -> ())
;;

let new_var (state : infer_state) ?equality level =
  Var (ref (Free { name = gensym state; level; equality }))
;;

let add_free_equality tv required =
  match !tv with
  | Bound _ -> failwith "internal: equality requirement expected a free variable"
  | Free info ->
    let equality =
      match info.equality with
      | None -> required
      | Some previous ->
        (match Equality_requirement.combine previous required with
         | Ok combined -> combined
         | Error Value_and_row ->
           raise (Type_error "Type variable cannot be both a value and a record row"))
    in
    tv := Free { info with equality = Some equality }
;;

let tv_ref_mem (needle : tv ref) (haystack : tv ref list) : bool =
  List.exists haystack ~f:(fun candidate -> phys_equal needle candidate)
;;

let rec_name_mem (needle : name) (haystack : name list) : bool =
  List.exists haystack ~f:(String.equal needle)
;;

let rec substitute_rec_var
          ?(checkpoint = fun () -> ())
          ~(target : name)
          ~(replacement : typ)
          (ty : typ)
  : typ
  =
  checkpoint ();
  let substitute = substitute_rec_var ~checkpoint ~target ~replacement in
  match ty with
  | Fun (params, ret) -> Fun (List.map params ~f:substitute, substitute ret)
  | Generic _ | Var _ | TInt | TFloat | Boolean | String | Empty_row | Unit | Shape_kind _
    -> ty
  | Con (name, args) -> Con (name, List.map args ~f:substitute)
  | Mu (binder, body) when String.equal binder target -> Mu (binder, body)
  | Mu (binder, body) -> Mu (binder, substitute body)
  | Rec_var binder when String.equal binder target -> replacement
  | Rec_var _ -> ty
  | Ref t -> Ref (substitute t)
  | Record row -> Record (substitute row)
  | Module row -> Module (substitute row)
  | Field_receiver (kind, row) -> Field_receiver (substitute kind, substitute row)
  | Variant row -> Variant (substitute row)
  | Tuple ts -> Tuple (List.map ts ~f:substitute)
  | Variant_payload ts -> Variant_payload (List.map ts ~f:substitute)
  | Array t -> Array (substitute t)
  | Row (fields, tail) -> Row (Env.map fields ~f:substitute, substitute tail)
;;

let unfold_mu ?checkpoint (ty : typ) : typ =
  match ty with
  | Mu (binder, body) as mu ->
    substitute_rec_var ?checkpoint ~target:binder ~replacement:mu body
  | other -> other
;;

(** --------------------------------------------------------------------- *)

(** 4. Generalisation / instantiation                                      *)

(** --------------------------------------------------------------------- *)

let instantiate (state : infer_state) ty =
  let table = Hashtbl.create (module String) in
  let kinds = Hashtbl.create (module String) in
  let rec inst_kind seen kind =
    checkpoint state;
    match kind with
    | Generic { name; equality = None } ->
      Hashtbl.find_or_add kinds name ~default:(fun () ->
        new_var state state.shared.current_lvl)
    | Generic { equality = Some _; _ } ->
      raise (Type_error "A representation kind cannot carry value equality")
    | Var ({ contents = Bound bound } as tv) ->
      if tv_ref_mem tv seen then Var tv else inst_kind (tv :: seen) bound
    | other -> other
  in
  let rec inst (seen : tv ref list) ty =
    checkpoint state;
    match ty with
    | Fun (ps, r) -> Fun (List.map ps ~f:(inst seen), inst seen r)
    | Generic { name; equality } ->
      let variable =
        Hashtbl.find_or_add table name ~default:(fun () ->
          new_var state ?equality state.shared.current_lvl)
      in
      (match variable with
       | Var tv -> Option.iter equality ~f:(add_free_equality tv)
       | _ -> assert false);
      variable
    | Con (n, args) -> Con (n, List.map args ~f:(inst seen))
    | Var ({ contents = Bound t } as tvref) ->
      if tv_ref_mem tvref seen then Var tvref else inst (tvref :: seen) t
    | Var ({ contents = Free _ } as tvref) -> Var tvref
    | Mu (binder, body) -> Mu (binder, inst seen body)
    | Rec_var _ as rec_var -> rec_var
    | Ref t -> Ref (inst seen t)
    | Record r -> Record (inst seen r)
    | Module r -> Module (inst seen r)
    | Field_receiver (kind, row) -> Field_receiver (inst_kind seen kind, inst seen row)
    | Variant r -> Variant (inst seen r)
    | Tuple ts -> Tuple (List.map ts ~f:(inst seen))
    | Variant_payload ts -> Variant_payload (List.map ts ~f:(inst seen))
    | Row (fields, t) -> Row (Env.map fields ~f:(inst seen), inst seen t)
    | Array t -> Array (inst seen t)
    | t -> t
  in
  inst [] ty
;;

(**
  Generalisation
  We now adopt the same strategy that the Nox checker uses: all free type variables that were
  introduced at a deeper level than the current one are turned into
  universally-quantified (Generic) type variables.
*)
let generalise (state : infer_state) ty =
  let rec gen (seen : tv ref list) ty =
    checkpoint state;
    match ty with
    | Fun (params, ret) -> Fun (List.map params ~f:(gen seen), gen seen ret)
    | Var ({ contents = Bound t } as tvref) ->
      if tv_ref_mem tvref seen then Var tvref else gen (tvref :: seen) t
    | Var { contents = Free { name; level; equality } }
      when level > state.shared.current_lvl -> Generic { name; equality }
    | Var ({ contents = Free _ } as tvref) -> Var tvref
    | Con (n, args) -> Con (n, List.map args ~f:(gen seen))
    | Mu (binder, body) -> Mu (binder, gen seen body)
    | Rec_var _ as rec_var -> rec_var
    | Ref t -> Ref (gen seen t)
    | Record row -> Record (gen seen row)
    | Module row -> Module (gen seen row)
    | Field_receiver (kind, row) -> Field_receiver (gen seen kind, gen seen row)
    | Variant row -> Variant (gen seen row)
    | Tuple ts -> Tuple (List.map ts ~f:(gen seen))
    | Variant_payload ts -> Variant_payload (List.map ts ~f:(gen seen))
    | Row (fields, tail) -> Row (Env.map fields ~f:(gen seen), gen seen tail)
    | Array t -> Array (gen seen t)
    | t -> t
  in
  gen [] ty
;;

let has_recursive_type = Chatml_type_validity.has_recursive_type
let check_contractive = Chatml_type_validity.check_contractive

(** --------------------------------------------------------------------- *)

(** 5. Row utilities                                                       *)

(** --------------------------------------------------------------------- *)

let merge_fields ?(checkpoint = fun () -> ()) row =
  let rec aux (seen : tv ref list) (seen_rec : name list) ty =
    checkpoint ();
    match ty with
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen then Env.empty, Var tv else aux (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else aux seen (binder :: seen_rec) (unfold_mu ~checkpoint mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Var _ as var -> Env.empty, var
    | Record r | Module r | Field_receiver (_, r) -> aux seen seen_rec r
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

let occurs ?(checkpoint = fun () -> ()) tv ty =
  let found = ref false in
  let rec aux (seen : tv ref list) ty =
    checkpoint ();
    match ty with
    | Fun (ps, r) ->
      List.iter ps ~f:(aux seen);
      aux seen r
    | Con (_n, args) -> List.iter args ~f:(aux seen)
    | Var tv' when phys_equal tv tv' -> found := true
    | Var ({ contents = Bound ty } as tv') ->
      if not (tv_ref_mem tv' seen) then aux (tv' :: seen) ty
    | Var ({ contents = Free ({ level = lvl; _ } as info) } as tv') ->
      let new_lvl =
        match !tv with
        | Free { level = lvl_tv; _ } -> Int.min lvl lvl_tv
        | _ -> lvl
      in
      tv' := Free { info with level = new_lvl }
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> ()
    | Ref t | Record t | Module t | Variant t | Array t -> aux seen t
    | Field_receiver (kind, row) ->
      aux seen kind;
      aux seen row
    | Tuple ts | Variant_payload ts -> List.iter ts ~f:(aux seen)
    | Row (fs, rest) ->
      Env.iter fs ~f:(fun _ t -> aux seen t);
      aux seen rest
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit | Shape_kind _ -> ()
  in
  aux [] ty;
  !found
;;

(** --------------------------------------------------------------------- *)

(** 7. Unification                                                         *)

(** --------------------------------------------------------------------- *)

let ensure_contractive_type ?checkpoint (ty : typ) : unit =
  match check_contractive ?checkpoint ty with
  | Ok () -> ()
  | Error msg -> raise (Type_error msg)
;;

let resolve_bound_type ?(checkpoint = fun () -> ()) (ty : typ) : typ =
  let rec aux (seen : tv ref list) (seen_rec : name list) ty =
    checkpoint ();
    match ty with
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen then Var tv else aux (tv :: seen) seen_rec bound_ty
    | Field_receiver (kind, row) ->
      (match aux seen seen_rec kind with
       | Shape_kind Record -> Record row
       | Shape_kind Module -> Module row
       | _ -> Field_receiver (kind, row))
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then mu
      else aux seen (binder :: seen_rec) (unfold_mu ~checkpoint mu)
    | other -> other
  in
  aux [] [] ty
;;

let rec can_infer_mu_through ?(checkpoint = fun () -> ()) (ty : typ) : bool =
  checkpoint ();
  match resolve_bound_type ~checkpoint ty with
  (* allow recursive *data* types *)
  | Variant _ | Record _ | Row _ | Tuple _ | Variant_payload _ | Con _ -> true
  (* optionally allow recursion through rows wrapped in Record/Variant already covered *)
  (* | Empty_row -> true  (* not needed *) *)

  (* disallow recursion through computational / mutable shapes *)
  | Fun _ | Ref _ | Array _ | Module _ | Field_receiver _ -> false
  (* be conservative for the rest *)
  | TInt | TFloat | Boolean | String | Unit | Shape_kind _ -> false
  | Generic _ -> false
  | Var { contents = Free _ } -> false
  | Var { contents = Bound t } -> can_infer_mu_through ~checkpoint t
  | Mu _ -> true (* allow cycles involving explicit Mu types *)
  | Rec_var _ ->
    true (* likewise, though Rec_var shouldn't appear without Mu when well-formed *)
  | Empty_row -> false
;;

module MuEnv = struct
  type id = int
  type t = id String.Map.t

  let empty : t = String.Map.empty
  let find = Map.find
  let add = Map.set
end

let ensure_guarded_cycle ?(checkpoint = fun () -> ()) (target : tv ref) (ty : typ) : unit =
  let rec bad (seen_tv : tv ref list) (seen_mu : name list) ~(guarded : bool) (t : typ)
    : bool
    =
    checkpoint ();
    match t with
    (* If we hit the target var again, it's only OK if we're under a "guard". *)
    | Var tv' when phys_equal target tv' -> not guarded
    (* Chase Bound vars, but avoid looping on cyclic Bound graphs. *)
    | Var ({ contents = Bound t' } as tvref) ->
      if tv_ref_mem tvref seen_tv
      then false
      else bad (tvref :: seen_tv) seen_mu ~guarded t'
    (* Other free vars are irrelevant to this check. *)
    | Var { contents = Free _ } -> false
    | Generic _ -> false
    (* Explicit recursive types (from user `type` decls). We can unfold them,
       but we must avoid infinite unfolding by tracking binders. *)
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_mu
      then false
      else bad seen_tv (binder :: seen_mu) ~guarded (unfold_mu ~checkpoint mu)
    | Rec_var _ ->
      (* A well-formed program should not have unbound Rec_var here.
         For guardedness of inferred cycles, ignore it. *)
      false
    (* Base types *)
    | TInt | TFloat | Boolean | String | Unit | Empty_row | Shape_kind _ -> false
    (* “Data guards” *)
    | Variant row -> bad seen_tv seen_mu ~guarded:true row
    | Record row | Module row -> bad seen_tv seen_mu ~guarded:true row
    | Field_receiver (kind, row) ->
      bad seen_tv seen_mu ~guarded kind || bad seen_tv seen_mu ~guarded:true row
    | Tuple ts | Variant_payload ts ->
      List.exists ts ~f:(bad seen_tv seen_mu ~guarded:true)
    | Con (_n, args) -> List.exists args ~f:(bad seen_tv seen_mu ~guarded:true)
    | Row (fields, tail) ->
      (* Row is only a *real* guard for its tail if it adds at least one label. *)
      let row_adds_structure = not (Env.is_empty fields) in
      let guarded_tail = guarded || row_adds_structure in
      Env.exists fields (fun _lbl fty -> bad seen_tv seen_mu ~guarded:true fty)
      || bad seen_tv seen_mu ~guarded:guarded_tail tail
    (* Non-guards: do not introduce guardedness, but preserve current guardedness. *)
    | Fun (ps, r) ->
      List.exists ps ~f:(bad seen_tv seen_mu ~guarded) || bad seen_tv seen_mu ~guarded r
    | Ref t' | Array t' -> bad seen_tv seen_mu ~guarded t'
  in
  if bad [] [] ~guarded:false ty
  then raise (Type_error "Unguarded recursive type (e.g. self application x(x))")
;;

let rec unify (state : infer_state) lhs rhs =
  let next_mu_id =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter
  in
  let same_scope = Map.equal Int.equal in
  let same_node lhs rhs =
    phys_equal lhs rhs
    ||
    match lhs, rhs with
    | Var left, Var right -> phys_equal left right
    | _ -> false
  in
  (* Recursive data types are finite graphs. Retain assumptions only along the
     current comparison path: siblings must still check their constraints after
     earlier unifications have bound mutable variables. Binder environments are
     part of the identity, so revisiting a node in another lexical scope does not
     discharge a different obligation. Guardedness is checked when cycles form. *)
  let rec go active (env_l : MuEnv.t) (env_r : MuEnv.t) lhs rhs =
    checkpoint state;
    if
      (same_node lhs rhs && same_scope env_l env_r)
      || List.exists active ~f:(fun (left, right, scope_l, scope_r) ->
        same_node lhs left
        && same_node rhs right
        && same_scope env_l scope_l
        && same_scope env_r scope_r)
    then ()
    else (
      let active = (lhs, rhs, env_l, env_r) :: active in
      match lhs, rhs with
      | Fun (ps1, r1), Fun (ps2, r2) ->
        if List.length ps1 <> List.length ps2
        then raise (Type_error "Function arity mismatch");
        List.iter2_exn ps1 ps2 ~f:(go active env_l env_r);
        go active env_l env_r r1 r2
      | Con (n1, a1), Con (n2, a2) ->
        if not (String.equal n1 n2) then raise (Type_error "Type constructor mismatch");
        if List.length a1 <> List.length a2
        then raise (Type_error "Type constructor arity mismatch");
        List.iter2_exn a1 a2 ~f:(go active env_l env_r)
      (* A free inference variable has no recursive binder interpretation. The
         same cell remains equal under alpha-renamed Mu scopes; do not pass this
         identity through the occurs check or relax identity for bound syntax. *)
      | Var ({ contents = Free _ } as left), Var right when phys_equal left right -> ()
      | Var { contents = Bound t1 }, t2 | t1, Var { contents = Bound t2 } ->
        go active env_l env_r t1 t2
      (* KEY CHANGE: Mu/Mu uses environments, not substitution. *)
      | Mu (b1, body1), Mu (b2, body2) ->
        let id = next_mu_id () in
        let env_l' = MuEnv.add env_l ~key:b1 ~data:id in
        let env_r' = MuEnv.add env_r ~key:b2 ~data:id in
        go active env_l' env_r' body1 body2
      (* Keep equi-recursive rule for Mu vs non-Mu by unfolding one step. *)
      | (Mu _ as mu), t ->
        ensure_contractive_type ~checkpoint:(fun () -> checkpoint state) mu;
        go active env_l env_r (unfold_mu ~checkpoint:(fun () -> checkpoint state) mu) t
      | t, (Mu _ as mu) ->
        ensure_contractive_type ~checkpoint:(fun () -> checkpoint state) mu;
        go active env_l env_r t (unfold_mu ~checkpoint:(fun () -> checkpoint state) mu)
      (* KEY CHANGE: Rec_var equality consults the environments. *)
      | Rec_var x, Rec_var y ->
        (match MuEnv.find env_l x, MuEnv.find env_r y with
         | Some id1, Some id2 when Int.equal id1 id2 -> ()
         | None, None when String.equal x y ->
           () (* free rec-vars: only ok if same name *)
         | _ -> raise (Type_error "Recursive type variables do not match"))
      (* If a Rec_var meets a non-Rec_var, it's a mismatch *)
      | Rec_var _, _ | _, Rec_var _ ->
        raise (Type_error "Cannot unify recursive type variable with non-recursive type")
      (* Your occurs-knot case (possibly gated to reject Fun recursion like x(x)) *)
      | Var ({ contents = Free info } as tv), t | t, Var ({ contents = Free info } as tv)
        ->
        Option.iter info.equality ~f:(fun requirement ->
          require_equality state requirement t);
        if occurs ~checkpoint:(fun () -> checkpoint state) tv t
        then (
          if not (can_infer_mu_through ~checkpoint:(fun () -> checkpoint state) t)
          then raise (Type_error "Recursive types");
          ensure_guarded_cycle ~checkpoint:(fun () -> checkpoint state) tv t;
          tv := Bound t (* cyclic bind; NO Rec_var/Mu synthesis *))
        else tv := Bound t
      | Ref t1, Ref t2 | Array t1, Array t2 -> go active env_l env_r t1 t2
      | Record r1, Record r2 | Module r1, Module r2 -> go_rows active env_l env_r r1 r2
      | Variant r1, Variant r2 -> go active env_l env_r r1 r2
      | Field_receiver (k1, r1), Field_receiver (k2, r2) ->
        go active env_l env_r k1 k2;
        go_rows active env_l env_r r1 r2
      | Field_receiver (kind, receiver_row), Record row ->
        go active env_l env_r kind (Shape_kind Record);
        go_rows active env_l env_r receiver_row row
      | Record row, Field_receiver (kind, receiver_row) ->
        go active env_l env_r (Shape_kind Record) kind;
        go_rows active env_l env_r row receiver_row
      | Field_receiver (kind, receiver_row), Module row ->
        go active env_l env_r kind (Shape_kind Module);
        go_rows active env_l env_r receiver_row row
      | Module row, Field_receiver (kind, receiver_row) ->
        go active env_l env_r (Shape_kind Module) kind;
        go_rows active env_l env_r row receiver_row
      | Shape_kind left, Shape_kind right when Shape_kind.equal left right -> ()
      | Variant_payload ts1, Variant_payload ts2 ->
        if List.length ts1 <> List.length ts2
        then raise (Type_error "Variant constructor arity mismatch");
        List.iter2_exn ts1 ts2 ~f:(go active env_l env_r)
      | Tuple ts1, Tuple ts2 ->
        if List.length ts1 <> List.length ts2
        then raise (Type_error "Tuple arity mismatch");
        List.iter2_exn ts1 ts2 ~f:(go active env_l env_r)
      | (Row _ as row1), (Row _ as row2) -> go_rows active env_l env_r row1 row2
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
             (Printf.sprintf
                "Cannot unify %s with %s"
                (show_type_for_diagnostic state lhs)
                (show_type_for_diagnostic state rhs))))
  and go_rows active env_l env_r lhs rhs =
    (* same as unify_rows but replace calls to [unify state] with [go env_l env_r] *)
    let map_l, tail_l = merge_fields ~checkpoint:(fun () -> checkpoint state) lhs in
    let map_r, tail_r = merge_fields ~checkpoint:(fun () -> checkpoint state) rhs in
    let rec collect l r missing_l missing_r =
      match l, r with
      | (lbl_l, ty_l) :: tl, (lbl_r, ty_r) :: tr ->
        (match String.compare lbl_l lbl_r with
         | 0 ->
           go active env_l env_r ty_l ty_r;
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
    (* Copy-updates may overlay fields on the same unknown base row. Equality
       of those rows requires the base to provide any field overridden on only
       one side. Use a fresh remainder rather than a recursive row equation;
       flatten again to check newly exposed field types. This is shared row
       unification, so nested records and container/function types obey the
       same rule as direct branch results. *)
    match tail_l, tail_r with
    | Var ({ contents = Free _ } as left), Var right
      when phys_equal left right && not (Env.is_empty missing_l && Env.is_empty missing_r)
      ->
      let required = Env.merge missing_l missing_r in
      go
        active
        env_l
        env_r
        tail_l
        (Row (required, new_var state state.shared.current_lvl));
      go_rows active env_l env_r lhs rhs
    | _ ->
      (match Env.is_empty missing_l, Env.is_empty missing_r with
       | true, true -> go active env_l env_r tail_l tail_r
       | true, false -> go active env_r env_l tail_r (Row (missing_r, tail_l))
       | false, true -> go active env_l env_r tail_l (Row (missing_l, tail_r))
       | false, false ->
         (match tail_l with
          | Var ({ contents = Free _ } as tv) ->
            let row_var = new_var state state.shared.current_lvl in
            go active env_r env_l tail_r (Row (missing_r, row_var));
            (match !tv with
             | Bound _ -> raise (Type_error "Recursive row types")
             | _ -> ());
            go active env_l env_r (Var tv) (Row (missing_l, row_var))
          | Empty_row -> go active env_l env_r tail_l (Row (missing_l, new_var state 0))
          | _ -> assert false))
  in
  go [] MuEnv.empty MuEnv.empty lhs rhs

(** --------------------------------------------------------------------- *)
(** 8. Pretty printer for types (used in error messages)                   *)
(** --------------------------------------------------------------------- *)

(* Print human-readable type names used in diagnostics. *)

and render_type ?(poll = fun () -> ()) ?bounds ty =
  let exception Truncated in
  let output = Buffer.create 128 in
  let nodes = ref 0 in
  let step depth =
    poll ();
    Int.incr nodes;
    match bounds with
    | Some (_, max_nodes, max_depth) when !nodes > max_nodes || depth > max_depth ->
      raise Truncated
    | _ -> ()
  in
  let add text =
    match bounds with
    | Some (max_bytes, _, _) when String.length text > max_bytes - Buffer.length output ->
      Buffer.add_substring output text ~pos:0 ~len:(max_bytes - Buffer.length output);
      raise Truncated
    | _ -> Buffer.add_string output text
  in
  let separated separator values f =
    let first = ref true in
    List.iter values ~f:(fun value ->
      (match !first with
       | true -> first := false
       | false -> add separator);
      f value)
  in
  let rec show depth seen seen_rec ty =
    step depth;
    let child = show (depth + 1) seen seen_rec in
    match ty with
    | TInt -> add "int"
    | TFloat -> add "float"
    | Boolean -> add "bool"
    | String -> add "string"
    | Unit -> add "unit"
    | Shape_kind kind -> add (Shape_kind.name kind)
    | Array t ->
      postfix depth seen seen_rec t;
      add " array"
    | Ref t ->
      add "ref(";
      child t;
      add ")"
    | Fun (ps, r) ->
      add "(";
      separated ", " ps child;
      add " -> ";
      child r;
      add ")"
    | Mu (binder, body) ->
      (match rec_name_mem binder seen_rec with
       | true -> add binder
       | false ->
         add "mu ";
         add binder;
         add ". ";
         show (depth + 1) seen (binder :: seen_rec) body)
    | Rec_var binder -> add binder
    | Record row ->
      add "{";
      show_row_fields depth seen seen_rec false row;
      add "}"
    | Module row ->
      add "module{";
      show_row_fields depth seen seen_rec false row;
      add "}"
    | Field_receiver (kind, row) ->
      (match resolve (depth + 1) seen kind with
       | Shape_kind Record -> add "{"
       | Shape_kind Module -> add "module{"
       | _ -> add "record_or_module{");
      show_row_fields depth seen seen_rec false row;
      add "}"
    | Tuple ts | Variant_payload ts ->
      add "(";
      separated " * " ts child;
      add ")"
    | Variant row ->
      add "[";
      show_row_fields depth seen seen_rec true row;
      add "]"
    | Row _ as row -> show_row_fields depth seen seen_rec false row
    | Empty_row -> ()
    | Generic { name; equality } ->
      add name;
      qualification equality
    | Var { contents = Free { name; equality; _ } } ->
      add "'";
      add name;
      qualification equality
    | Var ({ contents = Bound t } as tv) ->
      (match tv_ref_mem tv seen with
       | true -> add "'rec"
       | false -> show (depth + 1) (tv :: seen) seen_rec t)
    | Con ("task", [ arg ]) ->
      postfix depth seen seen_rec arg;
      add " task"
    | Con (n, args) ->
      add n;
      add "(";
      separated ", " args child;
      add ")"
  and qualification = function
    | None -> ()
    | Some Equality_requirement.Value -> add " [equality]"
    | Some (Equality_requirement.Row excluded) ->
      add " [row equality";
      if not (Set.is_empty excluded)
      then (
        add " except ";
        separated ", " (Set.to_list excluded) add);
      add "]"
  and postfix depth seen seen_rec ty =
    match resolve (depth + 1) seen ty with
    | Fun _ ->
      add "(";
      show (depth + 1) seen seen_rec ty;
      add ")"
    | _ -> show (depth + 1) seen seen_rec ty
  and row_fields depth seen seen_rec row =
    step depth;
    match row with
    | Var ({ contents = Bound ty } as tv) ->
      if tv_ref_mem tv seen
      then Env.empty, Var tv
      else row_fields (depth + 1) (tv :: seen) seen_rec ty
    | Mu (binder, _body) as mu ->
      if rec_name_mem binder seen_rec
      then Env.empty, mu
      else
        row_fields
          (depth + 1)
          seen
          (binder :: seen_rec)
          (unfold_mu ~checkpoint:(fun () -> step depth) mu)
    | Rec_var _ as rec_var -> Env.empty, rec_var
    | Record r | Module r | Field_receiver (_, r) | Variant r ->
      row_fields (depth + 1) seen seen_rec r
    | Row (fs, rest) ->
      let rest_fields, tail = row_fields (depth + 1) seen seen_rec rest in
      let fields =
        Env.fold fs ~init:rest_fields ~f:(fun ~key ~data acc ->
          step depth;
          Env.add key data acc)
      in
      fields, tail
    | Empty_row -> Env.empty, Empty_row
    | Var _ as var -> Env.empty, var
    | Generic _ as generic -> Env.empty, generic
    | other -> Env.empty, other
  and resolve depth seen ty =
    step depth;
    match ty with
    | Var ({ contents = Bound bound_ty } as tv) ->
      if tv_ref_mem tv seen then Var tv else resolve (depth + 1) (tv :: seen) bound_ty
    | _ -> ty
  and show_row_fields depth seen seen_rec variant row =
    let fields, tail = row_fields (depth + 1) seen seen_rec row in
    let separator = if variant then " | " else "; " in
    let first = ref true in
    Env.iter fields ~f:(fun key value ->
      (match !first with
       | true -> first := false
       | false -> add separator);
      match variant with
      | false ->
        add key;
        add ": ";
        show (depth + 1) seen seen_rec value
      | true ->
        add "`";
        add key;
        let payload =
          match resolve (depth + 1) seen value with
          | Variant_payload ts -> ts
          | _ -> raise (Type_error "Invalid variant constructor payload type")
        in
        (match payload with
         | [] -> ()
         | _ ->
           add "(";
           separated ", " payload (show (depth + 1) seen seen_rec);
           add ")"));
    match tail with
    | Empty_row -> ()
    | _ ->
      if not !first then add separator;
      add "..."
  in
  match show 0 [] [] ty with
  | () -> Buffer.contents output
  | exception Truncated ->
    let max_bytes, _, _ = Option.value_exn bounds in
    String.prefix (Buffer.contents output) (Int.max 0 (max_bytes - 3))
    ^ String.prefix "..." max_bytes

and show_type ty = render_type ty

and show_type_for_diagnostic state ty =
  (* This limits an error preview, not the type system or trusted evaluation.
     A shared DAG may have a small representation and exponential full display. *)
  render_type ~poll:(fun () -> checkpoint state) ~bounds:(8192, 1024, 128) ty

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
    | Record r | Module r | Field_receiver (_, r) -> aux seen seen_rec r
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
  | Variant_payload ts -> ts
  | _ -> raise (Type_error "Invalid variant constructor payload type")

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

and require_equality state requirement ty =
  let module Binder = struct
    type t =
      { name : name
      ; body : typ
      ; outer : t list
      }
  end
  in
  let same_scope left right = List.equal phys_equal left right in
  let unsupported what = raise (Type_error ("Equality is not supported for " ^ what)) in
  let rec require seen seen_rec scope requirement ty =
    checkpoint state;
    match ty with
    | Var ({ contents = Free _ } as tv) -> add_free_equality tv requirement
    | Var ({ contents = Bound bound } as tv) ->
      if
        not
          (List.exists seen ~f:(fun (prior, obligation, prior_scope) ->
             phys_equal tv prior
             && Equality_requirement.equal requirement obligation
             && same_scope scope prior_scope))
      then require ((tv, requirement, scope) :: seen) seen_rec scope requirement bound
    | Generic { name; equality } ->
      let sufficient =
        match equality with
        | None -> false
        | Some declared ->
          (match Equality_requirement.combine declared requirement with
           | Error _ -> false
           | Ok combined -> Equality_requirement.equal declared combined)
      in
      if not sufficient
      then raise (Type_error ("Equality requires a constrained type parameter '" ^ name))
    | Mu (name, body) ->
      require_binder seen seen_rec requirement { Binder.name; body; outer = scope }
    | Rec_var name ->
      (match List.find scope ~f:(fun binder -> String.equal binder.Binder.name name) with
       | Some binder -> require_binder seen seen_rec requirement binder
       | None -> raise (Type_error ("Unbound recursive equality variable '" ^ name)))
    | _ ->
      (match requirement with
       | Equality_requirement.Value ->
         (match ty with
          | TInt | TFloat | Boolean | String | Unit -> ()
          | Record row | Variant row ->
            require seen seen_rec scope (Row String.Set.empty) row
          | Field_receiver (kind, row) ->
            unify state kind (Shape_kind Record);
            require seen seen_rec scope (Row String.Set.empty) row
          | Module _ -> unsupported "modules"
          | Array _ -> unsupported "arrays"
          | Ref _ -> unsupported "refs"
          | Fun _ -> unsupported "functions"
          | Con ("task", _) -> unsupported "Task values"
          | Con (_, args) | Tuple args | Variant_payload args ->
            List.iter args ~f:(require seen seen_rec scope Value)
          | Row _ | Empty_row -> require seen seen_rec scope (Row String.Set.empty) ty
          | Shape_kind _ -> unsupported "representation kinds"
          | Var _ | Generic _ | Mu _ | Rec_var _ -> assert false)
       | Row excluded ->
         (match ty with
          | Empty_row -> ()
          | Row (fields, tail) ->
            Env.iter fields ~f:(fun label field ->
              checkpoint state;
              if not (Set.mem excluded label) then require seen seen_rec scope Value field);
            let shadowed =
              Set.union excluded (Set.of_list (module String) (Map.keys fields))
            in
            require seen seen_rec scope (Row shadowed) tail
          | Record row | Variant row -> require seen seen_rec scope requirement row
          | Field_receiver (kind, row) ->
            unify state kind (Shape_kind Record);
            require seen seen_rec scope requirement row
          | Module _ -> unsupported "modules"
          | TInt
          | TFloat
          | Boolean
          | String
          | Unit
          | Array _
          | Ref _
          | Fun _
          | Con _
          | Tuple _
          | Variant_payload _
          | Shape_kind _ -> raise (Type_error "Equality expected a record or variant row")
          | Var _ | Generic _ | Mu _ | Rec_var _ -> assert false))
  and require_binder seen seen_rec requirement binder =
    (* Names are lexical: shadowing a binder must not discharge a different
       obligation. Identity here belongs to one scoped traversal binding. *)
    if
      not
        (List.exists seen_rec ~f:(fun (prior, obligation) ->
           phys_equal binder prior && Equality_requirement.equal requirement obligation))
    then
      require
        seen
        ((binder, requirement) :: seen_rec)
        (binder :: binder.Binder.outer)
        requirement
        binder.body
  in
  require [] [] [] requirement ty

and ensure_equality_type state ty = require_equality state Equality_requirement.Value ty

(** --------------------------------------------------------------------- *)

(** 9. Typing environment                                                  *)

(** --------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* 9.1  Free variables / generalisation / instantiation                       *)
(* -------------------------------------------------------------------------- *)

type scheme = typ
type tenv = scheme Env.t
type type_env = Type_alias.t

(* Unlike a classical HM environment that stores only polymorphic schemes,
   ChatML's typing environment stores both:
   - generalized schemes (types containing [Generic] nodes), and
   - monomorphic shared types for lambda parameters / weak bindings.

   Preserving the latter by reference is important for record-heavy helper
   functions: if we instantiated every lookup we would lose the sharing that
   connects [st.tasks], [st.task_index] and the final [st] returned by the
   helper, causing accidental row narrowing. *)
let contains_generic ?(checkpoint = fun () -> ()) (ty : typ) : bool =
  let rec aux (seen : tv ref list) ty =
    checkpoint ();
    match ty with
    | Generic _ -> true
    | Fun (params, ret) -> List.exists params ~f:(aux seen) || aux seen ret
    | Var ({ contents = Bound t } as tv) ->
      if tv_ref_mem tv seen then false else aux (tv :: seen) t
    | Con (_n, args) -> List.exists args ~f:(aux seen)
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> false
    | Var { contents = Free _ } -> false
    | Ref t | Record t | Module t | Variant t | Array t -> aux seen t
    | Field_receiver (kind, row) -> aux seen kind || aux seen row
    | Tuple ts | Variant_payload ts -> List.exists ts ~f:(aux seen)
    | Row (fields, tail) ->
      Env.exists fields (fun _field ty' -> aux seen ty') || aux seen tail
    | TInt | TFloat | Boolean | String | Empty_row | Unit | Shape_kind _ -> false
  in
  aux [] ty
;;

let alias_result = function
  | Ok value -> value
  | Error diagnostic -> raise (Type_diagnostic diagnostic)
;;

let typ_of_type_expr ?source ?(checkpoint = fun () -> ()) types expr =
  Type_alias.elaborate ?source types expr ~checkpoint |> alias_result
;;

let infer_type_decl ?source ?(checkpoint = fun () -> ()) types declaration =
  Type_alias.declare ?source types declaration ~checkpoint |> alias_result
;;

let builtin_row_var_name (name : string) = "__builtin_row_" ^ name

let rec typ_of_builtin_ty ?(checkpoint = fun () -> ()) (ty : Builtin_spec.ty) : typ =
  checkpoint ();
  let convert = typ_of_builtin_ty ~checkpoint in
  match ty with
  | Builtin_spec.TVar name -> Generic { name; equality = None }
  | Builtin_spec.TCon (n, args) -> Con (n, List.map args ~f:convert)
  | Builtin_spec.TInt -> TInt
  | Builtin_spec.TFloat -> TFloat
  | Builtin_spec.TBool -> Boolean
  | Builtin_spec.TString -> String
  | Builtin_spec.TUnit -> Unit
  | Builtin_spec.TArray inner -> Array (convert inner)
  | Builtin_spec.TRef inner -> Ref (convert inner)
  | Builtin_spec.TTuple tys -> Tuple (List.map tys ~f:convert)
  | Builtin_spec.TRecord row -> Record (typ_of_builtin_row ~checkpoint row)
  | Builtin_spec.TModule row -> Module (typ_of_builtin_row ~checkpoint row)
  | Builtin_spec.TField_receiver (kind, row) ->
    Field_receiver
      (Generic { name = kind; equality = None }, typ_of_builtin_row ~checkpoint row)
  | Builtin_spec.TVariant row -> Variant (typ_of_builtin_variant_row ~checkpoint row)
  | Builtin_spec.TFun (params, ret) -> Fun (List.map params ~f:convert, convert ret)
  | Builtin_spec.TMu (binder, body) -> Mu (binder, convert body)
  | Builtin_spec.TRec_var name -> Rec_var name

and typ_of_builtin_row ?(checkpoint = fun () -> ()) (row : Builtin_spec.row) : typ =
  checkpoint ();
  match row with
  | Builtin_spec.TRow_empty -> Empty_row
  | Builtin_spec.TRow_var name ->
    Generic { name = builtin_row_var_name name; equality = None }
  | Builtin_spec.TRow_extend (fields, tail) ->
    Row
      ( Env.of_list
          (List.map fields ~f:(fun (label, ty) -> label, typ_of_builtin_ty ~checkpoint ty))
      , typ_of_builtin_row ~checkpoint tail )

and typ_of_builtin_variant_row ?(checkpoint = fun () -> ()) row : typ =
  checkpoint ();
  match row with
  | Builtin_spec.TVariant_row_empty -> Empty_row
  | Builtin_spec.TVariant_row_var name ->
    Generic { name = builtin_row_var_name name; equality = None }
  | Builtin_spec.TVariant_row_extend (cases, tail) ->
    Row
      ( Env.of_list
          (List.map cases ~f:(fun (tag, arguments) ->
             tag, Variant_payload (List.map arguments ~f:(typ_of_builtin_ty ~checkpoint))))
      , typ_of_builtin_variant_row ~checkpoint tail )
;;

let init_env_with_surface ?(checkpoint = fun () -> ()) (surface : Builtin_surface.surface)
  : tenv
  =
  let globals =
    surface.globals
    |> List.map ~f:(fun builtin ->
      builtin.name, typ_of_builtin_ty ~checkpoint builtin.scheme)
  in
  let modules =
    surface.modules
    |> List.map ~f:(fun builtin_module ->
      ( builtin_module.name
      , typ_of_builtin_ty ~checkpoint (Builtin_spec.module_scheme builtin_module) ))
  in
  Env.of_list (globals @ modules)
;;

let init_env () : tenv = init_env_with_surface Builtin_surface.core_surface

let init_types_with_surface
      ?(checkpoint = fun () -> ())
      (surface : Builtin_surface.surface)
  : type_env
  =
  List.fold surface.type_aliases ~init:Type_alias.empty ~f:(fun env alias ->
    let alias_ty = typ_of_builtin_ty ~checkpoint alias.body in
    match Type_alias.add_host env ~name:alias.name ~body:alias_ty ~checkpoint with
    | Ok types -> types
    | Error message -> raise (Type_error message))
;;

(* Instantiate only genuinely polymorphic schemes.  Monomorphic bindings are
   returned as-is so that all uses share the same mutable inference variables. *)
let lookup_scheme (env : tenv) x =
  match Env.find env x with
  | Some scheme -> scheme
  | None -> raise (Type_error (Printf.sprintf "Unknown variable '%s'" x))
;;

let lookup (state : infer_state) (env : tenv) x =
  let scheme = lookup_scheme env x in
  if contains_generic ~checkpoint:(fun () -> checkpoint state) scheme
  then instantiate state scheme
  else scheme
;;

let add_mono ?(checkpoint = fun () -> ()) (env : tenv) x ty : tenv =
  if has_recursive_type ~checkpoint ty then ensure_contractive_type ~checkpoint ty;
  Env.add x ty env
;;

let should_generalize_binding ?checkpoint (ty : typ) : bool =
  not (has_recursive_type ?checkpoint ty)
;;

let add_generalized (state : infer_state) (env : tenv) x ty : tenv =
  (* bindings whose type contains an explicit recursive type
     are kept monomorphic.  We therefore skip HM generalization entirely for
     such bindings and store the checked type as-is. *)
  if not (should_generalize_binding ~checkpoint:(fun () -> checkpoint state) ty)
  then (
    ensure_contractive_type ~checkpoint:(fun () -> checkpoint state) ty;
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
  | None -> add_mono ~checkpoint:(fun () -> checkpoint state) env name ty
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

let restrict_free_vars_to_level ?(checkpoint = fun () -> ()) (max_level : int) (ty : typ)
  : unit
  =
  let rec aux (seen : tv ref list) ty =
    checkpoint ();
    match ty with
    | Fun (params, ret) ->
      List.iter params ~f:(aux seen);
      aux seen ret
    | Generic _ | TInt | TFloat | Boolean | String | Empty_row | Unit | Shape_kind _ -> ()
    | Var ({ contents = Bound t } as tv) ->
      if not (tv_ref_mem tv seen) then aux (tv :: seen) t
    | Con (_n, args) -> List.iter args ~f:(aux seen)
    | Var ({ contents = Free ({ level = lvl; _ } as info) } as tv) ->
      if lvl > max_level then tv := Free { info with level = max_level }
    | Mu (_binder, body) -> aux seen body
    | Rec_var _ -> ()
    | Ref t | Record t | Module t | Variant t | Array t -> aux seen t
    | Field_receiver (kind, row) ->
      aux seen kind;
      aux seen row
    | Tuple ts | Variant_payload ts -> List.iter ts ~f:(aux seen)
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
    restrict_free_vars_to_level
      ~checkpoint:(fun () -> checkpoint state)
      state.shared.current_lvl
      rhs_ty;
    add_mono ~checkpoint:(fun () -> checkpoint state) env name rhs_ty)

and infer_recursive_bindings
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (bindings : (string * expr node) list)
  : tenv
  =
  validate_unique_binders
    ~what:"recursive binding"
    (List.map bindings ~f:(fun (name, rhs) -> name, rhs.span));
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
          add_mono
            ~checkpoint:(fun () -> checkpoint state)
            env_acc
            nm
            (new_var state state.shared.current_lvl))
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
  let base_row = new_var state state.shared.current_lvl in
  unify state base_ty (Record base_row);
  let base_fields, base_tail =
    merge_fields ~checkpoint:(fun () -> checkpoint state) base_row
  in
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

(* Binder names have no individual source spans in the AST. Anchor duplicate
   parameters at their lambda, and recursive names at the duplicate RHS. *)
and validate_unique_binders ~what binders =
  let seen = Hash_set.create (module String) in
  match
    List.find binders ~f:(fun (name, _span) ->
      if Hash_set.mem seen name
      then true
      else (
        Hash_set.add seen name;
        false))
  with
  | None -> ()
  | Some (name, span) ->
    raise (Type_error_with_loc (Printf.sprintf "Duplicate %s '%s'" what name, span))

and validate_lambda_binders (expr : expr node) =
  match expr.value with
  | ELambda (params, _) | ELambdaSlots (params, _, _) ->
    validate_unique_binders
      ~what:"lambda parameter"
      (List.map params ~f:(fun name -> name, expr.span))
  | _ -> ()

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

and record_field_type ?(checkpoint = fun () -> ()) (row_ty : typ) (label : string)
  : typ option
  =
  checkpoint ();
  match resolve_bound_type ~checkpoint row_ty with
  | Record row | Module row | Field_receiver (_, row) ->
    record_field_type ~checkpoint row label
  | Row (fields, tail) ->
    (match Env.find fields label with
     | Some ty -> Some ty
     | None -> record_field_type ~checkpoint tail label)
  | _ -> None

and is_closed_record_row ?(checkpoint = fun () -> ()) (row_ty : typ) : bool =
  checkpoint ();
  match resolve_bound_type ~checkpoint row_ty with
  | Record row -> is_closed_record_row ~checkpoint row
  | Row (_fields, tail) -> is_closed_record_row ~checkpoint tail
  | Empty_row -> true
  | _ -> false

and payload_component_types ?checkpoint (payload_ty : typ) : typ list =
  match resolve_bound_type ?checkpoint payload_ty with
  | Variant_payload ts -> ts
  | _ -> raise (Type_error "Invalid variant constructor payload type")

and show_missing_variant_case ?checkpoint (tag : string) (payload_ty : typ) : string =
  match payload_component_types ?checkpoint payload_ty with
  | [] -> Printf.sprintf "`%s" tag
  | components ->
    let placeholders = List.map components ~f:(fun _ -> "_") |> String.concat ~sep:", " in
    Printf.sprintf "`%s(%s)" tag placeholders

and pattern_totally_covers_type
      ?(checkpoint = fun () -> ())
      (pat : pattern)
      (expected_ty : typ)
  : bool
  =
  checkpoint ();
  match pat with
  | PUnit ->
    (match resolve_bound_type ~checkpoint expected_ty with
     | Unit -> true
     | _ -> false)
  | PWildcard | PVar _ -> true
  | PRecord (fields, is_open) ->
    let subpatterns_cover_all =
      List.for_all fields ~f:(fun (label, subpat) ->
        match record_field_type ~checkpoint expected_ty label with
        | Some field_ty -> pattern_totally_covers_type ~checkpoint subpat field_ty
        | None -> false)
    in
    if not subpatterns_cover_all
    then false
    else if is_open
    then true
    else (
      match resolve_bound_type ~checkpoint expected_ty with
      | Record row ->
        let row_fields, _tail = merge_fields ~checkpoint row in
        let field_names = List.map fields ~f:fst |> String.Set.of_list in
        Set.equal
          field_names
          (Env.bindings row_fields |> List.map ~f:fst |> String.Set.of_list)
        && is_closed_record_row ~checkpoint row
      | _ -> false)
  | _ -> false

and variant_constructor_info ?checkpoint (variant_ty : typ)
  : ((string * typ) list * typ) option
  =
  match resolve_bound_type ?checkpoint variant_ty with
  | Variant row ->
    let fields, tail = merge_fields ?checkpoint row in
    Some (Env.bindings fields, tail)
  | _ -> None

and pattern_fully_covers_variant_case
      ?(checkpoint = fun () -> ())
      (pat : pattern)
      ~(tag : string)
      ~(payload_ty : typ)
  : bool
  =
  checkpoint ();
  match pat with
  | PVariant (tag', subpats) when String.equal tag tag' ->
    let payload_tys = payload_component_types ~checkpoint payload_ty in
    List.length subpats = List.length payload_tys
    && List.for_all2_exn subpats payload_tys ~f:(pattern_totally_covers_type ~checkpoint)
  | _ -> false

and is_closed_variant_tail ?checkpoint (tail_ty : typ) : bool =
  match resolve_bound_type ?checkpoint tail_ty with
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

and validate_boolean_match_exhaustiveness
      ?checkpoint
      (scrut_ty : typ)
      (patterns : pattern list)
  : unit
  =
  match resolve_bound_type ?checkpoint scrut_ty with
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
      ?(checkpoint = fun () -> ())
      (scrut_ty : typ)
      (cases : (pattern * Source.span) list)
  : unit
  =
  match resolve_bound_type ~checkpoint scrut_ty with
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
    (match variant_constructor_info ~checkpoint scrut_ty with
     | None -> ()
     | Some (constructors, tail) ->
       let covered_tags = Hash_set.create (module String) in
       let payload_by_tag = String.Map.of_alist_exn constructors in
       let all_known_tags_covered () =
         List.for_all constructors ~f:(fun (tag, _payload_ty) ->
           Hash_set.mem covered_tags tag)
       in
       let closed_tail = is_closed_variant_tail ~checkpoint tail in
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
                     (show_missing_variant_case ~checkpoint tag covered_payload)
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
           if pattern_fully_covers_variant_case ~checkpoint pat ~tag ~payload_ty
           then Hash_set.add covered_tags tag)))
  | _ -> ()

and validate_match_exhaustiveness
      (state : infer_state)
      (scrut_ty : typ)
      (patterns : pattern list)
  : unit
  =
  let poll () = checkpoint state in
  if List.exists patterns ~f:is_catch_all_pattern
  then ()
  else (
    match resolve_bound_type ~checkpoint:poll scrut_ty with
    | Boolean -> validate_boolean_match_exhaustiveness ~checkpoint:poll scrut_ty patterns
    | Variant _ ->
      (match variant_constructor_info ~checkpoint:poll scrut_ty with
       | None -> ()
       | Some (constructors, tail) ->
         let missing_constructor =
           List.find constructors ~f:(fun (tag, payload_ty) ->
             not
               (List.exists patterns ~f:(fun pat ->
                  pattern_fully_covers_variant_case ~checkpoint:poll pat ~tag ~payload_ty)))
         in
         (match missing_constructor with
          | Some (tag, payload_ty) ->
            raise
              (Type_error
                 (Printf.sprintf
                    "Non-exhaustive variant match: missing case '%s'"
                    (show_missing_variant_case ~checkpoint:poll tag payload_ty)))
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
      if
        List.exists patterns ~f:(fun pat ->
          pattern_totally_covers_type ~checkpoint:poll pat scrut_ty)
      then ()
      else raise (Type_error "Non-exhaustive match on record: add '_' arm")
    | _ -> raise (Type_error "Non-exhaustive match: add '_' arm"))

and infer_pattern (state : infer_state) (env : tenv) (pat : pattern) (expected_ty : typ)
  : tenv
  =
  checkpoint state;
  match pat with
  | PUnit ->
    unify state expected_ty Unit;
    env
  | PWildcard -> env
  | PVar x -> add_mono ~checkpoint:(fun () -> checkpoint state) env x expected_ty
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
        let ty = new_var state state.shared.current_lvl in
        let env_acc' = infer_pattern state env_acc pat ty in
        env_acc', (lbl, ty) :: fields_acc)
    in
    let fields_env = Env.of_list (List.rev fields_acc) in
    let tail_row =
      if is_open then new_var state state.shared.current_lvl else Empty_row
    in
    let record_ty = Record (Row (fields_env, tail_row)) in
    unify state expected_ty record_ty;
    env_after
  | PVariant (tag, subpats) ->
    let env_after, sub_tys_rev =
      List.fold subpats ~init:(env, []) ~f:(fun (env_acc, tys) sp ->
        let ty = new_var state state.shared.current_lvl in
        let env_acc' = infer_pattern state env_acc sp ty in
        env_acc', ty :: tys)
    in
    let sub_tys = List.rev sub_tys_rev in
    let case_ty = Variant_payload sub_tys in
    let row_var = new_var state state.shared.current_lvl in
    let variant_ty = Variant (Row (Env.singleton tag case_ty, row_var)) in
    unify state expected_ty variant_ty;
    env_after

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
    ensure_equality_type state lhs_ty;
    Boolean

and join_type (state : infer_state) (lhs : typ) (rhs : typ) : typ =
  (* Keep one compatible result type, including the complete physical record
     shape. A field intersection would unsafely erase exactness constraints. *)
  unify state lhs rhs;
  lhs

and infer_expr_against_expected
      (state : infer_state)
      (env : tenv)
      (types : type_env)
      (expr : expr node)
      (expected_ty : typ)
  : typ
  =
  validate_lambda_binders expr;
  match
    expr.value, resolve_bound_type ~checkpoint:(fun () -> checkpoint state) expected_ty
  with
  | ELambda (params, body), Fun (expected_params, expected_ret)
    when List.length params = List.length expected_params ->
    with_new_level state ~f:(fun () ->
      let env_with_params =
        List.fold2_exn params expected_params ~init:env ~f:(fun env_acc param param_ty ->
          add_mono ~checkpoint:(fun () -> checkpoint state) env_acc param param_ty)
      in
      let body_ty = infer_expr state env_with_params types body in
      unify state body_ty expected_ret;
      expected_ty)
  | ELambdaSlots (params, _slots, body), Fun (expected_params, expected_ret)
    when List.length params = List.length expected_params ->
    with_new_level state ~f:(fun () ->
      let env_with_params =
        List.fold2_exn params expected_params ~init:env ~f:(fun env_acc param param_ty ->
          add_mono ~checkpoint:(fun () -> checkpoint state) env_acc param param_ty)
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
  checkpoint state;
  validate_lambda_binders expr;
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
              let t = new_var state state.shared.current_lvl in
              add_mono ~checkpoint:(fun () -> checkpoint state) env_acc p t, t :: tys_rev)
          in
          let body_ty = infer_expr state env_with_params types body in
          let param_tys = List.rev param_tys_rev in
          Fun (param_tys, body_ty))
      | ELambdaSlots (params, _slots, body) ->
        with_new_level state ~f:(fun () ->
          let env_with_params, param_tys_rev =
            List.fold params ~init:(env, []) ~f:(fun (env_acc, tys_rev) p ->
              let t = new_var state state.shared.current_lvl in
              add_mono ~checkpoint:(fun () -> checkpoint state) env_acc p t, t :: tys_rev)
          in
          let body_ty = infer_expr state env_with_params types body in
          let param_tys = List.rev param_tys_rev in
          Fun (param_tys, body_ty))
      | EApp (fn_expr, arg_exprs) ->
        let fn_ty = infer_expr state env types fn_expr in
        let arg_tys = List.map arg_exprs ~f:(fun arg -> infer_expr state env types arg) in
        let ret_ty = new_var state state.shared.current_lvl in
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
        let field_ty = new_var state state.shared.current_lvl in
        let tail_row = new_var state state.shared.current_lvl in
        unify
          state
          obj_ty
          (Field_receiver
             ( new_var state state.shared.current_lvl
             , Row (Env.singleton lbl field_ty, tail_row) ));
        field_ty
      | EArray elts ->
        let elt_ty = new_var state state.shared.current_lvl in
        List.iter elts ~f:(fun e -> unify state (infer_expr state env types e) elt_ty);
        Array elt_ty
      | EArrayGet (arr, idx) ->
        unify state (infer_expr state env types idx) TInt;
        let elt_ty = new_var state state.shared.current_lvl in
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
        let ty = new_var state state.shared.current_lvl in
        unify state (Ref ty) (infer_expr state env types e);
        ty
      | ESetRef (lhs, rhs) ->
        let lhs_ty = infer_expr state env types lhs in
        let rhs_ty = infer_expr state env types rhs in
        unify state (Ref (new_var state state.shared.current_lvl)) lhs_ty;
        unify state lhs_ty (Ref rhs_ty);
        Unit
      | EVariant (tag, exprs) ->
        let arg_tys = List.map exprs ~f:(fun ex -> infer_expr state env types ex) in
        let case_ty = Variant_payload arg_tys in
        let row_var = new_var state state.shared.current_lvl in
        Variant (Row (Env.singleton tag case_ty, row_var))
      | EAnnot (rhs, type_expr) ->
        let annotated_ty =
          typ_of_type_expr
            ?source:state.source
            ~checkpoint:(fun () -> checkpoint state)
            types
            type_expr
        in
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
        validate_typed_match_redundancy
          ~checkpoint:(fun () -> checkpoint state)
          scrut_ty
          case_spans;
        (match result_ty with
         | Some ty -> ty
         | None -> new_var state state.shared.current_lvl)
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
        validate_typed_match_redundancy
          ~checkpoint:(fun () -> checkpoint state)
          scrut_ty
          case_spans;
        (match result_ty with
         | Some ty -> ty
         | None -> new_var state state.shared.current_lvl)
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
let signature_engine state : Signature_check.Engine.t =
  { instantiate = instantiate state
  ; unify =
      (fun actual expected ->
        try
          unify state actual expected;
          Ok ()
        with
        | Type_error message -> Error message)
  ; fresh_name = (fun () -> gensym state)
  ; checkpoint = (fun () -> checkpoint state)
  }
;;

let check_module_signature state ~values ~types ~outer_types ~signature ~span =
  Chatml.Chatml_checkpoint.protect state.checkpoint ~f:(fun checkpoint ->
    let state = { state with checkpoint } in
    Signature_check.check
      ?source:state.source
      (signature_engine state)
      ~values
      ~types
      ~outer_types
      ~signature
      ~span)
;;

(* An interface never contributes offset keys to the implementation unit's
   annotation tables. Both units still borrow the same inference identities. *)
let check_captured_module_signature state interface ~values ~types ~outer_types =
  Chatml.Chatml_checkpoint.protect state.checkpoint ~f:(fun checkpoint ->
    let state = { state with checkpoint } in
    Signature_check.check_captured
      interface
      (signature_engine state)
      ~values
      ~types
      ~outer_types)
;;

let with_stmt_location (stmt : stmt node) ~f =
  try f stmt.value with
  | Type_error message -> raise (Type_error_with_loc (message, stmt.span))
;;

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
  | SType declaration ->
    ( env
    , infer_type_decl
        ?source:state.source
        ~checkpoint:(fun () -> checkpoint state)
        types
        declaration
    , [] )
  | SExpr e ->
    ignore (infer_expr state env types e);
    env, types, []
  | SModule { name = mname; signature; body = stmts } ->
    (* A module declaration is non-recursive: its new name enters scope only
       after its body has been checked. Functions capture the same lexical scope;
       a deferred read does not make the module's own binding available. *)
    let mod_env_final, mod_types_final, exported_names_rev =
      List.fold
        stmts
        ~init:(env, Type_alias.enter_scope types, [])
        ~f:(fun (env_acc, types_acc, names_rev) stmt_node ->
          let env_next, types_next, stmt_exports =
            with_stmt_location stmt_node ~f:(fun stmt ->
              infer_stmt_with_exports state env_acc stmt types_acc)
          in
          env_next, types_next, List.rev_append stmt_exports names_rev)
    in
    let exported_names = List.rev exported_names_rev in
    let seen = Hash_set.create (module String) in
    let exports_map =
      List.fold exported_names ~init:Env.empty ~f:(fun acc name ->
        if Hash_set.mem seen name
        then acc
        else (
          Hash_set.add seen name;
          match Env.find mod_env_final name with
          | Some ty -> Env.add name ty acc
          | None -> acc))
    in
    let type_names, module_names =
      List.fold stmts ~init:([], []) ~f:(fun (type_names, module_names) stmt ->
        checkpoint state;
        match stmt.value with
        | SType declaration -> declaration.name :: type_names, module_names
        | SModule { name; _ } -> type_names, name :: module_names
        | SLet _ | SLetRec _ | SOpen _ | SExpr _ -> type_names, module_names)
    in
    let exports =
      match
        Type_alias.exports
          mod_types_final
          ~type_names
          ~module_names
          ~checkpoint:(fun () -> checkpoint state)
      with
      | Ok exports -> exports
      | Error message -> raise (Type_error message)
    in
    let exports_map, exports =
      match signature with
      | None -> exports_map, exports
      | Some signature ->
        (match
           check_module_signature
             state
             ~values:exports_map
             ~types:exports
             ~outer_types:types
             ~signature:signature.value
             ~span:signature.span
         with
         | Error diagnostic -> raise (Type_diagnostic diagnostic)
         | Ok checked ->
           Hashtbl.set
             state.module_visibility
             ~key:(span_to_key signature.span)
             ~data:(Signature_check.visibility checked);
           Signature_check.values checked, Signature_check.types checked)
    in
    let module_typ = Module (Row (exports_map, Empty_row)) in
    ( add_generalized state env mname module_typ
    , Type_alias.add_module types ~name:mname ~exports
    , [ mname ] )
  | SOpen mname ->
    (* Opening introduces aliases for the existing member schemes. Instantiating
       the whole module here would share one monomorphic instance between all
       subsequent uses of a polymorphic export. *)
    (match lookup_scheme env mname with
     | Module row ->
       let fields, tail = merge_fields ~checkpoint:(fun () -> checkpoint state) row in
       (match tail with
        | Empty_row | Var { contents = Free _ } -> ()
        | _ -> ());
       let types =
         match
           Type_alias.open_module types ~name:mname ~checkpoint:(fun () ->
             checkpoint state)
         with
         | Ok types -> types
         | Error message -> raise (Type_error message)
       in
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

    - [Ok checked], where [checked] owns a copied span table for this unit. Its
      type values deliberately retain shared inference/weak-cell identities;
      this is not a deeply immutable semantic snapshot (OCH-127 owns that); or
    - [Error diagnostic], where [diagnostic] identifies the first typing
      error and never permits evaluation to proceed.

    The snapshot produced on success is the only source of type information
    consulted by the resolver in production. *)
let check_program_with_surface
      ?checkpoint:compiler_checkpoint
      ?source
      ?(required_bindings : (string * Builtin_spec.ty) list = [])
      (surface : Builtin_surface.surface)
      (prog : program)
  : (checked_program, diagnostic) result
  =
  let source_matches =
    match source with
    | None -> true
    | Some source -> String.equal (Chatml_source.contents source) prog.source_text
  in
  if not source_matches
  then Error (Chatml_diagnostic.create "Program does not match captured source bytes")
  else (
    let compiler_checkpoint = Option.value compiler_checkpoint ~default:(fun () -> ()) in
    Chatml.Chatml_checkpoint.protect compiler_checkpoint ~f:(fun compiler_checkpoint ->
      let state = create_state ~checkpoint:compiler_checkpoint ?source () in
      try
        let env =
          init_env_with_surface ~checkpoint:(fun () -> checkpoint state) surface
        in
        let types : type_env =
          init_types_with_surface ~checkpoint:(fun () -> checkpoint state) surface
        in
        let final_env, _final_types =
          List.fold
            prog.stmts
            ~init:(env, types)
            ~f:(fun (env_acc, types_acc) stmt_node ->
              with_stmt_location stmt_node ~f:(fun stmt ->
                infer_stmt state env_acc types_acc stmt))
        in
        (* Instantiate the entire contract together: repeated type variables relate
       bindings such as initial_state and on_event. Checking inferred bindings
       avoids evaluating initializers or introducing shadowable source wrappers. *)
        (match
           List.find_a_dup (List.map required_bindings ~f:fst) ~compare:String.compare
         with
         | Some name -> raise (Type_error ("Duplicate required binding '" ^ name ^ "'"))
         | None -> ());
        let required_types =
          Tuple
            (List.map required_bindings ~f:(fun (_, ty) ->
               typ_of_builtin_ty ~checkpoint:(fun () -> checkpoint state) ty))
          |> instantiate state
        in
        (match required_types with
         | Tuple types ->
           List.iter2_exn required_bindings types ~f:(fun (name, _) expected ->
             try unify state (lookup state final_env name) expected with
             | Type_error message ->
               raise (Type_error ("Invalid entrypoint '" ^ name ^ "': " ^ message)))
         | _ -> assert false);
        Ok
          { source = state.source
          ; span_types = Hashtbl.copy state.span_types
          ; module_visibility = Hashtbl.copy state.module_visibility
          }
      with
      | Type_diagnostic diagnostic -> Error diagnostic
      | Type_error msg -> Error (Chatml_diagnostic.create msg)
      | Type_error_with_loc (msg, span) ->
        let diagnostic = Chatml_diagnostic.create ~span msg in
        let diagnostic =
          match state.source with
          | None -> diagnostic
          | Some source ->
            Chatml.Chatml_owned_source.attach
              source
              diagnostic
              ~checkpoint:state.checkpoint
        in
        Error diagnostic))
;;

(** Captured parser units keep source/AST pairing invariant. Compilation does
    not evaluate initializers; failures retain their exact captured source. *)
let check_captured_program_with_surface ?checkpoint ?required_bindings surface unit =
  check_program_with_surface
    ?checkpoint
    ?required_bindings
    ~source:(Chatml.Chatml_parse.Unit.source unit)
    surface
    (Chatml.Chatml_parse.Unit.program unit)
;;

let check_program (prog : program) : (checked_program, diagnostic) result =
  check_program_with_surface Builtin_surface.core_surface prog
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
