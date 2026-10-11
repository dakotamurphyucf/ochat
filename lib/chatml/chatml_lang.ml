(** ChatML language core.

    This module defines:

    {ol
    {- The surface AST produced by the parser and consumed by the
       typechecker.}
    {- The resolved AST produced by the resolver and consumed by the
       evaluator.}
    {- Runtime value and environment types shared by the evaluator and
       builtin libraries.}
    {- Small helper functions for diagnostics, environments, and
       pattern matching.}}

    Evaluation itself lives in {!module:Chatml_eval}.  Keeping the
    evaluator out of this module makes the phase distinction explicit and
    avoids dependency cycles between the parser, typechecker, resolver,
    and interpreter.
*)

open Core

(***************************************************************************)
(* 1) AST Types                                                            *)
(***************************************************************************)

type 'a node =
  { value : 'a
  ; span : Source.span
  }
[@@deriving sexp]

type pattern =
  | PUnit
  | PWildcard
  | PVar of string
  | PInt of int
  | PBool of bool
  | PFloat of float
  | PString of string
  | PVariant of string * pattern list
  | PRecord of (string * pattern) list * bool
  | PTuple of pattern Chatml_tuple.t
[@@deriving sexp, compare]

type type_expr = Chatml_type_expr.t [@@deriving sexp]

type var_loc =
  { depth : int
  ; index : int
  ; slot : Frame_env.packed_slot
  }
[@@deriving sexp]

type match_case =
  { pat : pattern
  ; pat_span : Source.span
  ; rhs : expr node
  }
[@@deriving sexp]

and match_case_slots =
  { pat : pattern
  ; pat_span : Source.span
  ; slots : Frame_env.packed_slot list
  ; rhs : expr node
  }
[@@deriving sexp]

and resolved_match_case =
  { pat : pattern
  ; pat_span : Source.span
  ; slots : Frame_env.packed_slot list
  ; rhs : resolved_expr node
  }
[@@deriving sexp]

and unary_prim =
  | UNegInt
  | UNegFloat
[@@deriving sexp]

and binary_prim =
  | BIntAdd
  | BIntSub
  | BIntMul
  | BIntDiv
  | BFloatAdd
  | BFloatSub
  | BFloatMul
  | BFloatDiv
  | BStringConcat
  | BIntLt
  | BIntGt
  | BIntLe
  | BIntGe
  | BFloatLt
  | BFloatGt
  | BFloatLe
  | BFloatGe
  | BEq
  | BNeq
[@@deriving sexp]

and expr =
  | EUnit
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EString of string
  | EVar of string
  | EVarLoc of var_loc
  | EPrim1 of unary_prim * expr node
  | EPrim2 of binary_prim * expr node * expr node
  | ELambda of Chatml_binding_pattern.t list * expr node
  | ELambdaSlots of Chatml_binding_pattern.t list * Frame_env.packed_slot list * expr node
  | EApp of expr node * expr node list
  | EIf of expr node * expr node * expr node
  | EWhile of expr node * expr node
  | ELetIn of Chatml_binding_pattern.t * expr node * expr node
  | ELetRec of (string * expr node) list * expr node
  | ELetBlock of (Chatml_binding_pattern.t * expr node) list * expr node
  | ELetBlockSlots of
      (Chatml_binding_pattern.t * expr node) list * Frame_env.packed_slot list * expr node
  | ELetRecSlots of (string * expr node) list * Frame_env.packed_slot list * expr node
  | EMatch of expr node * match_case list
  | EMatchSlots of expr node * match_case_slots list
  | ERecord of (string * expr node) list
  | EFieldGet of expr node * string
  | EVariant of string * expr node list
  | EArray of expr node list
  | ETuple of expr node Chatml_tuple.t
  | EArrayGet of expr node * expr node
  | EArraySet of expr node * expr node * expr node
  | ERef of expr node
  | ESetRef of expr node * expr node
  | ESequence of expr node * expr node
  | EDeref of expr node
  | ERecordExtend of expr node * (string * expr node) list
  | EAnnot of expr node * type_expr
[@@deriving sexp]

and resolved_expr =
  | REUnit
  | REInt of int
  | REBool of bool
  | REFloat of float
  | REString of string
  | REVarGlobal of string
  | REConfigured_read of Chatml_module_reference.t
  | REVarLoc of var_loc
  | REPrim1 of unary_prim * resolved_expr node
  | REPrim2 of binary_prim * resolved_expr node * resolved_expr node
  | RELambda of
      Chatml_binding_pattern.t list * Frame_env.packed_slot list * resolved_expr node
  | REApp of resolved_expr node * resolved_expr node list
  | REIf of resolved_expr node * resolved_expr node * resolved_expr node
  | REWhile of resolved_expr node * resolved_expr node
  | RELetBlock of
      (Chatml_binding_pattern.t * resolved_expr node) list
      * Frame_env.packed_slot list
      * resolved_expr node
  | RELetRec of
      (string * resolved_expr node) list * Frame_env.packed_slot list * resolved_expr node
  | REMatch of resolved_expr node * resolved_match_case list
  | RERecord of (string * resolved_expr node) list
  | REFieldGet of resolved_expr node * string
  | REVariant of string * resolved_expr node list
  | REArray of resolved_expr node list
  | RETuple of resolved_expr node Chatml_tuple.t
  | REArrayGet of resolved_expr node * resolved_expr node
  | REArraySet of resolved_expr node * resolved_expr node * resolved_expr node
  | RERef of resolved_expr node
  | RESetRef of resolved_expr node * resolved_expr node
  | RESequence of resolved_expr node * resolved_expr node
  | REDeref of resolved_expr node
  | RERecordExtend of resolved_expr node * (string * resolved_expr node) list
[@@deriving sexp]

type stmt =
  | SLet of Chatml_binding_pattern.t * expr node
  | SLetRec of (string * expr node) list
  | SType of Chatml_type_expr.Declaration.t
  | SModule of
      { name : string
      ; signature : Chatml_module_signature.t node option
      ; body : stmt node list
      }
  | SOpen of string
  | SExpr of expr node
[@@deriving sexp]

type resolved_stmt =
  | RSLet of Chatml_binding_pattern.t * resolved_expr node
  | RSLetRec of (string * resolved_expr node) list
  | RSModule of
      { name : string
      ; visibility : Chatml_module_visibility.t option
      ; body : resolved_stmt node list
      }
  | RSOpen of string
  | RSExpr of resolved_expr node
[@@deriving sexp]

type stmt_node = stmt node [@@deriving sexp]
type resolved_stmt_node = resolved_stmt node [@@deriving sexp]

type program =
  { stmts : stmt_node list
  ; source_text : string
  }
[@@deriving sexp]

type resolved_program =
  { stmts : resolved_stmt_node list
  ; source_text : string
  }
[@@deriving sexp]

(***************************************************************************)
(* Runtime diagnostics                                                     *)
(***************************************************************************)

type runtime_error = Chatml_diagnostic.t

exception Runtime_error of runtime_error

let raise_runtime_error ?span ?site message =
  raise (Runtime_error (Chatml_diagnostic.create ?span ?site message))
;;

let format_runtime_error source_text err =
  Chatml_diagnostic.render_legacy
    err
    ~source:source_text
    ~label:"Runtime error"
    ~checkpoint:(fun () -> ())
;;

(***************************************************************************)
(* 2) Runtime Value Types                                                  *)
(***************************************************************************)

type execution_control =
  { checkpoint : unit -> unit
  ; diagnostic_checkpoint : unit -> unit
  ; allocate : int -> unit
  ; allocate_array : int -> unit
  ; before_builtin : name:string -> value list -> unit
  ; check_value : value -> unit
  ; before_json_import : Jsonaf.t -> unit
  ; before_json_export : value -> unit
  ; before_effect : name:string -> spawned:bool -> unit
  ; after_effect : value -> unit
  }

and configured_modules =
  | Unavailable
  | Completed of (Chatml_source.Id.t, value, Chatml_source.Id.comparator_witness) Map.t

and value =
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string
  | VVariant of string * value list
  | VRecord of value String.Map.t
  | VArray of value array
  | VTuple of value Chatml_tuple.t
  | VRef of value ref
  | VClosure of clos
  | VModule of env
  | VUnit
  | VBuiltin of (value list -> value)
  | VTask of task

and eff =
  { op : string
  ; args : value list
  }

and task =
  { site : Chatml_diagnostic.Site.t option
  ; step : task_step
  }

and task_step =
  | TPure of value
  | TBind of task * value
  | TMap of task * value
  | TArray of Chatml_task_array_mode.t * value array * value
  | TFail of string
  | TCatch of task * value
  | TPerform of eff
  | TSpawn of eff

and clos =
  { params : Chatml_binding_pattern.t list
  ; body : resolved_expr node
  ; env : env
  ; frames : Frame_env.env
  ; param_slots : Frame_env.packed_slot list
  }

and cell = value ref

and env =
  { bindings : (string, cell) Hashtbl.t
  ; control : execution_control option
  ; source : Chatml_source.t option
  ; configured_modules : configured_modules
  }

(***************************************************************************)
(* 3) Environment Helpers                                                  *)
(***************************************************************************)

module Task = struct
  type t = task

  let create step = { site = None; step }
  let site t = t.site
  let step t = t.step

  let with_fallback t site =
    match t.site, site with
    | Some _, _ | None, None -> t
    | None, Some site -> { t with site = Some site }
  ;;
end

module Configured_modules = struct
  type t = configured_modules

  module Error = struct
    type t =
      | Registry_unavailable
      | Module_unavailable of Chatml_source.Id.t
      | Member_unavailable of string
      | Invalid_public_module
      | Duplicate_module of Chatml_source.Id.t
  end

  let unavailable = Unavailable
  let empty = Completed (Map.empty (module Chatml_source.Id))

  let add t ~implementation ~public_module =
    match t, public_module with
    | Unavailable, _ -> Error Error.Registry_unavailable
    | Completed modules, VModule _ ->
      if Map.mem modules implementation
      then Error (Error.Duplicate_module implementation)
      else Ok (Completed (Map.set modules ~key:implementation ~data:public_module))
    | ( Completed _
      , ( VInt _
        | VBool _
        | VFloat _
        | VString _
        | VVariant _
        | VRecord _
        | VArray _
        | VTuple _
        | VRef _
        | VClosure _
        | VUnit
        | VBuiltin _
        | VTask _ ) ) -> Error Error.Invalid_public_module
  ;;

  let read t reference =
    let rec member value = function
      | [] -> Ok value
      | name :: rest ->
        (match value with
         | VModule env ->
           (match Hashtbl.find env.bindings name with
            | None -> Error (Error.Member_unavailable name)
            | Some cell -> member !cell rest)
         | VInt _
         | VBool _
         | VFloat _
         | VString _
         | VVariant _
         | VRecord _
         | VArray _
         | VTuple _
         | VRef _
         | VClosure _
         | VUnit
         | VBuiltin _
         | VTask _ -> Error Error.Invalid_public_module)
    in
    match t with
    | Unavailable -> Error Error.Registry_unavailable
    | Completed modules ->
      let implementation = Chatml_module_reference.implementation reference in
      (match Map.find modules implementation with
       | None -> Error (Error.Module_unavailable implementation)
       | Some value -> member value (Chatml_module_reference.member_path reference))
  ;;
end

let create_env ?control ?source ?(configured_modules = Configured_modules.unavailable) ()
  : env
  =
  { bindings = Hashtbl.create (module String); control; source; configured_modules }
;;

let copy_env (parent : env) : env =
  Option.iter parent.control ~f:(fun control ->
    control.allocate (32 * Hashtbl.length parent.bindings));
  { parent with bindings = Hashtbl.copy parent.bindings }
;;

let find_var_cell (e : env) (x : string) : cell option = Hashtbl.find e.bindings x

let find_var (e : env) (x : string) : value option =
  Hashtbl.find e.bindings x |> Option.map ~f:(fun cell -> !cell)
;;

let define_var (e : env) (x : string) (v : value) : unit =
  Hashtbl.set e.bindings ~key:x ~data:(ref v)
;;

let update_var (e : env) (x : string) (v : value) : unit =
  match find_var_cell e x with
  | Some cell -> cell := v
  | None -> define_var e x v
;;

let set_var (e : env) (x : string) (v : value) : unit = define_var e x v

let rec equal_value (lhs : value) (rhs : value) : bool =
  match lhs, rhs with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> Float.equal x y
  | VBool x, VBool y -> Bool.equal x y
  | VString x, VString y -> String.equal x y
  | VUnit, VUnit -> true
  | VVariant (tag_x, vals_x), VVariant (tag_y, vals_y) ->
    String.equal tag_x tag_y
    && List.length vals_x = List.length vals_y
    && List.for_all2_exn vals_x vals_y ~f:equal_value
  | VRecord fields_x, VRecord fields_y -> Map.equal equal_value fields_x fields_y
  | VTuple values_x, VTuple values_y ->
    List.equal equal_value (Chatml_tuple.to_list values_x) (Chatml_tuple.to_list values_y)
  | VArray arr_x, VArray arr_y -> phys_equal arr_x arr_y
  | VRef ref_x, VRef ref_y -> phys_equal ref_x ref_y
  | VClosure clos_x, VClosure clos_y -> phys_equal clos_x clos_y
  | VModule env_x, VModule env_y -> phys_equal env_x env_y
  | VBuiltin fn_x, VBuiltin fn_y -> phys_equal fn_x fn_y
  | VTask t_x, VTask t_y ->
    (* Metadata copies preserve the identity of the original semantic task. *)
    phys_equal t_x.step t_y.step
  | _ -> false
;;

(***************************************************************************)
(* 4) Pattern Matching                                                     *)
(***************************************************************************)

let rec match_pattern (v : value) (p : pattern) : (string * value) list option =
  match p, v with
  | PUnit, VUnit -> Some []
  | PWildcard, _ -> Some []
  | PVar x, _ -> Some [ x, v ]
  | PInt i, VInt j when i = j -> Some []
  | PBool b, VBool c when Bool.equal b c -> Some []
  | PFloat f, VFloat g when Float.equal f g -> Some []
  | PString s, VString t when String.equal s t -> Some []
  | PTuple patterns, VTuple values ->
    let rec combine patterns values acc =
      match patterns, values with
      | [], [] -> Some (List.rev acc)
      | pattern :: patterns, value :: values ->
        (match match_pattern value pattern with
         | None -> None
         | Some bindings -> combine patterns values (List.rev_append bindings acc))
      | [], _ :: _ | _ :: _, [] -> None
    in
    combine (Chatml_tuple.to_list patterns) (Chatml_tuple.to_list values) []
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
  | PRecord (fields, is_open), VRecord fields_map ->
    let rec match_fields fl acc =
      match fl with
      | [] -> Some acc
      | (lbl, pat) :: tl ->
        (match Map.find fields_map lbl with
         | None -> None
         | Some v_f ->
           (match match_pattern v_f pat with
            | None -> None
            | Some binds -> match_fields tl (acc @ binds)))
    in
    (match match_fields fields [] with
     | None -> None
     | Some binds ->
       if is_open
       then Some binds
       else if Map.length fields_map = List.length fields
       then Some binds
       else None)
  | _ -> None
;;

let collect_pattern_vars ?(checkpoint = fun () -> ()) (pat : pattern) : string list =
  let rec aux acc p =
    checkpoint ();
    match p with
    | PUnit -> acc
    | PVar x -> x :: acc
    | PWildcard | PInt _ | PBool _ | PFloat _ | PString _ -> acc
    | PVariant (_, ps) -> List.fold ps ~init:acc ~f:aux
    | PTuple patterns -> Chatml_tuple.fold patterns ~init:acc ~f:aux
    | PRecord (fields, _open) -> List.fold fields ~init:acc ~f:(fun ac (_, p) -> aux ac p)
  in
  List.rev (aux [] pat)
;;
