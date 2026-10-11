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

(** Source names carry optional exact parser coordinates. Resolved patterns
    carry strings only; their runtime comparison and matching retain no IDs. *)
type 'name pattern_desc =
  | PUnit
  | PWildcard
  | PVar of 'name
  | PInt of int
  | PBool of bool
  | PFloat of float
  | PString of string
  | PVariant of 'name * 'name pattern_desc list
  | PRecord of ('name * 'name pattern_desc) list * bool
  | PTuple of 'name pattern_desc Chatml_tuple.t
[@@deriving sexp, compare]

type pattern = Chatml_syntax_name.t pattern_desc [@@deriving sexp, compare]
type resolved_pattern = string pattern_desc [@@deriving sexp, compare]
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
  { pat : resolved_pattern
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
  | ELetRec of (Chatml_syntax_name.t * expr node) list * expr node
  | ELetBlock of (Chatml_binding_pattern.t * expr node) list * expr node
  | ELetBlockSlots of
      (Chatml_binding_pattern.t * expr node) list * Frame_env.packed_slot list * expr node
  | ELetRecSlots of
      (Chatml_syntax_name.t * expr node) list * Frame_env.packed_slot list * expr node
  | EMatch of expr node * match_case list
  | EMatchSlots of expr node * match_case_slots list
  | ERecord of (Chatml_syntax_name.t * expr node) list
  | EFieldGet of expr node * Chatml_syntax_name.t
  | EVariant of Chatml_syntax_name.t * expr node list
  | EArray of expr node list
  | ETuple of expr node Chatml_tuple.t
  | EArrayGet of expr node * expr node
  | EArraySet of expr node * expr node * expr node
  | ERef of expr node
  | ESetRef of expr node * expr node
  | ESequence of expr node * expr node
  | EDeref of expr node
  | ERecordExtend of expr node * (Chatml_syntax_name.t * expr node) list
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
  | SLetRec of (Chatml_syntax_name.t * expr node) list
  | SType of Chatml_type_expr.Declaration.t
  | SModule of
      { name : Chatml_syntax_name.t
      ; signature : Chatml_module_signature.t node option
      ; body : stmt node list
      }
  | SOpen of Chatml_syntax_name.t
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

type runtime_error = Chatml_diagnostic.t

exception Runtime_error of runtime_error

(** Raise only an ordinary script failure. Existing execution-control and
    cancellation exceptions are independent and must propagate unchanged. *)
val raise_runtime_error
  :  ?span:Source.span
  -> ?site:Chatml_diagnostic.Site.t
  -> string
  -> 'a

val format_runtime_error : string -> runtime_error -> string

type configured_modules

type execution_control =
  { checkpoint : unit -> unit
  ; diagnostic_checkpoint : unit -> unit
    (** Poll cancellation, deadlines, budget state and active scope lifetime
        without consuming expression/task execution fuel. Source/site values
        never retain this callback. *)
  ; allocate : int -> unit
  ; allocate_array : int -> unit
    (** Reserve an array of the given element count under the current host limits.
        This checks the array size and charges its allocation before creation. *)
  ; before_builtin : name:string -> value list -> unit
  ; check_value : value -> unit
  ; before_json_import : Jsonaf.t -> unit
  ; before_json_export : value -> unit
  ; before_effect : name:string -> spawned:bool -> unit
  ; after_effect : value -> unit
  }

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

and task = private
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

(** [params] determines call arity. [param_slots] has one slot per named leaf,
    in parameter and depth-first binder order; wildcards consume no slots. *)
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

(** Execution-owned persistent maps of completed public modules. Map structure is
    immutable; values/cells belong to one execution/session. No lazy initializer,
    inventory, configured-name fallback or callback authority. *)
module Configured_modules : sig
  type t = configured_modules

  module Error : sig
    type t =
      | Registry_unavailable
      | Module_unavailable of Chatml_source.Id.t
      | Member_unavailable of string
      | Invalid_public_module
      | Duplicate_module of Chatml_source.Id.t
  end

  val unavailable : t
  val empty : t

  (** Reject duplicate identity and non-module values; share the module object.
      This shape check does not attest checked visibility or grant authority. *)
  val add
    :  t
    -> implementation:Chatml_source.Id.t
    -> public_module:value
    -> (t, Error.t) Result.t

  val read : t -> Chatml_module_reference.t -> (value, Error.t) Result.t
end

(** Task origin is metadata outside the semantic step. Creating/attaching a
    site adds no execution/fuel/value-depth step. Copies preserve semantic task
    identity for [equal_value]; owned nested tasks retain their own source. *)
module Task : sig
  type t = task

  val create : task_step -> t
  val site : t -> Chatml_diagnostic.Site.t option
  val step : t -> task_step
  val with_fallback : t -> Chatml_diagnostic.Site.t option -> t
end

(** Environments own their mutable binding table and immutable optional source.
    A copy shares binding cells/control/source while copying the table. Closure
    bodies and module exports retain the defining unit's captured bytes only,
    never a complete prepared inventory or mutable current-origin field. *)
val create_env
  :  ?control:execution_control
  -> ?source:Chatml_source.t
  -> ?configured_modules:Configured_modules.t
  -> unit
  -> env

val copy_env : env -> env
val find_var_cell : env -> string -> cell option
val find_var : env -> string -> value option
val define_var : env -> string -> value -> unit
val update_var : env -> string -> value -> unit
val set_var : env -> string -> value -> unit
val equal_value : value -> value -> bool
val match_pattern : value -> resolved_pattern -> (string * value) list option
val collect_pattern_vars : ?checkpoint:(unit -> unit) -> pattern -> string list

val collect_resolved_pattern_vars
  :  ?checkpoint:(unit -> unit)
  -> resolved_pattern
  -> string list
