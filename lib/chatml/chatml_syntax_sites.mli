open Core

(** Invocation-owned identifier coordinates. Equality includes a fresh immutable
    owner witness; these IDs are neither persisted nor runtime identities. *)
module Id : sig
  type t [@@deriving equal, sexp_of]
end

module Role : sig
  type t =
    | Value_reference
    | Value_binder
    | Module_binder
    | Module_reference
    | Type_binder
    | Type_parameter_binder
    | Type_reference
    | Signature_value_binder
    | Field_label
    | Variant_tag
    | Constraint_name
    | Constraint_parameter_reference
  [@@deriving equal, sexp_of]
end

module Limits : sig
  (** Positive ceilings for retained sites and spelling bytes. These bound
      metadata, not the complete parser AST, source bytes or input depth. *)
  type t

  val create : max_sites:int -> max_name_bytes:int -> t Or_error.t
end

module Error : sig
  type t =
    | Site_limit
    | Name_byte_limit
    | Accounting_overflow
    | Invalid_location
  [@@deriving equal, sexp_of]
end

module Site : sig
  type t

  val id : t -> Id.t
  val name : t -> string
  val role : t -> Role.t

  (** Exact identifier token, including an authored apostrophe/backtick. *)
  val location : t -> Chatml_source.Location.t
end

(** Immutable table paired with exact captured bytes. No callback, collector or
    mutable staging escapes. Foreign IDs return None before direct indexing.
    Sites follow parser emission order, not a promised lexical sort order. *)
type t

val source : t -> Chatml_source.t
val find : t -> Id.t -> Site.t option

module At_span : sig
  type t =
    | Missing
    | Unique of Site.t
    | Ambiguous
end

(** Exact role/range lookup within this source owner. Duplicate rows produce
    Ambiguous, never a guessed first match. Built only on the enabled parser
    path, with admitted row counts and cooperative publication. *)
val at_span : t -> role:Role.t -> checkpoint:(unit -> unit) -> Source.span -> At_span.t

val sites : t -> Site.t list
val site_count : t -> int
val name_bytes : t -> int

(** Compiler parser parameter. The disabled grammar path does not call [add] or
    allocate a span/site row. Enabled admission occurs before row/ID allocation. *)
module type Sink = sig
  val enabled : bool
  val add : Role.t -> name:string -> span:Source.span -> Id.t

  (** Parser-owned top-level program reductions. Ordinary site collection is a
      no-op; a dedicated prefix observer can retain one completed boundary. *)
  val completed_program_statement : end_byte:int -> unit

  val completed_program_separator : end_byte:int -> unit
end

(** Shared parameter for the legacy parser and fixed generated token module.
    [add] is unreachable: grammar actions guard [enabled] before calling it. *)
module Disabled : Sink

(** Invocation-local parser collection. The sink is borrowed only during [f];
    callers must not retain it. A failed parse, admission or checkpoint publishes
    no table. Checkpoint exceptions preserve identity/backtrace. This does not
    parse source text or infer anything; the real parser calls the supplied sink. *)
val collect
  :  Chatml_source.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> f:((module Sink) -> 'a)
  -> ('a * t, Error.t) Result.t
