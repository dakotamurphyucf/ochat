open Core
module Captured = Chatml_source
module Module_id = Chatml_prepared_program.Module_id

(** Compiler-private metadata identities. Creation belongs to the Recorder;
    environments retain these values, never a recorder/callback or inference
    engine. All IDs are invocation-affine; they are not public snapshot IDs. *)
module Symbol_id : sig
  type t [@@deriving equal, sexp_of]
end

module Group_id : sig
  (** One actual scheme/contract quantification boundary, not a printed name. *)
  type t [@@deriving equal, sexp_of]
end

module Root_id : sig
  type t [@@deriving equal, sexp_of]
end

module Context_id : sig
  type t [@@deriving equal, sexp_of]
end

module Namespace : sig
  type t =
    | Value
    | Type
    | Type_parameter
  [@@deriving equal, sexp_of]
end

module Kind : sig
  type t =
    | Value
    | Parameter
    | Module
    | Type_alias
    | Type_parameter
  [@@deriving equal, sexp_of]
end

module Definition : sig
  type t =
    | Authored of Captured.Location.t
    | Implicit_specification_parameter of Captured.Location.t
    | Host of
        { contract : Captured.Digest.t
        ; member_path : string list
        }
end

(** Metadata of a real lexical binding. Namespaces store this next to their
    existing raw schemes/alias definitions. A new shadow has a new symbol;
    an open of a known public member reuses its symbol/group. An introduction
    location is a separate use/transition fact, not a replacement definition. *)
type t

val name : t -> string
val symbol : t -> Symbol_id.t
val kind : t -> Kind.t
val definition : t -> Definition.t
val group : t -> Group_id.t option
val scheme : t -> Root_id.t option

module Members : sig
  (** Exact known public module members only, with separate namespaces. Contains
      immutable metadata, not rows/rawtypes. Unknown structural/dynamic module
      members have no invented nominal target. A known first-class alias of an
      existing module can preserve this same metadata without affecting existing
      configured runtime provenance or resolver lowering. *)
  type binding = t

  type t

  val find
    :  t
    -> namespace:Namespace.t
    -> name:string
    -> checkpoint:(unit -> unit)
    -> binding option

  val values : t -> (string * binding) list
  val types : t -> (string * binding) list
end

val members : t -> Members.t option

(** Compiler construction port. These values are not certified public query
    data; only the invocation recorder owns allocation and admission. *)
module Private : sig
  module Owner : sig
    type t

    val create : unit -> t
    val equal : t -> t -> bool
  end

  val symbol : Owner.t -> int -> Symbol_id.t
  val group : Owner.t -> int -> Group_id.t
  val root : Owner.t -> int -> Root_id.t
  val context : Owner.t -> int -> Context_id.t
  val owns_symbol : Owner.t -> Symbol_id.t -> bool
  val owns_group : Owner.t -> Group_id.t -> bool
  val owns_root : Owner.t -> Root_id.t -> bool
  val owns_context : Owner.t -> Context_id.t -> bool
  val root_index : Root_id.t -> int
  val symbol_index : Symbol_id.t -> int
  val context_index : Context_id.t -> int
  val equal_name : string -> string -> checkpoint:(unit -> unit) -> bool

  val create
    :  Symbol_id.t
    -> name:string
    -> kind:Kind.t
    -> definition:Definition.t
    -> scheme:(Group_id.t * Root_id.t) option
    -> members:Members.t option
    -> t

  val with_scheme : t -> scheme:Group_id.t * Root_id.t -> t
  val with_members : t -> Members.t -> t

  val public_projection
    :  t
    -> scheme:Group_id.t * Root_id.t
    -> members:Members.t option
    -> t

  val members : values:(string * t) list -> types:(string * t) list -> Members.t
end
