open! Core

(** Maximum retained source positions across a window ledger. *)
val max_members : int

(** Host-qualified identity. [generation] identifies the admitted source lifetime;
    the opaque window ID remains independent of session and storage revisions. *)
module Scope : sig
  type t [@@deriving compare, equal, sexp_of]

  val create
    :  session:Session_ref.t
    -> generation:int
    -> window:Id.Context_window.t
    -> (t, Protocol_error.t) result

  val session : t -> Session_ref.t
  val generation : t -> int
  val window : t -> Id.Context_window.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Citation : sig
  type t [@@deriving equal, sexp_of]

  val create
    :  Scope.t
    -> occurrence:History.Id.t
    -> version:Recovery_item_version.t
    -> (t, Protocol_error.t) result

  val scope : t -> Scope.t
  val occurrence : t -> History.Id.t
  val version : t -> Recovery_item_version.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

(** Immutable ordered membership, independent of the physical location of each
    payload. Construction rejects foreign citations and duplicate exact members.
    Membership gives no permission to read the corresponding bytes. *)
type t

val create : Scope.t -> Citation.t list -> (t, Protocol_error.t) result
val scope : t -> Scope.t
val length : t -> int
val at : t -> ordinal:int -> Citation.t option
val ordinal : t -> Citation.t -> int option

(** The wire shape stores the common scope once. The member count is bounded
    to 65,536 and the encoded manifest to 8 MiB. Ordinals are zero-based. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Protocol_error.t) result
