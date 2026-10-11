open! Core

(** Indexed workspace definitions, confined to the creating Eio domain.
    Configured roots remain coordinates, never native authority. *)

type t

val create : Workspace_definition.t list -> (t, Agent_store.Store_error.t) result

(** Successful compatibility installation retains its existing behavior. Rejects
    a sealed/uncertain/foreign-domain owner before changing published pointers;
    rejected legacy admission raises [Invalid_argument]. *)
val install : t -> replacement:t -> unit

val definitions : t -> Workspace_definition.t list
val find : t -> Agent_protocol.Id.Workspace_definition.t -> Workspace_definition.t option
val find_by_name : t -> string -> Workspace_definition.t option

module Seal : sig
  module Error : sig
    type t =
      | Busy
      | Publication_uncertain
      | Foreign_domain
      | Stale_preparation
      | Aborted
      | Committed
    [@@deriving equal, sexp_of]
  end

  type t

  val abort : t -> (unit, Error.t) Result.t
  val mark_uncertain : t -> (unit, Error.t) Result.t
  val commit : t -> (unit, Error.t) Result.t
end

(** Snapshot the already-indexed replacement, then reserve the SAME destination
    generation. No replacement mutation can alter that immutable snapshot.
    Cooperatively checked before admission; no callback retained in Seal.
    Commit/abort/uncertain are same-domain nonyielding/nonallocating pointer/status
    operations with precomputed result carriers. Install rejects while sealed or
    uncertain, before changing any published pointer. Other service atomicity and
    external durability remain the host coordinator's responsibility. *)
val seal
  :  t
  -> replacement:t
  -> checkpoint:(unit -> unit)
  -> (Seal.t, Seal.Error.t) Result.t
