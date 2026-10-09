open! Core

(** Host-qualified lifecycle requests, independent of actor/store implementation.
    Restoring retains all session/workspace content but grants no execution. *)
module Revision : sig
  type t [@@deriving compare, equal, sexp]

  val zero : t
  val one : t
  val of_int64 : int64 -> (t, Protocol_error.t) Result.t
  val to_int64 : t -> int64
  val succ : t -> (t, Protocol_error.t) Result.t
end

module Expected : sig
  type t [@@deriving equal, sexp]

  val create
    :  reference:Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> lifecycle_revision:Revision.t
    -> (t, Protocol_error.t) Result.t

  val reference : t -> Session_ref.t
  val generation : t -> int
  val session_revision : t -> int64
  val lifecycle_revision : t -> Revision.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Result.t
end

module Request : sig
  (** Method determines Restore or Resume. The identity/generation/revisions and
      original idempotency key are part of the immutable original request digest.
      No attachment is required for an archived unloaded session. *)
  type t [@@deriving sexp]

  val create : expected:Expected.t -> idempotency_key:Idempotency_key.t -> t
  val expected : t -> Expected.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Result.t
end

module Result : sig
  module Status : sig
    type t =
      | Active
      | Archived
      | Removed
    [@@deriving equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
  end

  module Admission : sig
    type t =
      | Automatic
      | Explicit_resume_required
    [@@deriving equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
  end

  module Action : sig
    type t =
      | Archive
      | Restore
      | Resume
      | Remove
    [@@deriving equal, sexp]
  end

  module Disposition : sig
    type t =
      | Applied
      | Already_current
    [@@deriving equal, sexp]
  end

  (** Validating create/decoder enforces legal status/action/admission pairs,
      nonnegative anchors and nonzero archived/removed lifecycle revisions.
      Exposes actual current disposition rather than inventing replacement state. *)
  type t [@@deriving sexp]

  val expected : t -> Expected.t
  val latest_event_sequence : t -> int64
  val status : t -> Status.t
  val admission : t -> Admission.t
  val action : t -> Action.t
  val disposition : t -> Disposition.t
  val completed_at : t -> Timestamp.t

  val create
    :  reference:Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> latest_event_sequence:int64
    -> lifecycle_revision:Revision.t
    -> status:Status.t
    -> admission:Admission.t
    -> action:Action.t
    -> disposition:Disposition.t
    -> completed_at:Timestamp.t
    -> (t, Protocol_error.t) Core.Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
end

module Observation : sig
  (** Current nonactivating observation; never an execution capability. *)
  type t [@@deriving sexp]

  val create
    :  expected:Expected.t
    -> status:Result.Status.t
    -> admission:Result.Admission.t
    -> (t, Protocol_error.t) Core.Result.t

  val expected : t -> Expected.t
  val status : t -> Result.Status.t
  val admission : t -> Result.Admission.t
  val matches_session : t -> Session.t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) Core.Result.t
end
