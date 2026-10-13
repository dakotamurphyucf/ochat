(** Lossless internal job envelope for the shared capacity and scheduler path.
    Conversation retains the exact original Job.t, including legacy fields and
    codecs; developer has a distinct validated record and no Session ID. *)
open! Core

type t

val of_conversation : Job.t -> (t, Protocol_error.t) result
val of_developer : Developer_job.t -> (t, Protocol_error.t) result
val to_conversation : t -> Job.t option
val to_developer : t -> Developer_job.t option

module Identity : sig
  type t [@@deriving compare, equal, sexp_of]

  include Comparator.S with type t := t

  (** Owner-qualified stable job ID. Epoch/generation belongs to the owner;
      attempt remains a separate exact reservation/claim coordinate. *)
  val owner : t -> Execution_owner.t

  val job_id : t -> Id.Job.t
end

val identity : t -> Identity.t
val owner : t -> Execution_owner.t
val id : t -> Id.Job.t
val kind : t -> Job.kind
val status : t -> Job.status
val attempt : t -> int
val nested_depth : t -> int

module Lifecycle : sig
  (** One read-only classification used by capacity and the later scheduler for
      both records. [classify] requires a validated nonnegative attempt; call
      [lifecycle] for an admitted Host_job. It does not replace actor-owned
      state transitions. *)
  type t =
    | New_queued
    | Queued_retry
    | Claimed_or_waiting
    | Terminal
  [@@deriving equal, sexp_of]

  val classify : status:Job.status -> attempt:int -> t
end

val lifecycle : t -> Lifecycle.t
