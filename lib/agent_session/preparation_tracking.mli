open! Core

(** Shared private-owner ACK/revision and constructor-subset bookkeeping. This
    creates no principal, authorization, Run, source binding or effect permission.
    The actual Actor keeps its Owner private. Same creating Eio domain. *)
module Owner : sig
  type t

  val create : unit -> t
end

type t

val capture : owner:Owner.t -> Session_state.t -> t
val belongs_to : t -> owner:Owner.t -> bool
val acknowledged_revision : t -> int64
val is_closed : t -> bool

(** Exact Actor owner/session/generation/acknowledged revision only. Required
    original/source and command/Init policy belongs to the specific custody. *)
val check_basis
  :  t
  -> owner:Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** One actual owned ACK. Closes on mismatch; no rebasing of a request/source.
    The same retained revision remains readable after irreversible close. *)
val advance
  :  t
  -> owner:Owner.t
  -> previous:Session_state.t
  -> current:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Irreversible local revocation, clears constructor subset, retains ACK basis. *)
val close : t -> unit

module Constructor_jobs : sig
  type tracking := t
  type t

  (** Pure subset plan AFTER the specific custody's fresh original/policy check.
      Never a Job admission. Uses same private owner and exact live tracking. *)
  val prepare_add
    :  tracking
    -> owner:Owner.t
    -> state:Session_state.t
    -> job:Agent_protocol.Job.t
    -> (t, Agent_protocol.Error.t) result

  val prepare_handoff
    :  tracking
    -> owner:Owner.t
    -> state:Session_state.t
    -> (Agent_protocol.Id.Job.t list * t, Agent_protocol.Error.t) result

  val check_before_commit
    :  t
    -> owner:Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Only exact actual ACK; a closed tracker remains closed. *)
  val acknowledge : t -> unit
end
