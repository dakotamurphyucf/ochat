open! Core

(** Host lifecycle composition. Owns per-ID exclusion, the current loaded actor
    fence or retained indexed Handle, and exact original host command receipts.
    Reading/recovering an indexed target never constructs an actor or runtime. *)
type t

module Request : sig
  type t =
    | Delete of Agent_protocol.Session.Delete_request.t
    | Restore of Agent_protocol.Session_lifecycle.Request.t
    | Resume of Agent_protocol.Session_lifecycle.Request.t
end

module Failure : sig
  type disposition =
    | No_effect
    | Recovery_required
  [@@deriving equal, sexp_of]

  type t

  val error : t -> Agent_protocol.Error.t
  val disposition : t -> disposition

  (** Secondary durable rejection-completion failure. The primary [error] remains
      unchanged and [disposition] is Recovery_required. *)
  val completion_error : t -> Agent_store.Store_error.t option

  (** Secondary actual close/abort failure. The original primary stays unchanged;
      the registry retains exact cleanup ownership and both diagnostics for retry.
      Any such failure requires recovery and leaves original receipt Pending. *)
  val cleanup_error : t -> Session_registry.Cleanup_failure.t option
end

val create
  :  store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> idempotency:Agent_store.Idempotency_store.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> read_owned_session:
       (Agent_store.Session_store.Handle.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) Result.t)
  -> validate_removal:
       (Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> t

(** Caller has authorized the original method/current principal and durably
    admitted its exact Pending record. Reauthorize actual identity before every
    retained observation/replay. Loaded delete requires the original live writer;
    indexed management uses current visibility and guarded confirmation without
    manufacturing a connection attachment. Irreversible cleanup is cancellation
    protected. Uncertain publication keeps owner fenced for recovery.

    [No_effect] requires proof under the target reservation: a validated current
    lifecycle lookup found no matching outcome, this attempt never entered lifecycle
    publication, and its exact original generic Failure completed durably before
    reservation release. Runtime retirement cleanup may already have occurred;
    this classification never grants permission to reopen a closed runtime.
    Failed generic acknowledgement reports Recovery_required and preserves the
    primary error together with [Failure.completion_error]. [Recovery_required] preserves Pending, including reservation
    contention, unreadable authority, retained outcome replay and any attempted
    publication. No failed durability acknowledgement proves absence. Cancellation
    and unexpected exceptions propagate; they never become a no-effect result. *)
val execute
  :  t
  -> key:Agent_store.Idempotency_store.Key.t
  -> request_digest:string
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> validate_attachment:
       (Agent_protocol.Id.Session.t
        -> Agent_protocol.Id.Attachment.t
        -> (unit, Agent_protocol.Error.t) Result.t)
  -> Request.t
  -> (Agent_protocol.Method_result.t, Failure.t) Result.t

(** Startup retry before accepting user mutations. Original protected generic
    results must be durably reconciled in their retained host owner before any
    payload cleanup. No live actor/runtime or authorization grant is recreated. *)
val recover_removals : t -> (unit, Agent_protocol.Error.t) Result.t
