open Core

(** Trusted host adapter for a developer run's invocation ledger. This module
    owns dispatch/terminal transitions, not a developer run service or a second
    scheduler. The host supplies current authenticated custody and a durable CAS
    store; neither a caller's ID nor a pure [Host_invocation.t] grants execution. *)
type t

type active

type parent =
  | Root
  | Child of active

type storage =
  { read :
      Agent_protocol.Id.Invocation.t
      -> ((int * Agent_protocol.Host_invocation.t) option, Agent_protocol.Error.t) result
  ; compare_and_set :
      id:Agent_protocol.Id.Invocation.t
      -> expected_revision:int option
      -> after:Agent_protocol.Host_invocation.t
      -> (int, Agent_protocol.Error.t) result
  }

(** [current] reauthenticates the original principal, durable host ID, run
    epoch and the exact OCH-265 admitted capture fingerprint against the host's
    actual retained capture, not a same-looking caller digest. [ceiling] is the
    admitted immutable selection; [selected] reads current selection and may
    narrow it, never widen it. Host construction is internal to trusted server
    wiring, never from an RPC request or a retained editor overlay. [now] is a
    trusted clock for deadline checks. Creation calls [current] before issuing
    the adapter. The host builds the capture fingerprint with
    [Chatml_developer_capture.fingerprint actual_admitted_capture]. [now]
    must be a non-yielding clock sample. Each check invokes potentially yielding
    [current] first, then checks parent liveness and deadline without a yield
    before returning to the caller's next effect boundary. *)
val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> ceiling:Chat_response.Tool_capability.t
  -> selected:(unit -> Chat_response.Tool_capability.t)
  -> current:
       (owner:Agent_protocol.Execution_owner.developer
        -> capture_fingerprint:Chatml_source.Digest.t
        -> (unit, Agent_protocol.Error.t) result)
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> storage:storage
  -> (t, Agent_protocol.Error.t) result

val owner : t -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val ceiling : t -> Chat_response.Tool_capability.t

(** Read the current host selection, rejecting any binding outside the exact
    captured ceiling by capability ID and fingerprint. *)
val selected : t -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

val current : t -> (unit, Agent_protocol.Error.t) result

(** Repeat authenticated run/capture, exact parent issuer/liveness/domain,
    direct ancestry and deadline checks. Native admission calls this after any
    permission wait and immediately before each real host effect. *)
val check_invocation
  :  t
  -> parent:parent
  -> Agent_protocol.Host_invocation.t
  -> (unit, Agent_protocol.Error.t) result

(* [Root] admits no invocation/job parent. [Child active] requires exactly that
    live direct parent from this physical issuer, not merely an equal owner
    coordinate, and a no-later deadline. [active] is issued only after the
    parent's durable Dispatching CAS. It expires when that callback returns,
    including inherited child fibers; ordinary cross-domain use rejects without
    an explicit trusted handoff. A parent_job requires OCH-288's typed live
    job-owner proof; this foundation rejects it. *)

(** First durable atomic CAS saves Dispatching before the callback. The callback
    runs outside storage serialization and must use the common native admission
    path. The adapter checks current ownership after read and before CAS, again
    after CAS and before callback entry; the common policy path checks after the
    permission wait and immediately before a native effect. Nested host effects
    repeat currentness at their own entry. No claim is made about arbitrary
    unrelated yields. Status writes use the original ledger authority even if
    user execution authority has since been revoked: currentness failure,
    callback Error/exception and cancellation all attempt one terminal Fail or
    Cancelled CAS. Cancellation is re-raised unchanged after bookkeeping. A
    failed/uncertain terminal CAS returns a typed persistence Error (or re-raises
    cancellation) and leaves at least durable Dispatching/interruption evidence;
    the host may inspect/recover the record but must never replay the callback.
    Storage must compare revision, atomically durably commit the whole validated
    record before acknowledging, and return the new revision. Zero/negative,
    reused, and overflowed revisions reject before any callback or effect. *)
val with_invocation
  :  t
  -> parent:parent
  -> invocation:Agent_protocol.Host_invocation.t
  -> (active:active
      -> dispatched:Agent_protocol.Host_invocation.t
      -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result
