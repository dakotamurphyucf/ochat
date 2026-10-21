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
  | Source_child of active
  | Job_child of Developer_job_scope.Parent.t

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

(* [Root] admits no invocation/job parent. [Job_child] requires an actual
   entered worker Claim from this host/run/capture, no parent invocation, and
   exactly its job ID in context.parent_job. [Child active] requires this
   issuer's live direct parent, exact parent_invocation and its inherited job
   ID (if any). Its deadline may not exceed the direct parent's. [active] is
   issued only after durable Dispatching and expires when its callback joins;
   an inherited Claim cannot outlive either physical parent. *)

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

(** Selected developer invocation ledger over a physically registered native
    scope. This shares the validation, Dispatching and failure kernel above,
    while its exact dispatch writer is supplied per invocation by the host. No
    copied owner coordinate or stand-alone callback constitutes that writer. *)
module Selected : sig
  type t
  type active

  type parent =
    | Root
    | Child of active
    | Source_child of active
    | Job_child of Developer_job_scope.Parent.t

  type 'selection completion

  type dispatch_storage =
    { read :
        Agent_protocol.Id.Invocation.t
        -> ( (int * Agent_protocol.Host_invocation.t) option
             , Agent_protocol.Error.t )
             result
    ; save_dispatch :
        expected_revision:int option
        -> dispatched:Agent_protocol.Host_invocation.t
        -> (int, Agent_protocol.Error.t) result
    }

  (** Host-only construction. The server binds these values to one actual
      retained root, capture and current actor. [now] is nonyielding. *)
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
    -> (t, Agent_protocol.Error.t) result

  (** Trusted first-entry issuer for one saved-Agent child prepared under an
      already entered selected parent. Owner, capture fingerprint, current Root
      check and clock are derived from [parent_issuer]. The exact
      [parent_active] is retained physically, including its domain, lifetime,
      Worker ancestry and deadline; only that active may be used as the
      child's [Source_child] parent. Its liveness is rechecked for every
      ordinary nested [Child] too, including after yielding currentness work.
      This issuer cannot enter as [Root] or [Job_child], and later ordinary
      [Child] entries remain same-issuer.
      [ceiling] is an immutable child selection; [current_child] only adds a
      source/live restriction to the parent's currentness and cannot replace
      it. The owning server must first prove the saved Target, Root and live
      child package; this lower constructor alone does not grant them. *)
  val create_source_child
    :  parent_issuer:t
    -> parent_active:active
    -> ceiling:Chat_response.Tool_capability.t
    -> current_child:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  val selected : t -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
  val ceiling : t -> Chat_response.Tool_capability.t
  val same_issuer : t -> t -> bool

  (** The exact physical job ancestor of this entered selected callback. A
      nested Child inherits it; a root-origin callback returns [None]. The
      returned proof remains bound to the issuing worker and its live scope. *)
  val inherited_job_parent
    :  t
    -> active
    -> (Developer_job_scope.Parent.t option, Agent_protocol.Error.t) result

  val check_invocation
    :  t
    -> parent:parent
    -> Agent_protocol.Host_invocation.t
    -> (unit, Agent_protocol.Error.t) result

  (** Nonyielding physical issuer, live direct invocation, inherited Claim
      local state and actual direct-parent deadline check. The host calls this
      after its last yielding Claim read, with its nonyielding root witness and
      Claim local checks, immediately before effect entry. It does not replace
      authenticated [check_invocation] or the Claim's durable read. *)
  val check_parent_now
    :  t
    -> parent:parent
    -> Agent_protocol.Host_invocation.t
    -> (unit, Agent_protocol.Error.t) result

  (** [dispatch] belongs to the exact host native scope. The shared kernel
      admits a matching prior, saves Dispatching, and never calls [f] unless
      durability is acknowledged. After a save begins, errors, exceptions and
      cancellation are captured in an immutable completion; the host retains
      that completion's terminal on the physical scope before the bracket can
      exit. A failed/uncertain save never triggers handler retry after recovery.
      [parent_live_now] is a trusted nonyielding check of the actual enclosing
      callback Switch and cancellation context; it is stored on the physical
      active proof. A protected child fiber cannot borrow an active whose
      parent Switch was cancelled while joining children. There is no
      default-true selected path. [frozen] reads the issuing scope's already sealed outcome/selection without
      yielding. Once sealed, even a later exception or child-join failure cannot
      replace Complete/Pending plus jobs with Fail/None. The original exception
      remains available for rethrow after terminal bookkeeping. *)
  val with_invocation
    :  t
    -> parent:parent
    -> invocation:Agent_protocol.Host_invocation.t
    -> dispatch:dispatch_storage
    -> parent_live_now:(unit -> bool)
    -> frozen:(unit -> (Agent_protocol.Invocation.outcome * 'selection) option)
    -> (active:active
        -> dispatched:Agent_protocol.Host_invocation.t
        -> ( Agent_protocol.Invocation.outcome * 'selection option
             , Agent_protocol.Error.t )
             result)
    -> ('selection completion, Agent_protocol.Error.t) result

  module Completion : sig
    type 'selection t = 'selection completion

    type dispatch =
      | Awaiting_reconciliation
      | Installed of int

    type settlement =
      | Terminal_saved of int
      | Recovered_terminal_saved of
          { dispatch_revision : int
          ; terminal_revision : int
          }
      | Dispatch_absent
      | Uncertain of Agent_protocol.Error.t

    val dispatch : _ t -> dispatch
    val terminal : _ t -> Agent_protocol.Host_invocation.t
    val selected : 'selection t -> 'selection option

    (** Called after protected native submission or exact retained recovery.
        A directly saved terminal revision must follow the known dispatch
        revision. Retained recovery reports both revisions from the same
        physical host scope; this DTO checks their order, while that scope is
        responsible for proving actual durability.
        An original unexpected exception/cancellation is re-raised with its
        original backtrace, even when persistence is uncertain. A callback
        typed Error follows the existing durable Fail-record behavior. *)
    val finish
      :  'selection t
      -> settlement:settlement
      -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result
  end
end
