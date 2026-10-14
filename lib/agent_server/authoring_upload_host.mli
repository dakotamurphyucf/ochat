open! Core
module Store = Agent_store.Authoring_upload_store
module Bundle = Agent_protocol.Authoring_bundle.Bundle

(** Runtime-only facade for the private durable upload stage and publisher. The authenticated
    actor, current upload grant, optional parent provenance, and wall clock come
    from the host, never from a bundle request. The store owns the durable state
    machine; this facade validates captured closure and gates publication but
    does not install a protocol handler. *)
type t

(** Verified store host identity for comparison only; never an upload grant. *)
val host_id : t -> Agent_protocol.Id.Server.t

module Upload_policy : sig
  type t

  (** Both callbacks must be non-yielding and query current host policy.
      [authorize_upload] is mandatory and independent of Send_messages and
      project/source-read grants. [authorize_parent] checks an optional
      parent-context reference against the actual current context and read
      grant; a wire-supplied Expected value is never custody by itself. A
      request without a parent needs no source context. Unexpected callback
      exceptions propagate with their original backtrace. *)
  val create
    :  authorize_upload:(Operator_authorization.t -> bool)
    -> authorize_parent:
         (Operator_authorization.t
          -> Agent_protocol.Authoring_context.Context.Expected.t
          -> bool)
    -> t
end

module Publication_policy : sig
  type t

  module Selected : sig
    type t

    val create
      :  environment:string
      -> contract:Chatml.Chatml_compiler_contract.t
      -> previous:Chatml_project_build.t option
      -> t Core.Or_error.t

    val environment : t -> string
    val contract : t -> Chatml.Chatml_compiler_contract.t
    val previous : t -> Chatml_project_build.t option
  end

  (** [select] must be non-yielding, bounded and read already admitted current
      host state. It must not reload a catalog, read the filesystem or wait for
      another operation: the marker's final admission invokes it immediately
      before the durable rename, after yielding preparation. *)
  val create
    :  select:
         (Operator_authorization.t
          -> Bundle.Manifest.t
          -> Bundle.Manifest.Script.Selector.t option
          -> Selected.t option)
    -> t
end

module Publication : sig
  type t

  (** Optional for upload-only hosts. The directory is an inert display root
      for the captured generated ChatMD parser, not a source acquisition path.
      The host selects actual current contracts and previous Builds through
      [policy] before any captured-only validation. *)
  val create
    :  policy:Publication_policy.t
    -> limits:Chatml_authoring_bundle_project.Limits.t
    -> document_dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Rebind_policy : sig
  type t

  module Selected : sig
    type t

    val create
      :  environment:string
      -> contract:Chatml.Chatml_compiler_contract.t
      -> t Core.Or_error.t

    val environment : t -> string
    val contract : t -> Chatml.Chatml_compiler_contract.t
  end

  (** Independent bundle-read permission, separate from upload/write and
      message scopes. Both callbacks are bounded, non-yielding reads of
      already admitted current host state: no catalog reload, filesystem
      acquisition or wait. A saved receipt/manifest only supplies comparison
      coordinates. Source_only scripts select no Project contract. *)
  val create
    :  authorize_read:(Operator_authorization.t -> bool)
    -> select:
         (Operator_authorization.t
          -> Bundle.Manifest.t
          -> Bundle.Manifest.Script.Selector.t option
          -> Selected.t option)
    -> t
end

module Rebind : sig
  type t

  (** Optional for hosts with no published-bundle read service. The directory
      anchors generated parser display provenance, never a source fetch. *)
  val create
    :  policy:Rebind_policy.t
    -> limits:Chatml_authoring_bundle_project.Limits.t
    -> document_dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Rebound : sig
  type basis =
    | Project of Chatml_authoring_bundle_project.Validated.t
    | Chatmd_document of Chatml_authoring_bundle_document.Validated.t

  type t

  val receipt : t -> Bundle.Receipt.t
  val basis : t -> basis
end

module Context_capture : sig
  type t

  (** One initial protected rebind, its genuine selected-target comparison and
      a Store-instance publication pin. The immutable result is data, not a
      continuing bundle-read or execution grant. *)
  val rebound : t -> Rebound.t
end

module Error : sig
  module Store_code : sig
    type t =
      | Invalid_manifest
      | Limit_exceeded
      | Idempotency_conflict
      | Invalid_offset
      | Incomplete
      | Sealed
      | Expired
      | Missing_or_foreign
      | Busy
      | Storage_failure
    [@@deriving sexp_of]
  end

  type t =
    | Unauthorized
    | Expired_actor
    | Foreign_domain
    | Closed
    | Busy
    | Selection_unavailable
    | Stale_capture
    | Invalid_closure
    | Resource_limit
    | Store of Store_code.t

  (** Finite, path-free diagnostic projection. Foreign and missing upload IDs
      remain one store outcome; authorization is checked before looking either
      up. Storage exceptions collapse to Storage_failure before crossing this
      facade; a native path or source byte is not present in the error value. *)
  val sexp_of_t : t -> Sexp.t
end

(** Bounded non-yielding current actor/upload-grant check for a caller's final
    publication boundary after its own yielding lifecycle gate. This grants no
    store lookup or source read; each upload operation still uses its guarded
    host/store admission. *)
val check_upload_current_now
  :  t
  -> actor:Operator_authorization.t
  -> (unit, Error.t) result

(** Take exclusive lifetime ownership of one already opened store. Its owner
    switch must outlive [sw], and callers must make no direct store calls after
    this transfer. [clock] is a real or fake Eio wall clock; all
    deadline checks use its current value. The facade is confined to its
    creating Eio domain and owns cancellation/join of in-flight requests.
    Closing the facade also closes the transferred store, so it cannot be shared
    with another facade or used afterward. *)
val create
  :  ?publication:Publication.t
  -> ?rebind:Rebind.t
  -> Store.t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> upload_policy:Upload_policy.t
  -> t

(** Every request checks original actor currentness and the current upload
    grant before queueing, again after the facade gate, and within the store's
    own operation gate before recovery or private state access. The guarded
    checkpoint repeats those checks both before and after the caller's
    checkpoint, which may itself yield, during store work. The store samples
    trusted time only after reaching its own gate and completing any recovery,
    so waiting behind another operation cannot reuse an earlier timestamp.
    Ordinary stage operations use this admission timestamp; publication and
    published-capture borrowing additionally sample fresh time after their
    yielding preparation, immediately before marker rename or read admission.
    A revoked
    request has no successful reply, even if an already durable mutation must
    later be reconciled by an authorized retry/status. Optional parent
    provenance is checked at begin against current context authority; the
    uploaded immutable source inventory is thereafter independent of that
    read context. Reentrant calls from any active ancestor operation (including
    an inherited child fiber) return Busy before waiting for the host gate. A
    callback cancellation or unexpected exception is rethrown
    unchanged, without poisoning either operation gate. *)
val begin_upload
  :  t
  -> actor:Operator_authorization.t
  -> request:Bundle.Begin.request
  -> checkpoint:(unit -> unit)
  -> (Bundle.Begin.response, Error.t) result

val chunk
  :  t
  -> actor:Operator_authorization.t
  -> request:Bundle.Chunk.request
  -> checkpoint:(unit -> unit)
  -> (Bundle.Chunk.response, Error.t) result

val claim_commit
  :  t
  -> actor:Operator_authorization.t
  -> request:Bundle.Commit.request
  -> checkpoint:(unit -> unit)
  -> (Store.Claim_result.t, Error.t) result

(** Current host policy is selected before validation and checked again after
    waits, before Store admission, and at guarded checkpoints inside its gate.
    Validation uses only the authenticated sealed snapshot and genuine
    captured-only Project/ChatMD adapters. A failed closure is durably rejected
    with a coarse path-free reason before any intent; loss of selection or grant
    never causes a false rejection or publication. Upload-only hosts return
    Selection_unavailable. *)
val publish
  :  t
  -> actor:Operator_authorization.t
  -> sealed:Store.sealed
  -> checkpoint:(unit -> unit)
  -> (Bundle.Receipt.t, Error.t) result

(** Independently authorize current bundle read before Store lookup. Under the
    SAME host gate/cancellation scope, select current actual target/environment/
    compiler contracts from saved metadata before this request's captured bytes
    are materialized, then restore final saved Project locks and reparse genuine
    captured ChatMD declarations while Store holds a protected read lease.
    Current actor, read grant and selections are rechecked after waits and
    before reply. Foreign/missing IDs share one Store code; changed selected
    contract/receipt/graph identity returns Stale_capture. A competing borrow
    returns Busy. The immutable Rebound value is data, never a continuing
    read or execution grant; future use requires its own authorization. *)
val rebind
  :  t
  -> actor:Operator_authorization.t
  -> bundle_id:Agent_protocol.Id.Authoring_bundle.t
  -> checkpoint:(unit -> unit)
  -> (Rebound.t, Error.t) result

(** Initially restore the genuine saved closure once under the same protected
    read path as [rebind], retaining an owner-affine publication pin. *)
val capture_for_context
  :  t
  -> actor:Operator_authorization.t
  -> bundle_id:Agent_protocol.Id.Authoring_bundle.t
  -> checkpoint:(unit -> unit)
  -> (Context_capture.t, Error.t) result

(** Check both original/current actor, independent bundle-read grant, durable
    publication pin/deadline and every actual selected target/contract. This
    runs under the existing host/store gates but does not restore the graph or
    reread installed source bytes. Selection is checked again AFTER the
    yielding Store call releases its gate, before returning. A later explicit
    [rebind] performs full byte verification. *)
val check_context_current
  :  t
  -> actor:Operator_authorization.t
  -> Context_capture.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

(** Nonyielding final comparison after a caller's own publication gate. Recheck
    original/current actor, bundle-read grant and exact selected target/contract
    against the retained owner-affine capture. The caller MUST have performed
    [check_context_current] first for durable pin/deadline/current marker; this
    check alone never authenticates publication or installed bytes. Trusted
    policy callbacks must remain bounded and nonyielding. *)
val check_context_selection_now
  :  t
  -> actor:Operator_authorization.t
  -> Context_capture.t
  -> (unit, Error.t) result

(** This returns the Store's exact Open/Ready/Validating/Published/Rejected/
    Expired state. The facade never manufactures a receipt. *)
val status
  :  t
  -> actor:Operator_authorization.t
  -> request:Bundle.Status.request
  -> checkpoint:(unit -> unit)
  -> (Store.Status.t, Error.t) result

val cancel
  :  t
  -> actor:Operator_authorization.t
  -> upload_id:Agent_protocol.Id.Authoring_upload.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

(** Idempotently revoke admission, cancel active work, join it, and close the
    store. Queued calls observe Closed after the gate. Closing from any active
    ancestor operation returns Busy rather than self-joining. Cancellation of the
    caller does not interrupt required join/cleanup. *)
val close_and_wait : t -> (unit, Error.t) result
