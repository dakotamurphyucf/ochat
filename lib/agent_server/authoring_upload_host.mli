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

  module Operation : sig
    type t =
      | Write
      | Commit
      | Publish
  end

  (** All callbacks must be non-yielding and query current host policy.
      [authorize_write] independently gates begin/chunk/status/cancel;
      Commit requires both write and publication, and the already sealed
      marker requires [authorize_publish]. These are independent of
      Send_messages and project/source-read grants. When installed,
      [authorize_parent] is an additional configured parent grant; physical
      custody comes from the live Core [Authoring_parent_scope] whenever the
      persisted upload has parent provenance. A configured host with no parent
      policy returns Selection_unavailable for a parent-bearing begin before
      any Store write. A request without a parent needs no source context.
      Unexpected callback
      exceptions propagate with their original backtrace. *)
  val create
    :  authorize_write:(Operator_authorization.t -> bool)
    -> authorize_publish:(Operator_authorization.t -> bool)
    -> authorize_parent:
         (Operator_authorization.t
          -> Agent_protocol.Authoring_context.Context.Expected.t
          -> bool)
           option
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

    val create_standard
      :  environment:string
      -> admitted:Authoring_standard_contracts.Entry.t
      -> t Core.Or_error.t

    val environment : t -> string
    val contract : t -> Chatml.Chatml_compiler_contract.t
  end

  (** Independent bundle-read and rebind permissions, separate from upload/write and
      message scopes. Both callbacks are bounded, non-yielding reads of
      already admitted current host state: no catalog reload, filesystem
      acquisition or wait. A saved receipt/manifest only supplies comparison
      coordinates. Source_only scripts select no Project contract. *)
  val create
    :  authorize_read:(Operator_authorization.t -> bool)
    -> authorize_rebind:(Operator_authorization.t -> bool)
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

  (** The exact parent Store pin retained by this physical uploaded context.
      Comparison only: it does not authorize bundle read or publication. *)
  val published_pin : t -> Store.Published_pin.t

  (** The one actual top-level Project selection retained in this rebind.
      ChatMD document bundles have per-script selections and return [None].
      The contract is comparison data, never a fresh bundle-read grant. *)
  val project_contract : t -> Chatml.Chatml_compiler_contract.t option
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

module Parent_admission : sig
  type t

  module Operation : sig
    type t =
      | Begin
      | Publish
    [@@deriving equal]
  end

  (** Trusted host callback port, not authority minted from a wire Expected.
      The installed bundle handler obtains these closures only from a live
      [Authoring_context_owner.with_parent_scope] on the same physical Core
      connection. Host integrations that construct this port themselves own
      the equivalent physical-currentness proof; this constructor does not
      authenticate an arbitrary callback. [check_now] is bounded,
      nonyielding, and uses a try-lock rather than waiting while Upload_host
      or Store owns a gate. It checks original/current actors, physical
      connection, owner/generation, operation and current selected policy.
      [previous_for] walks only retained immutable genuine parent evidence
      under [checkpoint]; it never fetches a source or parses on completion.
      [parent_pin] is present only for an uploaded parent verified by the SAME
      Store. Store independently checks that pin before durable publication. *)
  val create
    :  host_id:Agent_protocol.Id.Server.t
    -> expected:Agent_protocol.Authoring.Context.Expected.t
    -> check_now:
         (Operation.t
          -> connection:Connection_context.t
          -> actor:Operator_authorization.t
          -> expected:Agent_protocol.Authoring.Context.Expected.t
          -> (unit, Error.t) result)
    -> previous_for:
         (Bundle.Manifest.t
          -> selector:Bundle.Manifest.Script.Selector.t option
          -> checkpoint:(unit -> unit)
          -> (Chatml_project_build.t option, Error.t) result)
    -> parent_pin:Store.Published_pin.t option
    -> unit
    -> t
end

(** Bounded non-yielding current actor/operation-grant check for a caller's final
    publication boundary after its own yielding lifecycle gate. This grants no
    store lookup or source read; each upload operation still uses its guarded
    host/store admission. *)
val check_upload_current_now
  :  t
  -> actor:Operator_authorization.t
  -> operation:Upload_policy.Operation.t
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

(** Every request checks original actor currentness and its exact method's
    independently selected grant before queueing, again after the facade gate, and within the store's
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
    provenance is checked at begin and exact commit retry against the current
    physical Core context authority, including original and request actors.
    The uploaded immutable source inventory is thereafter independent of that
    read context. Reentrant calls from any active ancestor operation (including
    an inherited child fiber) return Busy before waiting for the host gate. A
    callback cancellation or unexpected exception is rethrown
    unchanged, without poisoning either operation gate. *)
val begin_upload
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> parent_admission:Parent_admission.t option
  -> request:Bundle.Begin.request
  -> checkpoint:(unit -> unit)
  -> (Bundle.Begin.response, Error.t) result

(** [parent_admission] is [None] exactly when the request has no parent. A parent
    request requires a live Begin admission for this physical connection,
    host, actor and Expected. The host repeats that check at guarded
    checkpoints before and after yielding Store work. Late revocation may
    leave an exact idempotent begin, never a success reply. *)

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
    never causes a false rejection or publication. An installed configured
    upload-only publisher may publish under its explicit grants and selected
    target; an absent publisher returns Selection_unavailable. The parent
    Expected comes from the persisted sealed claim, not a commit DTO.
    [parent_admission] is required exactly for a parent-bearing claim and must
    be a live Publish admission for the same connection, actor and Expected. Native
    parents supply current Core/Source proof; uploaded parents additionally
    supply a same-Store publication pin. The marker's existing final admission
    checks parent, original/current actors, selected policy and pin after all
    yielding work. A mismatch leaves exact intent custody for authorized
    reconciliation; no second publisher or late post-write denial is used. *)
val publish
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> parent_admission:Parent_admission.t option
  -> sealed:Store.sealed
  -> checkpoint:(unit -> unit)
  -> (Bundle.Receipt.t, Error.t) result

(** Independently authorize current bundle read AND rebind before Store lookup. Under the
    SAME host gate/cancellation scope, select current actual target/environment/
    compiler contracts from saved metadata before this request's captured bytes
    are materialized, then restore final saved Project locks and reparse genuine
    captured ChatMD declarations while Store holds a protected read lease.
    Current actor, both grants and selections are rechecked after waits and
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

(** Check both original/current actor, independent bundle-read and rebind grants, durable
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

(** Nonyielding actor/read/rebind/selected-policy comparison only. The
    physical Core parent scope uses this while the child Store gate is
    already held, so it cannot recursively borrow that Store. It grants no
    publication-pin or durable integrity claim; the same Store marker callback
    independently compares the actual parent pin. Ordinary read-only final
    currentness uses [check_context_selection_now] below. *)
val check_context_selection_policy_now
  :  t
  -> actor:Operator_authorization.t
  -> Context_capture.t
  -> (unit, Error.t) result

(** Nonyielding final comparison after a caller's full
    [check_context_current]. Recheck original/current actor, bundle-read and
    rebind grants, exact selected target/contract, and the current in-memory
    same-Store publication pin/deadline through Store's nonblocking gate. Busy
    or changed marker fails closed. The earlier full check authenticates
    durable metadata; this final comparison performs no read or installed-byte
    verification. Trusted policy callbacks remain bounded and nonyielding. *)
val check_context_selection_now
  :  t
  -> actor:Operator_authorization.t
  -> Context_capture.t
  -> (unit, Error.t) result

(** Historical metadata-only read for the authentic published Store pin.
    These operations require the CURRENT independent Bundle_read grant but
    neither Rebind, an old Context_capture nor a selected compiler. The full
    check may yield during Store publication/expiry/prune verification; the
    final check is nonyielding, compares the same physical Store pin and fresh
    trusted time, and must follow a full check for this read attempt. Neither
    operation reads installed source bytes or restores a saved graph. *)
val check_historical_pin
  :  t
  -> actor:Operator_authorization.t
  -> Store.Published_pin.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

val check_historical_pin_now
  :  t
  -> actor:Operator_authorization.t
  -> Store.Published_pin.t
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
