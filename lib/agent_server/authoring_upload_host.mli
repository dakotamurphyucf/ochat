open! Core
module Store = Agent_store.Authoring_upload_store
module Bundle = Agent_protocol.Authoring_bundle.Bundle

(** Runtime-only facade for the private durable upload stage. The authenticated
    actor, current upload grant, optional parent provenance, and wall clock come
    from the host, never from a bundle request. The store owns the durable state
    machine; this facade does not publish a source bundle or install a protocol
    handler. *)
type t

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
    | Store of Store_code.t

  (** Finite, path-free diagnostic projection. Foreign and missing upload IDs
      remain one store outcome; authorization is checked before looking either
      up. Storage exceptions collapse to Storage_failure before crossing this
      facade; a native path or source byte is not present in the error value. *)
  val sexp_of_t : t -> Sexp.t
end

(** Take exclusive lifetime ownership of one already opened store. Its owner
    switch must outlive [sw], and callers must make no direct store calls after
    this transfer. [clock] is a real or fake Eio wall clock; all
    deadline checks use its current value. The facade is confined to its
    creating Eio domain and owns cancellation/join of in-flight requests.
    Closing the facade also closes the transferred store, so it cannot be shared
    with another facade or used afterward. *)
val create
  :  Store.t
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
    so waiting behind another operation cannot reuse an earlier timestamp. This
    is an operation-admission timestamp; work may complete after its deadline,
    with no implicit second expiry check after I/O. A revoked
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

(** This returns the store's Open/Ready/Rejected/Expired stage view. A
    proof-gated publisher may extend the wire status to Validating/Published after an authentic
    validated closure; this facade never manufactures those states. *)
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
