(* Joined owner for source/context and bundle authoring handlers. *)

open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Sources = Authoring_source_host
module Upload = Authoring_upload_host

module Limits : sig
  type t

  (** Positive bounded host ceilings. A live connection entry is charged even
      if it has no open source context; close/reinitialize releases it after
      joined work. [max_active_requests] bounds all core and later borrowed
      semantic/assistance work per connection, while source-host/project build
      and protocol per-request caps continue to apply. [max_response_bytes]
      bounds an individual constructed wire response, not caller retention
      after it is returned. [max_roots_per_connection] bounds the map of
      connection-local root labels. These limits cannot enlarge Ledger,
      Source_host, Upload_host or Initialize limits. *)
  val create
    :  max_connections:int
    -> max_active_requests_per_connection:int
    -> max_roots_per_connection:int
    -> max_response_bytes:int
    -> t Or_error.t
end

module Selection_policy : sig
  type t

  (** Trusted current host selection, not a client or on-disk resolver. The
      callback must be bounded/non-yielding and read already admitted host
      catalog/contract state. It returns the actual compiler contract for the
      exact requested project target/environment. The source host separately
      checks root grants and real project declaration/Build selection. On
      later requests the service repeats selection and compares the admitted
      contract identity; a retained context or digest is not authority. *)
  val create
    :  project_contract:
         (Operator_authorization.t
          -> root:Sources.Root.t
          -> project:Source.Portable_path.t
          -> selection:P.Authoring.Context.Selection.t
          -> Chatml.Chatml_compiler_contract.t option)
    -> document_static:
         (Operator_authorization.t
          -> requested_environment:string option
          -> Chatml_project_build.t option)
    -> t
end

module Supplied_document_policy : sig
  type t

  (** Independent host/operator grant for caller-supplied exact bytes. It
      conveys no native source root or project authority. The callback is a
      bounded non-yielding read of current host policy and is checked for the
      original and request actors on EVERY operation. The service uses the
      existing exact Chatml_source constructor and Ledger limits; it never
      reparses, normalizes UTF-8 or writes a host file. [None] at create means
      this origin is uninstalled even if an upload owner exists. *)
  val create : authorize:(Operator_authorization.t -> bool) -> t
end

module Error : sig
  type t =
    | Uninstalled
    | Unselected_feature
    | Unauthorized
    | Expired_actor
    | Foreign_connection
    | Stale_negotiation
    | Stale_context
    | Stale_source
    | Source_unavailable
    | Selection_unavailable
    | Busy
    | Closed
    | Resource_limit
    | Invalid_position of P.Authoring.Failure.invalid_position
    | Invalid_request
    | Storage_unavailable
    | Upload_failure of Upload.Error.t

  (** Finite/path-free translation to the existing protocol code + authoring
      Failure reason where one exists. No source, native path, environment
      details, host policy or exception text enter the wire error. *)
  val to_protocol : t -> P.Error.t
end

type t

(** Construct one domain-confined service owner from EXISTING host owners.
    Operations on a different OCaml domain fail closed before touching the
    owner gate. Borrowed Current data accessors also reject use on another domain.
    [sources] and [upload] are borrowed and must outlive this service; this
    service closes only its own contexts/activity, never opens an extra root,
    durable store or source resolver. Availability is per origin:
    Host_project/Host_document/Roots/Host_list/Host_read need [sources];
    Uploaded_project needs [upload] and its independent current bundle-read
    grant; Supplied_document needs [supplied_document], not [sources]. Once
    admitted, Context/Source/Document operations work for any basis even when
    [sources=None]. Unsupported origins return typed Uninstalled or
    Selection_unavailable without pretending that a client ID grants custody.
    The caller supplies the real daemon server ID,
    one immutable analysis Policy instance, and same finite protocol limits
    advertised to clients. *)
val create
  :  sw:Eio.Switch.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> host:P.Id.Server.t
  -> sources:Sources.t option
  -> upload:Upload.t option
  -> supplied_document:Supplied_document_policy.t option
  -> selection_policy:Selection_policy.t
  -> context_limits:Ledger.Limits.t
  -> analysis_policy:Project.Policy.t
  -> protocol_limits:P.Authoring.Limits.t
  -> limits:Limits.t
  -> t

(** The only mutable context ledger is [Project_cache.contexts]. The owner
    atomically applies every authorized Ledger transition to Project_cache
    and Document_cache under the SAME short host gate. Failed/cancelled
    preparation publishes neither a new context nor a new cache. Context_close
    commits revocation before joining resource cleanup; a later failure does
    not restore the closed context, and unfinished cleanup remains owned by
    the connection. No Eio wait, source read,
    parser, checker, callback or response encoding occurs under that gate. *)

module Current : sig
  type t

  type basis =
    | Project
    | Document

  val basis : t -> basis
  val expected : t -> P.Authoring.Context.Expected.t

  (** Borrowed request-scope exact inputs. Project and Document accessors
      reject the other basis. They check the live token and charge the one
      request work/byte policy before returning data. The returned immutable
      bytes/Build are comparison data, never a continuing source grant. *)
  val project_inputs : t -> checkpoint:(unit -> unit) -> (Ledger.Inputs.t, Error.t) result

  val document_inputs
    :  t
    -> checkpoint:(unit -> unit)
    -> (Ledger.Document.Inputs.t, Error.t) result
end

(** Internal sibling-service seam. Admit actor, selected core feature,
    connection incarnation and expected CAS; pin one current owner under a
    bounded joined activity. The trusted callback may yield. Recheck exact
    original/current actor, host source or upload-read grant or independent
    supplied-document policy, selected catalog,
    exact expected context generation/basis and connection negotiation after
    it returns and before returning the response. A Current token cannot be
    used after callback return; an escaped token fails closed. The short
    ledger/cache gate is not held across [f].
    Semantic/assistance children add actual parser/per-script selection checks
    before their own publication; this scope does not claim to prove them. *)
val with_current
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> f:(Current.t -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** Source/context request subset: Roots, Host_list/read, Context_open/
    refresh/close, Source_list/read, Document_version/replace/close. This
    function rejects other Request constructors as Uninstalled. Root IDs and
    context IDs are fresh connection-local labels, not grants; Roots repeats
    host permission on every use. Host_read is a fresh native observation;
    Source_read is pinned ledger bytes. Uploaded_project initially invokes
    Upload.capture_for_context and accepts only a Project basis; every later
    request invokes Upload.check_context_current on its opaque owner-affine pin
    without restoring the compiler graph again. The
    response uses existing validated Wire constructors and exact bytes.
    Capabilities is the remaining nominal core method but must be constructed
    by the final installer from ALL actual installed handlers, including a
    bundle-only host; this owner does not advertise its own subset as the whole
    service. *)
val handle_core
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Error.t) result

(** Route the four bundle methods through the independently authorized upload
    host. An upload-only connection needs the selected bundle feature but no
    core feature or source context. This handler does not advertise itself;
    installation and transport routing belong to the protocol installer. *)
val handle_bundle
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Error.t) result

(** Single serialized per-connection reinitialization transition. The
    installer first computes a SUCCESSFUL Initialize.Response, then calls this
    function instead of mark_initialized directly. The service marks the old
    epoch Revoking, rejects new old-epoch requests, cancels/joins active work,
    closes source contexts and atomically evicts ledger/cache entries. ONLY
    AFTER that wait does it call Connection_context.mark_initialized with the
    exact response version/features and admit the new epoch. Concurrent
    initialize transitions serialize behind a distinct lifecycle gate; no
    request is admitted in the revocation/publication gap. A failed initialize
    computation never calls this function and leaves the old epoch untouched.
    Reentrant initialization from an active authoring request returns Busy
    before waiting; cancellation/unexpected exceptions preserve identity and
    leave old admission closed rather than publishing a partial new epoch. *)
val publish_initialize_and_wait
  :  t
  -> connection:Connection_context.t
  -> response:P.Initialize.Response.t
  -> (unit, Error.t) result

(** Idempotent transport close and daemon close cancel/join work before
    releasing borrowed source/upload owners. Close from one's own active
    request rejects Busy instead of self-joining. No old context ID or root ID
    can address a new Connection_context even if transport connection_id is
    reused. Cancellation/unexpected callback exceptions retain identity and
    backtrace; expected errors are returned as Error.t without poisoning gates. *)
val close_connection_and_wait : t -> Connection_context.t -> (unit, Error.t) result

val close_and_wait : t -> (unit, Error.t) result
