open Core

(** Host-only read/capture authority for authoring. This adapter borrows the
    actual configured roots admitted by {!Project_source_host}, and delegates
    native reads to {!Chatml_local_roots} / {!Chatml_local_source} and complete
    local project preparation to {!Chatml_project_prepare}. It owns no second
    resolver, runtime workspace, agent session, writable root or Git fetch path.
    OCH-277 extends Project_source_host's joined borrow to lend the configured
    prefix paired with each admitted anchor and the exact Project_source_policy
    project policy/limits already built at startup; the current [with_host]
    exposes only anchors and Prompt_script_preparation.Policy, insufficient for
    standalone authoring capture/browse. No profile is independently rebuilt.
    The portable editor revision state lives in Chatml_authoring_context (OCH-278). *)

module Root : sig
  (** Host-incarnation-local selection of one configured external source-read
      anchor. Returned only after operator policy admits the original actor.
      An index or display value is never a grant; stale/foreign selections reject.
      The prompt receipt's private original root is not exposed here. *)
  type t

  val index : t -> int
end

module Read_policy : sig
  type t

  (** Trusted host/operator decision. The callback receives an actor and an
      index into this host's already admitted [external_roots] in config order.
      Initial selection checks its original actor; later operations check both
      that actor and the current request actor. The host must require an
      explicit source-read permission; the
      current generic Agent_protocol scopes grant none. No default allow-all and
      no policy derived from a workspace/project ID or client path. Callbacks
      must be pure and may not yield. Exceptions preserve identity and
      backtrace; they are never converted into authorization results. *)
  val create : authorize:(Operator_authorization.t -> root_index:int -> bool) -> t

  val deny_all : t
end

module Document_policy : sig
  type t

  (** Separate host/operator decision to admit client-supplied document bytes.
      No source-read root, project membership or workspace ID follows from it.
      The original and request actors are checked for the same current
      principal on every operation. This pure callback may not yield; its
      exceptions keep identity/backtrace. Callers explicitly pass [deny_all]
      unless supplied-document admission is authorized. *)
  val create : authorize:(Operator_authorization.t -> bool) -> t

  val deny_all : t
end

module Limits : sig
  type t

  (** Aggregate host admission and per-call result limits. Project acquisition
      additionally keeps Project_source_policy's existing file/graph/work caps;
      this value never raises or resets them. [max_manifest_entries] bounds
      returned capture rows including the selected lock; [max_browse_entries]
      charges every native directory entry before filtering. [max_read_bytes]
      bounds one explicit read, and [max_context_read_bytes] charges all such
      reads over one context's lifetime. [max_context_requests] reserves one
      bounded capture, browse or read attempt before work, including failed and
      cancelled attempts; with existing per-attempt policy this bounds lifetime
      work without resetting an attempt. Fields are positive; sums are checked. *)
  val create
    :  max_contexts:int
    -> max_context_requests:int
    -> max_manifest_entries:int
    -> max_browse_entries:int
    -> max_read_bytes:int
    -> max_context_read_bytes:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Unauthorized
    | Expired_actor
    | Foreign_root
    | Closed
    | Busy
    | Foreign_domain
    | Context_limit
    | Result_limit
    | Unsupported_source_selection
    | Sources of Project_source_host.Error.t
    | Root of Chatml_local_roots.Error.t
    | Local of Chatml_local_source.Error.t
    | Project of Chatml_project_prepare.Error.t
    | Source_capture of Core.Error.t

  (** Finite safe projection. Root errors expose only their finite code;
      project errors use a coarse tag. Neither exposes native paths, exception
      text, credentials or source bytes. *)
  val sexp_of_t : t -> Sexp.t
end

module Capture : sig
  (** Immutable successful local-only closure. No live root, actor proof, Eio
      owner, callback, cache, run permission or execution capability survives.
      Paths and digests describe exact acquired bytes, not durable filesystem
      identity or an all-file atomic observation. *)
  type t

  val build : t -> Chatml_project_build.t
  val prepared : t -> Chatml_prepared_program.t
  val fingerprint : t -> Chatml_source.Digest.t
  val sources : t -> Chatml_source.t list
end

module Entry : sig
  (** A portable candidate name returned by bounded nonrecursive directory
      enumeration. An entry is neither a regular-file proof nor a read grant;
      explicit [read_member] performs descriptor-relative admission again. *)
  type t

  val member : t -> Source.Portable_path.t
end

type t
type host = t

(** Bind the original daemon-owned Project_source_host and a mandatory trusted
    operator read policy. [sources] and its Eio switch outlive this owner. There
    is no filesystem path or root-opening constructor here. All methods remain
    on the creating Eio domain. Close revokes admission, cancels and joins work. *)
val create
  :  Project_source_host.t
  -> sw:Eio.Switch.t
  -> read_policy:Read_policy.t
  -> document_policy:Document_policy.t
  -> limits:Limits.t
  -> t

(** Enumerate only configured roots for which the original actor is current and
    the host read policy allows access now. A single enumeration at a time is
    tracked, cancelled and joined by host close; concurrent or reentrant calls
    reject [Busy], and enumeration cannot close its own host. The returned
    opaque selections remain host/actor-lifetime affine; every later method
    repeats both checks. The wire adapter may assign connection-local IDs,
    never deserialize a [Root.t]. *)
val roots
  :  t
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (Root.t list, Error.t) Result.t

module Context : sig
  (** Connection-scoped source selection, not a runtime workspace or prepared
      editor overlay. Stores the original actor and selected root; no native FD
      or Git grant is retained between operations. An authenticated request must
      supply its current actor each time. A new actor for the same principal may
      rebind only after policy reauthorization while the original proof remains
      current; reconnect otherwise opens a new context. *)
  type t

  val root : t -> Root.t

  (** Currentness check for pinned project captures, overlays and
      cached analysis. Reuse the original actor plus current request actor and
      every explicitly selected root's existing policy; do not read, recapture
      or infer a grant from the context ID. The handler checks before serving
      and after yielding work. An async handler separately bounds its request
      lifetime until that final check. *)
  val check_current : t -> actor:Operator_authorization.t -> (unit, Error.t) Result.t

  (** Select only a member of the already granted root. [project] is a validated
      portable project declaration member, [selection] is the existing project
      selector, and [contract] is the host's authentic static contract, never a
      digest decoded from the client. This does not capture or execute anything.
      The host constructs a fresh lock ID for pure composition; no file write.
      [dependency_roots] explicitly selects additional configured anchors for
      local dependencies. Reject duplicate or foreign roots. Never pass every
      configured anchor to the resolver unfiltered; reauthorize the selected
      root and each dependency root before and after yielding work. *)
  val open_root
    :  host
    -> actor:Operator_authorization.t
    -> root:Root.t
    -> dependency_roots:Root.t list
    -> project:Source.Portable_path.t
    -> selection:Chatml_project.Selection.request
    -> contract:Chatml.Chatml_compiler_contract.t
    -> (t, Error.t) Result.t

  (** Complete host capture with one selected source policy/deadline/work budget.
      Borrow the source host under its joined [with_host], select only this
      context's explicitly authorized root/dependency anchor set and pass NO
      Git store or grants to project preparation.
      An authored Git dependency fails closed; existing exact captured Git/lock
      bundles belong to the separate immutable upload/restore path. Use
      Build_without_previous for a local-only unsaved lock, never a silent Update
      or network retry. Recheck actor/current grant before and after all yielding
      work. On failure publish no partial graph or source list. *)
  val capture
    :  t
    -> actor:Operator_authorization.t
    -> clock:_ Eio.Time.Mono.t
    -> checkpoint:(unit -> unit)
    -> (Capture.t, Error.t) Result.t

  (** Bounded nonrecursive path assistance through the selected retained root.
      Count every native entry against the existing source policy before
      filtering nonportable names; no ambient cwd/fs, implicit dependency scan,
      symlink traversal or hidden unbounded list. Results are candidates only. *)
  val list_directory
    :  t
    -> actor:Operator_authorization.t
    -> directory:Source.Portable_path.t option
    -> checkpoint:(unit -> unit)
    -> (Entry.t list, Error.t) Result.t

  (** Exact bounded read for an explicitly selected portable member of this
      granted root. It uses Local.read's no-follow descriptor operation and
      observed-change check, not a pathname read or a write lease. The returned
      bytes are a new read observation; only [capture] assembles a graph. *)
  val read_member
    :  t
    -> actor:Operator_authorization.t
    -> member:Source.Portable_path.t
    -> checkpoint:(unit -> unit)
    -> (string, Error.t) Result.t

  (** Idempotently revoke this context, cancel and join in-flight source work.
      Closing the connection closes its contexts before source-host shutdown;
      an immutable Capture returned earlier has an independent lifetime. All
      operations require the creating domain. Concurrent or reentrant operations
      reject Busy without waiting on their own gate. A close called from its
      own active operation raises [Invalid_argument] rather than self-joining. *)
  val close_and_wait : t -> unit
end

module Document_capture : sig
  (** One exact host-owned source observation. A host member is read through
      the admitted local root; supplied bytes are captured under a fresh
      host-minted source ID with no root grant. The receipt contains no live
      actor, path capability, graph, parser result or execution permission. *)
  type t

  val source : t -> Chatml_source.t
end

module Document_context : sig
  type t

  (** Uses the same host-owned context activity gate, actor/root rechecks,
      attempt/read quotas, cancellation stack and joined switch lifetime as
      project source contexts. Nested A -> B -> A work must still see A as
      active: closing A or the host from that stack rejects self-join. There
      is no parallel document-specific authority or resolver. *)

  module Origin : sig
    type t =
      | Host_member
      | Supplied
    [@@deriving equal, sexp_of]
  end

  val origin : t -> Origin.t
  val source_id : t -> Chatml_source.Id.t

  (** Recheck the original actor, current request actor, retained root or
      supplied-byte policy, host lifetime and creating domain without reading
      or yielding. The handler calls this before serving pinned ledger
      bytes/overlays or syntax, and again after any yielding preparation before
      publishing a response. It does not grant project or execution authority.
      Concurrent pure reads need no activity gate; an async handler must retain
      its own bounded request lifetime until the final check. *)
  val check_current : t -> actor:Operator_authorization.t -> (unit, Error.t) Result.t

  (** Bind one exact portable member of an explicitly selected read root.
      Reuse the host's existing root policy, original actor and joined
      operation gate; no project declaration, Build, selection or compiler
      contract is needed. Capture exact bytes once through the existing
      no-follow local read owner before publishing the context. A failed read
      registers no context or source. The source ID is minted by this host and
      remains stable across explicit refresh; later context source reads use
      the pure ledger's pinned bytes, not a new host observation. *)
  val open_host
    :  host
    -> actor:Operator_authorization.t
    -> root:Root.t
    -> member:Source.Portable_path.t
    -> checkpoint:(unit -> unit)
    -> (t * Document_capture.t, Error.t) Result.t

  (** Admit exact bounded supplied bytes under [Document_policy], independent
      of [Read_policy]. [member] is a validated source label, never a host path
      lookup. Mint a fresh source ID and capture exact bytes under the host's
      document/source limits, with no project/root authority. Failed capture
      registers no context. Do not normalize invalid UTF-8 or newlines. Return
      one immutable initial capture, which the pure ledger then admits. *)
  val open_supplied
    :  host
    -> actor:Operator_authorization.t
    -> member:Source.Portable_path.t
    -> contents:string
    -> checkpoint:(unit -> unit)
    -> (t * Document_capture.t, Error.t) Result.t

  (** Explicitly re-read only the same retained host root/member after the
      host-authorized context refresh action. A supplied context returns
      [Unsupported_source_selection]; changing supplied base bytes requires a
      new context. The old capture stays immutable. Recheck original and request
      actor/policy before and after yielding work; charge the same context
      attempt/read-byte caps. The pure ledger separately checks the expected
      generation and publishes the new base. *)
  val refresh
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (Document_capture.t, Error.t) Result.t

  (** Bounded nonrecursive path candidates on a host-member root only. This
      grants no member read and uses the existing root owner/policy; a supplied
      document cannot browse a host root. *)
  val list_directory
    :  t
    -> actor:Operator_authorization.t
    -> directory:Source.Portable_path.t option
    -> checkpoint:(unit -> unit)
    -> (Entry.t list, Error.t) Result.t

  (** Idempotently revoke/cancel/join work, including from host switch release.
      The creating Eio domain owns this context. Concurrent/reentrant work
      fails [Busy]; closing one's own active operation, including through a
      nested other context, raises Invalid_argument rather than self-joining. *)
  val close_and_wait : t -> unit
end

(** Idempotently revoke, cancel and join every context, including from the
    owner's switch release hook. Explicit close unregisters that hook.
    Calling from an active operation on this host raises [Invalid_argument]. *)
val close_and_wait : t -> unit
