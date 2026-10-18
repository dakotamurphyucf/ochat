open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

(** Process-local, bounded custody for the existing Chatml_trace recorder. The
    Developer Host and REPL epoch select authentic executions; this module does
    not admit a command, evaluate code, read source bytes, or write a second
    result ledger. Its identities and retained provenance are comparison data,
    never a bearer read grant. *)
type t

module Limits : sig
  type t

  (** Positive finite aggregate count/byte/age caps, including source summaries
      and expired tombstones. Source identity strings are charged before
      retention, not merely counted; checked arithmetic rejects configurations
      that cannot fit one maximal admitted trace. Recorder limits remain
      independent per execution and are conservatively charged through their
      validated capacity accessors. Exhaustion may omit a trace or evict an old
      one; it never rejects the underlying execution. Age uses the supplied
      monotonic clock. *)
  val create
    :  max_traces:int
    -> max_retained_bytes:int
    -> max_source_refs_per_trace:int
    -> max_provenance_bytes_per_trace:int
    -> max_expired_tombstones:int
    -> max_age_seconds:float
    -> max_header_bytes:int
    -> recorder:Chatml_trace.Recorder.Limits.t
    -> t Or_error.t
end

module Provenance : sig
  type t

  type action =
    | Command of Store.Command.Receipt.Kind.t
    | Repl_statement

  (** A historical source lineage: original principal ID and authentication
      kind, exact admitted route and selected graph/contract fingerprint, and
      bounded native source ID/digest/byte-length summaries. It retains no
      actor token, source bytes, mutable editor context, old Root, or execution
      grant. Current scopes/attributes are deliberately absent. *)
  val of_prepared
    :  action:action
    -> original_actor:Operator_authorization.t
    -> route:Developer_command_original.Route.t
    -> capture_fingerprint:Chatml_source.Digest.t option
    -> selection_fingerprint:Chatml_source.Digest.t
    -> prepared:Chatml_prepared_program.t
    -> limits:Limits.t
    -> (t, P.Error.t) result

  (** Extend one REPL epoch's immutable source history only with a captured
      snippet produced by that epoch. The earlier summaries remain available
      for sites in retained closures. If the finite cap is exhausted, omit a
      subsequent trace rather than dropping provenance or blocking execution.
      This operation does not keep the snippet's bytes. *)
  val add_captured_source
    :  t
    -> Chatml_source.t
    -> limits:Limits.t
    -> (t, P.Error.t) result

  (** Comparison-only projection for a Task actually retained by this REPL
      epoch. It preserves the original principal, route, bounded source
      history and selected contract identity, sets only the action to
      [Repl_task_run], and adds the capture fingerprint from the actual
      Reserved Task row. The extra retained digest is charged against the
      provenance cap. The Host still checks the physical Task origin and row
      before creating an Entry. No source grant or actor authority is minted. *)
  val for_retained_task
    :  t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> limits:Limits.t
    -> t option

  val route : t -> Developer_command_original.Route.t
  val original_principal_id : t -> P.Id.Principal.t
  val original_authentication_kind : t -> string
  val selection_fingerprint : t -> Chatml_source.Digest.t
  val action : t -> action

  (** A native site is admitted only when its exact ID and digest occur in
      this immutable history AND its half-open byte range fits the retained
      captured source length. The caller never resolves it against the latest
      file, editor generation or snippet. The original Chatml_source validates
      canonical line/column metadata. No authored map is
      manufactured: absent a genuine map bound to that original capture, a
      later projection reports Unmapped with the native site intact. *)
  val contains_site : t -> Chatml_source.Location.t -> bool
end

module Association : sig
  type t

  (** Captured from the actual Reserved row as bounded immutable fields, not
      by retaining the Row object. A later persisted lookup must compare the
      entire row identity, kind, owner, key and both fingerprints. The row
      itself never grants trace read permission. *)
  val of_reserved_row : Store.Command.Row.t -> (t, P.Error.t) result

  (** Epoch-owned coordinates are issued from the physical instance and its
      key ledger, not decoded from a client. They remain valid historical
      comparison data after ordinary reset/close, while a new statement still
      requires the live source/epoch admission. *)
  val repl_statement
    :  run_id:P.Id.Developer_run.t
    -> instance_nonce:P.Id.Transaction.t
    -> epoch_serial:int
    -> key:P.Idempotency_key.t
    -> selection_fingerprint:Chatml_source.Digest.t
    -> (t, P.Error.t) result

  val matches_row : t -> Store.Command.Row.t -> bool
end

module Entry : sig
  type t

  (** Bounded token and association only. It never retains the Recorder,
      captured bytes or provenance. The owner keeps those in its charged
      table; eviction/close can release them even while a command/epoch keeps
      its Entry for historical no-replay comparison. *)
  val association : t -> Association.t

  val execution_id : t -> Chatml_trace.Execution_id.t
end

(** Comparison-only lookup in the owner's bounded charged table for an actual
    persisted Command/Task row. It scans at most [max_traces] entries and
    compares full immutable association fields. It cannot restore an evicted
    recording or authorize a read; the caller still uses begin_read/snapshot
    after its own row disclosure check. Active entries remain present until
    joined terminal notification. REPL statement keys retain their own
    independently bounded Entry handles. *)
val find_command_entry : t -> Store.Command.Row.t -> Entry.t option

module Availability : sig
  type t =
    | Unavailable
    | Expired
    | Retained of Chatml_trace.Recorder.Snapshot.t
end

module Read_policy : sig
  type t

  (** A distinct, trusted historical metadata/source-read decision. The
      callback examines the CURRENT authenticated actor and immutable source
      lineage; it may consult current configured root/project/bundle read
      grants, but must not recapture bytes or demand that the old execution's
      selected generation, Root or REPL epoch still be executable. An edited
      file does not reinterpret an old site. A revoked principal/source read
      grant denies it. Generic receipt scope or a row ID alone grants nothing.
      The callback is bounded, nonyielding and nonraising. *)
  val create : authorize:(Operator_authorization.t -> Provenance.t -> bool) -> t

  val deny_all : t
end

module Read_request : sig
  (** One Host read operation's original current actor and entry token, not
      retained provenance or a snapshot. It cannot pin an evicted recorder and
      must be reauthorized after the Host's yielding membership lookup. *)
  type t
end

module Configuration : sig
  type t

  (** An explicit installed mode and historical-read policy. Absence of this
      configuration at the Host is disabled tracing, not an implicit grant. *)
  val create
    :  mode:Chatml_trace.Recorder.Mode.t
    -> limits:Limits.t
    -> read_policy:Read_policy.t
    -> t
end

(** The daemon's original clock and switch own eviction/close; no background
    observer fiber or global sink is created. *)
val create
  :  sw:Eio.Switch.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> configuration:Configuration.t
  -> t

(** Comparison-only installed capacity for capturing finite provenance before
    a statement or command enters. This does not authorize retention/read. *)
val limits : t -> Limits.t

(** Issue a safe host-generated trace ID and bind a Prepared header to this
    exact association/provenance. [prepared] must be the already admitted
    capture or REPL selection, and its source summaries must match provenance.
    Command association kind/principal/capture fingerprint and REPL statement
    selection fingerprint must match the corresponding provenance/action and
    submission identity before any entry is published.
    A trace admission/limit failure returns None and cannot alter the
    command/statement. Active entries remain charged and cannot be evicted
    while their scope may append; when only active entries occupy capacity,
    omit the new trace. Settled entries may be evicted under finite count,
    byte and age policy. Shared cancellation still propagates unchanged. *)
val begin_prepared
  :  t
  -> association:Association.t
  -> provenance:Provenance.t
  -> submission_fingerprint:Chatml_source.Digest.t
  -> prepared:Chatml_prepared_program.t
  -> checkpoint:(unit -> unit)
  -> Entry.t option

(** A rejected or Ready REPL submission, or a durable Task using an earlier
    retained snippet, can have no new prepared graph. Bind its actual captured
    source to the authentic epoch/key or Task row; do not infer a graph/contract
    identity. An inert Need_more result has no execution trace. *)
val begin_submitted
  :  t
  -> association:Association.t
  -> provenance:Provenance.t
  -> submission_fingerprint:Chatml_source.Digest.t
  -> source:Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> Entry.t option

(** Bind only around the actual joined interpreter body. None is the normal
    disabled/omitted path. Independently scheduled jobs run with no inherited
    parent trace scope; the parent may retain only truthful admission events. *)
val within : t -> Entry.t option -> (unit -> 'a) -> 'a

(** Trusted authoritative owner call after execution children joined AND
    terminal receipt/outcome was retained. Idempotent for the same terminal;
    unknown settlement leaves Incomplete; a conflicting terminal expires the
    recording, never exposing a false Complete. This is nonyielding and
    never changes the execution result, even if trace publication fails. A
    later exact reconciliation can call it again without replaying work. *)
val finish : t -> Entry.t -> Chatml_trace.Terminal.t -> unit

(** Authenticate a retained physical entry for one read request. The actor is
    the ORIGINAL actor of this new read, not the old execution token. A fresh
    actor can therefore read a historical completed trace after reconnect,
    subject to same principal/authentication kind and current read policy. *)
val begin_read
  :  t
  -> actor:Operator_authorization.t
  -> Entry.t
  -> (Read_request.t, P.Error.t) result

(** After the Host's yielding exact Row or REPL-key observation, check the same
    physical entry, original/current read actors, current historical source
    policy and trusted nonyielding exact-membership suffix before copying a
    bounded immutable snapshot. This never treats a coordinate/cursor as a
    grant. An Expired tombstone keeps finite, charged provenance long enough
    for the same read policy; it has no Recorder or source bytes. Once the
    tombstone itself is evicted, Unavailable exposes no trace metadata and the
    Host still requires its ordinary row/epoch read permission. A request
    begun before eviction/close cannot pin old data or bypass reauthorization. *)
val snapshot
  :  t
  -> Read_request.t
  -> current_actor:Operator_authorization.t
  -> exact_membership:(Association.t -> bool)
  -> (Availability.t, P.Error.t) result

(** Joined owner shutdown discards all recorders and metadata. Process restart
    cannot reconstruct old events from durable command rows. *)
val close : t -> unit
