(** Typed per-execution observations and bounded retained-prefix recorder.
    Source ownership comes from native captured sites; current execution identity
    comes from the host-owned Eio scope. This library grants no execution or read
    authority and performs no source-content rendering or host I/O. *)

open Core

module Execution_id : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Host-generated opaque developer-run or REPL-action ID. This is neither
      Agent_protocol.Run (conversation) nor a job/source/session ID. The host
      maps its actual receipt identity here; this constructor grants no read
      authority. Reject empty, oversized, non-portable encodings. *)
  val of_opaque_string : string -> t Or_error.t

  val to_opaque_string : t -> string
end

module Header : sig
  type t

  module Source_ref : sig
    type t

    val id : t -> Chatml_source.Id.t
    val digest : t -> Chatml_source.Digest.t
    val byte_length : t -> int
  end

  module Admission : sig
    type t =
      | Submitted_capture
      | Prepared_graph
  end

  (** Freeze only bounded identity summaries, never source bytes or a borrowed
      editor context. The host allocates the execution ID before source/graph
      preparation. If that preparation fails, it can create a truthful
      Submitted_capture header from the immutable submitted capture receipt and
      any successfully captured sources. No graph/contract identity is invented.
      This constructor can admit an empty source inventory for a failure before
      a source was captured. The submission fingerprint is host-computed from
      the exact admitted payload/receipt, not a client assertion. Duplicate,
      ASCII-case-colliding or file/directory-conflicting source paths within a
      namespace reject rather than leaving an ambiguous inventory. *)
  val of_submitted_capture
    :  execution_id:Execution_id.t
    -> submission_fingerprint:Chatml_source.Digest.t
    -> sources:Chatml_source.t list
    -> selection_fingerprint:Chatml_source.Digest.t
    -> max_inventory_entries:int
    -> max_header_bytes:int
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  (** [prepared] supplies exact graph, contract, environment and source
      ID/digest inventory after preparation succeeded. The independent
      [submission_fingerprint] preserves the admitted payload receipt. A compile
      failure can therefore have a Prepared_graph header and zero runtime
      events. Both constructors charge inventory work to the finite host budget.
      Source sites from older REPL closures may lie outside this submission's
      inventory; their validated native identity stays on the event and OCH-273
      must retain and authorize that historic source custody separately. *)
  val of_prepared_graph
    :  execution_id:Execution_id.t
    -> submission_fingerprint:Chatml_source.Digest.t
    -> prepared:Chatml_prepared_program.t
    -> selection_fingerprint:Chatml_source.Digest.t
    -> max_inventory_entries:int
    -> max_header_bytes:int
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val execution_id : t -> Execution_id.t
  val admission : t -> Admission.t
  val submission_fingerprint : t -> Chatml_source.Digest.t
  val graph_fingerprint : t -> Chatml_source.Digest.t option
  val contract_fingerprint : t -> Chatml_source.Digest.t option

  (** Derived from the selected Prepared.Environment for a prepared graph; None
      before preparation. The authoritative OCH-273 receipt separately binds
      project/provenance, REPL session/epoch/revision and admission rights. *)
  val environment_fingerprint : t -> Chatml_source.Digest.t option

  val selection_fingerprint : t -> Chatml_source.Digest.t
  val source_inventory : t -> Source_ref.t list
end

module Event : sig
  module Phase : sig
    type t =
      | Preparation
      | Compilation
      | Dependency_initialization
      | Entry_initialization
      | Evaluation
      | Task
      | Host_wait
    [@@deriving equal, sexp_of]
  end

  module Operation : sig
    (** Fixed public labels only. The host maps known registry operations to
        these constructors; a source-defined/unknown/sensitive name is Other.
        There is no raw string operation constructor. *)
    type t =
      | Tool_call
      | Tool_spawn
      | Job_start
      | Model_call
      | Model_spawn
      | Process_run
      | Schedule
      | Job_read
      | Local_transaction
      | Other
    [@@deriving equal, sexp_of]
  end

  module Failure : sig
    (** Safe categories, independent of diagnostic/exception messages. *)
    type t =
      | Parse
      | Typecheck
      | Link
      | Runtime
      | Host_operation
      | Permission
      | Resource_limit
      | Other
    [@@deriving equal, sexp_of]
  end

  module Correlation : sig
    type t [@@deriving equal, sexp_of]

    (** Host-generated bounded opaque token. It contains no source, operation
        arguments/results, prompt, credential or user-controlled label. *)
    val of_host_token : string -> t Or_error.t
  end

  module Kind : sig
    (** Effect_host_returned only reports a host return. It does not claim
        transaction commit or rollback of any earlier external effect.
        Recovered_failure means an error reached Task.catch; the catch handler
        or later work can still fail, so only Terminal states run disposition. *)
    type t =
      | Phase_started of Phase.t
      | Phase_finished of Phase.t
      | Statement_entered
      | Call_entered
      | Effect_attempted of Operation.t
      | Effect_host_returned of Operation.t
      | Effect_host_failed of Operation.t
      | Host_wait_entered
      | Host_wait_resumed
      | Job_admitted
      | Job_terminal
      | Recovered_failure of Failure.t
      | Unrecovered_failure of Failure.t
    [@@deriving equal, sexp_of]
  end

  type t

  val sequence : t -> int64
  val kind : t -> Kind.t
  val site : t -> Chatml_source.Location.t option
  val correlation : t -> Correlation.t option
end

module Terminal : sig
  module Disposition : sig
    type t =
      | Succeeded
      | Failed of Event.Failure.t
      | Cancelled
      | Denied
      | Interrupted
    [@@deriving equal, sexp_of]
  end

  type t

  (** The lifecycle owner creates this only from its authoritative terminal
      receipt after cancellation/join. The recorder cannot infer success. Fixed
      disposition/version metadata always fits its reserved space; an arbitrary
      source ID (whose package namespace has no global length ceiling) cannot
      consume that reserve. The canonical diagnostic endpoint retains any
      primary failure site, and normal trace events may carry it when admitted. *)
  val create : receipt_version:int -> disposition:Disposition.t -> t Or_error.t

  val disposition : t -> Disposition.t
  val receipt_version : t -> int
end

module Recorder : sig
  type t

  module Mode : sig
    (** Summary keeps phases, effect/wait transitions and failures. Full also
        admits statement/call entries at existing evaluator boundaries. Disabled
        execution passes no recorder and incurs only the Scope guard branch. *)
    type t =
      | Summary
      | Full
    [@@deriving equal, sexp_of]
  end

  module Limits : sig
    type t

    (** The normal-event area may be zero. Fixed terminal/drop metadata uses
        eight 64-bit words (64 bytes), with a separate reserve at least that
        large; normal events cannot consume it. Normal-event accounting is a
        concrete binary v1 layout: six fixed 64-bit words; a site adds two
        length words, exact namespace/path bytes, 64 digest bytes and six
        position words; a correlation adds one length word and bounded bytes.
        Arbitrary namespace bytes remain lossless. A JSON wire adapter has an
        independent encoded-size bound and base64-encodes opaque ID bytes. *)
    val create
      :  max_events:int
      -> max_normal_bytes:int
      -> max_terminal_bytes:int
      -> t Or_error.t

    (** Comparison-only validated capacities for an outer host's conservative
        aggregate retention charge. These do not mint another recorder budget
        or change event admission. *)
    val max_normal_bytes : t -> int

    val max_terminal_bytes : t -> int
  end

  module Drop_count : sig
    type t

    val at_least : t -> int64
    val overflowed : t -> bool
  end

  module Snapshot : sig
    type state =
      | Incomplete
      | Complete of Terminal.t

    type t

    val header : t -> Header.t
    val prefix : t -> Event.t list
    val last_sequence : t -> int64
    val omitted_after_prefix : t -> Drop_count.t option

    (** The last source site in the retained prefix, explicitly contextual and
        possibly older than many omitted observations. Never a claimed failure
        origin or a substitute for the canonical structured diagnostic. *)
    val last_retained_site_context : t -> Chatml_source.Location.t option

    (** Number of retained event sites omitted by a historical source-read
        projection. Recorder snapshots start at zero; repeated projection
        preserves prior omissions and cannot restore a hidden site. This count
        is distinct from normal-event saturation/drop metadata. *)
    val omitted_site_count : t -> int

    (** Pure bounded copy over the retained prefix. [authorize] must be a
        nonyielding, nonraising site predicate. Unauthorized source locations
        become absent while event sequence/order/kind/correlation, header,
        terminal and existing drop metadata remain unchanged. The last-site
        context is recomputed from the still-authorized prefix. No source ID
        or position of an omitted site is returned. *)
    val with_authorized_sites : t -> authorize:(Chatml_source.Location.t -> bool) -> t

    val state : t -> state
  end

  val create : Header.t -> mode:Mode.t -> limits:Limits.t -> t

  (** Caller is the authoritative lifecycle owner on the creating domain. One
      terminal publication,
      after cancellation/join, leaves normal prefix/drop metadata untouched.
      An owner crash before this call leaves Incomplete. Terminal metadata must
      fit its independent reserve; an invalid reserve is rejected at create. *)
  val finish : t -> Terminal.t -> unit Or_error.t

  (** Bounded immutable read copy; OCH-273 owns retention, current read grants,
      stable pagination, wire DTOs, slow observers and reconnect. *)
  val snapshot : t -> Snapshot.t
end

module Scope : sig
  (* Bind an optional active recorder to this Eio fiber's execution scope.
      Nested scopes shadow rather than mix identities. The recorder belongs to
      its creating domain. Inherited child fibers on that SAME domain may emit
      while the scope is live: append does not yield, so their attempts have one
      serialized admission order. A different domain cannot bind/append to this
      recorder; a future cross-domain producer needs a separately bounded
      nonblocking handoff that marks queue loss before later owner appends.
      An active lifetime guard prevents late child writes after owner exit or
      finish; independently owned jobs must clear the binding.
      Source attribution is independent: eval uses the defining closure's
      env.source and tasks use their existing owned Site, never this scope's
      submission source. A domain-local enabled-scope guard avoids calling
      Eio.Fiber.get when plain evaluator callers run outside Eio.run; a disabled
      trace remains one cheap branch and cannot raise an unhandled Fiber effect.
      No mutable process-global text sink or callback. *)
  (** Requires an Eio fiber on the recorder's creating domain. Binding a
      foreign-domain recorder raises Invalid_argument before user code runs.
      Ordinary evaluator calls with no active scope remain valid outside Eio. *)
  val within : Recorder.t option -> (unit -> 'a) -> 'a

  val without_current : (unit -> 'a) -> 'a

  (* All observation functions are owner-domain, non-yielding and callback-free.
      They accept only finite typed metadata, no values, arguments, results,
      exception strings or source excerpts. Retained normal sequences start at
      one and are contiguous; first count/byte/encoding miss permanently
      saturates retention, and later attempts increment the drop counter without
      receiving sequence numbers. Closed/terminal scopes discard late attempts.
      Normal event site IDs and correlation lengths are checked before encoding
      or scanning, including arbitrarily long package namespaces and JSON
      escaping risk. Once saturated, attempts count as drops without location
      validation or allocation. No direct public Recorder.append bypasses this
      ownership boundary. *)

  (** Branch on disabled/saturated state before constructing any event or
      source location. Span conversion uses exact immutable source and a pure
      non-yielding checkpoint, never the execution budget callback; failure to
      validate a trace-only site degrades this observation without changing
      interpreter result or cancellation. [source=None] cannot invent a site. *)
  val observe_span
    :  kind:Event.Kind.t
    -> source:Chatml_source.t option
    -> span:Source.span
    -> correlation:Event.Correlation.t option
    -> unit

  (** Uses only the exact native ChatML site. A ChatMD declaration-related or
      approximate authored projection is not converted into a native location;
      OCH-273 may present that separate related context after validation. *)
  val observe_site
    :  kind:Event.Kind.t
    -> site:Chatml_diagnostic.Site.t option
    -> correlation:Event.Correlation.t option
    -> unit

  val observe_unlocated
    :  kind:Event.Kind.t
    -> correlation:Event.Correlation.t option
    -> unit

  (** A trusted host adapter brackets a real Eio-controlled blocking operation
      with wait-entered/resumed observations. A normal return, including an
      ordinary error result, resumes; cancellation and exceptions propagate
      unchanged and do not invent a resume. Never infer a wait from an operation
      name or a nonblocking status query. This does not authorize the operation
      or retain its callback, arguments or result. *)
  val with_host_wait : site:Chatml_diagnostic.Site.t option -> (unit -> 'a) -> 'a
end
