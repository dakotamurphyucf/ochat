(** Closed, versioned, read-only Developer trace pages. A locator, execution ID
    or cursor is comparison data, never source-read, execution or disclosure
    authority. The installed Host authenticates every page independently. No
    source contents, prompts, values, arguments or output enter this wire. *)

module Features : sig
  val trace : string
end

module Limits : sig
  type t

  (** [request] includes the locator and cursor; [page] includes the repeated
      Header, all selected events, metadata and next cursor; [response]
      includes method framing around the whole page. Each Document_schema
      limit covers exact compact JSON bytes (including escaping), depth,
      total fields and nodes. Startup also accounts for the JSON-RPC request
      and success/error envelope around these method values against the
      negotiated Initialize capacity, including the bounded request ID.
      [max_cursor_bytes] bounds the complete encoded
      cursor object, not merely its opaque fields. All bounds are finite and
      positive. Daemon installation compares them with Initialize and the
      actual Host retention owner before advertising the method. *)
  val create
    :  request:Document_schema.Limits.t
    -> page:Document_schema.Limits.t
    -> response:Document_schema.Limits.t
    -> max_page_items:int
    -> max_cursor_bytes:int
    -> (t, Error.t) result

  val request : t -> Document_schema.Limits.t
  val page : t -> Document_schema.Limits.t
  val response : t -> Document_schema.Limits.t
  val max_page_items : t -> int
  val max_cursor_bytes : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Locator : sig
  type t =
    | Command of Developer_common.Lookup.t
    | Repl_statement of
        { instance : Developer_repl_wire.Wire_id.Instance.t
        ; epoch : Developer_repl_wire.Wire_id.Epoch.t
        ; key : Idempotency_key.t
        }
    | Repl_task of Developer_common.Lookup.t

  (** The REPL statement's epoch is historical comparison data. It is resolved
      through the installed bounded registry to the exact physical token, not
      interpreted as a current editor lease. Command and Task share durable
      run/key syntax but remain different disclosure families. *)
  val to_json : t -> Jsonaf.t

  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
  val equal : t -> t -> bool
end

module Cursor : sig
  type t

  (** A page cursor binds the complete locator, host-minted Execution_id,
      exact admitted Header submission/selection fingerprints, schema version
      and last emitted sequence. One retained Entry exists per authenticated
      association; this identity is the comparison generation, with no
      redundant ordinal. A fabricated or foreign cursor fails only after the
      Host reauthorizes and obtains the exact current snapshot. Cursors cannot
      restore an evicted recording or authorize old source access. The
      positive Int64 sequence is encoded as a decimal string, avoiding
      JSON-number precision loss. *)
  val create
    :  limits:Limits.t
    -> locator:Locator.t
    -> execution_id:string
    -> submission_fingerprint:Chatml_source.Digest.t
    -> selection_fingerprint:Chatml_source.Digest.t
    -> after_sequence:int64
    -> (t, Error.t) result

  val locator : t -> Locator.t
  val execution_id : t -> string
  val submission_fingerprint : t -> Chatml_source.Digest.t
  val selection_fingerprint : t -> Chatml_source.Digest.t
  val after_sequence : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Page_request : sig
  type t

  (** [limit] is positive and at most installed max_page_items. Absent cursor
      starts at the first retained event. The request locator is repeated in
      and must match any cursor, before the Host later checks physical owner
      and current source-read policy. Explicit null is not absence. *)
  val create
    :  limits:Limits.t
    -> locator:Locator.t
    -> after:Cursor.t option
    -> limit:int
    -> (t, Error.t) result

  val locator : t -> Locator.t
  val after : t -> Cursor.t option
  val limit : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Location : sig
  type t

  (** Native captured source identity and digest plus the original half-open
      byte span and canonical byte-based line/column coordinates. Construction
      checks shape and portable source-ID syntax; only the Host's already
      authorized native location establishes captured-source membership. An
      absent genuine authored map stays absent; no current document lookup or
      guessed coordinates are performed here. The package namespace is opaque
      bytes and uses the existing lossless portable base64 source-ID encoding;
      the validated relative member remains UTF-8. *)
  val create
    :  source:Chatml_source.Id.t
    -> digest:Chatml_source.Digest.t
    -> start_byte:int
    -> end_byte:int
    -> start_line:int
    -> start_column:int
    -> end_line:int
    -> end_column:int
    -> (t, Error.t) result

  val source : t -> Chatml_source.Id.t
  val digest : t -> Chatml_source.Digest.t
  val start_byte : t -> int
  val end_byte : t -> int
  val start_line : t -> int
  val start_column : t -> int
  val end_line : t -> int
  val end_column : t -> int
end

module Header : sig
  module Source_ref : sig
    type t

    val create
      :  id:Chatml_source.Id.t
      -> digest:Chatml_source.Digest.t
      -> byte_length:int
      -> (t, Error.t) result

    val id : t -> Chatml_source.Id.t
    val digest : t -> Chatml_source.Digest.t
    val byte_length : t -> int
  end

  type admission =
    | Submitted_capture
    | Prepared_graph
  [@@deriving equal, sexp_of]

  type t

  (** Exact immutable native Header fields only. Submitted_capture requires
      all graph/contract/environment fingerprints absent; Prepared_graph
      requires all three present. Source IDs use lossless namespace encoding,
      and duplicate/colliding inventory entries reject on construction and
      decode rather than changing source identity. *)
  val create
    :  execution_id:string
    -> admission:admission
    -> submission_fingerprint:Chatml_source.Digest.t
    -> selection_fingerprint:Chatml_source.Digest.t
    -> graph_fingerprint:Chatml_source.Digest.t option
    -> contract_fingerprint:Chatml_source.Digest.t option
    -> environment_fingerprint:Chatml_source.Digest.t option
    -> sources:Source_ref.t list
    -> (t, Error.t) result

  val execution_id : t -> string
  val admission : t -> admission
  val submission_fingerprint : t -> Chatml_source.Digest.t
  val selection_fingerprint : t -> Chatml_source.Digest.t
  val graph_fingerprint : t -> Chatml_source.Digest.t option
  val contract_fingerprint : t -> Chatml_source.Digest.t option
  val environment_fingerprint : t -> Chatml_source.Digest.t option
  val sources : t -> Source_ref.t list
end

module Event : sig
  type phase =
    | Preparation
    | Compilation
    | Dependency_initialization
    | Entry_initialization
    | Evaluation
    | Task
    | Host_wait
  [@@deriving equal, sexp_of]

  type operation =
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

  type failure =
    | Parse
    | Typecheck
    | Link
    | Runtime
    | Host_operation
    | Permission
    | Resource_limit
    | Other
  [@@deriving equal, sexp_of]

  type kind =
    | Phase_started of phase
    | Phase_finished of phase
    | Statement_entered
    | Call_entered
    | Effect_attempted of operation
    | Effect_host_returned of operation
    | Effect_host_failed of operation
    | Host_wait_entered
    | Host_wait_resumed
    | Job_admitted
    | Job_terminal
    | Recovered_failure of failure
    | Unrecovered_failure of failure
  [@@deriving equal, sexp_of]

  type t

  (** Sequence is monotone in the original recorder and encoded as a decimal
      string; site filtering does not
      renumber events or remove kind/correlation. Correlation is a validated
      host-generated opaque token, never source-defined text. *)
  val create
    :  sequence:int64
    -> kind:kind
    -> site:Location.t option
    -> correlation:string option
    -> (t, Error.t) result

  val sequence : t -> int64
  val kind : t -> kind
  val site : t -> Location.t option
  val correlation : t -> string option
end

module Terminal : sig
  type disposition =
    | Succeeded
    | Failed of Event.failure
    | Cancelled
    | Denied
    | Interrupted
  [@@deriving equal, sexp_of]

  type t

  val create : receipt_version:int -> disposition:disposition -> (t, Error.t) result
  val receipt_version : t -> int
  val disposition : t -> disposition
end

module Page : sig
  type drop =
    { at_least : int64
    ; overflowed : bool
    }

  type state =
    | Incomplete
    | Complete of Terminal.t

  type t

  (** One immutable authorized snapshot. [events] retain original recorder
      sequence/order. [last_sequence] and drop/omitted-site metadata describe
      the entire snapshot at this read, not only this page. [resume] binds
      the final emitted event, or repeats [after] when the current page is
      empty. A client may pass [resume] on a later poll of an Incomplete trace
      even when [next=None], without replaying the retained prefix; an empty
      first page has no resume and is polled from its initial request.
      [next] is present only when this SAME snapshot has more retained events,
      and equals [resume]. A later read may observe a longer prefix or terminal
      but never rewinds within an Entry. No cursor points to an unreturned
      item. An item that cannot fit the complete encoded response yields a
      bounded Resource_limit error rather than an empty repeating page.

      Constructor AND decoder reject nonpositive/nonconsecutive event
      sequences, sequences above [last_sequence], an event at/before [after],
      or an [after] sequence above [last_sequence] even for an empty page,
      inconsistent locator/Header/cursor identity, a resume not at the final
      returned event (or not exactly [after] for an empty page), a [next] that
      differs from resume, is missing while more retained events remain, or
      accompanies an exhausted prefix, negative omitted-site
      counts and nonpositive drop counts. Header admission and location shape
      invariants are likewise rechecked on decoding. Event sites may be
      authentic older REPL closure sources outside this submission Header's
      source inventory; the Host's historical provenance check authorizes
      them before projection, and the wire validates their location shape. *)
  val create
    :  limits:Limits.t
    -> locator:Locator.t
    -> after:Cursor.t option
    -> header:Header.t
    -> events:Event.t list
    -> last_sequence:int64
    -> omitted_after_prefix:drop option
    -> last_retained_site_context:Location.t option
    -> omitted_site_count:int
    -> state:state
    -> resume:Cursor.t option
    -> next:Cursor.t option
    -> (t, Error.t) result

  val locator : t -> Locator.t
  val after : t -> Cursor.t option
  val header : t -> Header.t
  val events : t -> Event.t list
  val last_sequence : t -> int64
  val omitted_after_prefix : t -> drop option
  val last_retained_site_context : t -> Location.t option
  val omitted_site_count : t -> int
  val state : t -> state
  val resume : t -> Cursor.t option
  val next : t -> Cursor.t option
end

module Availability : sig
  type t =
    | Unavailable
    | Expired
    | Retained of Page.t
end

module Capabilities : sig
  type t

  (** Fixed bootstrap bound before reading peer-advertised Limits. The methods
      list is the actually installed trace family, not codec inventory. *)
  val max_json_bytes : int

  val create : methods:string list -> limits:Limits.t -> (t, Error.t) result
  val methods : t -> string list
  val limits : t -> Limits.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Request : sig
  type t =
    | Capabilities
    | Page of Page_request.t

  val method_name : t -> string
  val params : t -> Jsonaf.t

  val of_method_and_params
    :  limits:Limits.t
    -> method_:string
    -> params:Jsonaf.t
    -> (t, Error.t) result

  val supported_methods : string list
end

module Response : sig
  type t =
    | Capabilities of Capabilities.t
    | Page of Availability.t

  val method_name : t -> string

  (** Validate complete compact encoded method result, including repeated
      Header/cursor and outer framing, against installed page/response limits
      before serialization. Decode validates the same shape, bounds and
      version; unknown fields/constructors and explicit null for absent
      options reject. *)
  val to_json : t -> limits:Limits.t -> (Jsonaf.t, Error.t) result

  val of_method_and_json
    :  limits:Limits.t
    -> method_:string
    -> Jsonaf.t
    -> (t, Error.t) result
end
