open! Core

(** Pure single-session retained inference accounting. No provider/I/O dependency,
    archive index, pricing or authority granted to model/RPC input. Runtime actor
    publication remains behind the existing durable acknowledgement. *)
module Limit : sig
  type t =
    | Attempt_count
    | Turn_count
    | Retained_bytes
    | Protected_future_data
  [@@deriving equal, sexp_of]
end

module Error : sig
  type t =
    | Document of Document_schema.Error.t
    | Observation of Inference.Observation.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Conflicting_handle
    | Ordinal_exhausted
    | Invalid_transition
  [@@deriving equal, sexp_of]
end

module Limits : sig
  type t

  (** Positive bounds. Admission measures complete encoded ledger/rows, including
      identities, coverage and preserved unknown fields. No serialized-output
      allocation precedes bounded admission. These are encoded-byte bounds, not
      an OCaml heap guarantee or permission to forget an active attempt. *)
  val create
    :  max_attempts:int
    -> max_turns:int
    -> max_retained_bytes:int
    -> document_limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  (** Durable session profile: 256 attempts, 256 host turns, 4 MiB complete encoded
      ledger; structural admission depth 256, one million fields, two million nodes.
      State/actor/migration share this exact profile. *)
  val default : t
end

module Handle : sig
  type t

  (** An actual host admission, never constructed from client strings. Ordinal is
      positive, monotone and durable across generation replacement. Source is the
      actual host source qualified by this session; attempt ID is derived from
      the admitted ordinal, so retired scope cannot be allocated again. Fixed
      accounting/context IDs belong to this exact scope. A handle is returned to
      inference only AFTER actor persistence acknowledgement, tracked or not. *)
  val equal : t -> t -> bool

  val session_id : t -> Agent_protocol.Id.Session.t
  val generation : t -> int
  val ordinal : t -> int64
  val scope : t -> Transcript.Scope.t
  val accounting_id : t -> Inference.Observation.Observation_id.t
  val context_id : t -> Inference.Observation.Observation_id.t
  val operation_id : t -> Agent_protocol.Id.Operation.t option
  val invocation_id : t -> Agent_protocol.Id.Invocation.t option
end

module Row : sig
  type t

  val handle : t -> Handle.t
  val record : t -> Inference.Observation.Attempt_record.t
end

module Turn_handle : sig
  type t

  (** Identity is the existing actual host Turn operation ID plus generation;
      ordinal is retained-window ordering only, not another definition of turn.
      Compaction and provider-terminal events never create this receipt. *)
  val operation_id : t -> Agent_protocol.Id.Operation.t

  val generation : t -> int
end

type tracking =
  | Tracked
  | Untracked of Limit.t
[@@deriving equal, sexp_of]

type observation_disposition =
  | Added
  | Replaced
  | Duplicate
  | Stale
  | Ignored_retired
  | Ignored_untracked
  | Diagnostic_omitted
[@@deriving equal, sexp_of]

type t

(** Empty retained window. [before_tracking_unknown] is true for migrated data;
    it never invents historical zero or a fabricated historical attempt. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> before_tracking_unknown:bool
  -> limits:Limits.t
  -> (t, Error.t) Result.t

(** Changes the generation for FUTURE admissions; preserves ordinal, retained
    rows/receipts and coverage. Prior active rows must first be interrupted by the
    actual reset/recovery owner, rather than silently discarded. *)
val with_generation : t -> generation:int -> (t, Error.t) Result.t

(** Admit one actual dispatch before running it. Allocates scope/accounting IDs
    and durable ordinal regardless of UI observers. [source]/[relation] and host
    associations come from the actual runtime owner, never provider aliases.
    Configuration identifies actual opaque preparation and chosen transport.
    Oldest terminal rows are eligible for bounded retirement; active rows remain.
    Only an explicitly recognized unsafe-retirement carrier condition may select
    Protected_future_data. If no safe capacity exists, returns an Untracked handle
    and records partial coverage/limit status instead of deleting future data or
    blocking canonical work solely for telemetry capacity. Ordinal advance and
    Untracked coverage still require durable ACK before dispatch. Other codec,
    persistence, authentication and cancellation failures are NOT swallowed. *)
val admit
  :  t
  -> source:Transcript.Source_id.t
  -> relation:Transcript.Scope.relation
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> invocation_id:Agent_protocol.Id.Invocation.t option
  -> configuration:Inference.Observation.Configuration.t
  -> (t * Handle.t * tracking, Error.t) Result.t

(** Updates only the exact admitted present handle. Missing past ordinal returns
    Ignored_retired/untracked and never adds a charge; future/unallocated ordinal
    or contradictory scope/accounting/generation is an error. Late authoritative
    revisions can update a terminal retained row without reopening workflow.
    Usage must use accounting_id; Context must use context_id and actual matching
    preparation_id; Configuration must equal captured configuration. Complete
    snapshots replace, same revision conflicts reject, and no revisions are summed.
    Diagnostic ring quota omits display evidence with a durable omitted count;
    it does not interrupt canonical work. Non-diagnostic admission errors propagate.
    Error leaves t unchanged. No state revision is needed for duplicate/stale. *)
val observe
  :  t
  -> Handle.t
  -> Inference.Observation.t
  -> (t * observation_disposition, Error.t) Result.t

(** Actual host evidence only. Terminal Scope must match. Running may become
    Terminal/Interrupted; an ended attempt cannot reopen. Exact repeated state is
    idempotent; changed terminal evidence conflicts. No exception/cancellation is
    converted to a manufactured provider terminal. Missing retired/untracked row
    remains missing. *)
val set_state
  :  t
  -> Handle.t
  -> Inference.Observation.Attempt_record.state
  -> (t, Error.t) Result.t

(** Called in the SAME actor transaction as real Operation_started for kind Turn.
    Existing retained live identity dedupes; a retained terminal identity cannot
    be reused. The actual actor operation fence owns rejection after retirement;
    this ledger does not invent lifetime deduplication. Capacity uses the same
    explicit retained-window/tracking-limit contract. *)
val admit_turn
  :  t
  -> Agent_protocol.Operation.t
  -> (t * Turn_handle.t * tracking, Error.t) Result.t

(** Called only inside actual host operation terminal commit. Completed counts
    after final history admission/persistence acknowledgement, not provider done.
    Failed/cancelled/interrupted stay distinct. Duplicate/stale worker deliveries
    are rejected/fenced by the actor's actual operation owner before this call.
    Retired/untracked missing handle cannot add a completed turn retrospectively. *)
val finish_turn
  :  t
  -> Turn_handle.t
  -> Agent_protocol.Operation.t
  -> (t, Error.t) Result.t

(** Exact retained lookup; never admits a missing historical host occurrence. *)
val find_turn_handle
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> generation:int
  -> Turn_handle.t option

val session_id : t -> Agent_protocol.Id.Session.t
val generation : t -> int

(** Canonical session qualification for an actual host graph source; allocates no
    scope, ordinal or attempt. Admission uses this same validated operation. *)
val qualify_source
  :  t
  -> Transcript.Source_id.t
  -> (Transcript.Source_id.t, Error.t) Result.t

(** Check the exact identity and complete supplied profile. Immutable ledgers
    carry admission evidence for their original profile; semantically equal
    profiles reuse that evidence. Any different profile performs full document,
    domain and reserved-capacity admission. Abstract ledgers constructed under
    looser quotas cannot bypass State's profile. No normalization/reset or new
    ownership is published by validation. *)
val validate
  :  t
  -> limits:Limits.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Error.t) Result.t

val find : t -> ordinal:int64 -> Row.t option
val rows : t -> Row.t list

(** Read-only safe retained totals/coverage. Unknown/estimated/actual components
    stay distinct; inclusion subsets are never added into their container totals.
    Checked arithmetic exposes Overflow, never wrapped/exact invented totals. *)
val summary : t -> Agent_protocol.Inference_query.Summary.t

(** Host sets disclosure flags only AFTER visibility/Diagnostics authorization.
    False omits detailed fields; no raw Attempt_record encoding is exposed. *)
val row_view
  :  Row.t
  -> include_configuration:bool
  -> include_diagnostics:bool
  -> Agent_protocol.Inference_query.Attempt.t

val revision : t -> int64

(** Private captured document codec. Preserve future carrier fields through edits;
    public queries select typed safe fields only. Retirement conflicts must be
    explicitly classified during planning, never catch/authored-reset fallback.
    Parent Session State embeds this immutable child document as an owned value;
    the child remains responsible for its actual typed ownership/conversion.
    The immutable encoded document is computed only after complete admission of
    the final carrier and reused by [to_document]. [of_document] always admits
    its complete supplied original before issuing new evidence. *)
val to_document : t -> (Document_schema.Document.t, Error.t) Result.t

val of_document : Document_schema.Document.t -> limits:Limits.t -> (t, Error.t) Result.t

(** Admit an exact complete replacement after a prior durable ledger exists.
    Both originals use [Limits.default]; session identity is unchanged and
    generation, admission/revision/coverage counters never decrease. Retained
    actual identities/configuration and authoritative observations cannot rewind;
    active rows cannot disappear, ended rows cannot reopen, and retired ordinals
    cannot resurrect. Existing child adoption rules reject protected retirement,
    conflicting unknown members, or an incoming carrier that omits previous
    unknowns. Accepted incoming is unchanged; no silent repair or authority is
    granted. New initial ledgers have no prior update boundary. *)
val validate_update : t -> incoming:t -> (unit, Error.t) Result.t
