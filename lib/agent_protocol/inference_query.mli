(** Additive protocol2 inference reads. No OpenAI, private Target, raw request,
    credentials, instructions/tool bodies or arbitrary diagnostic messages.
    These are retained-window totals, NEVER lifetime claims after retirement. *)
module Features : sig
  (** Optional method/display support, independent of authorization. These are
      ordinary initialize features, not extension capability catalog entries. *)
  val observations : string

  val configuration : string
  val diagnostics : string
  val all : string list
end

module Summary_request : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Coverage : sig
  type limit_kind =
    | Attempt_count
    | Turn_count
    | Retained_bytes
    | Protected_future_data
  [@@deriving equal, sexp_of]

  type tracking_status =
    | Available
    | Limited of limit_kind
  [@@deriving equal, sexp_of]

  type t = private
    { before_tracking_unknown : bool
    ; retired_attempts : int64
    ; untracked_attempts : int64
    ; retired_turns : int64
    ; untracked_turns : int64
    ; tracking_status : tracking_status
    }

  val create
    :  before_tracking_unknown:bool
    -> retired_attempts:int64
    -> untracked_attempts:int64
    -> retired_turns:int64
    -> untracked_turns:int64
    -> tracking_status:tracking_status
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Metric : sig
  type sum =
    | Tokens of int64
    | Overflow
  [@@deriving equal, sexp_of]

  type unknown_counts =
    { not_reported : int64
    ; explicit_null : int64
    ; interrupted : int64
    ; not_submitted : int64
    ; before_tracking : int64
    }

  type t = private
    { actual : sum
    ; actual_attempts : int64
    ; estimated : sum
    ; estimated_attempts : int64
    ; mixed_estimators : bool
    ; unknown : unknown_counts
    }

  (** Nonnegative per-component contributions over retained attempts. Estimated
      contributions remain separately labelled; mixed_estimators prevents a
      fabricated uniform algorithm claim. Missing usage contributes explicit
      unknown reasons, not zero. No component total adds cache/reasoning subsets
      to input/output. Pricing/currency does not participate. *)
  val create
    :  actual:sum
    -> actual_attempts:int64
    -> estimated:sum
    -> estimated_attempts:int64
    -> mixed_estimators:bool
    -> unknown:unknown_counts
    -> (t, Error.t) Result.t
end

module Summary : sig
  type turns =
    { pending : int64
    ; completed : int64
    ; failed : int64
    ; cancelled : int64
    ; interrupted : int64
    }

  type components =
    { input : Metric.t
    ; output : Metric.t
    ; reported_total : Metric.t
    ; cached_input : Metric.t
    ; cache_write_input : Metric.t
    ; reasoning_output : Metric.t
    }

  type t [@@deriving sexp]

  (** Bounded 8 KiB optional session summary. Component values are independent
      metrics, not six quantities to sum. Attempt and actual host-turn counts
      refer to their retained windows; coverage records discarded/untracked
      evidence and pre-tracking uncertainty. Counts/revisions nonnegative. *)
  val create
    :  retained_attempts:int64
    -> turns:turns
    -> components:components
    -> coverage:Coverage.t
    -> accounting_revision:int64
    -> (t, Error.t) Result.t

  val retained_attempts : t -> int64
  val turns : t -> turns
  val components : t -> components
  val coverage : t -> Coverage.t
  val accounting_revision : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Attempt : sig
  type t

  (** Usage/Context/state rows require transcript scope AND session visibility.
      Detailed Configuration/account alias/diagnostics additionally require
      Diagnostics. The encoder never exposes hidden full
      Attempt_record JSON as a shortcut. Ordinal orders actual durable admission;
      Operation/Invocation are real host associations or absent. *)
  val create
    :  ordinal:int64
    -> generation:int
    -> scope:Transcript.Scope.t
    -> operation_id:Id.Operation.t option
    -> invocation_id:Id.Invocation.t option
    -> accounting_id:Inference.Observation.Observation_id.t
    -> state:Inference.Observation.Attempt_record.state
    -> usage:Inference.Observation.t option
    -> context:Inference.Observation.t option
    -> configuration:Inference.Observation.Configuration.t option
    -> diagnostics:Inference.Observation.t list option
    -> omitted_diagnostics:int64 option
    -> (t, Error.t) Result.t

  val ordinal : t -> int64
  val generation : t -> int
  val scope : t -> Transcript.Scope.t
  val operation_id : t -> Id.Operation.t option
  val invocation_id : t -> Id.Invocation.t option
  val accounting_id : t -> Inference.Observation.Observation_id.t
  val state : t -> Inference.Observation.Attempt_record.state
  val usage : t -> Inference.Observation.t option
  val context : t -> Inference.Observation.t option
  val configuration : t -> Inference.Observation.Configuration.t option

  (** Actual selected transport, disclosed with configuration. Initial nomination
      in Configuration is never presented as evidence of actual WS dispatch. *)
  val transport_selection : t -> Inference.Observation.t option

  val with_transport_selection
    :  t
    -> Inference.Observation.t option
    -> (t, Error.t) Result.t

  val diagnostics : t -> Inference.Observation.t list option
  val omitted_diagnostics : t -> int64 option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; include_configuration : bool
    ; include_diagnostics : bool
    }
  [@@deriving sexp]

  (** Default page128; actual request <=advertised max_page_size (normally1000)
      checked by host, because generic Page.Request permits a larger ceiling.
      Cursors bind principal/session/generation/accounting_revision/filter/order;
      changed accounting returns explicit Conflict/restart, never mixed pages. *)
  val create
    :  session_id:Id.Session.t
    -> page:Page.Request.t
    -> include_configuration:bool
    -> include_diagnostics:bool
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Response : sig
  type t [@@deriving sexp]

  (** Fully measured result within the inference-query per-RPC envelope policy
      (default16 MiB), using the allowance left after measuring the actual RPC ID
      and envelope wrapper, and including the actual signed cursor. HTTP batch
      aggregate and unrelated response limits are unchanged; input limits do not
      supply this response policy. Byte admission happens before
      next-row append/serialized allocation; a valid row that cannot fit returns
      a structured error, never a non-advancing cursor loop. *)
  val create
    :  summary:Summary.t
    -> attempts:Attempt.t Page.t
    -> max_bytes:int
    -> (t, Error.t) Result.t

  val summary : t -> Summary.t
  val attempts : t -> Attempt.t Page.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> max_bytes:int -> (t, Error.t) Result.t

  module Builder : sig
    type response = t
    type t

    (** [max_bytes] is the actual remaining result-body allowance after measuring
        the RPC/envelope wrapper, at most 16 MiB. Immutable builder caches bounded
        row measurements; no complete array is rebuilt on each append. *)
    val create : summary:Summary.t -> max_bytes:int -> (t, Error.t) Result.t

    (** None means capacity with prior builder unchanged; an empty page that
        cannot fit its first valid row errors. Cursor is the actual next cursor
        after this candidate, included in the admission BEFORE append. At most
        1000 rows. No cursor can silently point past an unappended row. *)
    val add
      :  t
      -> Attempt.t
      -> next_cursor:Page.Cursor.t option
      -> (t option, Error.t) Result.t

    val finish : t -> (response, Error.t) Result.t
  end
end
