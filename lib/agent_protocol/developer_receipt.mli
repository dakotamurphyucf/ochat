(** Pure bounded projections of the existing Developer Check, Fixture and Live
    terminal receipts. No Store, runtime, source handle or disclosure grant is
    retained. The server converts an already authorized Store receipt; a
    decoded value alone is never evidence of execution. *)

(** Check that a captured executable source identifier can be represented by a
    Developer diagnostic receipt. This validates protocol namespace/member
    bounds only; it grants no source authority and does not construct a
    diagnostic location. *)
val validate_source_id : Chatml_source.Id.t -> (unit, Error.t) result

module Kind : sig
  type t =
    | Check
    | Fixture_run
    | Live_run
  [@@deriving compare, equal, sexp_of]
end

module Diagnostic : sig
  module Location : sig
    type t = private
      { source : Chatml_source.Id.t
      ; digest : Chatml_source.Digest.t
      ; source_bytes : int
      ; start_byte : int
      ; end_byte : int
      }

    (** Offsets are half-open byte coordinates in this exact source/digest.
        The constructor validates ordering and bounds, not source ownership. *)
    val create
      :  source:Chatml_source.Id.t
      -> digest:Chatml_source.Digest.t
      -> source_bytes:int
      -> start_byte:int
      -> end_byte:int
      -> (t, Error.t) result
  end

  type t = private
    { code : string
    ; message : string
    ; location : Location.t option
    }

  (** Existing 1..256-byte code and at most 16-KiB message bounds. *)
  val create
    :  code:string
    -> message:string
    -> location:Location.t option
    -> (t, Error.t) result
end

module Check : sig
  type t =
    | Ready
    | Rejected of Diagnostic.t
    | Interrupted
    | Cancelled
end

module Fixture : sig
  module Failure : sig
    type t =
      | Compile of Diagnostic.t
      | Fixture_mismatch of
          { expected_step : int
          ; actual_tool : string
          }
      | Fixture_missing of { next_step : int }
      | Unsupported_effect of { operation : string }
      | Runtime of Diagnostic.t
      | Invalid_result of Diagnostic.t
      | Pending_without_owner
      | Admission_revoked
      | Output_limit
  end

  module Log : sig
    type level =
      | Debug
      | Info
      | Warn
      | Error

    type t = private
      { level : level
      ; message : string
      }

    val create : level:level -> message:string -> (t, Error.t) result
  end

  type outcome =
    | Succeeded of Jsonaf.t
    | Tool_failed of Invocation.tool_error
    | Failed of Failure.t

  type completed = private
    { graph_fingerprint : Chatml_source.Digest.t
    ; fixture_digest : Chatml_source.Digest.t
    ; tool_selection_fingerprint : Chatml_source.Digest.t option
    ; matched_steps : int
    ; logs : Log.t list
    ; outcome : outcome
    }

  (** The server supplies the authentic selected-ABI fingerprint for Tool_v1
      and omits it for One_off_v1; this pure value alone cannot prove which
      compiler target ran. Aggregate JSON admission is [create] below. *)
  val completed
    :  graph_fingerprint:Chatml_source.Digest.t
    -> fixture_digest:Chatml_source.Digest.t
    -> tool_selection_fingerprint:Chatml_source.Digest.t option
    -> matched_steps:int
    -> logs:Log.t list
    -> outcome:outcome
    -> (completed, Error.t) result

  type t =
    | Completed of completed
    | Interrupted
    | Cancelled
end

module Live : sig
  type t =
    | Succeeded of Jsonaf.t
    | Failed of Error.t
    | Limited of Error.t
    | Interrupted
    | Cancelled
end

type value =
  | Check of Check.t
  | Fixture of Fixture.t
  | Live of Live.t

type t

(** Validate every scalar and the complete nested JSON under one existing
    Document_schema limit before retaining/encoding. Unknown and duplicate
    fields reject; omitted optional fields and explicit null are distinct.
    No per-log allowance widens the aggregate receipt bound. *)
val create : limits:Document_schema.Limits.t -> value -> (t, Error.t) result

val value : t -> value
val kind : t -> Kind.t
val encoded_bytes : t -> int
val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : limits:Document_schema.Limits.t -> Jsonaf.t -> (t, Error.t) result
