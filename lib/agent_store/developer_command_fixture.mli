open! Core
module Diagnostic = Developer_command_diagnostic

(** Neutral deterministic fixture result. Logs and terminal JSON are subject to
    the enclosing receipt's ONE aggregate Document_schema limit and charged
    poll; no per-log or per-result byte ceiling is imposed here. *)
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
    | Output_limit
  [@@deriving sexp_of]
end

module Log : sig
  module Level : sig
    type t =
      | Debug
      | Info
      | Warn
      | Error
    [@@deriving equal, sexp_of]
  end

  type t =
    { level : Level.t
    ; message : string
    }
  [@@deriving sexp_of]
end

type outcome =
  | Succeeded of Jsonaf.t
  | Failed of Failure.t
[@@deriving sexp_of]

type completed =
  { graph_fingerprint : Chatml_source.Digest.t
  ; fixture_digest : Chatml_source.Digest.t
  ; matched_steps : int
  ; logs : Log.t list
  ; outcome : outcome
  }
[@@deriving sexp_of]

type t =
  | Completed of completed
  | Interrupted
  | Cancelled
[@@deriving sexp_of]

(** Validate domain scalars and log cardinality before the enclosing receipt
    allocates its JSON projection. Every list visit charges [poll]; no new
    allowance is created. The caller still validates the complete JSON tree. *)
val validate
  :  t
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (unit, Document_schema.Error.t) result

(** Projection after [validate]; the enclosing receipt performs complete
    charged JSON admission. Every log projection invokes the same [poll]. *)
val to_json : t -> poll:(unit -> unit) -> Jsonaf.t
