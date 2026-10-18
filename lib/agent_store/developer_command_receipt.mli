open! Core

(** Pure, bounded user-result projections for developer commands. These values
    contain no compiler artifact, runtime value, source grant or execution
    capability. The host separately projects authentic execution outcomes and
    authorizes disclosure; persistence validates structure, not provenance. *)

module Id : sig
  (** Exact shared protocol identity, with historical persisted bytes intact.
      [of_string] adapts the pure validator's error to the existing Store
      Document_schema channel; it does not apply a second validator. *)
  type t = Agent_protocol.Developer_command_id.t [@@deriving compare, equal, sexp_of]

  (** Bounded comparison identity. The Store allocates it only after proving a
      new idempotency key; decoding one does not reserve or execute a command. *)
  val of_string : string -> (t, Document_schema.Error.t) result

  val to_string : t -> string
end

module Kind : sig
  type t =
    | Check
    | Fixture_run
    | Live_run
    | Repl_task_run
  [@@deriving equal, sexp_of]
end

module Diagnostic = Developer_command_diagnostic

module Check : sig
  type t =
    | Ready
    | Rejected of Diagnostic.t
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

module Fixture = Developer_command_fixture

module Live : sig
  type t =
    | Succeeded of Jsonaf.t
    | Failed of Agent_protocol.Error.t
    | Limited of Agent_protocol.Error.t
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

module Task : sig
  (** Retained REPL Task projection. [value_preview] is the evaluator's
      already-bounded text preview, not a serialized language value. Absence
      is encoded by omitting the member; explicit null is invalid. Failure
      keeps the actual bounded diagnostic when one was produced and a typed
      error for admission/runtime classification. The row's
      [execution_started] flag records whether this outcome followed the
      unique durable Start; a success is valid only after Start. *)
  type t =
    | Succeeded of { value_preview : string option }
    | Failed of
        { error : Agent_protocol.Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Limited of
        { error : Agent_protocol.Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

type value =
  | Check of Check.t
  | Fixture of Fixture.t
  | Live of Live.t
  | Task of Task.t
[@@deriving sexp_of]

type t

(** Validate all domain fields and aggregate encoded JSON structure/bytes
    before retaining a projection. Diagnostic codes are nonempty and at most
    256 bytes; messages are at most 16 KiB, matching the portable check engine.
    Logs and terminal values share the caller's whole-value JSON bounds;
    this codec does not introduce a smaller per-log limit than the runtime.
    [poll] is the caller's charged cooperative checkpoint; cancellation and
    unexpected exceptions propagate unchanged. No new allowance is created. *)
val create
  :  value
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (t, Document_schema.Error.t) result

val value : t -> value
val kind : t -> Kind.t
val encoded_bytes : t -> int
val equal : t -> t -> bool
val to_json : t -> Jsonaf.t

(** Named-field codec with the same validation as [create]. Absent optional
    location is distinct from explicit null; unsupported fields/alternatives
    that change required semantics are rejected. The enclosing run document
    owns schema versioning and aggregate row/receipt/recovery work limits. *)
val of_json
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (t, Document_schema.Error.t) result
