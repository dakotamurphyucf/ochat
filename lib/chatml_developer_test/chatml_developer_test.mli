open Core

(** Non-executing check plus actual bounded deterministic developer test. The
    host owns actor/source authority, run IDs, idempotency, scheduling, and
    cancellation; this portable service neither creates conversation Runs/Jobs
    nor reads files, contacts tools, or borrows an editor lease. *)

module Diagnostic : sig
  (** Frozen bounded projection only: finite code, at most 16 KiB message,
      optional source ID/digest-bound byte location. No retained source text,
      linked artifact, raw exception, or live registry is exposed. *)
  type t

  val code : t -> string
  val message : t -> string
  val location : t -> Chatml_source.Location.t option
end

module Check : sig
  module Id : sig
    (** Host-issued check identity, distinct from run and conversation IDs. *)
    type t [@@deriving equal, sexp_of]

    val create : string -> t Or_error.t
  end

  type t

  module Outcome : sig
    type t =
      | Ready
      | Rejected of Diagnostic.t
  end

  (** Compile the exact captured graph in the existing joined worker. A compile
      failure is a source-owned, side-effect-free terminal check outcome; no
      initializer, fixture operation or host callback executes. Cancellation
      propagates after domain cleanup. A successful [t] privately retains the
      exact opaque capture and frozen Linked artifact for [run]; it cannot be
      forged from wire input. Policy errors also project to Rejected. *)
  val perform
    :  Chatml_developer_capture.t
    -> id:Id.t
    -> policy:Chatml_compilation.policy
    -> max_dependency_depth:int
    -> env:Eio_unix.Stdenv.base
    -> t

  val id : t -> Id.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val outcome : t -> Outcome.t
end

module Run_id : sig
  (** Host-issued identifier, separate from conversation Run and Job IDs. The
      host guarantees uniqueness and same-request idempotency; this value alone
      grants no inspection or execution authority. *)
  type t [@@deriving equal, sexp_of]

  val create : string -> t Or_error.t
end

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

module Error : sig
  type t =
    | No_fixtures
    | Mismatched_check
    | Invalid_selection
    | Invalid_output_limit
    | Invalid_execution_limits
  [@@deriving equal, sexp_of]
end

module Receipt : sig
  type outcome =
    | Succeeded of Jsonaf.t
    | Tool_failed of Agent_protocol.Invocation.tool_error
    | Failed of Failure.t

  type t

  module Log : sig
    type t

    val level : t -> Chatml_host_runtime.log_level
    val message : t -> string
  end

  val id : t -> Run_id.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val graph_fingerprint : t -> Chatml_source.Digest.t
  val fixture_digest : t -> Chatml_source.Digest.t
  val tool_selection_fingerprint : t -> Chatml_source.Digest.t option
  val matched_steps : t -> int
  val logs : t -> Log.t list
  val outcome : t -> outcome
end

(** Execute only a previously checked fixture capture through the existing
    linked runtime and Chatml_execution budget. First require the exact same
    opaque capture value retained by [check], not a caller-supplied digest or
    equal-looking reconstruction. Install a fixture-only Tool.call handler under
    the exact selected One_off_v1 standalone surface. The handler
    validates each call against the next ordered step and returns its bounded
    result/error. Pure language evaluation and Task.pure/bind/map/catch are
    allowed. Log.* is bounded local diagnostic output; every other host effect
    (including Job.*, spawn, Model.*, Process.*, Schedule.* and unknown names)
    is forbidden. A wrapper on the existing execution control's before_effect
    records sticky unexpected, mismatched and unsupported attempts even when a
    ChatML Task.catch handles the operation error. Missing required steps fail
    after the task completes. Fresh evaluation per [run] means repeated
    executions have distinct host IDs and do not share language state.
    Compilation failure yields a failed receipt with zero runtime events. The
    host may persist a separate idempotent receipt; this function itself has no
    durable store or live capability fallback. Cancellation/control exceptions
    propagate unchanged after cleanup. Input import, fixture matching,
    evaluation and result export use ONE bounded control scope; the receipt
    retains only bounded frozen result/error/site/output projections, never
    mutable runtime values or a Linked artifact. [max_output_bytes] bounds the
    aggregate retained logs and terminal JSON/diagnostic; result JSON also has
    a 1 MiB structural ceiling and is measured before serialization inside the
    active execution scope. *)
val run
  :  ?admit:(unit -> bool)
  -> Chatml_developer_capture.t
  -> check:Check.t
  -> id:Run_id.t
  -> limits:Chatml_execution.limits
  -> max_output_bytes:int
  -> env:Eio_unix.Stdenv.base
  -> (Receipt.t, Error.t) Result.t

module Tool_v1 : sig
  module Selection : sig
    type t

    (** Comparison-only fixture ABI selection. The host supplies the exact
        already selected Standard Tool_v1 contract, top-level tool label and
        schemas, actual developer run/epoch and persisted receipt ID. The
        constructor compares the physical capture and selected contract,
        validates the Tool_v1 two-argument entrypoint and bounded context
        fields, and retains no Source, Session, Invocation or capability.
        Source/current actor and policy admission remain with the host. *)
    val create
      :  capture:Chatml_developer_capture.t
      -> contract:Chatml.Chatml_compiler_contract.t
      -> run:Agent_protocol.Id.Developer_run.t
      -> id:Run_id.t
      -> generation:int
      -> tool_name:string
      -> input_schema:Chatmd_shell_spec.Tool_schema.t
      -> output_schema:Chatmd_shell_spec.Tool_schema.t
      -> created_at:Agent_protocol.Timestamp.t
      -> deadline:Agent_protocol.Timestamp.t option
      -> limits:Chatml_execution.limits
      -> max_output_bytes:int
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) result

    (** Includes the exact selected ABI/schema/tool/context policy. This is
        separate from the capture graph/input/fixture fingerprint and never
        replaces the original request digest or current target re-selection. *)
    val fingerprint : t -> Chatml_source.Digest.t

    (** Before execution, prove this selection belongs to the actual persisted
        receipt and exact effective budget supplied by the host. The capture
        itself is checked separately by [run]. *)
    val matches_execution
      :  t
      -> id:Run_id.t
      -> limits:Chatml_execution.limits
      -> max_output_bytes:int
      -> bool
  end

  (** Same joined interpreter and ordered inert Tool.call fixture kernel as
      [run]. The caller supplies its actual persisted [id] and effective
      [limits]/[max_output_bytes]; [run] compares them with [selection] and
      proves [check] belongs to the same physical capture before any effect.
      It then uses Tool_v1's two arguments and bounded Complete/Fail/Pending
      decoder. Complete and Fail remain different receipt outcomes. Pending
      cannot pass without a real owned completion path and is recorded as
      [Pending_without_owner]; no work is scheduled or resumed. Any prohibited
      effect remains sticky across Task.catch. Cancellation propagates. *)
  val run
    :  ?admit:(unit -> bool)
    -> Chatml_developer_capture.t
    -> check:Check.t
    -> selection:Selection.t
    -> id:Run_id.t
    -> limits:Chatml_execution.limits
    -> max_output_bytes:int
    -> env:Eio_unix.Stdenv.base
    -> (Receipt.t, Error.t) result
end
