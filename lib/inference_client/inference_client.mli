open! Core

(** Shared selected inference dispatch for host turns and auxiliary no-tool
    requests. No tool executor, canonical commit, retry or provider selection. *)
module Error : sig
  type t =
    | Preparation of Inference_runtime.Preparation_error.t
    | Attempt of Inference_runtime.Attempt.run_error
  [@@deriving equal, sexp_of]
end

module Completion : sig
  type outcome =
    | Returned of Inference.Event.Terminal.t
    | Interrupted of
        { reason : Inference.Observation.Attempt_record.interruption
        ; delivery : Inference.Event.Terminal.delivery
        }

  (** Host lifecycle evidence for one actual attempt. No raw provider output or
      private exception text; a returned terminal never asserts host turn commit. *)
  type t

  val attempt : t -> Inference_runtime.Attempt.t
  val outcome : t -> outcome
end

module Identity : sig
  (** Trusted host allocation ports. Preparation identity is an opaque label,
      never a private-input hash. Actual attempt allocation runs independently of
      UI callbacks; durable hosts acknowledge tracking before returning. Parent
      relation is actual known ownership, not inferred nesting. *)
  type t =
    { new_preparation_id : unit -> string
    ; new_attempt :
        Inference_runtime.Prepared.t
        -> relation:Transcript.Scope.relation
        -> Transcript.Scope.t * Inference.Observation.Observation_id.t
    }
end

(** Input is the final immutable request after moderation, guidance, tool
    selection and local asset resolution. [before_dispatch] performs the existing
    host admission against THIS prepared fingerprint. Only afterward is one
    actual scope/accounting identity allocated, independently of presentation
    callbacks. [on_attempt] durably acknowledges tracking before authentication
    or network dispatch. It must not run the attempt itself.

    [on_completion] runs once after an actual attempt returns, or on interruption
    during attempt acknowledgement/dispatch/observation. A normal completion
    callback failure propagates without reclassification or a second callback.
    On an already-raised exception, interruption reporting is cancellation-protected
    and best effort: its failure cannot replace the original exception/backtrace.
    Preparation/admission failures before an actual attempt exists create no
    fabricated completion. Durable hosts supply this independently of UI callbacks.

    Candidate events reach the caller's existing admission/commit/tool owner;
    Not_eligible is retained evidence, never a request to execute a tool. The
    receipt retains the runtime's explicit complete-response/prefix distinction.
    Finalized entries and Source_finished remain the host's responsibility after
    actual local admission. Strict callbacks, cancellation and unexpected
    exceptions propagate unchanged. No retry or ambient context is available. *)
val run
  :  Inference_runtime.Context.t
  -> sw:Eio.Switch.t
  -> identity:Identity.t
  -> relation:Transcript.Scope.relation
  -> request:Inference.Request.t
  -> before_dispatch:(Inference_runtime.Prepared.t -> unit)
  -> on_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_completion:(Completion.t -> unit)
  -> on_event:(Inference.Event.t -> unit)
  -> on_observation:(Inference.Observation.t -> unit)
  -> (Inference_runtime.Receipt.t, Error.t) Result.t

module Text : sig
  type t

  (** Canonical input IDs are explicitly owned by the supplied private request
      namespace. The host must not reuse these as durable history IDs. No role
      conversion/default or conversation mutation occurs. *)
  val history
    :  namespace:string
    -> (History_entry.Payload.Role.t * string) list
    -> (History_entry.t list, string) Result.t

  (** Extract only assistant output text and explicit refusals in actual response
      order. Reasoning, calls and opaque items stay in the original receipt and
      are not promoted to answer text. Even a Completed receipt is not a host
      turn commit. Callers must inspect its outcome before treating text as a
      successful domain result. *)
  val of_receipt : Inference_runtime.Receipt.t -> t

  val receipt : t -> Inference_runtime.Receipt.t
  val messages : t -> string list
  val refusals : t -> string list
end

module Execution : sig
  (** Immutable selected context and actual host admission/observation ports.
      Parent relation belongs to this invocation, never a mutable global scope. *)
  type t

  val create
    :  context:Inference_runtime.Context.t
    -> identity:Identity.t
    -> relation:Transcript.Scope.relation
    -> before_dispatch:(Inference_runtime.Prepared.t -> unit)
    -> on_attempt:(Inference_runtime.Attempt.t -> unit)
    -> on_completion:(Completion.t -> unit)
    -> on_observation:(Inference.Observation.t -> unit)
    -> t

  val context : t -> Inference_runtime.Context.t

  val run
    :  t
    -> sw:Eio.Switch.t
    -> request:Inference.Request.t
    -> on_event:(Inference.Event.t -> unit)
    -> (Inference_runtime.Receipt.t, Error.t) Result.t

  module Completion_error : sig
    type t =
      | Dispatch of Error.t
      | Outcome of Inference.Event.Terminal.outcome
      | No_text
    [@@deriving equal, sexp_of]
  end

  (** Selected no-tool auxiliary completion. Model omission inherits; supplied
      settings are explicit effective overrides (duplicate names reject). Existing
      tool_choice constraints are omitted because this operation advertises no
      tools and executes none. Existing structured [text.format] constraints are
      replaced by plain text for this operation, preserving verbosity and other
      text members; the original selected target is unchanged. Other captured
      settings are preserved. Use [run] for intentionally structured completions. Input IDs
      use a fresh opaque host-allocated private namespace, not provider aliases.
      Only a Completed result with assistant text succeeds; refusal, truncation,
      provider/transport/auth failure remain typed outcomes. Strict observer and
      cancellation exceptions propagate. No canonical conversation is changed. *)
  val complete_text
    :  t
    -> sw:Eio.Switch.t
    -> ?model:string
    -> settings:Inference.Request.Setting.t list
    -> messages:(History_entry.Payload.Role.t * string) list
    -> unit
    -> (string, Completion_error.t) Result.t
end
