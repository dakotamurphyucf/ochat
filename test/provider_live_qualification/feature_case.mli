open! Core

(** Fixed synthetic probes; execution, credentials, model, transport, admission,
    attempt identity and retry remain owned by the existing harness/Host. *)
module Case : sig
  type t =
    | Json_schema
    | Image
    | Reasoning
    | Document
    | Function_call
  [@@deriving equal, sexp_of]

  val all : t list
  val name : t -> string
  val maximum_attempts : t -> int
end

module Error : sig
  type t =
    | Invalid_fixture
    | Wrong_route
    | Outcome_not_completed
    | Missing_assistant_text
    | Output_mismatch
    | Configuration_mismatch
    | Transport_mismatch
    | Unexpected_tool_candidate
    | Evidence_limit
  [@@deriving equal, sexp_of]
end

module Input : sig
  type t

  type route =
    | Session
    | Canonical_request
  [@@deriving equal, sexp_of]

  val route : t -> route

  (** Fixed settings must be captured before preparing this isolated attempt.
      No generation limits are injected; unsupported controls are never stripped. *)
  val settings : t -> Inference.Request.Setting.t list

  (** Inline synthetic image, no file lookup/URL/upload. None for Document and Function_call. *)
  val session_content : t -> Agent_protocol.Session.Message_content.t option

  (** Document uses the existing Execution.run path and exact captured target;
      caller supplies its host-allocated private ID. No alternate provider client.
      Document advertises no tools and contains fixed inline PDF data.
      Function_call advertises only a strict qualification_echo function; it
      selects that function by the fixed target setting and grants no execution. *)
  val request
    :  t
    -> target:Inference.Request.Target.t
    -> history_id:History_entry.Id.t
    -> limits:Document_schema.Limits.t
    -> (Inference.Request.t, Error.t) Result.t
end

val input : Case.t -> (Input.t, Error.t) Result.t

module Evidence : sig
  type t

  val case : t -> Case.t
  val output_validated : t -> bool

  (** Exact fixed case setting matched in the actual captured target, and safe
      effective configuration matched its projection. JSON schema bytes are
      proved by target settings, not by the redacted Text_format observation. *)
  val configuration_validated : t -> bool

  val to_json : t -> Jsonaf.t
end

(** Caller MUST establish same-attempt association of target, terminal, canonical
    assistant output, configuration and selected transport using its existing
    receipt/ledger. Include ALL observed tool candidates; none are permitted.
    Function_call must use [validate_function], never this text validator.
    This pure validator does not authorize dispatch, infer support, or turn a
    rejected/unsupported route into success. No raw output enters Evidence. *)
val validate
  :  Case.t
  -> target:Inference.Request.Target.t
  -> outcome:Inference.Event.Terminal.outcome
  -> assistant_text:string list
  -> configuration:Inference.Observation.Configuration.t
  -> selected_transport:Inference.Observation.Transport_selection.transport
  -> expected_transport:Inference.Observation.Transport_selection.transport
  -> tool_candidates:int
  -> (Evidence.t, Error.t) Result.t

(** Wire-only function evidence, never native execution. Caller proves same-attempt
    receipt association and supplies all retained candidate payloads. The count is
    ALL Call/Unknown candidates, irrespective of local execution eligibility; Unknown
    or additional calls reject. Message/reasoning candidates carry no execution claim. *)
val validate_function
  :  target:Inference.Request.Target.t
  -> outcome:Inference.Event.Terminal.outcome
  -> configuration:Inference.Observation.Configuration.t
  -> selected_transport:Inference.Observation.Transport_selection.transport
  -> expected_transport:Inference.Observation.Transport_selection.transport
  -> tool_candidates:int
  -> candidates:History_entry.Payload.t list
  -> (Evidence.t, Error.t) Result.t
