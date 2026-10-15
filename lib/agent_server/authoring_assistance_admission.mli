open! Core
module P = Agent_protocol
module Tool_registry = Chat_response.Tool_capability
module Error = Authoring_context_service_core.Error

(** Pure bounded policy/registry admission shared by both assistance methods.
    Selection still comes from the trusted current host policy, never these
    comparison values. No registry implementation may enter a wire result. *)
module Limits : sig
  type t = private
    { protocol_limits : P.Authoring.Limits.t
    ; max_scan_work_steps : int
    ; max_assistance_work_steps : int
    ; max_contract_identity_bytes : int
    ; max_contract_work_steps : int
    ; max_projection_work_steps : int
    ; max_parser_input_bytes : int
    ; max_imported_inputs : int
    ; max_selected_tools : int
    ; max_tool_metadata_bytes : int
    ; max_capture_admission_bytes : int
    ; lexical : Language_highlighting.Highlight_snapshot.Limits.t
    ; chatmd : Chatmd_authoring_assistance.Limits.t
    ; chatml : Chatml_authoring_fallback.Limits.t
    ; mapped : Chatmd_authoring_fallback.Limits.t
    }

  (** [max_contract_identity_bytes] is caller-selected and at most 16 MiB;
      canonical contract conversion debits its conservative allocation and
      checkpoint work before any selected-catalog suggestion is admitted. *)
  val create
    :  protocol_limits:P.Authoring.Limits.t
    -> max_scan_work_steps:int
    -> max_assistance_work_steps:int
    -> max_contract_identity_bytes:int
    -> max_contract_work_steps:int
    -> max_projection_work_steps:int
    -> max_parser_input_bytes:int
    -> max_imported_inputs:int
    -> max_selected_tools:int
    -> max_tool_metadata_bytes:int
    -> t Or_error.t
end

module Selected_tool_policy : sig
  module Registry : sig
    type t

    val admit
      :  Tool_registry.t
      -> limits:Limits.t
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) result

    val selected : t -> Tool_registry.t
    val references : t -> Tool_registry.reference list
    val selected_tools : t -> Chatmd_authoring_assistance.Selected_tool.t list
    val fingerprint : t -> string
    val equal_identity : t -> t -> bool
    val within_limits : t -> Limits.t -> bool
  end

  type t

  val create
    :  current:
         (original_actor:Operator_authorization.t
          -> actor:Operator_authorization.t
          -> context:P.Authoring.Context.t
          -> Registry.t option)
    -> t

  val current
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> context:P.Authoring.Context.t
    -> Registry.t option
end
