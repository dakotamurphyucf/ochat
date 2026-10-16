open Core

(** Bounded JSON projection shared with moderator events and state. It preserves
    the existing runtime-value traversal, schema validation, and encoded byte
    limit; failures are local validation errors. *)
module Json : sig
  val check_value
    :  ?code:string
    -> ?max_depth:int
    -> ?max_array_items:int
    -> max_bytes:int
    -> Chatml.Chatml_lang.value
    -> (unit, string) result

  val project
    :  ?control:Chatml.Chatml_lang.execution_control
    -> max_bytes:int
    -> Chatml.Chatml_lang.value
    -> (Jsonaf.t, string) result
end

(** Pure Tool_v1 value projection shared by real managed invocations and inert
    developer fixtures. This module creates no Invocation, Session, registry,
    work owner, or effect authority. Callers prove their own dispatch and current
    policy before using a projected value. *)
module Context : sig
  type capability =
    { id : string
    ; name : string
    ; implementation_revision : string
    ; fingerprint : string
    ; input_schema : Jsonaf.t
    }

  module Limits : sig
    type t

    (** Existing selected script limits, without resetting the active control. *)
    val of_script
      :  Chatmd_shell_spec.Chatmd_script_spec.limits
      -> max_nested_calls:int
      -> max_invocation_depth:int
      -> (t, string) result

    (** Exact effective fixture execution limits and selected output ceiling.
        A value beyond the Tool_v1 ABI's finite limit domain rejects before
        interpreter entry; the context never advertises a smaller policy than
        the [Chatml_execution.with_control] scope that actually runs it. *)
    val of_execution
      :  Chatml_execution.limits
      -> max_output_bytes:int
      -> (t, string) result
  end

  type t

  (** Real invocation callers supply values from their already dispatched,
      physically checked owner. This pure record constructor grants nothing. *)
  val create
    :  invocation_id:string
    -> provider_call_id:string option
    -> session_label:string
    -> generation:int
    -> origin:Agent_protocol.Invocation.origin
    -> parent_invocation:string option
    -> parent_event:string option
    -> parent_job:string option
    -> tool_name:string
    -> implementation_revision:string
    -> capability_fingerprint:string
    -> created_at:Agent_protocol.Timestamp.t
    -> deadline:Agent_protocol.Timestamp.t option
    -> limits:Limits.t
    -> available_tools:capability list
    -> checkpoint:(unit -> unit)
    -> (t, string) result

  (** Inert fixture identity. The caller derives [execution_id] from its actual
      persisted command receipt and supplies its admitted developer run ID and
      epoch. The codec uses namespaced ABI strings, Script origin, no provider
      or parent IDs, and an empty tool list/fingerprint; it never constructs a
      typed Session or Invocation or publishes a callable capability. *)
  val developer_fixture
    :  run:Agent_protocol.Id.Developer_run.t
    -> execution_id:string
    -> generation:int
    -> tool_name:string
    -> implementation_revision:string
    -> created_at:Agent_protocol.Timestamp.t
    -> deadline:Agent_protocol.Timestamp.t option
    -> limits:Limits.t
    -> (t, string) result

  (** The one canonical capability fingerprint algorithm used by the real
      selected registry and the inert empty selection. The supplied list is in
      the selected registry's canonical name order, not display/dependency
      order. The full selected fingerprint can differ from the displayed
      dependency subset. This pure hash grants no registry selection. *)
  val capability_fingerprint : string list -> string

  val project
    :  ?control:Chatml.Chatml_lang.execution_control
    -> t
    -> (Chatml.Chatml_lang.value, string) result
end

module Input : sig
  (** Schema is the explicit, already selected top-level Tool_v1 input schema,
      never a default True schema. JSON import and value traversal share the
      active execution control and selected limits. *)
  val prepare
    :  ?control:Chatml.Chatml_lang.execution_control
    -> schema:Chatmd_shell_spec.Tool_schema.t
    -> limits:Context.Limits.t
    -> Jsonaf.t
    -> (Chatml.Chatml_lang.value, string) result
end

module Outcome : sig
  type error =
    | Invalid of string
    | Work_rejected of string
  [@@deriving sexp_of]

  (** Decode the actual Complete, Fail or Pending variant with protocol and
      output-schema validation and a whole-envelope byte bound. [validate_work]
      belongs to the real invocation owner; an inert fixture supplies an
      always-denying checker, so Pending cannot invent resumable work. This
      codec does not resolve or persist anything. *)
  val decode
    :  ?control:Chatml.Chatml_lang.execution_control
    -> output_schema:Chatmd_shell_spec.Tool_schema.t
    -> limits:Context.Limits.t
    -> validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
    -> Chatml.Chatml_lang.value
    -> (Agent_protocol.Invocation.outcome, error) result
end
