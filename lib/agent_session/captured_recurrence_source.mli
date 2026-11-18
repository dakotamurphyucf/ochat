open! Core

(** Full captured ChatMD restoration through the existing artifact and parser
    owners. No original source read, initializer, script runtime, native binding,
    provider call or Run admission occurs. This is not an execution grant. *)
type t

(** The caller has resolved the genuine lower record and separately authorizes
    current use. Exact publication digest and installed inventories are checked,
    including when the child reuses the controller's source revision. Captured
    root_file is provenance only. Original cancellation/checkpoint exceptions
    propagate through the existing bounded artifact/parser adapters. *)
val restore
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> record:Agent_store.Delegation_store.record
  -> state:Session_state.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

val revision : t -> Prompt_revision.t
val origin : t -> Recurrence_child_origin.t
