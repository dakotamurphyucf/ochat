open! Core

(** Typed prompt and workspace discovery for client-facing configuration names. *)

val prompts
  :  Connection.t
  -> (Agent_protocol.Prompt.t list, Agent_protocol.Error.t) result

val workspaces
  :  Connection.t
  -> (Agent_protocol.Workspace.t list, Agent_protocol.Error.t) result

val resolve_prompt
  :  Connection.t
  -> name:string
  -> (Agent_protocol.Prompt.t, Agent_protocol.Error.t) result

val resolve_workspace
  :  Connection.t
  -> name:string
  -> (Agent_protocol.Workspace.t, Agent_protocol.Error.t) result
