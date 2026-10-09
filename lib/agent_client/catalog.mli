open! Core

(** Typed prompt and workspace discovery for client-facing configuration names. *)

val prompts_page
  :  Connection.t
  -> Agent_protocol.Prompt.List_request.t
  -> (Agent_protocol.Prompt.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

val workspaces_page
  :  Connection.t
  -> Agent_protocol.Workspace.List_request.t
  -> (Agent_protocol.Workspace.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

(** Complete filtered enumeration from a fresh query with explicit bounds.
    Does not retry catalog changes or return a silently truncated prefix. *)
val enumerate_prompts
  :  Connection.t
  -> query:Agent_protocol.Prompt.List_request.t
  -> max_prompts:int
  -> max_pages:int
  -> (Agent_protocol.Prompt.t list, Agent_protocol.Error.t) result

val enumerate_workspaces
  :  Connection.t
  -> query:Agent_protocol.Workspace.List_request.t
  -> max_workspaces:int
  -> max_pages:int
  -> (Agent_protocol.Workspace.t list, Agent_protocol.Error.t) result

(** Unfiltered complete enumeration, bounded to 100000 items and 100 pages.
    Bound exhaustion is an error. Name resolution uses the same complete view. *)

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
