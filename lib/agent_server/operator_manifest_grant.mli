open! Core

(** Compiled operator authorization for one exact prompt source and shell
    manifest in explicitly named workspaces. *)

type t

val create : Config.Manifest_grant.t -> (t, Agent_protocol.Error.t) result

(** [authorizes] accepts only an exact prompt, workspace, source digest,
    manifest digest, and optional principal binding. An empty configured
    principal list authorizes every authenticated principal. *)
val authorizes
  :  t
  -> prompt_definition_id:Agent_protocol.Id.Prompt_definition.t
  -> workspace_definition_id:Agent_protocol.Id.Workspace_definition.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> source_sha256:string
  -> manifest_sha256:string
  -> bool

(** [id t] returns the operator-facing grant ID. *)
val id : t -> string
