open! Core

(** Converts validated human-readable server configuration into immutable
    runtime catalogs with stable opaque IDs. *)

type t =
  { workspaces : Agent_session.Workspace_catalog.t
  ; prompts : Agent_session.Prompt_definition.t list
  ; permission_profiles : Agent_session.Permission_policy.t list
  ; manifest_grants : Operator_manifest_grant.t list
  }

type reviewer_resolver =
  Agent_session.Permission_reviewer.kind
  -> string
  -> Agent_session.Permission_reviewer.t option

type policy_evaluator_resolver =
  string -> (string * Agent_session.Permission_policy.evaluator) option

(** [build ?reviewer_resolver ?policy_evaluator_resolver config] compiles
    validated configuration into runtime catalogs. Missing named reviewers and
    policy evaluators compile to unavailable fail-closed implementations. *)
val build
  :  ?reviewer_resolver:reviewer_resolver
  -> ?policy_evaluator_resolver:policy_evaluator_resolver
  -> Config.t
  -> (t, Agent_store.Store_error.t) result
