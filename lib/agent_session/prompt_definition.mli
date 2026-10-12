open! Core

(** Validated catalog prompt configuration. *)

type t = private
  { id : Agent_protocol.Id.Prompt_definition.t
  ; config_name : string
  ; root_file : string
  ; root_invocation :
      Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  ; allowed_workspaces : Agent_protocol.Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  ; enabled : bool
  ; description : string option
  }
[@@deriving sexp]

val create
  :  id:Agent_protocol.Id.Prompt_definition.t
  -> config_name:string
  -> root_file:string
  -> root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> allowed_workspaces:Agent_protocol.Id.Workspace_definition.t list
  -> permission_profile:string
  -> runtime_policy:string option
  -> enabled:bool
  -> description:string option
  -> (t, Agent_store.Store_error.t) result

val allows_workspace : t -> Agent_protocol.Id.Workspace_definition.t -> bool

(** Actual host-selected root contract; names/source/grants never infer it. *)
val root_invocation
  :  t
  -> Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
