(** Validated workspace configuration. A definition is a coordinate and
    concurrency policy, not an authorization grant or sandbox. *)

type temporary_location =
  | System_tmp
  | Session_dir
[@@deriving compare, equal, sexp]

type cleanup =
  | On_session_stop
  | On_session_delete
  | Retain
[@@deriving compare, equal, sexp]

type source =
  | Physical of { configured_root : string }
  | Temporary of
      { location : temporary_location
      ; cleanup : cleanup
      ; managed_root : string option
      }
[@@deriving compare, equal, sexp]

type access =
  | Read_only
  | Shared_write
  | Exclusive
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type prompt_limit =
  { prompt_id : Agent_protocol.Id.Prompt_definition.t
  ; max_root_agents : int
  ; overflow : overflow
  }
[@@deriving sexp]

type t = private
  { id : Agent_protocol.Id.Workspace_definition.t
  ; config_name : string
  ; source : source
  ; access : access
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  }
[@@deriving sexp]

val create
  :  id:Agent_protocol.Id.Workspace_definition.t
  -> config_name:string
  -> source:source
  -> access:access
  -> conflict_domain:string option
  -> prompt_limits:prompt_limit list
  -> (t, Agent_store.Store_error.t) result

val prompt_limit : t -> Agent_protocol.Id.Prompt_definition.t -> prompt_limit option
