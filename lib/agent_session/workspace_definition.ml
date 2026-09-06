open Core

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

type t =
  { id : Agent_protocol.Id.Workspace_definition.t
  ; config_name : string
  ; source : source
  ; access : access
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  }
[@@deriving sexp]

let validate_source = function
  | Physical { configured_root } when not (Filename.is_absolute configured_root) ->
    Error (Agent_store.Store_error.Corrupt "physical workspace root must be absolute")
  | Temporary { location = System_tmp; managed_root = None; _ } ->
    Error
      (Agent_store.Store_error.Corrupt
         "system temporary workspace requires a managed root")
  | Temporary { managed_root = Some root; _ } when not (Filename.is_absolute root) ->
    Error (Agent_store.Store_error.Corrupt "temporary managed root must be absolute")
  | Physical _ | Temporary _ -> Ok ()
;;

let create ~id ~config_name ~source ~access ~conflict_domain ~prompt_limits =
  let open Result.Let_syntax in
  let%bind () = validate_source source in
  if String.is_empty config_name
  then Error (Agent_store.Store_error.Corrupt "workspace config name must be nonempty")
  else if List.exists prompt_limits ~f:(fun limit -> limit.max_root_agents <= 0)
  then Error (Agent_store.Store_error.Corrupt "workspace prompt limits must be positive")
  else if
    List.contains_dup prompt_limits ~compare:(fun left right ->
      Agent_protocol.Id.Prompt_definition.compare left.prompt_id right.prompt_id)
  then
    Error (Agent_store.Store_error.Corrupt "workspace prompt limits contain duplicates")
  else Ok { id; config_name; source; access; conflict_domain; prompt_limits }
;;

let prompt_limit t prompt_id =
  List.find t.prompt_limits ~f:(fun limit ->
    Agent_protocol.Id.Prompt_definition.compare limit.prompt_id prompt_id = 0)
;;
