open Core

(** Explicit trusted-registration metadata. Tool names do not imply authoring
    behavior. Metadata travels with the actual selected implementation. *)
type task =
  | One_off_script [@jsonaf.name "one_off_script"]
  | Standalone_tool [@jsonaf.name "standalone_tool"]
  | Moderator_tool [@jsonaf.name "moderator_tool"]
  | Child_agent [@jsonaf.name "child_agent"]
  | Background_workflow [@jsonaf.name "background_workflow"]
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type helper =
  | Reference [@jsonaf.name "ochat_authoring_context"]
  | Validation [@jsonaf.name "ochat_validate"]
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type help =
  { version : int
  ; package : string
  ; tasks : task list
  ; topics : string list
  ; required_helpers : helper list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { authoring : help option
  ; helper : helper option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

val empty : t
val task_id : task -> string
val helper_name : helper -> string
val valid_topic : string -> bool
val validate_help : help -> (unit, string) result
val validate : tool_name:string -> t -> (unit, string) result
