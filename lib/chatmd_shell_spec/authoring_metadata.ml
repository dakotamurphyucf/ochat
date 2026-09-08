open Core
open Jsonaf.Export

type task =
  | One_off_script [@jsonaf.name "one_off_script"]
  | Standalone_tool [@jsonaf.name "standalone_tool"]
  | Moderator_tool [@jsonaf.name "moderator_tool"]
  | Child_agent [@jsonaf.name "child_agent"]
  | Background_workflow [@jsonaf.name "background_workflow"]
[@@deriving sexp, compare, equal, bin_io, jsonaf]

type helper =
  | Reference [@jsonaf.name "ochat_authoring_context"]
  | Validation [@jsonaf.name "ochat_validate"]
[@@deriving sexp, compare, equal, bin_io, jsonaf]

type help =
  { version : int
  ; package : string
  ; tasks : task list
  ; topics : string list
  ; required_helpers : helper list
  }
[@@deriving sexp, compare, equal, bin_io, jsonaf]

type t =
  { authoring : help option
  ; helper : helper option
  }
[@@deriving sexp, compare, equal, bin_io, jsonaf]

let empty = { authoring = None; helper = None }

let task_id = function
  | One_off_script -> "one_off_script"
  | Standalone_tool -> "standalone_tool"
  | Moderator_tool -> "moderator_tool"
  | Child_agent -> "child_agent"
  | Background_workflow -> "background_workflow"
;;

let helper_name = function
  | Reference -> "ochat_authoring_context"
  | Validation -> "ochat_validate"
;;

let valid_topic value =
  String.length value > 0
  && String.length value <= 128
  && String.for_all value ~f:(function
    | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':' | '/' -> true
    | _ -> false)
;;

let unique compare values = Option.is_none (List.find_a_dup values ~compare)

let validate_help help =
  if
    help.version <> 1
    || (not (valid_topic help.package))
    || List.is_empty help.tasks
    || List.length help.tasks > 5
    || (not (unique compare_task help.tasks))
    || List.is_empty help.topics
    || List.length help.topics > 32
    || (not (List.for_all help.topics ~f:valid_topic))
    || (not (unique String.compare help.topics))
    || List.length help.required_helpers > 2
    || not (unique compare_helper help.required_helpers)
  then Error "invalid authoring help metadata"
  else Ok ()
;;

let validate ~tool_name metadata =
  match metadata.authoring, metadata.helper with
  | Some _, Some _ -> Error "a read-only helper cannot itself request authoring helpers"
  | Some help, None -> validate_help help
  | None, Some helper when not (String.equal tool_name (helper_name helper)) ->
    Error "helper metadata does not match its reserved callable name"
  | None, _ -> Ok ()
;;
