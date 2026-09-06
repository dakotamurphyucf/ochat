open! Core
open Jsonaf.Export

type stdin_mode =
  | No_stdin
  | Optional_stdin
  | Required_stdin
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type rationale_mode =
  | No_rationale
  | Optional_rationale
  | Required_rationale
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type result_format =
  | Combined
  | Stdout
  | Structured_result
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type stream_mode =
  | Finalized
  | Sanitized
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type nonzero =
  | Result
  | Error
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type arguments_mode =
  | No_arguments
  | Optional_arguments
  | Required_arguments
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type arguments =
  { mode : arguments_mode
  ; min_count : int option
  ; max_count : int option
  ; max_item_bytes : int option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type fixed_argument =
  | Literal of string
  | Secret_env of
      { name : string
      ; prefix : string
      }
  | Path of Path_expr.t
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type fixed_command =
  | Compact of string
  | Argv of
      { program : string option
      ; executable_ref : string option
      ; arguments : fixed_argument list
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type verification_mode = Verify_sha256
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type raw_shell =
  { executable : Path_expr.t
  ; arguments_before_script : string list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type script_file =
  { script : Path_expr.t
  ; interpreter : Path_expr.t option
  ; executable : bool
  ; fixed_arguments : string list
  ; verification : verification_mode
  ; max_source_bytes : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type mode =
  | Fixed of
      { command : fixed_command
      ; model_arguments : arguments
      }
  | Structured
  | Chain
  | Raw of raw_shell
  | Script_file of script_file
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { name : string
  ; description : string option
  ; runtime : string
  ; mode : mode
  ; stdin : stdin_mode
  ; rationale : rationale_mode
  ; result : result_format
  ; stream : stream_mode
  ; nonzero : nonzero
  ; source : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

exception Parse_error of Diagnostic.t

let fail source code message =
  raise_notrace (Parse_error (Diagnostic.error ~source ~code message))
;;

let reject_duplicates source attributes =
  match List.find_a_dup (List.map attributes ~f:fst) ~compare:String.compare with
  | None -> ()
  | Some name ->
    fail source "shell.tool_duplicate_attribute" ("duplicate attribute: " ^ name)
;;

let reject_unknown source attributes =
  let allowed =
    String.Set.of_list
      [ "name"
      ; "type"
      ; "mode"
      ; "runtime"
      ; "description"
      ; "command"
      ; "executable_ref"
      ; "executable"
      ; "script"
      ; "interpreter"
      ; "arguments_before_script"
      ; "fixed_arguments"
      ; "verification"
      ; "max_source_bytes"
      ; "stdin"
      ; "rationale"
      ; "result"
      ; "stream"
      ; "nonzero"
      ]
  in
  List.iter attributes ~f:(fun (name, _) ->
    if not (Set.mem allowed name)
    then
      fail source "shell.tool_unknown_attribute" ("unknown shell tool attribute: " ^ name))
;;

let common_attributes =
  String.Set.of_list
    [ "name"
    ; "type"
    ; "mode"
    ; "runtime"
    ; "description"
    ; "stdin"
    ; "rationale"
    ; "result"
    ; "stream"
    ; "nonzero"
    ]
;;

let mode_attributes = function
  | "fixed" -> [ "command"; "executable_ref" ]
  | "structured" | "chain" -> []
  | "raw" -> [ "executable"; "arguments_before_script" ]
  | "script" ->
    [ "script"
    ; "interpreter"
    ; "executable"
    ; "fixed_arguments"
    ; "verification"
    ; "max_source_bytes"
    ]
  | _ -> []
;;

let reject_mode_incompatible source attributes mode =
  let allowed = Set.union common_attributes (String.Set.of_list (mode_attributes mode)) in
  List.iter attributes ~f:(fun (name, _) ->
    if not (Set.mem allowed name)
    then
      fail
        source
        "shell.tool_mode_attribute"
        (sprintf "attribute %s is not valid for %s mode" name mode))
;;

let find attributes name = List.Assoc.find attributes name ~equal:String.equal

let require source attributes name =
  match find attributes name with
  | Some value when not (String.is_empty value) -> value
  | _ -> fail source "shell.tool_missing_attribute" ("missing required attribute: " ^ name)
;;

let enum source name value choices =
  match List.Assoc.find choices value ~equal:String.equal with
  | Some value -> value
  | None ->
    fail source "shell.tool_invalid_attribute" (sprintf "invalid %s value: %s" name value)
;;

let parse_path source value =
  match Path_expr.parse value with
  | Ok path -> path
  | Error message -> fail source "shell.tool_invalid_path" message
;;

let string_list source name value =
  try
    match Jsonaf.of_string value with
    | `Array values ->
      List.map values ~f:(function
        | `String value -> value
        | _ -> fail source "shell.tool_invalid_attribute" (name ^ " must contain strings"))
    | _ -> fail source "shell.tool_invalid_attribute" (name ^ " must be a JSON array")
  with
  | Parse_error _ as exn -> raise exn
  | _ -> fail source "shell.tool_invalid_attribute" (name ^ " must be a JSON array")
;;

let positive_int source name value =
  match Int.of_string_opt value with
  | Some value when value > 0 -> value
  | _ -> fail source "shell.tool_invalid_attribute" (name ^ " must be positive")
;;

let default_arguments =
  { mode = Optional_arguments; min_count = None; max_count = None; max_item_bytes = None }
;;

let fixed_mode source attributes long_command model_arguments =
  match long_command, find attributes "command", find attributes "executable_ref" with
  | Some command, None, None -> Fixed { command; model_arguments }
  | Some _, _, _ ->
    fail
      source
      "shell.tool_conflicting_command"
      "long-form command conflicts with command attributes"
  | None, Some command, None when not (String.is_empty command) ->
    Fixed { command = Compact command; model_arguments }
  | None, None, Some executable_ref ->
    Fixed
      { command =
          Argv { program = None; executable_ref = Some executable_ref; arguments = [] }
      ; model_arguments
      }
  | None, Some _, Some _ ->
    fail source "shell.tool_conflicting_command" "command and executable_ref conflict"
  | None, Some _, None | None, None, None ->
    fail
      source
      "shell.tool_missing_command"
      "fixed tool requires command or executable_ref"
;;

let script_mode source attributes =
  let script = parse_path source (require source attributes "script") in
  let interpreter = Option.map (find attributes "interpreter") ~f:(parse_path source) in
  let executable =
    Option.value_map
      (find attributes "executable")
      ~default:(Option.is_none interpreter)
      ~f:(fun value -> enum source "executable" value [ "true", true; "false", false ])
  in
  if executable && Option.is_some interpreter
  then
    fail
      source
      "shell.tool_conflicting_script"
      "executable script cannot declare interpreter";
  let fixed_arguments =
    Option.value_map
      (find attributes "fixed_arguments")
      ~default:[]
      ~f:(string_list source "fixed_arguments")
  in
  let verification =
    enum
      source
      "verification"
      (Option.value (find attributes "verification") ~default:"sha256")
      [ "sha256", Verify_sha256 ]
  in
  let max_source_bytes =
    Option.value_map
      (find attributes "max_source_bytes")
      ~default:1_048_576
      ~f:(positive_int source "max_source_bytes")
  in
  Script_file
    { script
    ; interpreter
    ; executable
    ; fixed_arguments
    ; verification
    ; max_source_bytes
    }
;;

let parse_mode source attributes mode long_command model_arguments =
  match mode with
  | "fixed" -> fixed_mode source attributes long_command model_arguments
  | "structured" -> Structured
  | "chain" -> Chain
  | "raw" ->
    Raw
      { executable = parse_path source (require source attributes "executable")
      ; arguments_before_script =
          Option.value_map
            (find attributes "arguments_before_script")
            ~default:[ "-c" ]
            ~f:(string_list source "arguments_before_script")
      }
  | "script" -> script_mode source attributes
  | value -> fail source "shell.tool_invalid_mode" ("invalid shell tool mode: " ^ value)
;;

let parse_common source attributes mode =
  let stdin =
    enum
      source
      "stdin"
      (Option.value (find attributes "stdin") ~default:"none")
      [ "none", No_stdin; "optional", Optional_stdin; "required", Required_stdin ]
  in
  let rationale =
    enum
      source
      "rationale"
      (Option.value
         (find attributes "rationale")
         ~default:(if String.equal mode "fixed" then "none" else "optional"))
      [ "none", No_rationale
      ; "optional", Optional_rationale
      ; "required", Required_rationale
      ]
  in
  stdin, rationale
;;

let parse_output source attributes =
  let result =
    enum
      source
      "result"
      (Option.value (find attributes "result") ~default:"combined")
      [ "combined", Combined; "stdout", Stdout; "structured", Structured_result ]
  in
  let stream =
    enum
      source
      "stream"
      (Option.value (find attributes "stream") ~default:"finalized")
      [ "finalized", Finalized; "sanitized", Sanitized ]
  in
  let nonzero =
    enum
      source
      "nonzero"
      (Option.value (find attributes "nonzero") ~default:"result")
      [ "result", Result; "error", Error ]
  in
  result, stream, nonzero
;;

let parse_exn ~source ~command ~arguments attributes =
  reject_duplicates source attributes;
  reject_unknown source attributes;
  let type_ = Option.value (find attributes "type") ~default:"shell" in
  if not (String.equal type_ "shell")
  then fail source "shell.tool_invalid_type" "tool type must be shell";
  let mode_name = Option.value (find attributes "mode") ~default:"fixed" in
  reject_mode_incompatible source attributes mode_name;
  if (not (String.equal mode_name "fixed")) && Option.is_some command
  then fail source "shell.tool_mode_child" "command child is valid only for fixed mode";
  if (not (String.equal mode_name "fixed")) && Option.is_some arguments
  then fail source "shell.tool_mode_child" "arguments child is valid only for fixed mode";
  let stdin, rationale = parse_common source attributes mode_name in
  let result, stream, nonzero = parse_output source attributes in
  { name = require source attributes "name"
  ; description = find attributes "description"
  ; runtime = require source attributes "runtime"
  ; mode =
      parse_mode
        source
        attributes
        mode_name
        command
        (Option.value arguments ~default:default_arguments)
  ; stdin
  ; rationale
  ; result
  ; stream
  ; nonzero
  ; source
  }
;;

let parse_compact_attributes ~source attributes =
  try Ok (parse_exn ~source ~command:None ~arguments:None attributes) with
  | Parse_error diagnostic -> Error [ diagnostic ]
;;

let parse_declaration ~source ~attributes ~command ~arguments =
  try Ok (parse_exn ~source ~command ~arguments attributes) with
  | Parse_error diagnostic -> Error [ diagnostic ]
;;
