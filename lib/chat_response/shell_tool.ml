open! Core
module M = Chatmd_shell_spec.Manifest
module R = Shell_runtime.Result
module S = Chatmd_shell_spec.Shell_tool_spec
module SA = Shell_access
module Res = Openai.Responses

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

type input =
  { program : string option
  ; command : string option
  ; script : string option
  ; arguments : string list option
  ; stdin : string option
  ; rationale : string option
  }

exception Tool_error of error

let fail code message = raise_notrace (Tool_error { code; message })

let object_fields = function
  | `Object fields ->
    (match List.find_a_dup (List.map fields ~f:fst) ~compare:String.compare with
     | None -> fields
     | Some name -> fail "shell.tool_duplicate_input" ("duplicate input field: " ^ name))
  | _ -> fail "shell.tool_invalid_input" "shell tool input must be an object"
;;

let field fields name = List.Assoc.find fields name ~equal:String.equal

let string_field fields name =
  Option.map (field fields name) ~f:(function
    | `String value -> value
    | _ -> fail "shell.tool_invalid_input" (name ^ " must be a string"))
;;

let arguments_field fields =
  Option.map (field fields "arguments") ~f:(function
    | `Array values ->
      List.map values ~f:(function
        | `String value -> value
        | _ -> fail "shell.tool_invalid_input" "arguments must contain strings")
    | _ -> fail "shell.tool_invalid_input" "arguments must be an array")
;;

let input_of_string value =
  let fields = Jsonaf.of_string value |> object_fields in
  let allowed =
    String.Set.of_list
      [ "program"; "command"; "script"; "arguments"; "stdin"; "rationale" ]
  in
  List.iter fields ~f:(fun (name, _) ->
    if not (Set.mem allowed name)
    then fail "shell.tool_unknown_input" ("unknown shell tool input field: " ^ name));
  { program = string_field fields "program"
  ; command = string_field fields "command"
  ; script = string_field fields "script"
  ; arguments = arguments_field fields
  ; stdin = string_field fields "stdin"
  ; rationale = string_field fields "rationale"
  }
;;

let string_schema = `Object [ "type", `String "string" ]
let array_schema = `Object [ "type", `String "array"; "items", string_schema ]

let add_property condition name schema properties =
  if condition then (name, schema) :: properties else properties
;;

let is_stdin_enabled = function
  | S.No_stdin -> false
  | Optional_stdin | Required_stdin -> true
;;

let is_rationale_enabled = function
  | S.No_rationale -> false
  | Optional_rationale | Required_rationale -> true
;;

let fixed_accepts_arguments = function
  | S.Fixed { model_arguments = { mode = No_arguments; _ }; _ } -> false
  | Fixed _ -> true
  | Script_file _ -> true
  | Structured | Chain | Raw _ -> false
;;

let properties (tool : S.t) =
  []
  |> add_property (S.equal_mode tool.S.mode Structured) "program" string_schema
  |> add_property (S.equal_mode tool.mode Chain) "command" string_schema
  |> add_property
       (match tool.mode with
        | Raw _ -> true
        | Fixed _ | Structured | Chain | Script_file _ -> false)
       "script"
       string_schema
  |> add_property
       (S.equal_mode tool.mode Structured || fixed_accepts_arguments tool.mode)
       "arguments"
       array_schema
  |> add_property (is_stdin_enabled tool.stdin) "stdin" string_schema
  |> add_property (is_rationale_enabled tool.rationale) "rationale" string_schema
  |> List.rev
;;

let required (tool : S.t) =
  let required =
    match tool.S.mode with
    | Structured -> [ "program"; "arguments" ]
    | Chain -> [ "command" ]
    | Raw _ -> [ "script" ]
    | Fixed { model_arguments = { mode = Required_arguments; _ }; _ } -> [ "arguments" ]
    | Fixed _ | Script_file _ -> []
  in
  let required =
    if S.equal_stdin_mode tool.stdin Required_stdin then "stdin" :: required else required
  in
  if S.equal_rationale_mode tool.rationale Required_rationale
  then "rationale" :: required
  else required
;;

let schema (tool : S.t) =
  `Object
    [ "type", `String "object"
    ; "properties", `Object (properties tool)
    ; "required", `Array (List.map (required tool) ~f:(fun name -> `String name))
    ; "additionalProperties", `False
    ]
;;

let require_some code message = function
  | Some value -> value
  | None -> fail code message
;;

let validate_stdin runtime (tool : S.t) input =
  let stdin =
    match tool.S.stdin, input.stdin with
    | No_stdin, None | Optional_stdin, None -> ""
    | No_stdin, Some _ -> fail "shell.tool_stdin_forbidden" "stdin is disabled"
    | Required_stdin, None -> fail "shell.tool_stdin_required" "stdin is required"
    | (Optional_stdin | Required_stdin), Some value -> value
  in
  if String.length stdin > Shell_runtime.Runtime.max_stdin_bytes runtime
  then fail "shell.tool_stdin_too_large" "stdin exceeds the runtime limit";
  stdin
;;

let validate_rationale (tool : S.t) input =
  match tool.S.rationale, input.rationale with
  | No_rationale, None | Optional_rationale, None -> None
  | No_rationale, Some _ -> fail "shell.tool_rationale_forbidden" "rationale is disabled"
  | Required_rationale, None ->
    fail "shell.tool_rationale_required" "rationale is required"
  | (Optional_rationale | Required_rationale), Some value -> Some value
;;

let validate_model_arguments constraints input =
  let arguments = Option.value input.arguments ~default:[] in
  let count = List.length arguments in
  let minimum =
    Option.value
      constraints.S.min_count
      ~default:
        (if S.equal_arguments_mode constraints.mode Required_arguments then 1 else 0)
  in
  if
    S.equal_arguments_mode constraints.mode No_arguments && Option.is_some input.arguments
  then fail "shell.tool_arguments_forbidden" "arguments are disabled";
  if count < minimum
  then fail "shell.tool_too_few_arguments" "too few arguments were supplied";
  Option.iter constraints.max_count ~f:(fun maximum ->
    if count > maximum
    then fail "shell.tool_too_many_arguments" "too many arguments were supplied");
  Option.iter constraints.max_item_bytes ~f:(fun maximum ->
    if List.exists arguments ~f:(fun argument -> String.length argument > maximum)
    then fail "shell.tool_argument_too_large" "an argument exceeds the configured limit");
  arguments
;;

let fixed_argument runtime source = function
  | S.Literal value -> value
  | Secret_env { name; prefix } ->
    let value =
      Shell_runtime.Runtime.environment_value runtime name
      |> require_some
           "shell.tool_secret_missing"
           ("required secret environment value is missing: " ^ name)
    in
    prefix ^ value
  | Path path ->
    (match Shell_runtime.Runtime.resolve_path runtime ~source path with
     | Ok path -> path
     | Error error -> fail error.code error.message)
;;

let fixed_request runtime (tool : S.t) input command constraints =
  let model_arguments = validate_model_arguments constraints input in
  match command with
  | S.Compact command_line ->
    (match SA.Request.custom_tool ~command_line ~arguments:model_arguments with
     | Ok request -> request
     | Error message -> fail "shell.tool_invalid_fixed_command" message)
  | Argv { program; executable_ref; arguments } ->
    let program =
      Option.first_some program executable_ref
      |> require_some "shell.tool_missing_program" "fixed command has no program"
    in
    let fixed = List.map arguments ~f:(fixed_argument runtime tool.S.source) in
    SA.Request.command (SA.Command.create program (fixed @ model_arguments))
;;

let structured_request input =
  let program =
    require_some "shell.tool_program_required" "program is required" input.program
  in
  let arguments =
    require_some "shell.tool_arguments_required" "arguments are required" input.arguments
  in
  SA.Request.command (SA.Command.create program arguments)
;;

let chain_request input =
  let command =
    require_some "shell.tool_command_required" "command is required" input.command
  in
  match SA.Chain.parse command with
  | Ok chain -> SA.Request.Structured chain
  | Error error ->
    fail
      "shell.tool_chain_parse"
      (sprintf "command parse failed at byte %d: %s" error.offset error.message)
;;

let raw_request runtime source (spec : S.raw_shell) input =
  let script = require_some "shell.tool_script_required" "script is required" input.script in
  let executable =
    match Shell_runtime.Runtime.resolve_path runtime ~source spec.executable with
    | Ok path -> path
    | Error error -> fail error.code error.message
  in
  SA.Request.raw_shell
    ~arguments_before_script:spec.arguments_before_script
    ~executable
    script
;;

let script_request (prepared : Shell_runtime.Registry.script) constraints input =
  let arguments = validate_model_arguments constraints input in
  SA.Request.script_file
    ~executable:prepared.executable_path
    ~arguments:(prepared.arguments_before_model @ arguments)
    ~path:prepared.verification_path
    ~source_sha256:prepared.sha256
    ~executable_sha256:prepared.executable_sha256
    ~max_source_bytes:prepared.max_source_bytes
;;

let reject_field name = function
  | None -> ()
  | Some _ -> fail "shell.tool_field_forbidden" (name ^ " is not valid for this mode")
;;

let request runtime (tool : S.t) prepared_script input =
  match tool.S.mode with
  | Fixed { command; model_arguments } ->
    reject_field "program" input.program;
    reject_field "command" input.command;
    reject_field "script" input.script;
    fixed_request runtime tool input command model_arguments
  | Structured ->
    reject_field "command" input.command;
    reject_field "script" input.script;
    structured_request input
  | Chain ->
    reject_field "program" input.program;
    reject_field "script" input.script;
    reject_field "arguments" input.arguments;
    chain_request input
  | Raw spec ->
    reject_field "program" input.program;
    reject_field "command" input.command;
    reject_field "arguments" input.arguments;
    raw_request runtime tool.source spec input
  | Script_file _ ->
    reject_field "program" input.program;
    reject_field "command" input.command;
    reject_field "script" input.script;
    script_request
      (Option.value_exn prepared_script)
      { S.mode = Optional_arguments
      ; min_count = None
      ; max_count = None
      ; max_item_bytes = None
      }
      input
;;

let is_success = function
  | R.Exited 0 -> true
  | Exited _ | Signaled _ -> false
;;

let render_result (tool : S.t) result =
  if S.equal_nonzero tool.S.nonzero S.Error && not (is_success result.R.status)
  then fail "shell.tool_nonzero" "shell command exited unsuccessfully";
  match tool.result with
  | Combined -> result.stdout ^ result.stderr
  | Stdout -> result.stdout
  | Structured_result -> R.jsonaf_of_t result |> Jsonaf.to_string
;;

let run registry runtime (tool : S.t) prepared_script input =
  let stdin = validate_stdin runtime tool input in
  let rationale = validate_rationale tool input in
  let request = request runtime tool prepared_script input in
  match
    SA.Executor.run
      (Shell_runtime.Runtime.executor_config runtime)
      { request
      ; input = (if String.is_empty stdin then SA.Input.Empty else Text stdin)
      ; rationale
      ; origin = SA.Context.Tool
      }
  with
  | Error error ->
    let error = R.error_of_executor error in
    fail error.code error.message
  | Ok result ->
    let manifest = Shell_runtime.Registry.manifest registry in
    R.of_executor
      ~runtime_id:(Shell_runtime.Runtime.id runtime)
      ~manifest_sha256:manifest.M.sha256
      result
    |> render_result tool
;;

let create_exn registry (tool : S.t) =
  match Shell_runtime.Registry.runtime registry tool.S.runtime with
  | None ->
    Error
      { code = "shell.tool_runtime_missing"
      ; message = "shell tool runtime is not instantiated: " ^ tool.runtime
      }
  | Some runtime ->
    let prepared_script =
      match tool.mode with
      | Script_file _ ->
        Some
          (Shell_runtime.Registry.script registry tool.name
           |> require_some
                "shell.tool_script_missing"
                ("script tool was not prepared during registry instantiation: "
                 ^ tool.name))
      | Fixed _ | Structured | Chain | Raw _ -> None
    in
    let module Definition : Ochat_function.Def with type input = input = struct
      type nonrec input = input

      let name = tool.name
      let type_ = "function"
      let description = tool.description
      let parameters = schema tool
      let input_of_string = input_of_string
    end
    in
    Ok
      (Ochat_function.create_streaming_function
         (module Definition)
         (fun ~invocation:_ input ->
            Res.Tool_output.Output.Text
              (run registry runtime tool prepared_script input)))
;;

let create registry tool =
  try create_exn registry tool with
  | Tool_error error -> Error error
;;
