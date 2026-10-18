open! Core
module Error = Protocol_error
module C = Developer_codec
module D = Document_schema
module R = Developer_receipt

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

let option fields name decode =
  let* value = C.optional fields name in
  Option.value_map value ~default:(Ok None) ~f:(fun value ->
    Result.map (decode value) ~f:Option.some)
;;

module Target = struct
  type t =
    | One_off_v1
    | Tool_v1
    | Developer_tool_v1
    | Moderator_v1
    | Delegated_moderator_v1
  [@@deriving compare, equal, sexp_of]

  let to_string = function
    | One_off_v1 -> "one_off_v1"
    | Tool_v1 -> "tool_v1"
    | Developer_tool_v1 -> "developer_tool_v1"
    | Moderator_v1 -> "moderator_v1"
    | Delegated_moderator_v1 -> "delegated_moderator_v1"
  ;;

  let of_string = function
    | "one_off_v1" -> Ok One_off_v1
    | "tool_v1" -> Ok Tool_v1
    | "developer_tool_v1" -> Ok Developer_tool_v1
    | "moderator_v1" -> Ok Moderator_v1
    | "delegated_moderator_v1" -> Ok Delegated_moderator_v1
    | _ -> C.invalid "unknown developer target"
  ;;
end

module Route = Developer_route.Route
module Fixture = Developer_fixture.Fixture

module Execution_limits = struct
  type t =
    { fuel : int
    ; max_tasks : int
    ; wall_seconds : float
    ; max_value_bytes : int
    ; max_array_items : int
    ; max_depth : int
    ; allocation_bytes : int
    ; max_calls : int
    ; max_invocation_depth : int
    }

  let create
        ~fuel
        ~max_tasks
        ~wall_seconds
        ~max_value_bytes
        ~max_array_items
        ~max_depth
        ~allocation_bytes
        ~max_calls
        ~max_invocation_depth
    =
    if
      fuel <= 0
      || List.exists
           [ fuel
           ; max_tasks
           ; max_value_bytes
           ; max_array_items
           ; max_depth
           ; allocation_bytes
           ; max_calls
           ; max_invocation_depth
           ]
           ~f:(fun value -> value > Authoring_codec.maximum_wire_integer)
      || max_tasks < 0
      || (not (Float.is_finite wall_seconds))
      || Float.(wall_seconds <= 0.)
      || max_value_bytes <= 0
      || max_array_items <= 0
      || max_depth <= 0
      || allocation_bytes <= 0
      || max_calls < 0
      || max_invocation_depth <= 0
    then C.invalid "invalid developer execution limits"
    else
      Ok
        { fuel
        ; max_tasks
        ; wall_seconds
        ; max_value_bytes
        ; max_array_items
        ; max_depth
        ; allocation_bytes
        ; max_calls
        ; max_invocation_depth
        }
  ;;

  let to_json t =
    `Object
      [ "fuel", C.integer_json t.fuel
      ; "max_tasks", C.integer_json t.max_tasks
      ; "wall_seconds", `Number (Float.to_string t.wall_seconds)
      ; "max_value_bytes", C.integer_json t.max_value_bytes
      ; "max_array_items", C.integer_json t.max_array_items
      ; "max_depth", C.integer_json t.max_depth
      ; "allocation_bytes", C.integer_json t.allocation_bytes
      ; "max_calls", C.integer_json t.max_calls
      ; "max_invocation_depth", C.integer_json t.max_invocation_depth
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "fuel"
          ; "max_tasks"
          ; "wall_seconds"
          ; "max_value_bytes"
          ; "max_array_items"
          ; "max_depth"
          ; "allocation_bytes"
          ; "max_calls"
          ; "max_invocation_depth"
          ]
        json
    in
    let* fuel = get fields "fuel" C.positive in
    let* max_tasks = get fields "max_tasks" C.nonnegative in
    let* wall_seconds = get fields "wall_seconds" C.finite_positive_float in
    let* max_value_bytes = get fields "max_value_bytes" C.positive in
    let* max_array_items = get fields "max_array_items" C.positive in
    let* max_depth = get fields "max_depth" C.positive in
    let* allocation_bytes = get fields "allocation_bytes" C.positive in
    let* max_calls = get fields "max_calls" C.nonnegative in
    let* max_invocation_depth = get fields "max_invocation_depth" C.positive in
    create
      ~fuel
      ~max_tasks
      ~wall_seconds
      ~max_value_bytes
      ~max_array_items
      ~max_depth
      ~allocation_bytes
      ~max_calls
      ~max_invocation_depth
  ;;
end

module Submit = struct
  type t =
    { run_id : Id.Developer_run.t
    ; key : Idempotency_key.t
    ; kind : R.Kind.t
    ; route : Route.t
    ; target : Target.t
    ; prompt : Id.Prompt_definition.t option
    ; workspace : Id.Workspace_definition.t option
    ; entrypoint : string
    ; input : Jsonaf.t
    ; fixtures : Fixture.t option
    ; requested_execution : Execution_limits.t option
    ; requested_max_output_bytes : int option
    }

  let to_json t =
    `Object
      ([ "run_id", Id.Developer_run.to_json t.run_id
       ; "key", Idempotency_key.to_json t.key
       ; ( "kind"
         , `String
             (match t.kind with
              | R.Kind.Check -> "check"
              | Fixture_run -> "fixture_run"
              | Live_run -> "live_run") )
       ; "route", Route.to_json t.route
       ; "target", `String (Target.to_string t.target)
       ; "entrypoint", `String t.entrypoint
       ; "input", t.input
       ]
       @ C.optional_field "prompt" t.prompt Id.Prompt_definition.to_json
       @ C.optional_field "workspace" t.workspace Id.Workspace_definition.to_json
       @ C.optional_field "fixtures" t.fixtures Fixture.to_json
       @ C.optional_field
           "requested_execution"
           t.requested_execution
           Execution_limits.to_json
       @ C.optional_field
           "requested_max_output_bytes"
           t.requested_max_output_bytes
           C.integer_json)
  ;;

  let create
        ~limits
        ~run_id
        ~key
        ~kind
        ~route
        ~target
        ~prompt
        ~workspace
        ~entrypoint
        ~input
        ~fixtures
        ~requested_execution
        ~requested_max_output_bytes
    =
    let valid_resources =
      match kind, prompt, workspace with
      | R.Kind.Live_run, Some _, Some _ -> true
      | (Check | Fixture_run), None, None -> true
      | _ -> false
    in
    let valid_target =
      match kind, target with
      | (R.Kind.Check | Fixture_run), (Target.One_off_v1 | Tool_v1) | Live_run, One_off_v1
        -> true
      | _ -> false
    in
    if
      (not valid_resources)
      || (not valid_target)
      || String.is_empty entrypoint
      || String.length entrypoint > 256
      || Option.exists requested_max_output_bytes ~f:(fun x ->
        x <= 0 || x > Developer_metadata.Limits.max_output_bytes limits)
    then C.invalid "invalid developer submission"
    else
      let* () =
        match fixtures with
        | None -> Ok ()
        | Some fixture ->
          let* _ =
            Fixture.create
              ~limits:(Developer_metadata.Limits.request limits)
              ~max_steps:(Developer_metadata.Limits.max_fixture_steps limits)
              ~max_bytes:(Developer_metadata.Limits.max_fixture_bytes limits)
              ~steps:(Fixture.steps fixture)
          in
          Ok ()
      in
      let t =
        { run_id
        ; key
        ; kind
        ; route
        ; target
        ; prompt
        ; workspace
        ; entrypoint
        ; input
        ; fixtures
        ; requested_execution
        ; requested_max_output_bytes
        }
      in
      let* () =
        C.validate ~limits:(Developer_metadata.Limits.request limits) (to_json t)
      in
      Ok t
  ;;

  let run_id t = t.run_id
  let key t = t.key
  let kind t = t.kind
  let route t = t.route
  let target t = t.target
  let prompt t = t.prompt
  let workspace t = t.workspace
  let entrypoint t = t.entrypoint
  let input t = t.input
  let fixtures t = t.fixtures
  let requested_execution t = t.requested_execution
  let requested_max_output_bytes t = t.requested_max_output_bytes

  let of_json ~limits json =
    let* () = C.validate ~limits:(Developer_metadata.Limits.request limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "run_id"
          ; "key"
          ; "kind"
          ; "route"
          ; "target"
          ; "prompt"
          ; "workspace"
          ; "entrypoint"
          ; "input"
          ; "fixtures"
          ; "requested_execution"
          ; "requested_max_output_bytes"
          ]
        json
    in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* key = get fields "key" Idempotency_key.of_json in
    let* kind = get fields "kind" (C.string ~max_bytes:16) in
    let* kind =
      match kind with
      | "check" -> Ok R.Kind.Check
      | "fixture_run" -> Ok Fixture_run
      | "live_run" -> Ok Live_run
      | _ -> C.invalid "unknown developer kind"
    in
    let* route =
      get
        fields
        "route"
        (Route.of_json ~limits:(Developer_metadata.Limits.request limits))
    in
    let* target =
      get fields "target" (fun json ->
        let* value = C.string ~max_bytes:40 json in
        Target.of_string value)
    in
    let* prompt = option fields "prompt" Id.Prompt_definition.of_json in
    let* workspace = option fields "workspace" Id.Workspace_definition.of_json in
    let* entrypoint = get fields "entrypoint" (C.string ~max_bytes:256) in
    let* input = C.required fields "input" in
    let* fixtures =
      option
        fields
        "fixtures"
        (Fixture.of_json
           ~limits:(Developer_metadata.Limits.request limits)
           ~max_steps:(Developer_metadata.Limits.max_fixture_steps limits)
           ~max_bytes:(Developer_metadata.Limits.max_fixture_bytes limits))
    in
    let* requested_execution =
      option fields "requested_execution" Execution_limits.of_json
    in
    let* requested_max_output_bytes =
      option fields "requested_max_output_bytes" C.positive
    in
    create
      ~limits
      ~run_id
      ~key
      ~kind
      ~route
      ~target
      ~prompt
      ~workspace
      ~entrypoint
      ~input
      ~fixtures
      ~requested_execution
      ~requested_max_output_bytes
  ;;
end

module Lookup = struct
  type t =
    { run_id : Id.Developer_run.t
    ; key : Idempotency_key.t
    }

  let create ~run_id ~key = { run_id; key }

  let to_json t =
    `Object
      [ "run_id", Id.Developer_run.to_json t.run_id
      ; "key", Idempotency_key.to_json t.key
      ]
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits json in
    let* fields = C.fields ~allowed:[ "run_id"; "key" ] json in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* key = get fields "key" Idempotency_key.of_json in
    Ok { run_id; key }
  ;;
end
