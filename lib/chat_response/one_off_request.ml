open Core
module X = Chatml_execution
module D = Chatmd_shell_spec.Diagnostic
module S = Chatmd_shell_spec.Chatmd_script_spec
module Duration = Chatmd_shell_spec.Duration
module Schema = Chatmd_shell_spec.Tool_schema

type policy =
  { compilation : Chatml_compilation.limits
  ; execution : X.limits
  ; max_output_bytes : int
  }

let default_policy =
  { compilation = Chatml_compilation.default_limits
  ; execution = X.default_limits
  ; max_output_bytes = 1024 * 1024
  }
;;

type t =
  { source : string
  ; input : Jsonaf.t
  ; tools : string list
  ; policy : policy
  }

let error path code message = Error [ D.error ~path ~code message ]
let invalid path message = error path "chatml.invalid_request" message
let positive = `Object [ "type", `String "integer"; "minimum", `Number "1" ]
let nonnegative = `Object [ "type", `String "integer"; "minimum", `Number "0" ]

let integer_limits =
  [ "fuel", positive
  ; "max_tasks", nonnegative
  ; "max_calls", nonnegative
  ; "max_invocation_depth", positive
  ; "allocation_bytes", positive
  ; "max_value_bytes", positive
  ; "max_output_bytes", positive
  ; "max_array_items", positive
  ; "max_depth", positive
  ; "max_source_bytes", positive
  ; "compile_timeout_ms", positive
  ]
;;

let parameters =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ "source", `Object [ "type", `String "string" ]
          ; "input", `True
          ; ( "tools"
            , `Object
                [ "type", `String "array"; "items", `Object [ "type", `String "string" ] ]
            )
          ; "timeout_ms", positive
          ; ( "limits"
            , `Object
                [ "type", `String "object"
                ; "properties", `Object integer_limits
                ; "additionalProperties", `False
                ] )
          ] )
    ; "required", `Array [ `String "source"; `String "input"; `String "tools" ]
    ; "additionalProperties", `False
    ]
;;

let schema =
  match Schema.compile parameters with
  | Ok schema -> schema
  | Error _ -> assert false
;;

let script_limits_for policy =
  let seconds =
    Duration.parse (Float.to_string policy.execution.wall_seconds ^ "s")
    |> Result.ok_or_failwith
  in
  let bytes value = Duration.parse_bytes (Int.to_string value) |> Result.ok_or_failwith in
  S.
    { wall_time = seconds
    ; fuel = policy.execution.fuel
    ; max_tasks = policy.execution.max_tasks
    ; max_value_bytes = bytes policy.execution.max_value_bytes
    ; max_output_bytes = bytes policy.max_output_bytes
    ; max_array_items = policy.execution.max_array_items
    ; max_depth = policy.execution.max_depth
    }
;;

let script_limits request = script_limits_for request.policy

let decode ~policy request =
  let open Result.Let_syntax in
  let%bind () =
    Chatml_compilation.validate_limits policy.compilation
    |> Result.map_error ~f:(fun error ->
      [ D.error ~path:[ "limits" ] ~code:error.code error.message ])
  in
  let%bind () =
    let x = policy.execution in
    match
      Float.is_finite x.wall_seconds
      && Float.(x.wall_seconds > 0.)
      && List.for_all
           [ x.fuel
           ; x.max_value_bytes
           ; x.max_array_items
           ; x.max_depth
           ; x.allocation_bytes
           ; x.max_invocation_depth
           ; policy.max_output_bytes
           ]
           ~f:(fun n -> n > 0)
      && x.max_tasks >= 0
      && x.max_calls >= 0
    with
    | true -> Ok ()
    | false ->
      error [ "limits" ] "chatml.invalid_limits" "invalid host one-off resource policy"
  in
  let%bind () =
    Schema.validate schema request
    |> Result.map_error ~f:(fun _ ->
      [ D.error
          ~code:"chatml.invalid_request"
          "request does not match the run_chatml schema"
      ])
  in
  let%bind fields =
    match request with
    | `Object fields -> Ok fields
    | _ -> invalid [] "expected request object"
  in
  let get fields name = List.Assoc.find fields name ~equal:String.equal in
  let%bind source =
    match get fields "source" with
    | Some (`String source) -> Ok source
    | _ -> invalid [ "source" ] "expected ChatML source text"
  in
  let%bind input =
    match get fields "input" with
    | Some input -> Ok input
    | None -> invalid [ "input" ] "input is required"
  in
  let%bind tools =
    match get fields "tools" with
    | Some (`Array names) ->
      List.map names ~f:(function
        | `String name -> Ok name
        | _ -> invalid [ "tools" ] "expected exact tool names")
      |> Result.all
    | _ -> invalid [ "tools" ] "tools is required; use an empty array to select none"
  in
  let%bind () =
    match List.find_a_dup tools ~compare:String.compare with
    | None -> Ok ()
    | Some _ -> invalid [ "tools" ] "tool selection contains duplicate names"
  in
  let%bind limits =
    match get fields "limits" with
    | None -> Ok []
    | Some (`Object fields) -> Ok fields
    | _ -> invalid [ "limits" ] "expected limit overrides"
  in
  let integer path = function
    | `Number value ->
      (match Int.of_string_opt value with
       | Some value -> Ok value
       | None -> invalid path "expected an integer within the host range")
    | _ -> invalid path "expected integer limit"
  in
  let lower name maximum =
    match get limits name with
    | None -> Ok maximum
    | Some json ->
      let%bind value = integer [ "limits"; name ] json in
      (match value <= maximum with
       | true -> Ok value
       | false ->
         error
           [ "limits"; name ]
           "chatml.limit_escalation"
           "requested limit exceeds the host ceiling")
  in
  let timeout path fields name maximum =
    match get fields name with
    | None -> Ok maximum
    | Some json ->
      let%bind milliseconds = integer path json in
      let seconds = Float.of_int milliseconds /. 1000. in
      (match Float.(seconds <= maximum) with
       | true -> Ok seconds
       | false ->
         error path "chatml.limit_escalation" "requested timeout exceeds the host ceiling")
  in
  let x = policy.execution in
  let%bind wall_seconds = timeout [ "timeout_ms" ] fields "timeout_ms" x.wall_seconds in
  let%bind fuel = lower "fuel" x.fuel in
  let%bind max_tasks = lower "max_tasks" x.max_tasks in
  let%bind max_calls = lower "max_calls" x.max_calls in
  let%bind max_invocation_depth = lower "max_invocation_depth" x.max_invocation_depth in
  let%bind allocation_bytes = lower "allocation_bytes" x.allocation_bytes in
  let%bind max_value_bytes = lower "max_value_bytes" x.max_value_bytes in
  let%bind max_output_bytes = lower "max_output_bytes" policy.max_output_bytes in
  let%bind max_array_items = lower "max_array_items" x.max_array_items in
  let%bind max_depth = lower "max_depth" x.max_depth in
  let%bind max_source_bytes =
    lower "max_source_bytes" policy.compilation.max_source_bytes
  in
  let%bind compile_seconds =
    timeout
      [ "limits"; "compile_timeout_ms" ]
      limits
      "compile_timeout_ms"
      policy.compilation.wall_seconds
  in
  let policy =
    { compilation = { max_source_bytes; wall_seconds = compile_seconds }
    ; execution =
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
    ; max_output_bytes
    }
  in
  let%bind () =
    match String.length source <= max_source_bytes with
    | true -> Ok ()
    | false ->
      error [ "source" ] "chatml.source_limit" "source exceeds the effective byte limit"
  in
  let%map _ =
    Moderator_invocation.snapshot_state
      ~limits:(script_limits_for policy)
      (Chatml.Chatml_value_codec.jsonaf_to_value input)
    |> Result.map_error ~f:(fun _ ->
      [ D.error
          ~path:[ "input" ]
          ~code:"chatml.input_limit"
          "input exceeds the effective value limits"
      ])
  in
  { source; input; tools; policy }
;;
