open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module A = Goal_native_admission
module C = Chat_response.Tool_capability
module Service = Goal_native_metadata_service

module Schema = struct
  let text = `Object [ "type", `String "string" ]

  let counter =
    `Object
      [ "type", `String "string"
      ; "minLength", `Number "1"
      ; "description", `String "Canonical nonnegative decimal string; no leading zeroes"
      ; "maxLength", `Number "19"
      ]
  ;;

  let integer = `Object [ "type", `String "integer"; "minimum", `Number "0" ]

  let object_ fields =
    `Object
      [ "type", `String "object"
      ; "properties", `Object fields
      ; "required", `Array (List.map fields ~f:(fun (name, _) -> `String name))
      ; "additionalProperties", `False
      ]
  ;;

  let nullable value =
    `Object [ "anyOf", `Array [ value; `Object [ "type", `String "null" ] ] ]
  ;;

  let constant value =
    `Object [ "type", `String "string"; "enum", `Array [ `String value ] ]
  ;;

  let source =
    object_
      [ "script_id", text
      ; "source_sha256", text
      ; "generation", integer
      ; "installation_epoch", counter
      ]
  ;;

  let basis =
    [ "generation", counter
    ; "goal_id", text
    ; "expected_catalog_revision", counter
    ; "key", text
    ]
  ;;

  let revision = "expected_goal_revision", counter
  let get = object_ [ "goal_id", nullable text ]

  let component =
    `Object
      [ "type", `String "string"
      ; ( "enum"
        , `Array
            (List.map
               [ "input"
               ; "output"
               ; "reported_total"
               ; "cached_input"
               ; "cache_write_input"
               ; "reasoning_output"
               ]
               ~f:(fun name -> `String name)) )
      ]
  ;;

  let budget =
    object_
      [ "admitted_turns", nullable counter
      ; "admitted_attempts", nullable counter
      ; ( "observed_tokens"
        , nullable (object_ [ "component", component; "ceiling", counter ]) )
      ; "deadline", nullable text
      ]
  ;;

  let create = object_ (basis @ [ "source", source; "objective", text; "budget", budget ])
  let control kind = object_ (basis @ [ revision; "kind", constant kind; "reason", text ])

  let edit kind =
    object_ (basis @ [ revision; "source", source; "kind", constant kind; "value", text ])
  ;;
end

let get_request json =
  let open Result.Let_syntax in
  let%bind () = P.Json_codec.validate_limits ~max_bytes:1024 ~max_depth:2 json in
  let%bind fields = P.Json_codec.fields json in
  let%bind () =
    if
      List.for_all (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
        String.equal name "goal_id")
    then Ok ()
    else Error (P.Error.invalid_request "unknown Goal metadata query field")
  in
  let%bind value = P.Json_codec.required fields "goal_id" in
  match value with
  | `Null -> Ok None
  | value -> P.Id.Goal.of_json value |> Result.map ~f:Option.some
;;

let limits ~max_bytes ~max_depth ~max_fields ~max_nodes =
  Document_schema.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let get_limits = limits ~max_bytes:1024 ~max_depth:2 ~max_fields:4 ~max_nodes:8
let mutation_limits = limits ~max_bytes:32768 ~max_depth:16 ~max_fields:128 ~max_nodes:512

let decode_text_exn effect_kind text =
  let limits =
    match effect_kind with
    | A.Effect.Get -> get_limits
    | Create | Hold | Abandon | Objective | Progress | Resume -> mutation_limits
  in
  Document_schema.Json.decode ~limits text
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let with_service effect_kind run =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(A.Effect.name effect_kind)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "Goal native binding is unavailable")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let%bind admission = A.create token ~binding ~effect_kind in
  let%bind services =
    Script_tool_calls.current_native_services ()
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let check () =
    Script_tool_calls.check_current_metadata_admission
      services
      (A.invocation admission)
      binding
  in
  let%bind () = check () in
  let%bind service =
    Script_tool_calls.goal_metadata_service services
    |> Result.of_option
         ~error:
           (P.Error.create
              Method_not_found
              ~message:"Goal metadata service is unavailable"
              ~retryable:false
              ())
  in
  let%bind value = run service admission in
  let%map () = check () in
  value
;;

let registration effect_kind ~parameters ~description ~run =
  let module Definition = struct
    type input = Jsonaf.t

    let name = A.Effect.name effect_kind
    let type_ = "function"
    let description = Some description
    let parameters = parameters
    let input_of_string = decode_text_exn effect_kind
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun json ->
         let outcome =
           match
             with_service effect_kind (fun service admission ->
               run service admission json)
           with
           | Ok value -> P.Invocation.Complete value
           | Error error ->
             Fail
               { code = P.Error.code_to_string error.code
               ; message = error.message
               ; retryable = error.retryable
               ; details = error.data
               }
         in
         Openai.Responses.Tool_output.Output.Text
           (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest
          ("ochat.goal.metadata.native.v1:" ^ A.Effect.name effect_kind)
    ; result_contract = Invocation_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;

let staged effect_kind =
  `Object [ "staged", `True; "effect", `String (A.Effect.name effect_kind) ]
;;

let get =
  registration
    Get
    ~parameters:Schema.get
    ~description:
      "Read this actual session's bounded Goal metadata, budget and canonical aggregate \
       accounting. Required goal_id is null for all Goals, or a Goal ID to filter one \
       Goal. Returned generation, source and revisions are selector DATA; freeze them \
       with a new command key. Reading grants no execution permission."
    ~run:(fun service admission json ->
      let open Result.Let_syntax in
      let%bind goal_id = get_request json in
      Service.get service ~admission ~goal_id)
;;

let create =
  registration
    Create
    ~parameters:Schema.create
    ~description:
      "Stage a new held Goal in this callback transaction using explicit read \
       generation/source/catalog CAS and stable key. The result is local staged DATA \
       until callback ACK. It never starts or enables execution. Retry the identical \
       request."
    ~run:(fun service admission json ->
      let open Result.Let_syntax in
      let%bind request = P.Goal_native_create_request.of_json json in
      let%map () = Service.create_goal service ~admission ~request in
      staged Create)
;;

let control effect_kind kind =
  registration
    effect_kind
    ~parameters:(Schema.control kind)
    ~description:
      "Stage this Goal's explicit hold or abandonment using the exact read \
       generation/catalog/Goal CAS and stable key. Requires the matching selected native \
       effect. Passive obligations and already-admitted work retain their owners. This \
       is local staged DATA, not callback ACK or Run settlement."
    ~run:(fun service admission json ->
      let open Result.Let_syntax in
      let%bind request = P.Goal_native_control_request.of_json json in
      let matches =
        match effect_kind, P.Goal_native_control_request.mutation request with
        | A.Effect.Hold, Hold _ | Abandon, Abandon _ -> true
        | ( (Get | Create | Hold | Abandon | Objective | Progress | Resume)
          , (Hold _ | Abandon _) ) -> false
      in
      let%bind () =
        if matches
        then Ok ()
        else
          Error
            (P.Error.invalid_request
               "Goal control kind differs from selected native effect")
      in
      let%map () = Service.control service ~admission ~request in
      staged effect_kind)
;;

let hold = control Hold "hold"
let abandon = control Abandon "abandon"

let edit effect_kind kind =
  registration
    effect_kind
    ~parameters:(Schema.edit kind)
    ~description:
      "Stage an objective/progress edit in this callback transaction with explicit \
       frozen generation/source/catalog/Goal CAS and stable key. Requires its matching \
       native effect. Earlier selected metadata edits are visible to later Get; durable \
       acceptance requires callback ACK. No budget or execution authority changes."
    ~run:(fun service admission json ->
      let open Result.Let_syntax in
      let%bind request = P.Goal_edit.of_json json in
      let matches =
        match effect_kind, P.Goal_edit.mutation request with
        | A.Effect.Objective, Revise_objective _ | Progress, Record_progress _ -> true
        | ( (Get | Create | Hold | Abandon | Objective | Progress | Resume)
          , (Revise_objective _ | Record_progress _) ) -> false
      in
      let%bind () =
        if matches
        then Ok ()
        else
          Error
            (P.Error.invalid_request "Goal edit kind differs from selected native effect")
      in
      let%map () = Service.edit service ~admission ~request in
      staged effect_kind)
;;

let objective = edit Objective "objective"
let progress = edit Progress "progress"
