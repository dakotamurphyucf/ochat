open! Core
module R = Inference.Request
module O = Inference.Observation
module D = Responses_driver
module E = Inference_runtime.Preparation_error
module P = History_entry.Payload.Presence

let invalid result = Result.map_error result ~f:(fun _ -> E.Invalid_preparation)

let capture_target profile ~profile_revision ~model ~settings ~limits =
  let open Result.Let_syntax in
  let%bind effective =
    D.Profile.effective_settings profile settings
    |> Result.map_error ~f:(fun _ -> E.Unsupported_setting)
  in
  let%bind settings =
    List.map effective ~f:(fun setting ->
      let value =
        match D.Setting.value setting with
        | Absent -> P.Absent
        | Null -> Null
        | Value value -> Value value
      in
      let provenance =
        match D.Setting.provenance setting with
        | Execution_override -> R.Setting.Execution_override
        | Captured_prompt -> Captured_prompt
        | Profile_default -> Profile_default
      in
      R.Setting.create ~name:(D.Setting.name setting) ~value ~provenance ~limits
      |> Result.map_error ~f:(fun error -> E.Invalid_request error))
    |> Result.all
  in
  R.Target.create
    ~adapter:"openai.responses"
    ~profile:(D.Profile.id profile)
    ~profile_revision
    ~account:(D.Profile.account profile)
    ~endpoint:(D.Profile.endpoint profile)
    ~model
    ~settings
    ~limits
  |> Result.map_error ~f:(fun error -> E.Invalid_request error)
;;

let optional value =
  Option.value_map value ~default:P.Absent ~f:(fun value -> P.Value value)
;;

let tool_spec tool ~limits =
  let create ~name ~description ~view =
    R.Tool_spec.create
      ~name
      ~description:(optional description)
      ~output_schema:Absent
      ~view
      ~limits
    |> Result.map_error ~f:(fun error -> E.Invalid_request error)
  in
  match tool with
  | Responses.Request.Tool.File_search _ | Web_search _ -> Error E.Unsupported_input
  | Function { name; description; parameters; strict; type_ = _ } ->
    create
      ~name
      ~description
      ~view:
        (Function
           { parameters =
               (match parameters with
                | `Null -> Null
                | value -> Value value)
           ; strict = Value strict
           })
  | Custom_function { name; description; format; type_ = _ } ->
    let open Result.Let_syntax in
    let%bind () =
      Document_schema.Json.validate ~limits format
      |> Result.map_error ~f:(fun _ -> E.Unsupported_input)
    in
    let%bind _ =
      Responses_request.Tool.of_jsonaf
        (`Object [ "type", `String "custom"; "name", `String name; "format", format ])
      |> Result.map_error ~f:(fun _ -> E.Unsupported_input)
    in
    let%bind format =
      match format with
      | `Object fields ->
        (match List.Assoc.find fields ~equal:String.equal "type" with
         | Some (`String "text") -> Ok R.Tool_spec.Custom_format.Text
         | Some (`String "grammar") ->
           (match
              ( List.Assoc.find fields ~equal:String.equal "syntax"
              , List.Assoc.find fields ~equal:String.equal "definition" )
            with
            | Some (`String ("lark" as syntax)), Some (`String definition)
            | Some (`String ("regex" as syntax)), Some (`String definition) ->
              Ok
                (Grammar
                   { syntax = (if String.equal syntax "lark" then `Lark else `Regex)
                   ; definition
                   })
            | _ -> Error E.Unsupported_input)
         | _ -> Error E.Unsupported_input)
      | _ -> Error E.Unsupported_input
    in
    create ~name ~description ~view:(Custom { format = Value format })
;;

let capabilities driver_profile selected_model =
  let open O.Configuration in
  let declarations : (feature * D.Capability.feature) list =
    [ Text_input, Text_input
    ; Image_input, Image_input
    ; Document_input, Document_input
    ; Function_tools, Function_tools
    ; Custom_tools, Custom_tools
    ; Opaque_replay, Opaque_replay
    ; Setting Instructions, Setting "instructions"
    ; Setting Max_output_tokens, Setting "max_output_tokens"
    ; Setting Parallel_tool_calls, Setting "parallel_tool_calls"
    ; Setting Temperature, Setting "temperature"
    ; Setting Top_p, Setting "top_p"
    ; Setting Reasoning_effort, Setting "reasoning"
    ; Setting Reasoning_summary, Setting "reasoning"
    ; Setting Text_verbosity, Setting "text"
    ; Setting Text_format, Setting "text"
    ; Setting Tool_choice, Setting "tool_choice"
    ; Setting Prompt_cache_key, Setting "prompt_cache_key"
    ; Setting Prompt_cache_retention, Setting "prompt_cache_retention"
    ]
  in
  List.map declarations ~f:(fun (feature, driver_feature) ->
    let support =
      match
        D.Profile.capability driver_profile ~model:selected_model ~feature:driver_feature
      with
      | Supported -> Supported
      | Unsupported -> Unsupported
      | Unknown -> Unknown
    in
    feature, support)
;;

let create ?(auth_binding = P.Absent) driver ~profile ~profile_revision ~auth ~limits =
  let bind target =
    if
      String.equal (R.Target.adapter target) "openai.responses"
      && History_entry.Payload.Presence.equal
           R.Auth_binding.equal
           (R.Target.auth_binding target)
           auth_binding
      && String.equal (R.Target.profile target) (D.Profile.id profile)
      && Option.equal String.equal (R.Target.profile_revision target) profile_revision
      && Option.equal String.equal (R.Target.account target) (D.Profile.account profile)
      && String.equal (R.Target.endpoint target) (D.Profile.endpoint profile)
    then Ok ()
    else Error E.Target_mismatch
  in
  Inference_runtime.Adapter.create
    ~id:"openai.responses"
    ~limits
    ~bind
    ~prepare:(fun ~preparation_id request ->
      let open Result.Let_syntax in
      let%bind () = bind (R.target request) in
      let%bind prepared = Inference_input.prepare profile request in
      let%bind configuration =
        O.Configuration.of_target
          (R.target request)
          ~preparation_id
          ~transport:Http_sse
          ~capabilities:(capabilities profile (R.Target.model (R.target request)))
          ~limits:O.Admission.observation
        |> invalid
      in
      Inference_runtime.Plan.create
        ~request
        ~configuration
        ~fingerprint:(D.Prepared.fingerprint prepared)
        ~run:(fun ~sw ~scope ~accounting_id ~note_delivery ~on_event ~on_observation:_ ->
          Eio.Switch.check sw;
          let projector =
            Inference_output.create
              ~target:(R.target request)
              ~scope
              ~accounting_id
              ~limits
            |> function
            | Ok value -> value
            | Error _ ->
              raise (Inference_runtime.Contract_violation Configuration_mismatch)
          in
          let result =
            D.run driver ~auth ~prepared ~on_event:(fun event ->
              (match event with
               | D.Event.Update _ | Finalized _ -> note_delivery Response_started
               | Terminal (Provider _) -> note_delivery Response_started
               | Terminal (Failed _) -> ());
              Inference_output.event projector event ~on_event)
          in
          Inference_output.finish projector result))
;;
