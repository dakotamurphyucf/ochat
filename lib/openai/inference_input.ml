open! Core
module R = Inference.Request
module P = History_entry.Payload
module D = Responses_driver
module E = Inference_runtime.Preparation_error
module Field = Responses_request.Field

let invalid = Error E.Unsupported_input
let unsupported result = Result.map_error result ~f:(fun _ -> E.Unsupported_input)

let presence f : _ P.Presence.t -> _ Field.t = function
  | Absent -> Absent
  | Null -> Null
  | Value x -> Value (f x)
;;

let fields name f : _ P.Presence.t -> (string * Jsonaf.t) list = function
  | Absent -> []
  | Null -> [ name, `Null ]
  | Value x -> [ name, f x ]
;;

let origin target =
  P.Origin.create
    ~adapter:(R.Target.adapter target)
    ~provider:(R.Target.profile target)
    ~account:(R.Target.account target)
    ~endpoint:(R.Target.endpoint target)
    ~profile:(Some (R.Target.profile target))
    ~model:(Some (R.Target.model target))
    ~replay_version:1
  |> Result.map_error ~f:(fun _ -> E.Invalid_preparation)
;;

let wire_origin target =
  Responses_wire.Origin.create
    ~provider:(R.Target.profile target)
    ~account:(R.Target.account target)
    ~endpoint:(R.Target.endpoint target)
  |> Result.map_error ~f:(fun _ -> E.Invalid_preparation)
;;

let string s = `String s

let role : P.Role.t -> string = function
  | System -> "system"
  | Developer -> "developer"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let inline_asset assets uri =
  match Map.find assets uri with
  | Some (asset, encoded) ->
    (match R.Asset.kind asset with
     | Image -> Ok (Lazy.force encoded)
     | Document _ -> Error E.Asset_unavailable)
  | None ->
    (* Inline bytes are already immutable and bounded by Request admission;
       the selected request codec validates media type and base64 syntax. *)
    if String.is_prefix uri ~prefix:"data:" then Ok uri else Error E.Asset_unavailable
;;

let content assets ~output = function
  | P.Content.Text { text; annotations; logprobs } ->
    if output
    then
      Ok
        (`Object
            ([ "type", string "output_text"
             ; "text", string text
             ; "annotations", `Array annotations
             ]
             @ fields "logprobs" Fn.id logprobs))
    else if
      List.is_empty annotations
      && P.Presence.equal Document_schema.Json.equal logprobs Absent
    then Ok (`Object [ "type", string "input_text"; "text", string text ])
    else invalid
  | Refusal text ->
    if output
    then Ok (`Object [ "type", string "refusal"; "refusal", string text ])
    else invalid
  | Image { uri; detail } ->
    let open Result.Let_syntax in
    let%map uri = inline_asset assets uri in
    `Object
      ([ "type", string "input_image"; "image_url", string uri ]
       @ fields "detail" string detail)
  | Unknown { kind; raw } ->
    (* Explicit authored file data is admitted by the selected codec. External
       references may only name an already resolved immutable host asset. *)
    if not (String.equal kind "input_file")
    then invalid
    else (
      match raw with
      | `Object original ->
        let open Result.Let_syntax in
        let%bind () =
          match Document_schema.Json.field raw ~name:"type" with
          | Value (`String "input_file") -> Ok ()
          | _ -> invalid
        in
        (match List.Assoc.find original ~equal:String.equal "file_url" with
         | Some (`String reference) ->
           let open Result.Let_syntax in
           let%bind () =
             match
               ( Document_schema.Json.field raw ~name:"type"
               , Document_schema.Json.field raw ~name:"file_data" )
             with
             | Value (`String "input_file"), (Absent | Null) -> Ok ()
             | _ -> invalid
           in
           (match Map.find assets reference with
            | Some (asset, encoded) ->
              (match R.Asset.kind asset with
               | Image -> Error E.Asset_unavailable
               | Document { filename } ->
                 let rest =
                   List.filter original ~f:(fun (key, _) ->
                     not (List.mem [ "file_url"; "file_data" ] key ~equal:String.equal))
                 in
                 let rest =
                   if List.Assoc.mem rest ~equal:String.equal "filename"
                   then rest
                   else
                     Option.value_map filename ~default:rest ~f:(fun name ->
                       rest @ [ "filename", string name ])
                 in
                 Ok (`Object (rest @ [ "file_data", string (Lazy.force encoded) ])))
            | None -> Error E.Asset_unavailable)
         | Some (`Null | `True | `False | `Number _ | `Object _ | `Array _) -> invalid
         | None -> Ok raw)
      | `Null | `True | `False | `Number _ | `String _ | `Array _ -> invalid)
;;

let semantic assets semantic =
  let open Result.Let_syntax in
  let metadata = P.Semantic.metadata semantic in
  let ids =
    fields "id" string metadata.item_id @ fields "status" string metadata.status
  in
  match P.Semantic.view semantic with
  | Message { form; role = message_role; content = parts; phase } ->
    let output = P.Semantic.equal_message_form form Output in
    let%map parts = Result.all (List.map parts ~f:(content assets ~output)) in
    `Object
      ([ "type", string "message"
       ; "role", string (role message_role)
       ; "content", `Array parts
       ]
       @ ids
       @ fields "phase" string phase)
  | Call { kind; name; namespace; input_bytes; async } ->
    let type_, input =
      match kind with
      | P.Call_kind.Function -> "function_call", "arguments"
      | Custom -> "custom_tool_call", "input"
    in
    Ok
      (`Object
          ([ "type", string type_; "name", string name; input, string input_bytes ]
           @ ids
           @ fields "call_id" string metadata.call_id
           @ fields "namespace" string namespace
           @ fields "async" (fun b -> if b then `True else `False) async))
  | Result { kind; output; relation = _ } ->
    let%map output =
      match output with
      | P.Output.Text text -> Ok (string text)
      | Content parts ->
        Result.all (List.map parts ~f:(content assets ~output:false))
        |> Result.map ~f:(fun parts -> `Array parts)
    in
    let type_ =
      match kind with
      | P.Call_kind.Function -> "function_call_output"
      | Custom -> "custom_tool_call_output"
    in
    `Object
      ([ "type", string type_; "output", output ]
       @ fields "call_id" string metadata.call_id
       @ ids)
  | Reasoning { readable_summary } ->
    Ok
      (`Object
          ([ "type", string "reasoning"
           ; ( "summary"
             , `Array
                 (List.map readable_summary ~f:(fun text ->
                    `Object [ "type", string "summary_text"; "text", string text ])) )
           ]
           @ ids))
  | Unknown _ -> invalid
;;

let history request =
  let open Result.Let_syntax in
  let%bind expected_origin = origin (R.target request) in
  let%bind wire_origin = wire_origin (R.target request) in
  let assets =
    List.fold (R.assets request) ~init:String.Map.empty ~f:(fun assets asset ->
      let encoded =
        lazy
          ("data:"
           ^ R.Asset.media_type asset
           ^ ";base64,"
           ^ Base64.encode_string (R.Asset.bytes asset))
      in
      Map.set assets ~key:(R.Asset.reference asset) ~data:(asset, encoded))
  in
  List.map (R.history request) ~f:(fun entry ->
    let payload = History_entry.payload entry in
    match P.representation payload with
    | Authored -> semantic assets (P.semantic payload)
    | Reconstructed _ ->
      (* This explicitly marked legacy representation is verified by its existing
         boundary; it is never relabelled as an actual wire capture. *)
      Responses_history.to_item payload
      |> unsupported
      |> Result.map ~f:Responses.Item.jsonaf_of_t
    | Captured { origin = actual_origin; raw } ->
      if
        not
          (Document_schema.Json.equal
             (P.Origin.to_json actual_origin)
             (P.Origin.to_json expected_origin))
      then Error E.Incompatible_replay
      else (
        let%bind wire =
          Responses_wire.Item.decode raw ~origin:wire_origin
          |> Result.map_error ~f:(fun _ -> E.Incompatible_replay)
        in
        let%bind projected =
          Responses_history.of_wire_item wire
          |> Result.map_error ~f:(fun _ -> E.Incompatible_replay)
        in
        if
          Document_schema.Json.equal
            (P.to_json (P.authored (P.semantic payload)))
            (P.to_json (P.authored (P.semantic projected)))
        then Ok raw
        else Error E.Incompatible_replay))
  |> Result.all
;;

let tool spec =
  match R.Tool_spec.view spec with
  | Function { parameters; strict } ->
    Responses_request.Tool.function_
      ~name:(R.Tool_spec.name spec)
      ~parameters:(presence Fn.id parameters)
      ~strict:(presence Fn.id strict)
      ~description:(presence Fn.id (R.Tool_spec.description spec))
      ~output_schema:(presence Fn.id (R.Tool_spec.output_schema spec))
      ()
    |> unsupported
  | Custom { format } ->
    if
      not
        (P.Presence.equal
           Document_schema.Json.equal
           (R.Tool_spec.output_schema spec)
           Absent)
    then invalid
    else
      Responses_request.Tool.custom
        ~name:(R.Tool_spec.name spec)
        ~description:(presence Fn.id (R.Tool_spec.description spec))
        ~format:
          (presence
             (function
               | R.Tool_spec.Custom_format.Text ->
                 Responses_request.Tool.Custom_format.Text
               | Grammar { syntax; definition } -> Grammar { syntax; definition })
             format)
        ()
      |> unsupported
;;

let setting setting =
  let provenance =
    match R.Setting.provenance setting with
    | Execution_override -> D.Setting.Execution_override
    | Captured_prompt -> Captured_prompt
    | Profile_default -> Profile_default
  in
  D.Setting.create
    ~name:(R.Setting.name setting)
    ~value:(presence Fn.id (R.Setting.value setting))
    ~provenance
  |> Result.map_error ~f:(fun _ -> E.Unsupported_setting)
;;

let prepare profile request =
  let open Result.Let_syntax in
  let%bind () =
    if
      R.encoded_bytes request
      > Document_schema.Limits.max_bytes Document_schema.Limits.default
    then
      Error
        (E.Request_limit (Document_schema.Error.Limit_exceeded "neutral request bytes"))
    else Ok ()
  in
  let has_capture =
    List.exists (R.history request) ~f:(fun entry ->
      match P.representation (History_entry.payload entry) with
      | Captured _ -> true
      | Authored | Reconstructed _ -> false)
  in
  let%bind () =
    if
      has_capture
      && not
           (D.Capability.equal_support
              (D.Profile.capability
                 profile
                 ~model:(R.Target.model (R.target request))
                 ~feature:Opaque_replay)
              Supported)
    then Error E.Incompatible_replay
    else Ok ()
  in
  let%bind history = history request in
  let%bind tools = Result.all (List.map (R.tools request) ~f:tool) in
  let%bind settings =
    Result.all (List.map (R.Target.settings (R.target request)) ~f:setting)
  in
  D.Prepared.of_captured_settings
    profile
    ~model:(R.Target.model (R.target request))
    ~history
    ~tools
    ~settings
  |> unsupported
;;
