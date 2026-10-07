open! Core
module P = History_entry.Payload
module S = P.Semantic
module Item = Responses.Item
module Wire = Responses_wire

let wire_presence f = function
  | Wire.Presence.Absent -> P.Presence.Absent
  | Null -> Null
  | Value value -> Value (f value)
;;

let present_fields raw key =
  match Jsonaf.member key raw with
  | None -> P.Presence.Absent
  | Some `Null -> Null
  | Some (`String value) -> Value value
  | Some _ -> Absent
;;

let metadata raw =
  P.Metadata.
    { item_id = present_fields raw "id"
    ; response_id = present_fields raw "response_id"
    ; call_id = present_fields raw "call_id"
    ; status = present_fields raw "status"
    }
;;

let text text = P.Content.Text { text; annotations = []; logprobs = Absent }

let phase = function
  | None -> P.Presence.Absent
  | Some value -> Value value
;;

let role = function
  | Responses.Input_message.System -> P.Role.System
  | Developer -> Developer
  | User -> User
  | Assistant -> Assistant
;;

let output = function
  | Responses.Tool_output.Output.Text value -> P.Output.Text value
  | Content parts ->
    P.Output.Content
      (List.map parts ~f:(function
         | Responses.Tool_output.Output_part.Input_text { text = value } -> text value
         | Input_image { image_url; detail } ->
           P.Content.Image
             { uri = image_url
             ; detail =
                 Option.value_map detail ~default:P.Presence.Absent ~f:(fun detail ->
                   Value
                     (match detail with
                      | High -> "high"
                      | Low -> "low"
                      | Auto -> "auto"))
             }))
;;

let output_to_neutral = output

let semantic_of_item ?(call_relation = P.Call_relation.Unresolved) item =
  let raw = Item.jsonaf_of_t item in
  let view =
    match item with
    | Item.Input_message message ->
      S.Message
        { form = Input
        ; role = role message.role
        ; content =
            List.map message.content ~f:(function
              | Responses.Input_message.Text { text = value; _ } -> text value
              | Image { image_url; detail; _ } ->
                P.Content.Image { uri = image_url; detail = Value detail })
        ; phase = Absent
        }
    | Output_message message ->
      S.Message
        { form = Output
        ; role = Assistant
        ; content =
            List.map message.content ~f:(fun part ->
              P.Content.Text
                { text = part.text
                ; annotations =
                    List.map part.annotations ~f:Responses.Annotation.jsonaf_of_t
                ; logprobs = Absent
                })
        ; phase = phase message.phase
        }
    | Function_call call ->
      S.Call
        { kind = Function
        ; name = call.name
        ; namespace = Absent
        ; input_bytes = call.arguments
        ; async = Absent
        }
    | Custom_tool_call call ->
      S.Call
        { kind = Custom
        ; name = call.name
        ; namespace = Absent
        ; input_bytes = call.input
        ; async = Absent
        }
    | Function_call_output result ->
      S.Result
        { relation = call_relation; kind = Function; output = output result.output }
    | Custom_tool_call_output result ->
      S.Result { relation = call_relation; kind = Custom; output = output result.output }
    | Reasoning reasoning ->
      S.Reasoning
        { readable_summary = List.map reasoning.summary ~f:(fun part -> part.text) }
    | Web_search_call _ -> S.Unknown { provider_kind = "web_search_call" }
    | File_search_call _ -> S.Unknown { provider_kind = "file_search_call" }
  in
  S.create view ~metadata:(metadata raw)
;;

let of_item ?call_relation item =
  Result.bind (semantic_of_item ?call_relation item) ~f:(fun semantic ->
    P.reconstructed semantic ~provider:"openai.responses" ~raw:(Item.jsonaf_of_t item))
;;

let authored_output ~kind ~call_id ~call_relation ~output:host_output =
  let%map.Result semantic =
    S.create
      (Result { relation = call_relation; kind; output = output host_output })
      ~metadata:{ P.Metadata.empty with call_id = Value call_id }
  in
  P.authored semantic
;;

let wire_part part =
  match Wire.Part.view part with
  | Output_text { text; annotations; logprobs } ->
    P.Content.Text { text; annotations; logprobs = wire_presence Fn.id logprobs }
  | Refusal value -> P.Content.Refusal value
  | Summary_text value | Reasoning_text value -> text value
  | Unknown kind -> P.Content.Unknown { kind; raw = Wire.Part.raw part }
;;

let of_wire_item ?(call_relation = P.Call_relation.Unresolved) item =
  let open Result.Let_syntax in
  let view =
    match Wire.Item.view item with
    | Message { content; phase } ->
      S.Message
        { form = Output
        ; role = Assistant
        ; content = List.map content ~f:wire_part
        ; phase =
            wire_presence
              (function
                | Wire.Phase.Commentary -> "commentary"
                | Final_answer -> "final_answer"
                | Other value -> value)
              phase
        }
    | Call (Function { name; namespace; arguments; async; _ }) ->
      S.Call
        { kind = Function
        ; name
        ; namespace = wire_presence Fn.id namespace
        ; input_bytes = arguments
        ; async = wire_presence Fn.id async
        }
    | Call (Custom { name; namespace; input; async; _ }) ->
      S.Call
        { kind = Custom
        ; name
        ; namespace = wire_presence Fn.id namespace
        ; input_bytes = input
        ; async = wire_presence Fn.id async
        }
    | Reasoning { summary; _ } ->
      S.Reasoning
        { readable_summary =
            List.filter_map summary ~f:(fun part ->
              match Wire.Part.view part with
              | Summary_text value | Reasoning_text value -> Some value
              | Output_text _ | Refusal _ | Unknown _ -> None)
        }
    | Unknown provider_kind -> S.Unknown { provider_kind }
  in
  let%bind semantic = S.create view ~metadata:(metadata (Wire.Item.raw item)) in
  let source = Wire.Item.origin item in
  let%bind origin =
    P.Origin.create
      ~adapter:"openai.responses"
      ~provider:(Wire.Origin.provider source)
      ~account:(Wire.Origin.account source)
      ~endpoint:(Wire.Origin.endpoint source)
      ~profile:None
      ~model:None
      ~replay_version:1
  in
  let _ = call_relation in
  P.captured semantic ~origin ~raw:(Wire.Item.raw item)
;;

let string_presence = function
  | P.Presence.Value value -> Some value
  | Absent | Null -> None
;;

let annotation value =
  Result.try_with (fun () -> Responses.Annotation.t_of_jsonaf value)
  |> Result.map_error ~f:(fun _ -> "legacy runtime cannot project an unknown annotation")
;;

let lower_output = function
  | P.Output.Text value -> Ok (Responses.Tool_output.Output.Text value)
  | Content parts ->
    Result.map
      (Result.all
         (List.map parts ~f:(function
            | P.Content.Text { text; _ } ->
              Ok (Responses.Tool_output.Output_part.Input_text { text })
            | Image { uri; detail } ->
              let open Result.Let_syntax in
              let%map detail =
                match detail with
                | Absent | Null -> Ok None
                | Value "high" -> Ok (Some Responses.Input_message.High)
                | Value "low" -> Ok (Some Low)
                | Value "auto" -> Ok (Some Auto)
                | Value _ -> Error "legacy runtime cannot project an unknown image detail"
              in
              Responses.Tool_output.Output_part.Input_image { image_url = uri; detail }
            | Refusal _ | Unknown _ ->
              Error "legacy runtime cannot project this output content")))
      ~f:(fun parts -> Responses.Tool_output.Output.Content parts)
;;

let lower_semantic semantic =
  let open Result.Let_syntax in
  let metadata = S.metadata semantic in
  let item_id = string_presence metadata.item_id in
  let status = string_presence metadata.status in
  let provider_call_id () =
    match string_presence metadata.call_id with
    | Some value -> Ok value
    | None -> Error "legacy runtime needs provider call metadata"
  in
  match S.view semantic with
  | Message { form = Input; role; content; _ } ->
    let%bind role =
      match role with
      | P.Role.System -> Ok Responses.Input_message.System
      | Developer -> Ok Developer
      | User -> Ok User
      | Assistant -> Ok Assistant
      | Tool -> Error "legacy runtime cannot project a tool-role input message"
    in
    let%map content =
      Result.all
        (List.map content ~f:(function
           | P.Content.Text { text; _ } ->
             Ok (Responses.Input_message.Text { text; _type = "input_text" })
           | Image { uri; detail } ->
             Ok
               (Responses.Input_message.Image
                  { image_url = uri
                  ; detail = Option.value (string_presence detail) ~default:"auto"
                  ; _type = "input_image"
                  })
           | Refusal _ | Unknown _ ->
             Error "legacy runtime cannot project this input content"))
    in
    Item.Input_message { role; content; _type = "message" }
  | Message { form = Output; role = Assistant; content; phase } ->
    let%map content =
      Result.all
        (List.map content ~f:(function
           | P.Content.Text { text; annotations; _ } ->
             Result.map
               (Result.all (List.map annotations ~f:annotation))
               ~f:(fun annotations ->
                 Responses.Output_message.{ annotations; text; _type = "output_text" })
           | Refusal _ | Image _ | Unknown _ ->
             Error "legacy runtime cannot project this assistant content"))
    in
    Item.Output_message
      { role = Assistant
      ; id = Option.value item_id ~default:""
      ; content
      ; status = Option.value status ~default:"completed"
      ; phase = string_presence phase
      ; _type = "message"
      }
  | Message { form = Output; _ } -> Error "legacy output role is not assistant"
  | Call { kind; name; namespace; input_bytes; async } ->
    let%bind () =
      match namespace, async with
      | P.Presence.Absent, (P.Presence.Absent | Null | Value false) -> Ok ()
      | _ -> Error "legacy runtime cannot project namespaced or asynchronous calls"
    in
    let%map call_id = provider_call_id () in
    (match kind with
     | P.Call_kind.Function ->
       Item.Function_call
         { name
         ; arguments = input_bytes
         ; call_id
         ; _type = "function_call"
         ; id = item_id
         ; status
         }
     | Custom ->
       Item.Custom_tool_call
         { name; input = input_bytes; call_id; _type = "custom_tool_call"; id = item_id })
  | Result { kind; output; _ } ->
    let%bind call_id = provider_call_id () in
    let%map output = lower_output output in
    (match kind with
     | P.Call_kind.Function ->
       Item.Function_call_output
         { output; call_id; _type = "function_call_output"; id = item_id; status }
     | Custom ->
       Item.Custom_tool_call_output
         { output; call_id; _type = "custom_tool_call_output"; id = item_id })
  | Reasoning { readable_summary } ->
    Ok
      (Item.Reasoning
         { summary =
             List.map readable_summary ~f:(fun text ->
               Responses.Reasoning.{ text; _type = "summary_text" })
         ; _type = "reasoning"
         ; id = Option.value item_id ~default:""
         ; status
         })
  | Unknown _ -> Error "legacy runtime cannot project an unknown provider item"
;;

let to_item payload =
  match P.representation payload with
  | Authored -> lower_semantic (P.semantic payload)
  | Captured _ -> Error "captured provider history requires the neutral runtime adapter"
  | Reconstructed { provider; raw } ->
    if not (String.equal provider "openai.responses")
    then Error "legacy runtime cannot project a different provider representation"
    else
      let open Result.Let_syntax in
      let%bind item =
        Result.try_with (fun () -> Item.t_of_jsonaf raw)
        |> Result.map_error ~f:(fun _ -> "invalid reconstructed legacy Responses item")
      in
      let expected = P.semantic payload in
      let call_relation =
        match S.view expected with
        | Result { relation; _ } -> relation
        | Message _ | Call _ | Reasoning _ | Unknown _ -> P.Call_relation.Unresolved
      in
      let%bind actual = semantic_of_item ~call_relation item in
      if
        Jsonaf.exactly_equal
          (P.to_json (P.authored expected))
          (P.to_json (P.authored actual))
      then Ok item
      else Error "reconstructed provider representation differs from neutral semantics"
;;

let to_presentation_item payload =
  match P.representation payload with
  | Captured _ -> lower_semantic (P.semantic payload)
  | Authored | Reconstructed _ -> to_item payload
;;

let create ~allocator item =
  Result.bind (of_item item) ~f:(History_entry.create ~allocator)
;;

let create_with_id_exn ?call_relation ~id item =
  of_item ?call_relation item |> Result.ok_or_failwith |> History_entry.create_with_id ~id
;;

let item_exn entry = to_item (History_entry.payload entry) |> Result.ok_or_failwith
let items_exn entries = List.map entries ~f:item_exn

let with_item_exn entry item =
  let previous = S.view (P.semantic (History_entry.payload entry)) in
  let call_relation =
    match previous, item with
    | Result { kind = Function; relation; _ }, Item.Function_call_output _
    | Result { kind = Custom; relation; _ }, Custom_tool_call_output _ -> relation
    | ( (Message _ | Call _ | Result _ | Reasoning _ | Unknown _)
      , ( Item.Input_message _
        | Output_message _
        | Function_call _
        | Custom_tool_call _
        | Function_call_output _
        | Custom_tool_call_output _
        | Reasoning _
        | Web_search_call _
        | File_search_call _ ) ) -> P.Call_relation.Unresolved
  in
  let semantic = semantic_of_item ~call_relation item |> Result.ok_or_failwith in
  History_entry.with_payload entry (P.authored semantic)
;;

let relation_for_item ~history item =
  let kind_and_id =
    match item with
    | Item.Function_call_output output -> Some (P.Call_kind.Function, output.call_id)
    | Custom_tool_call_output output -> Some (P.Call_kind.Custom, output.call_id)
    | Input_message _
    | Output_message _
    | Function_call _
    | Custom_tool_call _
    | Reasoning _
    | Web_search_call _
    | File_search_call _ -> None
  in
  match kind_and_id with
  | None -> P.Call_relation.Unresolved
  | Some (kind, provider_id) ->
    let nearest =
      List.find_map (List.rev history) ~f:(fun entry ->
        let candidate = P.semantic (History_entry.payload entry) in
        match S.view candidate, (S.metadata candidate).call_id with
        | (Call { kind = other; _ } | Result { kind = other; _ }), Value actual
          when P.Call_kind.equal kind other && String.equal provider_id actual ->
          Some (entry, candidate)
        | ( (Message _ | Call _ | Result _ | Reasoning _ | Unknown _)
          , (Absent | Null | Value _) ) -> None)
    in
    (match nearest with
     | Some (entry, candidate) ->
       (match S.view candidate with
        | Call _ -> P.Call_relation.Bound (History_entry.id entry)
        | Message _ | Result _ | Reasoning _ | Unknown _ -> Unresolved)
     | None -> Unresolved)
;;

let of_items ~preceding ~allocator items =
  let open Result.Let_syntax in
  let key kind provider_id =
    (match kind with
     | P.Call_kind.Function -> "function:"
     | Custom -> "custom:")
    ^ provider_id
  in
  let update nearest entry =
    let semantic = P.semantic (History_entry.payload entry) in
    match S.view semantic, (S.metadata semantic).call_id with
    | Call { kind; _ }, Value provider_id ->
      Map.set nearest ~key:(key kind provider_id) ~data:(Some (History_entry.id entry))
    | Result { kind; _ }, Value provider_id ->
      Map.set nearest ~key:(key kind provider_id) ~data:None
    | (Message _ | Call _ | Result _ | Reasoning _ | Unknown _), (Absent | Null | Value _)
      -> nearest
  in
  let nearest = List.fold preceding ~init:String.Map.empty ~f:update in
  let%map _, reversed =
    List.fold_result items ~init:(nearest, []) ~f:(fun (nearest, acc) item ->
      let%bind payload = of_item item in
      let semantic = P.semantic payload in
      let relation =
        match S.view semantic, (S.metadata semantic).call_id with
        | Result { kind; _ }, Value provider_id ->
          (match Map.find nearest (key kind provider_id) with
           | Some (Some id) -> P.Call_relation.Bound id
           | Some None | None -> Unresolved)
        | ( (Message _ | Call _ | Result _ | Reasoning _ | Unknown _)
          , (Absent | Null | Value _) ) -> P.Call_relation.Unresolved
      in
      let%bind payload = of_item ~call_relation:relation item in
      let%map entry = History_entry.create ~allocator payload in
      update nearest entry, entry :: acc)
  in
  List.rev reversed
;;
