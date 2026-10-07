open! Core
module T = Transcript
module P = History_entry.Payload
module R = Responses.Response_stream

module Host_id = struct
  module T = struct
    type t = History_entry.Id.t

    let compare = History_entry.Id.compare
    let sexp_of_t = History_entry.Id.sexp_of_t
  end

  include T
  include Comparator.Make (T)
end

type t =
  { scope : T.Scope.t
  ; limits : Document_schema.Limits.t
  ; items : T.Item.t Map.M(T.Item_id).t
  ; item_bytes : int Map.M(T.Item_id).t
  ; retained_bytes : int
  ; hosts : T.Item_id.t Map.M(Host_id).t
  ; finalized_items : Set.M(T.Item_id).t
  ; started : bool
  ; finished : bool
  ; completion : T.Stream.completion option
  }

let create ~scope ~limits =
  { scope
  ; limits
  ; items = Map.empty (module T.Item_id)
  ; item_bytes = Map.empty (module T.Item_id)
  ; retained_bytes = 0
  ; hosts = Map.empty (module Host_id)
  ; finalized_items = Set.empty (module T.Item_id)
  ; started = false
  ; finished = false
  ; completion = None
  }
;;

let completion t = t.completion

let events t views =
  List.map views ~f:(fun view -> T.Stream.create view ~limits:t.limits) |> Result.all
;;

let started t =
  if t.started
  then Ok (t, [])
  else
    Result.map
      (events t [ Source_started { scope = t.scope; origin = P.Origin.unavailable } ])
      ~f:(fun events -> { t with started = true }, events)
;;

let start = started

let item t ?(finalized = false) ~alias ~entry_id ~header ~call_name () =
  let open Result.Let_syntax in
  let%bind supplied_id = T.Item_id.of_string alias in
  let id =
    Option.bind entry_id ~f:(Map.find t.hosts) |> Option.value ~default:supplied_id
  in
  let%bind () =
    match Map.find t.items supplied_id, entry_id with
    | Some existing, Some host ->
      if
        Option.exists existing.entry_id ~f:(fun existing ->
          not (History_entry.Id.equal existing host))
      then Error "provider alias changed its actual host identity"
      else Ok ()
    | None, _ | Some _, None -> Ok ()
  in
  let%bind proposed = T.Item.create ~scope:t.scope ~id ~entry_id ~header ~call_name in
  let%bind descriptor =
    match Map.find t.items id with
    | None -> Ok proposed
    | Some previous ->
      if finalized
      then (
        match previous.entry_id, proposed.entry_id with
        | Some before, Some after when not (History_entry.Id.equal before after) ->
          Error "conflicting final host identity"
        | None, _ | Some _, _ -> Ok proposed)
      else T.Item.refine previous proposed
  in
  let%bind hosts =
    match descriptor.entry_id with
    | None -> Ok t.hosts
    | Some host ->
      (match Map.find t.hosts host with
       | Some previous when not (T.Item_id.equal previous id) ->
         Error "host entry correlated with conflicting live items"
       | None | Some _ -> Ok (Map.set t.hosts ~key:host ~data:id))
  in
  let%bind admitted = T.Stream.create (Item_announced descriptor) ~limits:t.limits in
  let bytes = T.Stream.encoded_bytes admitted in
  let previous_bytes = Option.value (Map.find t.item_bytes id) ~default:0 in
  let remaining = t.retained_bytes - previous_bytes in
  let cap = Document_schema.Limits.max_bytes t.limits in
  if bytes > cap || remaining > cap - bytes
  then Error "provider live identity budget exceeded"
  else
    Ok
      ( { t with
          items = Map.set t.items ~key:id ~data:descriptor
        ; item_bytes = Map.set t.item_bytes ~key:id ~data:bytes
        ; retained_bytes = remaining + bytes
        ; hosts
        }
      , descriptor )
;;

let part item ~index ~kind =
  let kind_name =
    match kind with
    | T.Part.Text -> "text"
    | Refusal -> "refusal"
    | Reasoning_summary -> "reasoning_summary"
    | Reasoning_text -> "reasoning_text"
    | Image -> "image"
    | Unknown name -> "unknown:" ^ name
  in
  let open Result.Let_syntax in
  let%bind id = T.Part_id.of_string (kind_name ^ ":" ^ Int.to_string index) in
  T.Part.create ~item ~id ~index:(Some index) ~kind
;;

let content_change t ~alias ~entry_id ~index ~kind ~change =
  let open Result.Let_syntax in
  let%bind t, item = item t ~alias ~entry_id ~header:None ~call_name:None () in
  let%bind part = part item ~index ~kind in
  let%map observations = events t [ Changed { target = Content part; change } ] in
  t, observations
;;

let call_change t ~alias ~entry_id ~kind ~change =
  let open Result.Let_syntax in
  let%bind t, item =
    item t ~alias ~entry_id ~header:(Some (Call kind)) ~call_name:None ()
  in
  let%map observations = events t [ Changed { target = Call_input item; change } ] in
  t, observations
;;

let legacy_item = function
  | R.Item.Input_message value -> Responses.Item.Input_message value
  | Output_message value -> Output_message value
  | Function_call value -> Function_call value
  | Custom_function value -> Responses.Item.Custom_tool_call value
  | Reasoning value -> Reasoning value
;;

let semantic_name semantic =
  match P.Semantic.view semantic with
  | Call { name; _ } -> Some name
  | Message _ | Result _ | Reasoning _ | Unknown _ -> None
;;

let alias_of_semantic semantic ~output_index =
  match (P.Semantic.metadata semantic).item_id with
  | P.Presence.Value alias -> Ok alias
  | Absent | Null ->
    (match (P.Semantic.metadata semantic).call_id with
     | Value alias -> Ok alias
     | Absent | Null ->
       if output_index < 0
       then Error "negative provider output index"
       else Ok ("output-index:" ^ Int.to_string output_index))
;;

let semantic_parts descriptor semantic =
  let open Result.Let_syntax in
  let content index content =
    let kind, text =
      match content with
      | P.Content.Text { text; _ } -> T.Part.Text, Some text
      | Refusal text -> Refusal, Some text
      | Image _ -> Image, None
      | Unknown { kind; _ } -> Unknown kind, None
    in
    let%map part = part descriptor ~index ~kind in
    T.Stream.Part_announced part
    ::
    (match text with
     | None -> []
     | Some text -> [ Changed { target = Content part; change = Replace text } ])
  in
  match P.Semantic.view semantic with
  | Message { content = contents; _ } ->
    List.mapi contents ~f:content |> Result.all |> Result.map ~f:List.concat
  | Call { input_bytes; _ } ->
    Ok [ Changed { target = Call_input descriptor; change = Replace input_bytes } ]
  | Reasoning { readable_summary } ->
    List.mapi readable_summary ~f:(fun index text ->
      let%map part = part descriptor ~index ~kind:Reasoning_summary in
      [ T.Stream.Part_announced part
      ; Changed { target = Content part; change = Replace text }
      ])
    |> Result.all
    |> Result.map ~f:List.concat
  | Result _ | Unknown _ -> Ok []
;;

let observe_item t ~entry_id ~output_index ~done_ value =
  let open Result.Let_syntax in
  let%bind payload = Responses_history.of_item (legacy_item value) in
  let semantic = P.semantic payload in
  let%bind alias = alias_of_semantic semantic ~output_index in
  let%bind t, descriptor =
    item
      t
      ~alias
      ~entry_id
      ~header:(Some (T.Header.of_semantic semantic))
      ~call_name:(semantic_name semantic)
      ()
  in
  let%bind parts = if done_ then semantic_parts descriptor semantic else Ok [] in
  let%map observations = events t (Item_announced descriptor :: parts) in
  t, observations
;;

let content_part t ~entry_id ~alias ~index ~done_ value =
  let open Result.Let_syntax in
  let%bind t, item = item t ~alias ~entry_id ~header:None ~call_name:None () in
  let kind, text =
    match value with
    | R.Part.Output_text value -> T.Part.Text, value.text
    | Refusal value -> Refusal, value.refusal
  in
  let%bind part = part item ~index ~kind in
  let views =
    T.Stream.Part_announced part
    ::
    (if done_ || not (String.is_empty text)
     then [ Changed { target = Content part; change = Replace text } ]
     else [])
  in
  let%map observations = events t views in
  t, observations
;;

let opaque t event =
  let raw = R.jsonaf_of_t event in
  let provider_kind =
    match Jsonaf.member "type" raw with
    | Some (`String value) when not (String.is_empty value) -> value
    | None | Some _ -> "unclassified_legacy_observation"
  in
  Result.map
    (events t [ Unknown_event { scope = t.scope; provider_kind; raw } ])
    ~f:(fun events -> t, events)
;;

let observed_alias = function
  | R.Output_item_added { item; output_index; _ }
  | Output_item_done { item; output_index; _ } ->
    Some
      (match item with
       | R.Item.Output_message value -> value.id
       | Reasoning value -> value.id
       | Function_call value -> Option.value value.id ~default:value.call_id
       | Custom_function value -> Option.value value.id ~default:value.call_id
       | Input_message _ -> "output-index:" ^ Int.to_string output_index)
  | Output_text_delta { item_id; _ }
  | Output_text_done { item_id; _ }
  | Response_refusal_delta { item_id; _ }
  | Response_refusal_done { item_id; _ }
  | Reasoning_summary_text_delta { item_id; _ }
  | Function_call_arguments_delta { item_id; _ }
  | Function_call_arguments_done { item_id; _ }
  | Custom_tool_call_input_delta { item_id; _ }
  | Custom_tool_call_input_done { item_id; _ }
  | Content_part_added { item_id; _ }
  | Content_part_done { item_id; _ } -> Some item_id
  | Annotation_added _
  | Response_created _
  | Response_in_progress _
  | Response_completed _
  | Response_incomplete _
  | Response_failed _
  | Error _
  | Unknown _
  | File_search_call_in_progress _
  | File_search_call_searching _
  | File_search_call_completed _
  | Web_search_call_in_progress _
  | Web_search_call_searching _
  | Web_search_call_completed _ -> None
;;

let observe_legacy t ~entry_id event =
  if t.finished
  then Error "provider observation after live source finished"
  else if
    Option.exists (observed_alias event) ~f:(fun alias ->
      let finalized_alias =
        match T.Item_id.of_string alias with
        | Error _ -> false
        | Ok alias -> Set.mem t.finalized_items alias
      in
      finalized_alias
      || Option.exists entry_id ~f:(fun host ->
        Option.exists (Map.find t.hosts host) ~f:(Set.mem t.finalized_items)))
  then Ok (t, [])
  else
    let open Result.Let_syntax in
    let%bind t, prefix = started t in
    let%map t, observations =
      match event with
      | R.Output_item_added value ->
        observe_item t ~entry_id ~output_index:value.output_index ~done_:false value.item
      | Output_item_done value ->
        observe_item t ~entry_id ~output_index:value.output_index ~done_:true value.item
      | Output_text_delta value ->
        content_change
          t
          ~alias:value.item_id
          ~entry_id
          ~index:value.content_index
          ~kind:Text
          ~change:(Append value.delta)
      | Output_text_done value ->
        content_change
          t
          ~alias:value.item_id
          ~entry_id
          ~index:value.content_index
          ~kind:Text
          ~change:(Replace value.text)
      | Response_refusal_delta value ->
        content_change
          t
          ~alias:value.item_id
          ~entry_id
          ~index:value.content_index
          ~kind:Refusal
          ~change:(Append value.delta)
      | Response_refusal_done value ->
        content_change
          t
          ~alias:value.item_id
          ~entry_id
          ~index:value.content_index
          ~kind:Refusal
          ~change:(Replace value.refusal)
      | Reasoning_summary_text_delta value ->
        content_change
          t
          ~alias:value.item_id
          ~entry_id
          ~index:value.summary_index
          ~kind:Reasoning_summary
          ~change:(Append value.delta)
      | Function_call_arguments_delta value ->
        call_change
          t
          ~alias:value.item_id
          ~entry_id
          ~kind:Function
          ~change:(Append value.delta)
      | Function_call_arguments_done value ->
        call_change
          t
          ~alias:value.item_id
          ~entry_id
          ~kind:Function
          ~change:(Replace value.arguments)
      | Custom_tool_call_input_delta value ->
        call_change
          t
          ~alias:value.item_id
          ~entry_id
          ~kind:Custom
          ~change:(Append value.delta)
      | Custom_tool_call_input_done value ->
        call_change
          t
          ~alias:value.item_id
          ~entry_id
          ~kind:Custom
          ~change:(Replace value.input)
      | Content_part_added value ->
        content_part
          t
          ~entry_id
          ~alias:value.item_id
          ~index:value.content_index
          ~done_:false
          value.part
      | Content_part_done value ->
        content_part
          t
          ~entry_id
          ~alias:value.item_id
          ~index:value.content_index
          ~done_:true
          value.part
      | Response_created _ | Response_in_progress _ -> Ok (t, [])
      | Response_completed _ -> Ok ({ t with completion = Some Complete }, [])
      | Response_incomplete _ -> Ok ({ t with completion = Some Incomplete }, [])
      | Response_failed _ | Error _ ->
        Result.map (opaque t event) ~f:(fun (t, observations) ->
          { t with completion = Some Failed }, observations)
      | Annotation_added _
      | File_search_call_in_progress _
      | File_search_call_searching _
      | File_search_call_completed _
      | Web_search_call_in_progress _
      | Web_search_call_searching _
      | Web_search_call_completed _
      | Unknown _ -> opaque t event
    in
    t, prefix @ observations
;;

let finalized t entry =
  if t.finished
  then Error "entry finalized after live source finished"
  else
    let open Result.Let_syntax in
    let%bind t, prefix = started t in
    let semantic = P.semantic (History_entry.payload entry) in
    let host = History_entry.id entry in
    let alias =
      match Map.find t.hosts host with
      | Some id -> T.Item_id.to_string id
      | None ->
        (match (P.Semantic.metadata semantic).item_id with
         | Value alias -> alias
         | Absent | Null -> "host:" ^ History_entry.Id.to_string host)
    in
    let%bind t, descriptor =
      item
        t
        ~finalized:true
        ~alias
        ~entry_id:(Some host)
        ~header:(Some (T.Header.of_semantic semantic))
        ~call_name:(semantic_name semantic)
        ()
    in
    let%map observations = events t [ Item_finalized { item = descriptor; entry } ] in
    ( { t with finalized_items = Set.add t.finalized_items descriptor.id }
    , prefix @ observations )
;;

let finish t ~completion =
  if t.finished
  then Error "live source already finished"
  else
    let open Result.Let_syntax in
    let%bind t, prefix = started t in
    let%map observations = events t [ Source_finished { scope = t.scope; completion } ] in
    { t with finished = true; completion = Some completion }, prefix @ observations
;;
