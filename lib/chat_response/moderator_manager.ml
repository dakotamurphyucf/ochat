open! Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Runtime = Chatml_moderator_runtime
module Res = Openai.Responses
module Value_codec = Chatml.Chatml_value_codec
module Snapshot = Session.Snapshot

module Registry = struct
  type artifact =
    { script_id : string
    ; source_hash : string
    ; compiled : Runtime.compiled_script
    }

  type t = artifact String.Map.t

  let empty = String.Map.empty
  let artifact_count = Map.length

  let source_text (script : CM.script) =
    match script.source with
    | Inline source_text -> source_text
    | Src { source_text; _ } -> source_text
  ;;

  let source_hash (source_text : string) = Md5.digest_string source_text |> Md5.to_hex

  let compile_script (t : t) (script : CM.script) : (t * artifact, string) result =
    let source_hash = source_hash (source_text script) in
    match Map.find t source_hash with
    | Some artifact -> Ok (t, artifact)
    | None ->
      Runtime.compile_script ~source:(source_text script) ()
      |> Result.map ~f:(fun compiled ->
        let artifact = { script_id = script.id; source_hash; compiled } in
        Map.set t ~key:source_hash ~data:artifact, artifact)
  ;;

  let of_elements (t : t) (elements : CM.top_level_elements list)
    : (t * artifact option, string) result
    =
    List.fold
      elements
      ~init:(Ok (t, None))
      ~f:(fun acc element ->
        let open Result.Let_syntax in
        let%bind registry, artifact = acc in
        match element with
        | CM.Script script ->
          let%map registry, compiled = compile_script registry script in
          registry, Some compiled
        | _ -> Ok (registry, artifact))
  ;;

  let script_id artifact = artifact.script_id
  let source_hash artifact = artifact.source_hash
end

type t =
  { artifact : Registry.artifact
  ; runtime : Runtime.session
  ; mutable overlay : Moderation.Overlay.t
  ; mutable projection : Moderation.Projection.t
  ; mutable processed_effect_count : int
  ; mutable next_overlay_message_id : int
  }

let entrypoints =
  Runtime.{ initial_state_name = "initial_state"; on_event_name = "on_event" }
;;

let snapshot_of_jsonaf (json : Jsonaf.t) : (Snapshot.t, string) result =
  Value_codec.Snapshot.of_value (Value_codec.jsonaf_to_value json)
;;

let jsonaf_of_snapshot ~name (snapshot : Snapshot.t) : (Jsonaf.t, string) result =
  let open Result.Let_syntax in
  let%bind value = Value_codec.Snapshot.to_value snapshot in
  match Value_codec.value_to_jsonaf_result value with
  | Ok json -> Ok json
  | Error msg -> Error (Printf.sprintf "%s: %s" name msg)
;;

let persisted_item_of_item (item : Moderation.Item.t)
  : (Session.Moderator_snapshot.Item.t, string) result
  =
  Result.map (snapshot_of_jsonaf item.value) ~f:(fun value ->
    Session.Moderator_snapshot.Item.{ id = item.id; value })
;;

let item_of_persisted (item : Session.Moderator_snapshot.Item.t)
  : (Moderation.Item.t, string) result
  =
  Result.map
    (jsonaf_of_snapshot ~name:"moderator overlay item value" item.value)
    ~f:(fun value -> Moderation.Item.create ~id:item.id ~value)
;;

let persisted_overlay_of_overlay (overlay : Moderation.Overlay.t)
  : (Session.Moderator_snapshot.Overlay.t, string) result
  =
  let open Result.Let_syntax in
  let%bind prepended_system_items =
    Result.all (List.map overlay.prepended_system_items ~f:persisted_item_of_item)
  in
  let%bind appended_items =
    Result.all (List.map overlay.appended_items ~f:persisted_item_of_item)
  in
  let%bind replacements =
    Result.all
      (List.map overlay.replacements ~f:(fun replacement ->
         let%map item = persisted_item_of_item replacement.item in
         Session.Moderator_snapshot.Overlay.{ target_id = replacement.target_id; item }))
  in
  Ok
    Session.Moderator_snapshot.Overlay.
      { prepended_system_items
      ; appended_items
      ; replacements
      ; deleted_item_ids = overlay.deleted_item_ids
      ; halted_reason = overlay.halted_reason
      }
;;

let overlay_of_persisted (overlay : Session.Moderator_snapshot.Overlay.t)
  : (Moderation.Overlay.t, string) result
  =
  let open Result.Let_syntax in
  let%bind prepended_system_items =
    Result.all (List.map overlay.prepended_system_items ~f:item_of_persisted)
  in
  let%bind appended_items =
    Result.all (List.map overlay.appended_items ~f:item_of_persisted)
  in
  let%bind replacements =
    Result.all
      (List.map overlay.replacements ~f:(fun replacement ->
         let%map item = item_of_persisted replacement.item in
         Moderation.Overlay.{ target_id = replacement.target_id; item }))
  in
  Ok
    Moderation.Overlay.
      { prepended_system_items
      ; appended_items
      ; replacements
      ; deleted_item_ids = overlay.deleted_item_ids
      ; halted_reason = overlay.halted_reason
      }
;;

let overlay_suffix (id : string) : int option =
  let prefix = "moderation-overlay-" in
  match String.chop_prefix id ~prefix with
  | None -> None
  | Some suffix -> Int.of_string_opt suffix
;;

let max_overlay_message_id (overlay : Moderation.Overlay.t) : int =
  let items =
    overlay.prepended_system_items
    @ overlay.appended_items
    @ List.map overlay.replacements ~f:(fun replacement -> replacement.item)
  in
  List.fold items ~init:0 ~f:(fun acc item ->
    match overlay_suffix item.id with
    | None -> acc
    | Some value -> Int.max acc value)
;;

let restored_runtime_values (snapshot : Session.Moderator_snapshot.t)
  : (Chatml.Chatml_lang.value * Chatml.Chatml_lang.value list, string) result
  =
  let open Result.Let_syntax in
  let%bind current_state = Value_codec.Snapshot.to_value snapshot.current_state in
  let%bind queued_internal_events =
    Result.all (List.map snapshot.queued_internal_events ~f:Value_codec.Snapshot.to_value)
  in
  Ok (current_state, queued_internal_events)
;;

let create
      ~(artifact : Registry.artifact)
      ~(capabilities : Moderation.Capabilities.t)
      ?snapshot
      ()
  : (t, string) result
  =
  let handlers = Moderation.Capabilities.runtime_handlers capabilities in
  let config = Runtime.default_runtime_config ~handlers () in
  let open Result.Let_syntax in
  let%bind runtime = Runtime.instantiate_session config artifact.compiled ~entrypoints in
  let%bind overlay, next_overlay_message_id =
    match snapshot with
    | None -> Ok (Moderation.Overlay.empty, 1)
    | Some (snapshot : Session.Moderator_snapshot.t) ->
      if not (String.equal snapshot.script_id artifact.script_id)
      then
        Error
          (Printf.sprintf
             "Moderator snapshot script id %S does not match prompt script id %S."
             snapshot.script_id
             artifact.script_id)
      else if not (String.equal snapshot.script_source_hash artifact.source_hash)
      then
        Error
          (Printf.sprintf
             "Moderator snapshot source hash %S does not match prompt source hash %S."
             snapshot.script_source_hash
             artifact.source_hash)
      else (
        let%bind current_state, queued_internal_events =
          restored_runtime_values snapshot
        in
        let%bind () =
          Runtime.restore
            runtime
            ~state:current_state
            ~queued_events:queued_internal_events
            ~halted:snapshot.halted
        in
        let%map overlay = overlay_of_persisted snapshot.overlay in
        overlay, max_overlay_message_id overlay + 1)
  in
  Ok
    { artifact
    ; runtime
    ; overlay
    ; projection = Moderation.Projection.empty
    ; processed_effect_count = 0
    ; next_overlay_message_id
    }
;;

let next_overlay_item (t : t) ~(role : Res.Input_message.role) ~(content : string) =
  let id = Printf.sprintf "moderation-overlay-%d" t.next_overlay_message_id in
  t.next_overlay_message_id <- t.next_overlay_message_id + 1;
  Moderation.Item.text_input_message ~id ~role ~text:content
;;

let update_replacements
      (replacements : Moderation.Overlay.replacement list)
      (replacement : Moderation.Overlay.replacement)
  =
  List.filter replacements ~f:(fun existing ->
    not (String.equal existing.target_id replacement.target_id))
  @ [ replacement ]
;;

let apply_overlay_op (t : t) (op : Moderation.Overlay.op) : unit =
  match op with
  | Moderation.Overlay.Prepend_system text ->
    let item = next_overlay_item t ~role:Res.Input_message.System ~content:text in
    t.overlay
    <- { t.overlay with
         prepended_system_items = t.overlay.prepended_system_items @ [ item ]
       }
  | Append_item item ->
    t.overlay <- { t.overlay with appended_items = t.overlay.appended_items @ [ item ] }
  | Replace_item replacement ->
    t.overlay
    <- { t.overlay with
         replacements = update_replacements t.overlay.replacements replacement
       }
  | Delete_item id ->
    if not (List.mem t.overlay.deleted_item_ids id ~equal:String.equal)
    then
      t.overlay
      <- { t.overlay with deleted_item_ids = t.overlay.deleted_item_ids @ [ id ] }
  | Halt reason -> t.overlay <- { t.overlay with halted_reason = Some reason }
;;

let project_context
      (t : t)
      ~session_id
      ~now_ms
      ~phase
      ~history
      ~available_tools
      ~session_meta
  =
  let projection, context =
    Moderation.Projection.project_context
      ~projection:t.projection
      ~session_id
      ~now_ms
      ~phase
      ~history
      ~available_tools
      ~session_meta
  in
  t.projection <- projection;
  context
;;

let new_committed_effects (t : t) : Chatml.Chatml_lang.eff list =
  List.drop (Runtime.committed_local_effects t.runtime) t.processed_effect_count
;;

let handle_event
      (t : t)
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~(event : Moderation.Event.t)
  : (Moderation.Outcome.t, string) result
  =
  let context =
    project_context
      t
      ~session_id
      ~now_ms
      ~phase:(Moderation.Event.phase event)
      ~history
      ~available_tools
      ~session_meta
  in
  let open Result.Let_syntax in
  let%bind () =
    Runtime.handle_event
      t.runtime
      ~context:(Moderation.Context.to_value context)
      ~event:(Moderation.Event.to_value event)
  in
  let new_effects = new_committed_effects t in
  t.processed_effect_count <- t.processed_effect_count + List.length new_effects;
  let%bind decoded = Runtime.decode_local_effects new_effects in
  let%map outcome = Moderation.Outcome.of_runtime_effects decoded in
  List.iter outcome.overlay_ops ~f:(apply_overlay_op t);
  outcome
;;

let rec drain_loop
          (t : t)
          ~session_id
          ~now_ms
          ~history
          ~available_tools
          ~session_meta
          ~remaining
          ~acc
  : (Moderation.Outcome.t list, string) result
  =
  if remaining = 0
  then Error "Exceeded moderator internal event replay limit."
  else (
    match Runtime.take_queued_event t.runtime with
    | None -> Ok (List.rev acc)
    | Some event ->
      (match
         handle_event
           t
           ~session_id
           ~now_ms
           ~history
           ~available_tools
           ~session_meta
           ~event:(Moderation.Event.Internal_event event)
       with
       | Error msg -> Error msg
       | Ok outcome ->
         drain_loop
           t
           ~session_id
           ~now_ms
           ~history
           ~available_tools
           ~session_meta
           ~remaining:(remaining - 1)
           ~acc:(outcome :: acc)))
;;

let drain_internal_events
      ?(max_events = 100)
      (t : t)
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
  =
  drain_loop
    t
    ~session_id
    ~now_ms
    ~history
    ~available_tools
    ~session_meta
    ~remaining:max_events
    ~acc:[]
;;

let effective_items (t : t) (history : Res.Item.t list) : Moderation.Item.t list =
  let projection, items = Moderation.Projection.project_history t.projection history in
  t.projection <- projection;
  Moderation.Overlay.apply t.overlay items
;;

let effective_history (t : t) (history : Res.Item.t list)
  : (Res.Item.t list, string) result
  =
  Result.all (List.map (effective_items t history) ~f:Moderation.Item.to_response_item)
;;

let snapshot (t : t) : (Session.Moderator_snapshot.t, string) result =
  let open Result.Let_syntax in
  let%bind current_state =
    Value_codec.Snapshot.of_value (Runtime.current_state t.runtime)
  in
  let%bind queued_internal_events =
    Result.all
      (List.map (Runtime.queued_events t.runtime) ~f:Value_codec.Snapshot.of_value)
  in
  let%map overlay = persisted_overlay_of_overlay t.overlay in
  Session.Moderator_snapshot.
    { script_id = Registry.script_id t.artifact
    ; script_source_hash = Registry.source_hash t.artifact
    ; current_state
    ; queued_internal_events
    ; halted = Runtime.is_halted t.runtime
    ; overlay
    }
;;

let enqueue_internal_event (t : t) (event : Chatml.Chatml_lang.value)
  : (unit, string) result
  =
  Runtime.enqueue_internal_event t.runtime event
;;
