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

let persisted_message_of_message (message : Moderation.Message.t)
  : (Session.Moderator_snapshot.Message.t, string) result
  =
  Result.map (snapshot_of_jsonaf message.meta) ~f:(fun meta ->
    Session.Moderator_snapshot.Message.
      { id = message.id; role = message.role; content = message.content; meta })
;;

let message_of_persisted (message : Session.Moderator_snapshot.Message.t)
  : (Moderation.Message.t, string) result
  =
  Result.map
    (jsonaf_of_snapshot ~name:"moderator overlay message meta" message.meta)
    ~f:(fun meta ->
      Moderation.Message.create
        ~id:message.id
        ~role:message.role
        ~content:message.content
        ~meta)
;;

let persisted_overlay_of_overlay (overlay : Moderation.Overlay.t)
  : (Session.Moderator_snapshot.Overlay.t, string) result
  =
  let open Result.Let_syntax in
  let%bind prepended_system_messages =
    Result.all
      (List.map overlay.prepended_system_messages ~f:persisted_message_of_message)
  in
  let%bind appended_messages =
    Result.all (List.map overlay.appended_messages ~f:persisted_message_of_message)
  in
  let%bind replacements =
    Result.all
      (List.map overlay.replacements ~f:(fun replacement ->
         let%map message = persisted_message_of_message replacement.message in
         Session.Moderator_snapshot.Overlay.{ target_id = replacement.target_id; message }))
  in
  Ok
    Session.Moderator_snapshot.Overlay.
      { prepended_system_messages
      ; appended_messages
      ; replacements
      ; deleted_message_ids = overlay.deleted_message_ids
      ; halted_reason = overlay.halted_reason
      }
;;

let overlay_of_persisted (overlay : Session.Moderator_snapshot.Overlay.t)
  : (Moderation.Overlay.t, string) result
  =
  let open Result.Let_syntax in
  let%bind prepended_system_messages =
    Result.all (List.map overlay.prepended_system_messages ~f:message_of_persisted)
  in
  let%bind appended_messages =
    Result.all (List.map overlay.appended_messages ~f:message_of_persisted)
  in
  let%bind replacements =
    Result.all
      (List.map overlay.replacements ~f:(fun replacement ->
         let%map message = message_of_persisted replacement.message in
         Moderation.Overlay.{ target_id = replacement.target_id; message }))
  in
  Ok
    Moderation.Overlay.
      { prepended_system_messages
      ; appended_messages
      ; replacements
      ; deleted_message_ids = overlay.deleted_message_ids
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
  let messages =
    overlay.prepended_system_messages
    @ overlay.appended_messages
    @ List.map overlay.replacements ~f:(fun replacement -> replacement.message)
  in
  List.fold messages ~init:0 ~f:(fun acc message ->
    match overlay_suffix message.id with
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

let next_overlay_message (t : t) ~(role : string) ~(content : string) =
  let id = Printf.sprintf "moderation-overlay-%d" t.next_overlay_message_id in
  t.next_overlay_message_id <- t.next_overlay_message_id + 1;
  Moderation.Message.create ~id ~role ~content ~meta:`Null
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
    let message = next_overlay_message t ~role:"system" ~content:text in
    t.overlay
    <- { t.overlay with
         prepended_system_messages = t.overlay.prepended_system_messages @ [ message ]
       }
  | Append_message message ->
    t.overlay
    <- { t.overlay with appended_messages = t.overlay.appended_messages @ [ message ] }
  | Replace_message replacement ->
    t.overlay
    <- { t.overlay with
         replacements = update_replacements t.overlay.replacements replacement
       }
  | Delete_message id ->
    if not (List.mem t.overlay.deleted_message_ids id ~equal:String.equal)
    then
      t.overlay
      <- { t.overlay with deleted_message_ids = t.overlay.deleted_message_ids @ [ id ] }
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

let effective_messages (t : t) (history : Res.Item.t list) : Moderation.Message.t list =
  let projection, messages = Moderation.Projection.project_history t.projection history in
  t.projection <- projection;
  Moderation.Overlay.apply t.overlay messages
;;

let input_role_of_string (role : string) : Res.Input_message.role =
  match String.lowercase role with
  | "system" -> Res.Input_message.System
  | "user" -> Res.Input_message.User
  | "assistant" -> Res.Input_message.Assistant
  | "developer" -> Res.Input_message.Developer
  | _ -> Res.Input_message.Assistant
;;

let input_item_of_message (m : Moderation.Message.t) : Res.Item.t =
  (* NOTE: adjust fields if your Openai.Responses.Input_message.Text record differs. *)
  let role = input_role_of_string m.role in
  let _type =
    match role with
    | Res.Input_message.System -> "input_text"
    | Res.Input_message.User -> "input_text"
    | Res.Input_message.Assistant -> "output_text"
    | Res.Input_message.Developer -> "input_text"
  in
  let content = [ Res.Input_message.Text { text = m.content; _type } ] in
  Res.Item.Input_message { role; content; _type = "message" }
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
