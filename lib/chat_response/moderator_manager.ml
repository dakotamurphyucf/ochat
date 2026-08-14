open! Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Runtime = Chatml_host_runtime
module Res = Openai.Responses
module Builtin_spec = Chatml.Chatml_builtin_spec
module Builtin_surface = Chatml.Chatml_builtin_surface
module Debug_log = Chatml.Chatml_debug_log
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

  let surface_key (surface : Builtin_surface.surface) =
    let names values ~f =
      values |> List.map ~f |> List.sort ~compare:String.compare |> String.concat ~sep:","
    in
    String.concat
      ~sep:";"
      [ "globals:" ^ names surface.globals ~f:(fun builtin -> builtin.name)
      ; "modules:" ^ names surface.modules ~f:(fun builtin_module -> builtin_module.name)
      ; "aliases:" ^ names surface.type_aliases ~f:(fun alias -> alias.name)
      ]
  ;;

  let compile_script
        ?(surface = Builtin_surface.moderator_surface)
        (t : t)
        (script : CM.script)
    : (t * artifact, string) result
    =
    let source_hash = source_hash (source_text script) in
    let key = source_hash ^ "|" ^ surface_key surface in
    match Map.find t key with
    | Some artifact -> Ok (t, artifact)
    | None ->
      Runtime.compile_script ~surface ~source:(source_text script) ()
      |> Result.map ~f:(fun compiled ->
        let artifact = { script_id = script.id; source_hash; compiled } in
        Map.set t ~key ~data:artifact, artifact)
  ;;

  let of_elements
        ?(surface = Builtin_surface.moderator_surface)
        (t : t)
        (elements : CM.top_level_elements list)
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
          let%map registry, compiled = compile_script ~surface registry script in
          registry, Some compiled
        | _ -> Ok (registry, artifact))
  ;;

  let script_id artifact = artifact.script_id
  let source_hash artifact = artifact.source_hash
end

type subscription =
  { pending : Moderation.Overlay_change.t Queue.t
  ; on_wakeup : unit -> unit
  ; mutable active : bool
  }

type t =
  { artifact : Registry.artifact
  ; runtime : Runtime.session
  ; execution_mutex : Eio.Mutex.t
  ; mutable overlay : Moderation.Overlay.t
  ; mutable identity_overlay : Moderation.Identity_overlay.t
  ; mutable projection : Moderation.Projection.t
  ; mutable processed_effect_count : int
  ; mutable next_overlay_message_id : int
  ; allocator : History_entry.Allocator.t option
  ; mutable last_history : History_entry.t list
  ; mutable subscriptions : subscription list
  ; mutable suspended_phase : Moderation.Phase.t option
  }

type pending_ui_request = Runtime.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

let entrypoints =
  Runtime.{ initial_state_name = "initial_state"; on_event_name = "on_event" }
;;

let with_execution_lock t f = Eio.Mutex.use_ro t.execution_mutex f

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
      ?on_process_run
      ?snapshot
      ()
  : (t, string) result
  =
  let handlers = Moderation.Capabilities.runtime_handlers capabilities in
  let handlers =
    Option.value_map on_process_run ~default:handlers ~f:(fun on_process_run ->
      { handlers with on_process_run })
  in
  let config =
    Runtime.default_runtime_config
      ~surface:(Runtime.compiled_surface artifact.compiled)
      ~handlers
      ()
  in
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
    ; execution_mutex = Eio.Mutex.create ()
    ; overlay
    ; identity_overlay = Moderation.Identity_overlay.empty
    ; projection = Moderation.Projection.empty
    ; processed_effect_count = 0
    ; next_overlay_message_id
    ; allocator = None
    ; last_history = []
    ; subscriptions = []
    ; suspended_phase = None
    }
;;

let identity_overlay_of_snapshot (snapshot : Session.Moderator_state.Identity_snapshot.t)
  : (Moderation.Identity_overlay.t, string) result
  =
  let open Result.Let_syntax in
  let item value =
    let%bind value = jsonaf_of_snapshot ~name:"moderator identity item" value in
    try Ok (Res.Item.t_of_jsonaf value) with
    | exn -> Error (Exn.to_string exn)
  in
  let inserted
        ({ entry_id; change_id; value; script_label } :
          Session.Moderator_state.Identity_snapshot.Inserted.t)
    =
    let%map item = item value in
    Moderation.Identity_overlay.
      { entry = History_entry.create_with_id ~id:entry_id item; change_id; script_label }
  in
  let replacement
        ({ target_id; change_id; value; script_label } :
          Session.Moderator_state.Identity_snapshot.Replacement.t)
    =
    let%map item = item value in
    Moderation.Identity_overlay.{ target_id; item; change_id; script_label }
  in
  let%bind prepended_items = Result.all (List.map snapshot.prepended_items ~f:inserted) in
  let%bind appended_items = Result.all (List.map snapshot.appended_items ~f:inserted) in
  let%map replacements = Result.all (List.map snapshot.replacements ~f:replacement) in
  let tombstones =
    List.map snapshot.tombstones ~f:(fun { target_id; change_id } ->
      Moderation.Identity_overlay.{ target_id; change_id })
  in
  Moderation.Identity_overlay.
    { revision = snapshot.revision
    ; next_change_id = snapshot.next_change_id
    ; prepended_items
    ; appended_items
    ; replacements
    ; tombstones
    ; halted_reason = snapshot.halted_reason
    }
;;

let create_entries
      ~(artifact : Registry.artifact)
      ~(capabilities : Moderation.Capabilities.t)
      ~allocator
      ?on_process_run
      ?snapshot
      ()
  =
  let open Result.Let_syntax in
  let%bind t = create ~artifact ~capabilities ?on_process_run () in
  let%bind identity_overlay =
    match snapshot with
    | None -> Ok Moderation.Identity_overlay.empty
    | Some (snapshot : Session.Moderator_state.Identity_snapshot.t) ->
      if not (String.equal snapshot.script_id artifact.script_id)
      then Error "Moderator identity snapshot script id does not match prompt script id."
      else if not (String.equal snapshot.script_source_hash artifact.source_hash)
      then
        Error "Moderator identity snapshot source hash does not match prompt source hash."
      else (
        let%bind state, queued_events =
          let%bind state = Value_codec.Snapshot.to_value snapshot.current_state in
          let%map queued =
            Result.all
              (List.map snapshot.queued_internal_events ~f:Value_codec.Snapshot.to_value)
          in
          state, queued
        in
        let%bind () =
          Runtime.restore t.runtime ~state ~queued_events ~halted:snapshot.halted
        in
        identity_overlay_of_snapshot snapshot)
  in
  Ok { t with allocator = Some allocator; identity_overlay }
;;

let uses_allocator t allocator = Option.exists t.allocator ~f:(phys_equal allocator)
let history_allocator t = t.allocator

let subscribe_committed_changes t ~on_wakeup =
  let subscription = { pending = Queue.create (); on_wakeup; active = true } in
  t.subscriptions <- subscription :: t.subscriptions;
  subscription
;;

let drain_committed_changes subscription =
  let changes = if subscription.active then Queue.to_list subscription.pending else [] in
  Queue.clear subscription.pending;
  changes
;;

let unsubscribe subscription =
  subscription.active <- false;
  Queue.clear subscription.pending
;;

let overlay_revision t = t.identity_overlay.revision

let publish_change t change =
  t.subscriptions
  <- List.filter t.subscriptions ~f:(fun subscription -> subscription.active);
  List.iter (List.rev t.subscriptions) ~f:(fun subscription ->
    Queue.enqueue subscription.pending change;
    try subscription.on_wakeup () with
    | _ -> ())
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

let identity_target_ids t =
  List.map t.last_history ~f:History_entry.id
  @ List.map
      (t.identity_overlay.prepended_items @ t.identity_overlay.appended_items)
      ~f:(fun inserted -> History_entry.id inserted.entry)
;;

let decode_target t encoded =
  let open Result.Let_syntax in
  let%bind id = History_entry.Id.of_string encoded in
  if List.mem (identity_target_ids t) id ~equal:History_entry.Id.equal
  then Ok id
  else Error (Printf.sprintf "Unknown canonical moderation target %S." encoded)
;;

type planned_identity_op =
  | Planned_prepend of string
  | Planned_append of Moderation.Item.t * Res.Item.t
  | Planned_replace of History_entry.Id.t * Moderation.Item.t * Res.Item.t
  | Planned_delete of History_entry.Id.t
  | Planned_halt of string

let plan_identity_op t (op : Moderation.Overlay.op) : (planned_identity_op, string) result
  =
  let open Result.Let_syntax in
  match op with
  | Prepend_system text -> Ok (Planned_prepend text)
  | Append_item item ->
    let%map payload = Moderation.Item.to_response_item item in
    Planned_append (item, payload)
  | Replace_item replacement ->
    let%bind target_id = decode_target t replacement.target_id in
    let%map payload = Moderation.Item.to_response_item replacement.item in
    Planned_replace (target_id, replacement.item, payload)
  | Delete_item target ->
    let%map target_id = decode_target t target in
    Planned_delete target_id
  | Halt reason -> Ok (Planned_halt reason)
;;

let replace_by_target
      (replacements : Moderation.Identity_overlay.replacement list)
      (replacement : Moderation.Identity_overlay.replacement)
  =
  List.filter replacements ~f:(fun (current : Moderation.Identity_overlay.replacement) ->
    not (History_entry.Id.equal current.target_id replacement.target_id))
  @ [ replacement ]
;;

let tombstone_by_target
      (tombstones : Moderation.Identity_overlay.tombstone list)
      (tombstone : Moderation.Identity_overlay.tombstone)
  =
  List.filter tombstones ~f:(fun (current : Moderation.Identity_overlay.tombstone) ->
    not (History_entry.Id.equal current.target_id tombstone.target_id))
  @ [ tombstone ]
;;

let prepare_identity_ops t ~phase ops =
  let open Result.Let_syntax in
  if List.is_empty ops
  then Ok (fun () -> ())
  else (
    let%bind planned = Result.all (List.map ops ~f:(plan_identity_op t)) in
    let insertion_count =
      List.count planned ~f:(function
        | Planned_prepend _ | Planned_append _ -> true
        | Planned_replace _ | Planned_delete _ | Planned_halt _ -> false)
    in
    let allocator = Option.value_exn t.allocator in
    let%map reserved = History_entry.Allocator.reserve allocator ~count:insertion_count in
    let reserved = Queue.of_list reserved in
    let next_change_id = ref t.identity_overlay.next_change_id in
    let change_id () =
      let id = !next_change_id in
      Int.incr next_change_id;
      id
    in
    let operations_rev = ref [] in
    let affected_rev = ref [] in
    let inserted_ids_rev = ref [] in
    let overlay =
      List.fold planned ~init:t.identity_overlay ~f:(fun overlay op ->
        let change_id = change_id () in
        match op with
        | Planned_prepend text ->
          let id = Queue.dequeue_exn reserved in
          operations_rev
          := Moderation.Overlay_change.Inserted
               { entry_id = id; change_id; position = Prepended; script_label = None }
             :: !operations_rev;
          affected_rev := id :: !affected_rev;
          inserted_ids_rev := id :: !inserted_ids_rev;
          let item =
            Res.Item.Input_message
              { role = Res.Input_message.System
              ; content = [ Res.Input_message.Text { text; _type = "input_text" } ]
              ; _type = "message"
              }
          in
          let inserted =
            Moderation.Identity_overlay.
              { entry = History_entry.create_with_id ~id item
              ; change_id
              ; script_label = None
              }
          in
          { overlay with prepended_items = overlay.prepended_items @ [ inserted ] }
        | Planned_append (script_item, item) ->
          let id = Queue.dequeue_exn reserved in
          operations_rev
          := Moderation.Overlay_change.Inserted
               { entry_id = id
               ; change_id
               ; position = Appended
               ; script_label = Some script_item.id
               }
             :: !operations_rev;
          affected_rev := id :: !affected_rev;
          inserted_ids_rev := id :: !inserted_ids_rev;
          let inserted =
            Moderation.Identity_overlay.
              { entry = History_entry.create_with_id ~id item
              ; change_id
              ; script_label = Some script_item.id
              }
          in
          { overlay with appended_items = overlay.appended_items @ [ inserted ] }
        | Planned_replace (target_id, script_item, item) ->
          operations_rev
          := Moderation.Overlay_change.Replaced
               { target_id; change_id; script_label = Some script_item.id }
             :: !operations_rev;
          affected_rev := target_id :: !affected_rev;
          let replacement =
            Moderation.Identity_overlay.
              { target_id; item; change_id; script_label = Some script_item.id }
          in
          { overlay with
            replacements = replace_by_target overlay.replacements replacement
          }
        | Planned_delete target_id ->
          operations_rev
          := Moderation.Overlay_change.Deleted { target_id; change_id } :: !operations_rev;
          affected_rev := target_id :: !affected_rev;
          let tombstone = Moderation.Identity_overlay.{ target_id; change_id } in
          { overlay with tombstones = tombstone_by_target overlay.tombstones tombstone }
        | Planned_halt reason ->
          operations_rev
          := Moderation.Overlay_change.Halted { reason; change_id } :: !operations_rev;
          { overlay with halted_reason = Some reason })
    in
    let overlay =
      { overlay with revision = overlay.revision + 1; next_change_id = !next_change_id }
    in
    let operations = List.rev !operations_rev in
    let change =
      Moderation.Overlay_change.
        { revision = overlay.revision
        ; operations
        ; affected_entry_ids = List.rev !affected_rev
        ; allocated_inserted_ids = List.rev !inserted_ids_rev
        ; script_id = Registry.script_id t.artifact
        ; script_source_hash = Registry.source_hash t.artifact
        ; phase
        ; visible_history_changed =
            List.exists operations ~f:(function
              | Inserted _ | Replaced _ | Deleted _ -> true
              | Halted _ -> false)
        }
    in
    fun () ->
      t.identity_overlay <- overlay;
      publish_change t change)
;;

let install_identity_ops t ~phase ops =
  Result.map (prepare_identity_ops t ~phase ops) ~f:(fun install -> install ())
;;

let committed_outcome (t : t) : (Moderation.Outcome.t option, string) result =
  let open Result.Let_syntax in
  let new_effects = new_committed_effects t in
  match new_effects with
  | [] -> Ok None
  | _ ->
    let%bind decoded = Runtime.decode_local_effects new_effects in
    let%bind outcome = Moderation.Outcome.of_runtime_effects decoded in
    let%map () =
      match t.allocator with
      | None ->
        List.iter outcome.overlay_ops ~f:(apply_overlay_op t);
        Ok ()
      | Some _ ->
        install_identity_ops t ~phase:Moderation.Phase.Internal_event outcome.overlay_ops
    in
    t.processed_effect_count <- t.processed_effect_count + List.length new_effects;
    Some outcome
;;

let handle_event_unlocked
      (t : t)
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~(event : Moderation.Event.t)
  : (Moderation.Outcome.t, string) result
  =
  let event_value = Moderation.Event.to_value event in
  Debug_log.emitf
    "[moderator-manager] handle_event session=%s event=%s"
    session_id
    (Builtin_spec.value_to_pretty_string event_value);
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
  let%map outcome = committed_outcome t in
  let outcome = Option.value outcome ~default:Moderation.Outcome.empty in
  Debug_log.emitf
    "[moderator-manager] handle_event_ok session=%s overlay_ops=%d runtime_requests=%d \
     emitted_events=%d tool_moderation=%b"
    session_id
    (List.length outcome.overlay_ops)
    (List.length outcome.runtime_requests)
    (List.length outcome.emitted_events)
    (Option.is_some outcome.tool_moderation);
  outcome
;;

let handle_event t ~session_id ~now_ms ~history ~available_tools ~session_meta ~event =
  with_execution_lock t (fun () ->
    handle_event_unlocked
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~event)
;;

let handle_event_entries_unlocked
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~event
  =
  t.last_history <- history;
  let context =
    Moderation.Entry_projection.project_context
      ~session_id
      ~now_ms
      ~phase:(Moderation.Event.phase event)
      ~history
      ~available_tools
      ~session_meta
  in
  let open Result.Let_syntax in
  let outcome = ref Moderation.Outcome.empty in
  let prepare_commit ~local_effects =
    let%bind decoded = Runtime.decode_local_effects local_effects in
    let%bind prepared = Moderation.Outcome.of_runtime_effects decoded in
    let%map install_overlay =
      prepare_identity_ops t ~phase:(Moderation.Event.phase event) prepared.overlay_ops
    in
    fun () ->
      install_overlay ();
      t.processed_effect_count <- t.processed_effect_count + List.length local_effects;
      outcome := prepared
  in
  let%map () =
    Runtime.handle_event
      ~prepare_commit
      t.runtime
      ~context:(Moderation.Context.to_value context)
      ~event:(Moderation.Event.to_value event)
  in
  t.suspended_phase
  <- (match Runtime.pending_ui_request t.runtime with
      | None -> None
      | Some _ -> Some (Moderation.Event.phase event));
  !outcome
;;

let handle_event_entries
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~event
  =
  with_execution_lock t (fun () ->
    handle_event_entries_unlocked
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
      ~event)
;;

let pending_ui_request (t : t) : pending_ui_request option =
  Runtime.pending_ui_request t.runtime
;;

let resume_ui_request_unlocked (t : t) ~response
  : (Moderation.Outcome.t list, string) result
  =
  let open Result.Let_syntax in
  match t.allocator with
  | None ->
    let%bind () = Runtime.resume_ui_request t.runtime ~response in
    let%map outcome = committed_outcome t in
    Option.to_list outcome
  | Some _ ->
    let outcome = ref None in
    let phase = Option.value t.suspended_phase ~default:Moderation.Phase.Internal_event in
    let prepare_commit ~local_effects =
      let%bind decoded = Runtime.decode_local_effects local_effects in
      let%bind prepared = Moderation.Outcome.of_runtime_effects decoded in
      let%map install_overlay = prepare_identity_ops t ~phase prepared.overlay_ops in
      fun () ->
        install_overlay ();
        t.processed_effect_count <- t.processed_effect_count + List.length local_effects;
        outcome := Some prepared
    in
    let%map () = Runtime.resume_ui_request ~prepare_commit t.runtime ~response in
    if Option.is_none (Runtime.pending_ui_request t.runtime)
    then t.suspended_phase <- None;
    Option.to_list !outcome
;;

let resume_ui_request t ~response =
  with_execution_lock t (fun () -> resume_ui_request_unlocked t ~response)
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
  then Ok (List.rev acc)
  else if Option.is_some (Runtime.pending_ui_request t.runtime)
  then Error "Session is waiting for UI input."
  else (
    match Runtime.take_queued_event t.runtime with
    | None -> Ok (List.rev acc)
    | Some event ->
      Debug_log.emitf
        "[moderator-manager] drain_internal_event session=%s event=%s remaining=%d"
        session_id
        (Builtin_spec.value_to_pretty_string event)
        remaining;
      (match
         handle_event_unlocked
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

let drain_internal_events_unlocked
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

let drain_internal_events
      ?max_events
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
  =
  with_execution_lock t (fun () ->
    drain_internal_events_unlocked
      ?max_events
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta)
;;

let rec drain_entries_loop
          t
          ~session_id
          ~now_ms
          ~history
          ~available_tools
          ~session_meta
          ~remaining
          ~acc
  =
  if remaining = 0
  then Ok (List.rev acc)
  else if Option.is_some (Runtime.pending_ui_request t.runtime)
  then Error "Session is waiting for UI input."
  else (
    match Runtime.take_queued_event t.runtime with
    | None -> Ok (List.rev acc)
    | Some event ->
      (match
         handle_event_entries_unlocked
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
         drain_entries_loop
           t
           ~session_id
           ~now_ms
           ~history
           ~available_tools
           ~session_meta
           ~remaining:(remaining - 1)
           ~acc:(outcome :: acc)))
;;

let drain_internal_events_entries_unlocked
      ?(max_events = 100)
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
  =
  drain_entries_loop
    t
    ~session_id
    ~now_ms
    ~history
    ~available_tools
    ~session_meta
    ~remaining:max_events
    ~acc:[]
;;

let drain_internal_events_entries
      ?max_events
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta
  =
  with_execution_lock t (fun () ->
    drain_internal_events_entries_unlocked
      ?max_events
      t
      ~session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta)
;;

let effective_entries t history =
  Moderation.Identity_overlay.apply t.identity_overlay history
;;

let effective_history_entries t history =
  List.map (effective_entries t history) ~f:(fun effective -> effective.entry)
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

let snapshot_unlocked (t : t) : (Session.Moderator_snapshot.t, string) result =
  let open Result.Let_syntax in
  let%bind () =
    match Runtime.pending_ui_request t.runtime with
    | None -> Ok ()
    | Some _ -> Error "Cannot snapshot moderator while approval is suspended."
  in
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

let snapshot t = with_execution_lock t (fun () -> snapshot_unlocked t)

let identity_snapshot_unlocked t =
  let open Result.Let_syntax in
  let%bind () =
    match Runtime.pending_ui_request t.runtime with
    | None -> Ok ()
    | Some _ -> Error "Cannot snapshot moderator while approval is suspended."
  in
  let%bind current_state =
    Value_codec.Snapshot.of_value (Runtime.current_state t.runtime)
  in
  let%bind queued_internal_events =
    Result.all
      (List.map (Runtime.queued_events t.runtime) ~f:Value_codec.Snapshot.of_value)
  in
  let inserted (inserted : Moderation.Identity_overlay.inserted) =
    let%map value =
      History_entry.item inserted.Moderation.Identity_overlay.entry
      |> Res.Item.jsonaf_of_t
      |> snapshot_of_jsonaf
    in
    Session.Moderator_state.Identity_snapshot.Inserted.
      { entry_id = History_entry.id inserted.entry
      ; change_id = inserted.change_id
      ; value
      ; script_label = inserted.script_label
      }
  in
  let replacement (replacement : Moderation.Identity_overlay.replacement) =
    let%map value = replacement.item |> Res.Item.jsonaf_of_t |> snapshot_of_jsonaf in
    Session.Moderator_state.Identity_snapshot.Replacement.
      { target_id = replacement.target_id
      ; change_id = replacement.change_id
      ; value
      ; script_label = replacement.script_label
      }
  in
  let overlay = t.identity_overlay in
  let%bind prepended_items = Result.all (List.map overlay.prepended_items ~f:inserted) in
  let%bind appended_items = Result.all (List.map overlay.appended_items ~f:inserted) in
  let%map replacements = Result.all (List.map overlay.replacements ~f:replacement) in
  let tombstones =
    List.map
      overlay.tombstones
      ~f:(fun (tombstone : Moderation.Identity_overlay.tombstone) ->
        Session.Moderator_state.Identity_snapshot.Tombstone.
          { target_id = tombstone.target_id; change_id = tombstone.change_id })
  in
  Session.Moderator_state.Identity_snapshot.
    { script_id = Registry.script_id t.artifact
    ; script_source_hash = Registry.source_hash t.artifact
    ; current_state
    ; queued_internal_events
    ; halted = Runtime.is_halted t.runtime
    ; revision = overlay.revision
    ; next_change_id = overlay.next_change_id
    ; prepended_items
    ; appended_items
    ; replacements
    ; tombstones
    ; halted_reason = overlay.halted_reason
    }
;;

let identity_snapshot t = with_execution_lock t (fun () -> identity_snapshot_unlocked t)

let enqueue_internal_event_unlocked (t : t) (event : Chatml.Chatml_lang.value)
  : (unit, string) result
  =
  Runtime.enqueue_internal_event t.runtime event
;;

let enqueue_internal_event t event =
  with_execution_lock t (fun () -> enqueue_internal_event_unlocked t event)
;;
