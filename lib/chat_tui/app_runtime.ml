open Core
open Eio.Std
module Manager = Chat_response.Moderator_manager
module Runtime_semantics = Chat_response.Runtime_semantics
module Stream_moderator = Chat_response.In_memory_stream
module Shell_broker = Shell_runtime.Approval_broker

type op =
  | Streaming of
      { sw : Switch.t
      ; id : int
      }
  | Compacting of
      { sw : Switch.t
      ; id : int
      }
  | Starting_streaming of { id : int }
  | Starting_compaction of { id : int }

type typeahead_op =
  | Typeahead of
      { sw : Switch.t
      ; id : int
      }
  | Starting_typeahead of { id : int }

type submit_request =
  { text : string
  ; draft_mode : Model.draft_mode
  }

type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup

type deferred_user_note = { entry : History_entry.t }

type moderator_approval = Stream_moderator.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

type pending_input =
  | Moderator of moderator_approval
  | Shell of Shell_broker.ui_request

type moderator_startup_state =
  | Starting
  | Ready
  | Failed of string

type automatic_turn_decision =
  | Allow_automatic_turn
  | Suppress_automatic_turn of
      { notice_key : string
      ; notice_text : string
      }

type session_controller_state =
  { mutable moderator_dirty : bool
  ; mutable pending_overlay_revision : int option
  ; mutable projected_overlay_revision : int
  ; deferred_user_notes : deferred_user_note Queue.t
  ; mutable pending_turn_request : turn_start_reason option
  ; mutable started_followup_turns_since_user_submit : int
  ; mutable started_followup_turn_timestamps_ms : int list
  }

type queued_action =
  | Submit of submit_request
  | Compact

type startup_render_state =
  { mutable config : Chat_render_worker_runtime.Config.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; code_cache_capacity : int
  ; mutable generation : int
  ; mutable cancel : bool Atomic.t
  ; mutable snapshot : Chat_startup_render.snapshot option
  ; mutable jobs : Chat_message_render_job.t list
  }

type startup_render =
  | Synchronous
  | Warming of startup_render_state

type t =
  { model : Model.t
  ; chat_render_worker : Chat_render_worker.t option
  ; history_allocator : History_entry.Allocator.t
  ; agent_page_kind_by_name :
      Chat_response.Tool_execution_event.agent_page_kind String.Table.t
  ; mutable op : op option
  ; mutable typeahead_op : typeahead_op option
  ; moderator : Stream_moderator.moderator option
  ; shell_approval_broker : Shell_broker.t option
  ; approval_store : Shell_runtime.Approval_store.t option
  ; session_state : Session.t ref option
  ; session_controller : session_controller_state
  ; shown_notice_keys : String.Hash_set.t
  ; mutable active_turn_start_reason : turn_start_reason option
  ; mutable halted_reason : string option
  ; mutable moderator_startup_state : moderator_startup_state
  ; mutable pending_input : pending_input option
  ; pending : queued_action Queue.t
  ; quit_via_esc : bool ref
  ; mutable next_op_id : int
  ; mutable cancel_streaming_on_start : bool
  ; mutable cancel_compaction_on_start : bool
  ; mutable cancel_typeahead_on_start : bool
  ; mutable pending_agent_toggle : int option
  ; mutable startup_render : startup_render
  ; mutable startup_render_metrics : Jsonaf.t option
  ; mutable startup_render_started_at : Time_ns.t option
  }

let moderator_approval_equal left right =
  let choices_equal left right =
    let left = Array.to_list left in
    let right = Array.to_list right in
    List.equal String.equal left right
  in
  match left, right with
  | Ask_text { prompt = left }, Ask_text { prompt = right } -> String.equal left right
  | ( Ask_choice { prompt = left_prompt; choices = left_choices }
    , Ask_choice { prompt = right_prompt; choices = right_choices } ) ->
    String.equal left_prompt right_prompt && choices_equal left_choices right_choices
  | Ask_text _, Ask_choice _ | Ask_choice _, Ask_text _ -> false
;;

let pending_input_equal left right =
  match left, right with
  | None, None -> true
  | Some (Moderator left), Some (Moderator right) -> moderator_approval_equal left right
  | Some (Shell left), Some (Shell right) -> String.equal left.id right.id
  | Some _, Some _ | None, Some _ | Some _, None -> false
;;

let render_moderator_approval = function
  | Ask_text { prompt } -> "Approval requested: " ^ prompt
  | Ask_choice { prompt; choices } ->
    "Approval requested: "
    ^ prompt
    ^ "\nChoices: "
    ^ String.concat ~sep:", " (Array.to_list choices)
;;

let string_of_sandbox = function
  | Shell_access.Capabilities.Required -> "required"
  | Preferred -> "preferred"
  | Direct_unsafe -> "direct_unsafe"
;;

let render_policy_matches matches =
  match matches with
  | [] -> "none"
  | matches ->
    List.map matches ~f:(fun (match_ : Shell_access.Policy.match_) ->
      sprintf "%s:%s" match_.rule_id (Shell_access.Policy.string_of_action match_.action))
    |> String.concat ~sep:", "
;;

let render_capabilities (capabilities : Shell_access.Capabilities.t) =
  sprintf
    "sandbox=%s network=%b child_processes=%b arbitrary_code=%b privilege_change=%b"
    (string_of_sandbox capabilities.sandbox)
    capabilities.network
    capabilities.allow_child_processes
    capabilities.allow_arbitrary_code
    capabilities.allow_privilege_change
;;

let render_shell_approval request =
  let approval = request.Shell_broker.request in
  let context = approval.Shell_access.Approval.context in
  let effects =
    Shell_access.Effect.to_strings context.effects |> String.concat ~sep:", "
  in
  let fingerprint = String.prefix context.executable.fingerprint.sha256 12 in
  sprintf
    "Shell approval requested\n\
     Command: %s\n\
     Executable: %s\n\
     Fingerprint: %s\n\
     CWD: %s\n\
     Effects: %s\n\
     Policy: %s\n\
     Policy matches: %s\n\
     Capabilities: %s\n\
     Runtime: %s\n\
     Manifest: %s\n\
     Reply: once, session, deny, or deny REASON"
    approval.display_command
    context.executable.canonical_path
    fingerprint
    context.cwd
    effects
    approval.policy.reason
    (render_policy_matches approval.policy.matches)
    (render_capabilities context.capabilities)
    request.runtime_id
    (String.prefix request.manifest_sha256 12)
;;

let render_pending_input = function
  | Moderator approval -> render_moderator_approval approval
  | Shell request -> render_shell_approval request
;;

let visible_history_items_of_history (t : t) (history : History_entry.t list)
  : History_entry.t list
  =
  match t.moderator with
  | None -> history
  | Some moderator -> Manager.effective_history_entries moderator.manager history
;;

let visible_messages_of_history (t : t) (history : History_entry.t list)
  : Types.message list
  =
  visible_history_items_of_history t history
  |> History_entry.items
  |> Conversation.of_history
;;

let refresh_messages ?(viewport_height = 0) (t : t) =
  let history = Model.history_items t.model in
  let effective_entries =
    match t.moderator with
    | None ->
      List.map history ~f:(fun entry ->
        Chat_response.Moderation.Effective_entry.{ entry; provenance = Canonical })
    | Some moderator -> Manager.effective_entries moderator.manager history
  in
  let projection = Conversation.project_effective_entries effective_entries in
  let damage =
    Model.reconcile_projected_messages_with_damage
      t.model
      ~viewport_height
      ~rows:(Conversation.rows projection)
      ~messages:(Conversation.messages projection)
  in
  Model.rebuild_tool_output_index_for_items
    t.model
    (List.map effective_entries ~f:(fun effective -> effective.entry));
  damage
;;

let moderator_snapshot (t : t) : (Session.Moderator_snapshot.t option, string) result =
  match t.moderator with
  | None -> Ok None
  | Some moderator -> Result.map (Manager.snapshot moderator.manager) ~f:Option.some
;;

let validate_moderator_allocator ~moderator ~history_allocator =
  match moderator with
  | None -> Ok ()
  | Some (moderator : Stream_moderator.moderator) ->
    if Manager.uses_allocator moderator.manager history_allocator
    then Ok ()
    else Error "Chat-TUI moderator and runtime must share one history allocator."
;;

let create
      ?moderator
      ?halted_reason
      ?chat_render_worker
      ?history_allocator
      ?shell_approval_broker
      ?approval_store
      ?session_state
      ?(moderator_startup_state = Ready)
      ?(agent_page_classifications = [])
      ~model
      ()
  =
  let history_allocator =
    Option.first_some
      history_allocator
      (Option.bind moderator ~f:(fun (moderator : Stream_moderator.moderator) ->
         Manager.history_allocator moderator.manager))
    |> Option.value_or_thunk ~default:(fun () ->
      let namespace =
        match Model.history_items model with
        | entry :: _ -> History_entry.id entry |> History_entry.Id.namespace
        | [] -> "chat-tui-runtime"
      in
      let next_sequence =
        List.filter_map (Model.history_items model) ~f:(fun entry ->
          let id = History_entry.id entry in
          if String.equal (History_entry.Id.namespace id) namespace
          then Some (History_entry.Id.sequence id + 1)
          else None)
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0
      in
      History_entry.Allocator.create ~namespace ~next_sequence |> Result.ok_or_failwith)
  in
  validate_moderator_allocator ~moderator ~history_allocator |> Result.ok_or_failwith;
  let pending_input =
    match Option.bind shell_approval_broker ~f:Shell_broker.pending with
    | Some request -> Some (Shell request)
    | None ->
      Option.bind moderator ~f:Stream_moderator.pending_ui_request
      |> Option.map ~f:(fun value -> Moderator value)
  in
  (match pending_input, shell_approval_broker with
   | Some (Shell request), Some broker ->
     Model.open_shell_approval_modal
       model
       ~request
       ~queue_count:(Shell_broker.pending_count broker)
   | (None | Some (Moderator _)), _ | Some (Shell _), None -> ());
  { model
  ; chat_render_worker
  ; history_allocator
  ; agent_page_kind_by_name = String.Table.of_alist_exn agent_page_classifications
  ; op = None
  ; typeahead_op = None
  ; moderator
  ; shell_approval_broker
  ; approval_store
  ; session_state
  ; session_controller =
      { moderator_dirty = false
      ; pending_overlay_revision = None
      ; projected_overlay_revision =
          Option.value_map moderator ~default:0 ~f:(fun moderator ->
            Manager.overlay_revision moderator.manager)
      ; deferred_user_notes = Queue.create ()
      ; pending_turn_request = None
      ; started_followup_turns_since_user_submit = 0
      ; started_followup_turn_timestamps_ms = []
      }
  ; shown_notice_keys = Hash_set.create (module String)
  ; active_turn_start_reason = None
  ; halted_reason
  ; moderator_startup_state
  ; pending_input
  ; pending = Queue.create ()
  ; quit_via_esc = ref false
  ; next_op_id = 0
  ; cancel_streaming_on_start = false
  ; cancel_compaction_on_start = false
  ; cancel_typeahead_on_start = false
  ; pending_agent_toggle = None
  ; startup_render = Synchronous
  ; startup_render_metrics = None
  ; startup_render_started_at = None
  }
;;

let submit_initial_target_width_batches t =
  match t.chat_render_worker, Model.width_preparation t.model with
  | None, _ | _, None -> ()
  | Some worker, Some preparation ->
    let resize_generation = Model.width_preparation_request_generation preparation in
    (match Renderer_page_chat.initial_target_width_batches ~model:t.model () with
     | None -> ()
     | Some (_, batches) ->
       List.iter batches ~f:(fun (batch_id, jobs) ->
         ignore
           (Chat_render_worker.submit_batch worker ~resize_generation ~batch_id jobs
            : Chat_render_worker.submit_result list)))
;;

let reprioritize_target_width_batches t =
  match t.chat_render_worker, Model.width_preparation t.model with
  | None, _ | _, None -> ()
  | Some worker, Some preparation ->
    let resize_generation = Model.width_preparation_request_generation preparation in
    (match Renderer_page_chat.current_target_width_batches ~model:t.model () with
     | None -> ()
     | Some (_, batches) ->
       List.iter batches ~f:(fun (batch_id, jobs) ->
         Chat_render_worker.reprioritize_batch worker ~resize_generation ~batch_id jobs;
         ignore
           (Chat_render_worker.submit_batch worker ~resize_generation ~batch_id jobs
            : Chat_render_worker.submit_result list)))
;;

let submit_destination_target_width_batches t =
  match t.chat_render_worker, Model.width_preparation t.model with
  | None, _ | _, None -> ()
  | Some worker, Some preparation ->
    let resize_generation = Model.width_preparation_request_generation preparation in
    (match Renderer_page_chat.destination_target_width_batches ~model:t.model () with
     | None -> ()
     | Some (_, batches) ->
       List.iter batches ~f:(fun (batch_id, jobs) ->
         ignore
           (Chat_render_worker.submit_batch worker ~resize_generation ~batch_id jobs
            : Chat_render_worker.submit_result list)))
;;

let pump_target_width_completion t =
  match t.chat_render_worker, Model.width_preparation t.model with
  | None, _ | _, None -> ()
  | Some worker, Some preparation ->
    let slots = Chat_render_worker.available_slots worker in
    if slots > 0
    then (
      let resize_generation = Model.width_preparation_request_generation preparation in
      match Renderer_page_chat.remaining_target_width_batches ~model:t.model () with
      | None -> ()
      | Some batches ->
        let rec submit remaining = function
          | [] -> ()
          | _ when remaining <= 0 -> ()
          | (batch_id, jobs) :: rest ->
            let jobs = List.take jobs remaining in
            if List.is_empty jobs
            then submit remaining rest
            else (
              let results =
                Chat_render_worker.submit_batch worker ~resize_generation ~batch_id jobs
              in
              let queued =
                List.count results ~f:(function
                  | Chat_render_worker.Queued -> true
                  | Already_pending | Rejected -> false)
              in
              submit (remaining - queued) rest)
        in
        submit slots batches)
;;

let cancel_current_target_width_preparation t =
  Option.iter (Model.width_preparation t.model) ~f:(fun preparation ->
    let request_generation = Model.width_preparation_request_generation preparation in
    Option.iter t.chat_render_worker ~f:(fun worker ->
      Chat_render_worker.cancel_generation worker ~resize_generation:request_generation);
    ignore (Model.cancel_width_preparation t.model ~request_generation : bool))
;;

let agent_page_kind t ~name = Hashtbl.find t.agent_page_kind_by_name name

let close_startup_render t =
  match t.startup_render with
  | Synchronous -> ()
  | Warming state ->
    Atomic.set state.cancel true;
    t.startup_render <- Synchronous
;;

let complete_startup_render t =
  close_startup_render t;
  Model.set_chat_materialization_warm t.model
;;

let record_startup_publication t ~publication_latency =
  let loader_duration =
    Option.value_map
      t.startup_render_started_at
      ~default:Time_ns.Span.zero
      ~f:(fun started_at -> Time_ns.diff (Time_ns.now ()) started_at)
  in
  let fields =
    match t.startup_render_metrics with
    | Some (`Object fields) -> fields
    | None | Some (`Null | `False | `True | `Number _ | `String _ | `Array _) -> []
  in
  t.startup_render_metrics
  <- Some
       (`Object
           (fields
            @ [ ( "startup_loader_duration_ms"
                , `Number (Float.to_string (Time_ns.Span.to_ms loader_duration)) )
              ; ( "publication_latency_ms"
                , `Number (Float.to_string (Time_ns.Span.to_ms publication_latency)) )
              ]));
  t.startup_render_started_at <- None
;;

let arm_startup_render t ~domain_mgr ~config ~code_cache_capacity =
  t.startup_render_started_at <- Some (Time_ns.now ());
  Model.set_chat_materialization_loading t.model;
  Model.set_normal_input_enabled t.model false;
  t.startup_render
  <- Warming
       { config
       ; domain_mgr
       ; code_cache_capacity
       ; generation = 0
       ; cancel = Atomic.make false
       ; snapshot = None
       ; jobs = []
       }
;;

let startup_render_is_warming t =
  match t.startup_render with
  | Synchronous -> false
  | Warming _ -> true
;;

let begin_startup_render t =
  match t.startup_render with
  | Synchronous -> None
  | Warming state ->
    Atomic.set state.cancel true;
    let cancel = Atomic.make false in
    state.cancel <- cancel;
    state.generation <- state.generation + 1;
    let theme_generation =
      Chat_render_worker_runtime.Config.theme_generation state.config
    in
    let grammar_generation =
      Chat_render_worker_runtime.Config.grammar_generation state.config
    in
    let snapshot =
      Chat_startup_render.snapshot ~model:t.model ~theme_generation ~grammar_generation
    in
    state.snapshot <- snapshot;
    let jobs =
      Option.value_map snapshot ~default:[] ~f:(fun _ ->
        Renderer_page_chat.startup_background_jobs
          ~theme_generation
          ~grammar_generation
          ~model:t.model)
    in
    state.jobs <- jobs;
    Some
      ( state.generation
      , state.domain_mgr
      , state.config
      , state.code_cache_capacity
      , cancel
      , snapshot
      , jobs )
;;

let update_startup_render_config t config =
  match t.startup_render with
  | Synchronous -> ()
  | Warming state -> state.config <- config
;;

let startup_render_metrics t = t.startup_render_metrics

let startup_render_accepts t ~generation snapshot =
  match t.startup_render with
  | Synchronous -> false
  | Warming state ->
    Int.equal state.generation generation
    && Option.exists state.snapshot ~f:(Poly.equal snapshot)
    && Chat_startup_render.snapshot_is_current snapshot ~model:t.model
;;

let startup_render_generation_is_current t generation =
  match t.startup_render with
  | Synchronous -> false
  | Warming state -> Int.equal state.generation generation
;;

let alloc_op_id t =
  let id = t.next_op_id in
  t.next_op_id <- id + 1;
  id
;;

let streaming_op_id t =
  match t.op with
  | Some (Streaming { id; _ } | Starting_streaming { id }) -> Some id
  | Some (Compacting _ | Starting_compaction _) | None -> None
;;

let toggle_pending_agent t =
  match streaming_op_id t with
  | None -> false
  | Some id ->
    t.pending_agent_toggle
    <- (match t.pending_agent_toggle with
        | Some pending when Int.equal pending id -> None
        | None | Some _ -> Some id);
    true
;;

let consume_pending_agent t ~op_id =
  match t.pending_agent_toggle with
  | Some pending when Int.equal pending op_id ->
    t.pending_agent_toggle <- None;
    true
  | None | Some _ -> false
;;

let clear_pending_agent t ~op_id =
  match t.pending_agent_toggle with
  | Some pending when Int.equal pending op_id -> t.pending_agent_toggle <- None
  | None | Some _ -> ()
;;

let has_active_turn t =
  match t.op with
  | Some (Streaming _ | Starting_streaming _) -> true
  | Some (Compacting _ | Starting_compaction _) | None -> false
;;

let has_active_op t = Option.is_some t.op
let is_idle t = not (has_active_op t)

let is_moderator_startup_ready t =
  match t.moderator_startup_state with
  | Ready -> true
  | Starting | Failed _ -> false
;;

let is_moderator_starting t =
  match t.moderator_startup_state with
  | Starting -> true
  | Ready | Failed _ -> false
;;

let moderator_startup_state t = t.moderator_startup_state
let complete_moderator_startup t = t.moderator_startup_state <- Ready
let fail_moderator_startup t message = t.moderator_startup_state <- Failed message

let may_start_turn_now t =
  is_idle t && is_moderator_startup_ready t && Option.is_none t.halted_reason
;;

let runtime_policy t =
  Option.value_map
    t.moderator
    ~default:Runtime_semantics.default_policy
    ~f:(fun moderator -> moderator.runtime_policy)
;;

let is_moderator_dirty t = t.session_controller.moderator_dirty
let pending_overlay_revision t = t.session_controller.pending_overlay_revision
let projected_overlay_revision t = t.session_controller.projected_overlay_revision
let has_pending_turn_request t = Option.is_some t.session_controller.pending_turn_request
let pending_input t = t.pending_input
let has_pending_input t = Option.is_some t.pending_input

let string_of_turn_start_reason = function
  | User_submit -> "user_submit"
  | Moderator_request -> "moderator_request"
  | Idle_followup -> "idle_followup"
;;

let active_turn_start_reason t = t.active_turn_start_reason

let is_followup_turn_reason = function
  | User_submit -> false
  | Moderator_request | Idle_followup -> true
;;

let has_pause_condition
      (policy : Runtime_semantics.policy)
      (condition : Runtime_semantics.pause_condition)
  =
  List.exists policy.budget.pause_conditions ~f:(fun candidate ->
    match condition, candidate with
    | Runtime_semantics.Pause_followup_turns, Runtime_semantics.Pause_followup_turns ->
      true
    | ( Runtime_semantics.Pause_internal_event_drains
      , Runtime_semantics.Pause_internal_event_drains ) -> true
    | ( Runtime_semantics.Pause_followup_turns
      , Runtime_semantics.Pause_internal_event_drains )
    | ( Runtime_semantics.Pause_internal_event_drains
      , Runtime_semantics.Pause_followup_turns ) -> false)
;;

let should_pause_internal_event_drains ~(policy : Runtime_semantics.policy) =
  has_pause_condition policy Runtime_semantics.Pause_internal_event_drains
;;

let decide_automatic_turn
      ~(policy : Runtime_semantics.policy)
      ~(followup_turns_started_since_user_submit : int)
      ~(started_followup_turn_timestamps_ms : int list)
      ~(now_ms : int)
      ~(reason : turn_start_reason)
  : automatic_turn_decision
  =
  if not (is_followup_turn_reason reason)
  then Allow_automatic_turn
  else if has_pause_condition policy Runtime_semantics.Pause_followup_turns
  then
    Suppress_automatic_turn
      { notice_key = "budget:pause-followup-turns"
      ; notice_text = "Automatic follow-up turns are paused by budget policy."
      }
  else (
    let suppress_for_count () =
      if followup_turns_started_since_user_submit >= policy.budget.max_followup_turns
      then
        Suppress_automatic_turn
          { notice_key = "budget:max-followup-turns"
          ; notice_text =
              "Automatic follow-up turn suppressed after reaching the follow-up limit."
          }
      else Allow_automatic_turn
    in
    match policy.budget.turn_rate_limit with
    | None -> suppress_for_count ()
    | Some { max_turns; window_ms } ->
      let cutoff_ms = now_ms - window_ms in
      let recent_turn_count =
        List.count started_followup_turn_timestamps_ms ~f:(fun started_ms ->
          started_ms >= cutoff_ms)
      in
      if recent_turn_count >= max_turns
      then
        Suppress_automatic_turn
          { notice_key = "budget:turn-rate-limit"
          ; notice_text =
              "Automatic follow-up turn suppressed by the follow-up rate limit."
          }
      else suppress_for_count ())
;;

let mark_moderator_dirty t = t.session_controller.moderator_dirty <- true
let clear_moderator_dirty t = t.session_controller.moderator_dirty <- false

let note_overlay_revision t revision =
  let state = t.session_controller in
  if revision > state.projected_overlay_revision
  then
    state.pending_overlay_revision
    <- Some
         (Option.value_map
            state.pending_overlay_revision
            ~default:revision
            ~f:(Int.max revision))
;;

let refresh_pending_overlay ?(viewport_height = 0) t =
  match t.session_controller.pending_overlay_revision with
  | None -> None
  | Some revision when not (is_idle t) -> None
  | Some revision ->
    let damage = refresh_messages ~viewport_height t in
    t.session_controller.projected_overlay_revision <- revision;
    t.session_controller.pending_overlay_revision <- None;
    Some damage
;;

let note_current_overlay_projected t =
  Option.iter t.moderator ~f:(fun moderator ->
    let revision = Manager.overlay_revision moderator.manager in
    t.session_controller.projected_overlay_revision <- revision;
    t.session_controller.pending_overlay_revision
    <- Option.filter t.session_controller.pending_overlay_revision ~f:(fun pending ->
         pending > revision))
;;

let request_turn_start t reason = t.session_controller.pending_turn_request <- Some reason
let clear_pending_turn_request t = t.session_controller.pending_turn_request <- None

let dequeue_pending_turn_request t =
  let pending_turn_request = t.session_controller.pending_turn_request in
  t.session_controller.pending_turn_request <- None;
  pending_turn_request
;;

let note_started_turn t ~(now_ms : int) ~(reason : turn_start_reason) =
  let state = t.session_controller in
  match reason with
  | User_submit -> state.started_followup_turns_since_user_submit <- 0
  | Moderator_request | Idle_followup ->
    state.started_followup_turns_since_user_submit
    <- state.started_followup_turns_since_user_submit + 1;
    (match (runtime_policy t).budget.turn_rate_limit with
     | None -> ()
     | Some { window_ms; _ } ->
       let cutoff_ms = now_ms - window_ms in
       state.started_followup_turn_timestamps_ms
       <- List.filter state.started_followup_turn_timestamps_ms ~f:(fun started_ms ->
            started_ms >= cutoff_ms);
       state.started_followup_turn_timestamps_ms
       <- state.started_followup_turn_timestamps_ms @ [ now_ms ])
;;

let set_active_turn_start_reason t reason = t.active_turn_start_reason <- Some reason
let clear_active_turn_start_reason t = t.active_turn_start_reason <- None

let sync_pending_input t =
  Option.iter t.session_state ~f:(fun state ->
    let snapshot = Model.shell_security_snapshot t.model in
    Model.set_shell_security_snapshot
      t.model
      { snapshot with
        manifest_grants = (!state).Session.shell_state.manifest_grants
      ; grants = (!state).Session.shell_state.approval_grants
      ; interrupted_requests = (!state).shell_state.interrupted_requests
      });
  let moderator_request =
    Option.bind t.moderator ~f:Stream_moderator.pending_ui_request
  in
  let next =
    match Option.bind t.shell_approval_broker ~f:Shell_broker.pending with
    | Some request -> Some (Shell request)
    | None -> Option.map moderator_request ~f:(fun request -> Moderator request)
  in
  let modal_changed =
    match next, t.shell_approval_broker with
    | Some (Shell request), Some broker ->
      let previous_queue_count =
        Option.map (Model.shell_approval_modal t.model) ~f:(fun modal -> modal.queue_count)
      in
      let queue_count = Shell_broker.pending_count broker in
      Model.open_shell_approval_modal t.model ~request ~queue_count;
      not (Option.equal Int.equal previous_queue_count (Some queue_count))
    | (None | Some (Moderator _)), _ | Some (Shell _), None ->
      let was_open = Option.is_some (Model.shell_approval_modal t.model) in
      Model.close_shell_approval_modal t.model;
      was_open
  in
  let moderator_modal_changed =
    match moderator_request with
    | Some request ->
      let was_open = Option.is_some (Model.moderator_modal t.model) in
      Model.open_moderator_modal t.model request;
      not was_open
    | None ->
      let was_open = Option.is_some (Model.moderator_modal t.model) in
      Model.close_moderator_modal t.model;
      was_open
  in
  let pending_changed = not (pending_input_equal t.pending_input next) in
  if pending_changed then t.pending_input <- next;
  pending_changed || modal_changed || moderator_modal_changed
;;

let resume_pending_input t ~response =
  match t.moderator with
  | None -> Error "Session is not waiting for UI input."
  | Some moderator -> Stream_moderator.resume_ui_request moderator ~response
;;

let respond_shell_approval t request response =
  match t.shell_approval_broker with
  | None -> Error Shell_broker.Closed
  | Some broker -> Shell_broker.respond broker ~id:request.Shell_broker.id response
;;

let respond_shell_approval_by_id t ~id response =
  match t.shell_approval_broker with
  | None -> Error Shell_broker.Closed
  | Some broker -> Shell_broker.respond broker ~id response
;;

let cancel_pending_input t =
  match t.pending_input with
  | Some (Shell request) ->
    Option.iter t.shell_approval_broker ~f:(fun broker ->
      Shell_broker.cancel broker ~id:request.id)
  | Some (Moderator _) | None -> ()
;;

let add_placeholder_message t ~role ~text =
  ignore (Model.apply_patch t.model (Add_placeholder_message { role; text }))
;;

let add_system_notice t text = add_placeholder_message t ~role:"system" ~text

let add_system_notice_once t ~key text =
  if Hash_set.mem t.shown_notice_keys key
  then false
  else (
    Hash_set.add t.shown_notice_keys key;
    add_system_notice t text;
    true)
;;

let enqueue_deferred_user_note t (submit_request : submit_request) =
  let text = String.strip submit_request.text in
  if String.is_empty text
  then Ok false
  else (
    match submit_request.draft_mode with
    | Model.Raw_xml ->
      Error "Raw XML cannot be submitted while an assistant turn is active."
    | Model.Plain ->
      let item =
        Openai.Responses.Item.Input_message
          { role = Openai.Responses.Input_message.User
          ; content =
              [ Openai.Responses.Input_message.Text { text; _type = "input_text" } ]
          ; _type = "message"
          }
      in
      let entry =
        History_entry.create ~allocator:t.history_allocator item |> Result.ok_or_failwith
      in
      Queue.enqueue t.session_controller.deferred_user_notes { entry };
      Ok true)
;;

let has_deferred_user_notes t =
  not (Queue.is_empty t.session_controller.deferred_user_notes)
;;

let dequeue_deferred_user_notes t =
  let rec loop acc =
    match Queue.dequeue t.session_controller.deferred_user_notes with
    | None -> List.rev acc
    | Some note -> loop (note :: acc)
  in
  loop []
;;

let render_deferred_user_note ({ entry } : deferred_user_note) =
  match History_entry.item entry with
  | Openai.Responses.Item.Input_message message ->
    List.filter_map message.content ~f:(function
      | Text { text; _ } -> Some text
      | Image _ -> None)
    |> String.concat ~sep:"\n"
  | _ -> ""
;;

let safe_point_input_source t =
  Stream_moderator.Safe_point_input.
    { consume_entries =
        (fun () -> dequeue_deferred_user_notes t |> List.map ~f:(fun note -> note.entry))
    ; consume_compatibility_text = (fun () -> None)
    }
;;
