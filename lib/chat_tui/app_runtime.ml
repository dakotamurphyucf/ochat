open Core
open Eio.Std
module Manager = Chat_response.Moderator_manager
module Runtime_semantics = Chat_response.Runtime_semantics
module Stream_moderator = Chat_response.In_memory_stream

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

type deferred_user_note = { text : string }

type pending_approval = Stream_moderator.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

type automatic_turn_decision =
  | Allow_automatic_turn
  | Suppress_automatic_turn of
      { notice_key : string
      ; notice_text : string
      }

type session_controller_state =
  { mutable moderator_dirty : bool
  ; deferred_user_notes : deferred_user_note Queue.t
  ; mutable pending_turn_request : turn_start_reason option
  ; mutable started_followup_turns_since_user_submit : int
  ; mutable started_followup_turn_timestamps_ms : int list
  }

type queued_action =
  | Submit of submit_request
  | Compact

type t =
  { model : Model.t
  ; mutable op : op option
  ; mutable typeahead_op : typeahead_op option
  ; moderator : Stream_moderator.moderator option
  ; session_controller : session_controller_state
  ; shown_notice_keys : String.Hash_set.t
  ; mutable active_turn_start_reason : turn_start_reason option
  ; mutable halted_reason : string option
  ; mutable pending_approval : pending_approval option
  ; pending : queued_action Queue.t
  ; quit_via_esc : bool ref
  ; mutable next_op_id : int
  ; mutable cancel_streaming_on_start : bool
  ; mutable cancel_compaction_on_start : bool
  ; mutable cancel_typeahead_on_start : bool
  }

let pending_approval_equal left right =
  let choices_equal left right =
    let left = Array.to_list left in
    let right = Array.to_list right in
    List.equal String.equal left right
  in
  match left, right with
  | None, None -> true
  | Some (Ask_text { prompt = left }), Some (Ask_text { prompt = right }) ->
    String.equal left right
  | ( Some (Ask_choice { prompt = left_prompt; choices = left_choices })
    , Some (Ask_choice { prompt = right_prompt; choices = right_choices }) ) ->
    String.equal left_prompt right_prompt && choices_equal left_choices right_choices
  | Some _, Some _ | None, Some _ | Some _, None -> false
;;

let render_pending_approval = function
  | Ask_text { prompt } -> "Approval requested: " ^ prompt
  | Ask_choice { prompt; choices } ->
    "Approval requested: "
    ^ prompt
    ^ "\nChoices: "
    ^ String.concat ~sep:", " (Array.to_list choices)
;;

let visible_history_items_of_history (t : t) (history : Openai.Responses.Item.t list)
  : Openai.Responses.Item.t list
  =
  match t.moderator with
  | None -> history
  | Some moderator ->
    Manager.effective_history moderator.manager history
    |> Result.ok_or_failwith
;;

let visible_messages_of_history (t : t) (history : Openai.Responses.Item.t list)
  : Types.message list
  =
  let messages = visible_history_items_of_history t history |> Conversation.of_history in
  match t.pending_approval with
  | None -> messages
  | Some pending_approval ->
    messages @ [ "system", render_pending_approval pending_approval ]
;;

let refresh_messages (t : t) : unit =
  let history = Model.history_items t.model in
  let visible_history = visible_history_items_of_history t history in
  Model.set_messages t.model (visible_messages_of_history t history);
  Model.rebuild_tool_output_index_for_items t.model visible_history;
  Model.clamp_selected_message t.model;
  Model.clear_all_img_caches t.model
;;

let moderator_snapshot (t : t) : (Session.Moderator_snapshot.t option, string) result =
  match t.moderator with
  | None -> Ok None
  | Some moderator -> Result.map (Manager.snapshot moderator.manager) ~f:Option.some
;;

let create ?moderator ?halted_reason ~model () =
  { model
  ; op = None
  ; typeahead_op = None
  ; moderator
  ; session_controller =
      { moderator_dirty = false
      ; deferred_user_notes = Queue.create ()
      ; pending_turn_request = None
      ; started_followup_turns_since_user_submit = 0
      ; started_followup_turn_timestamps_ms = []
      }
  ; shown_notice_keys = Hash_set.create (module String)
  ; active_turn_start_reason = None
  ; halted_reason
  ; pending_approval = Option.bind moderator ~f:Stream_moderator.pending_ui_request
  ; pending = Queue.create ()
  ; quit_via_esc = ref false
  ; next_op_id = 0
  ; cancel_streaming_on_start = false
  ; cancel_compaction_on_start = false
  ; cancel_typeahead_on_start = false
  }
;;

let alloc_op_id t =
  let id = t.next_op_id in
  t.next_op_id <- id + 1;
  id
;;

let has_active_turn t =
  match t.op with
  | Some (Streaming _ | Starting_streaming _) -> true
  | Some (Compacting _ | Starting_compaction _)
  | None -> false
;;

let has_active_op t = Option.is_some t.op
let is_idle t = not (has_active_op t)
let may_start_turn_now t = is_idle t && Option.is_none t.halted_reason
let runtime_policy t = Option.value_map t.moderator ~default:Runtime_semantics.default_policy ~f:(fun moderator -> moderator.runtime_policy)
let is_moderator_dirty t = t.session_controller.moderator_dirty
let has_pending_turn_request t = Option.is_some t.session_controller.pending_turn_request
let pending_approval t = t.pending_approval
let has_pending_approval t = Option.is_some t.pending_approval
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

let request_turn_start t reason =
  t.session_controller.pending_turn_request <- Some reason
;;

let clear_pending_turn_request t =
  t.session_controller.pending_turn_request <- None
;;

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

let set_active_turn_start_reason t reason =
  t.active_turn_start_reason <- Some reason
;;

let clear_active_turn_start_reason t =
  t.active_turn_start_reason <- None
;;

let sync_pending_approval t =
  let next = Option.bind t.moderator ~f:Stream_moderator.pending_ui_request in
  if pending_approval_equal t.pending_approval next
  then false
  else (
    t.pending_approval <- next;
    true)
;;

let resume_pending_approval t ~response =
  match t.moderator with
  | None -> Error "Session is not waiting for UI input."
  | Some moderator -> Stream_moderator.resume_ui_request moderator ~response
;;

let add_placeholder_message t ~role ~text =
  ignore (Model.apply_patch t.model (Add_placeholder_message { role; text }))
;;

let add_system_notice t text =
  add_placeholder_message t ~role:"system" ~text
;;

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
  then false
  else (
    Queue.enqueue t.session_controller.deferred_user_notes { text };
    true)
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

let render_deferred_user_note ({ text } : deferred_user_note) =
  Printf.sprintf "This is a Note From the User:\n%s" text
;;

let render_deferred_user_notes (notes : deferred_user_note list) =
  match notes with
  | [] -> None
  | notes ->
    Some
      (List.map notes ~f:(fun note ->
         Printf.sprintf
           "\n<system-reminder>\n%s\n</system-reminder>\n"
           (render_deferred_user_note note))
       |> String.concat ~sep:"")
;;

let consume_deferred_user_notes_for_safe_point t =
  dequeue_deferred_user_notes t |> render_deferred_user_notes
;;

let safe_point_input_source t =
  Stream_moderator.Safe_point_input.
    { consume = (fun () -> consume_deferred_user_notes_for_safe_point t) }
;;
