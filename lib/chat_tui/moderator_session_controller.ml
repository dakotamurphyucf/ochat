open! Core

module Moderation = Chat_response.Moderation
module Manager = Chat_response.Moderator_manager
module Stream_moderator = Chat_response.In_memory_stream
module Runtime = App_runtime
module Runtime_semantics = Chat_response.Runtime_semantics

type turn_request =
  | Ignore
  | Schedule of Runtime.turn_start_reason

type t =
  { request_refresh : bool
  ; request_compact : bool
  ; request_turn : Runtime.turn_start_reason option
  ; halt_reason : string option
  ; system_notices : string list
  ; internal_events_remain : bool
  ; internal_events_to_enqueue : App_events.internal_event list
  }

let system_notices_of_halt_reason = function
  | None -> []
  | Some reason -> [ Printf.sprintf "Session ended by moderator: %s" reason ]
;;

let request_turn_action ~policy ~turn_request ~halt_reason requests =
  match halt_reason, turn_request with
  | Some _, _ | None, Ignore -> None
  | None, Schedule reason ->
    if policy.Runtime_semantics.honor_request_turn
       && Runtime_semantics.request_turn requests
    then Some reason
    else None
;;

let internal_events_to_enqueue ~request_compact ~request_turn =
  let compact_event =
    if request_compact then [ `Compact_requested ] else []
  in
  let turn_event =
    Option.value_map request_turn ~default:[] ~f:(fun reason -> [ `Start_turn reason ])
  in
  compact_event @ turn_event
;;

let request_refresh_of_outcomes outcomes =
  List.exists outcomes ~f:(fun (outcome : Moderation.Outcome.t) ->
    not (List.is_empty outcome.overlay_ops))
;;

let ui_notifications_of_outcomes outcomes =
  List.concat_map outcomes ~f:(fun (outcome : Moderation.Outcome.t) -> outcome.ui_notifications)
;;

let runtime_requests_of_outcomes outcomes =
  List.concat_map outcomes ~f:(fun (outcome : Moderation.Outcome.t) -> outcome.runtime_requests)
;;

let of_runtime_requests ~policy ~turn_request requests =
  let requests = Runtime_semantics.collapse requests in
  let halt_reason = Runtime_semantics.should_end_session requests in
  let request_compact =
    policy.Runtime_semantics.honor_request_compaction
    && Runtime_semantics.request_compaction requests
  in
  let request_turn =
    request_turn_action ~policy ~turn_request ~halt_reason requests
  in
  let system_notices = system_notices_of_halt_reason halt_reason in
  let internal_events_to_enqueue =
    internal_events_to_enqueue ~request_compact ~request_turn
  in
  { request_refresh = false
  ; request_compact
  ; request_turn
  ; halt_reason
  ; system_notices
  ; internal_events_remain = false
  ; internal_events_to_enqueue
  }
;;

let of_runtime_request ~policy ~turn_request request =
  of_runtime_requests ~policy ~turn_request [ request ]
;;

let of_outcomes ~policy ~turn_request outcomes =
  let requests = runtime_requests_of_outcomes outcomes in
  let outcome = of_runtime_requests ~policy ~turn_request requests in
  { outcome with
    request_refresh = request_refresh_of_outcomes outcomes
  ; system_notices = outcome.system_notices @ ui_notifications_of_outcomes outcomes
  }
;;

let drain_internal_events
      ~(moderator : Stream_moderator.moderator)
      ~now_ms
      ~history
      ~available_tools
      ~turn_request
  =
  let open Result.Let_syntax in
  let%bind outcomes =
    Manager.drain_internal_events
      ~max_events:moderator.runtime_policy.budget.max_internal_event_drain
      moderator.manager
      ~session_id:moderator.session_id
      ~now_ms
      ~history
      ~available_tools
      ~session_meta:moderator.session_meta
  in
  let%map snapshot = Manager.snapshot moderator.manager in
  let internal_events_remain =
    not (List.is_empty snapshot.Session.Moderator_snapshot.queued_internal_events)
  in
  let outcome = of_outcomes ~policy:moderator.runtime_policy ~turn_request outcomes in
  { outcome with internal_events_remain }
;;

let%test_unit "end_session suppresses scheduled turn" =
  let policy =
    { Runtime_semantics.default_policy with honor_request_compaction = true }
  in
  let outcome =
    of_runtime_requests
      ~policy
      ~turn_request:(Schedule Runtime.Idle_followup)
      [ Moderation.Runtime_request.Request_turn
      ; Moderation.Runtime_request.Request_compaction
      ; Moderation.Runtime_request.End_session "done"
      ]
  in
  [%test_result: bool] outcome.request_compact ~expect:true;
  [%test_result: bool] (Option.is_none outcome.request_turn) ~expect:true;
  [%test_result: string option] outcome.halt_reason ~expect:(Some "done");
  [%test_result: string list]
    outcome.system_notices
    ~expect:[ "Session ended by moderator: done" ]
;;

let%test_unit "outcomes request refresh when overlay changes" =
  let outcome =
    of_outcomes
      ~policy:Runtime_semantics.default_policy
      ~turn_request:Ignore
      [ { Moderation.Outcome.empty with
          overlay_ops = [ Moderation.Overlay.Prepend_system "policy" ]
        }
      ]
  in
  [%test_result: bool] outcome.request_refresh ~expect:true
;;

let%test_unit "outcomes surface ui notifications as system notices" =
  let outcome =
    of_outcomes
      ~policy:Runtime_semantics.default_policy
      ~turn_request:Ignore
      [ { Moderation.Outcome.empty with ui_notifications = [ "watch this" ] } ]
  in
  [%test_result: string list] outcome.system_notices ~expect:[ "watch this" ]
;;

let%test_unit "request_turn can be ignored for active-turn paths" =
  let outcome =
    of_runtime_request
      ~policy:Runtime_semantics.default_policy
      ~turn_request:Ignore
      Moderation.Runtime_request.Request_turn
  in
  [%test_result: bool] (Option.is_none outcome.request_turn) ~expect:true
;;
