open! Core

type stop =
  | Quiescent
  | Halted
  | End_session_requested of string
  | Waiting_for_ui of Moderator_manager.pending_ui_request
  | Paused
  | Event_limit_reached

type t =
  { outcomes : Moderation.Outcome.t list
  ; runtime_requests : Moderation.Runtime_request.t list
  ; processed_events : int
  ; spawned_jobs : int
  ; stop : stop
  }

let run_entries
      ~executor
      ~(moderator : In_memory_stream.moderator)
      ~now_ms
      ~history
      ~available_tools
      ?on_runtime_request
      ()
  : (t, string) result
  =
  let max_events = moderator.runtime_policy.budget.max_internal_event_drain in
  if Option.is_some moderator.event_handlers
  then
    Error "Local model completion cannot bypass transactional moderator event handlers."
  else if max_events < 0
  then Error "Internal-event drain limit must be nonnegative."
  else (
    let paused =
      List.mem
        moderator.runtime_policy.budget.pause_conditions
        Runtime_semantics.Pause_internal_event_drains
        ~equal:Runtime_semantics.equal_pause_condition
    in
    let finish ~outcomes_rev ~runtime_requests_rev ~processed_events ~stop =
      Ok
        { outcomes = List.rev outcomes_rev
        ; runtime_requests = List.rev runtime_requests_rev
        ; processed_events
        ; spawned_jobs =
            Model_executor.session_job_count executor ~session_id:moderator.session_id
        ; stop
        }
    in
    let observe () =
      Moderator_manager.drain_internal_events_entries_bounded
        ~max_events:0
        moderator.manager
        ~session_id:moderator.session_id
        ~now_ms:(now_ms ())
        ~history
        ~available_tools
        ~session_meta:moderator.session_meta
    in
    let rec loop ~outcomes_rev ~runtime_requests_rev ~processed_events =
      let open Result.Let_syntax in
      let cursor =
        Model_executor.session_progress_cursor executor ~session_id:moderator.session_id
      in
      let%bind observed = observe () in
      match observed.stop with
      | Halted ->
        finish ~outcomes_rev ~runtime_requests_rev ~processed_events ~stop:Halted
      | Waiting_for_ui request ->
        finish
          ~outcomes_rev
          ~runtime_requests_rev
          ~processed_events
          ~stop:(Waiting_for_ui request)
      | Empty | Limit_reached ->
        if paused
        then finish ~outcomes_rev ~runtime_requests_rev ~processed_events ~stop:Paused
        else (
          match observed.stop with
          | Limit_reached ->
            let remaining = max_events - processed_events in
            if remaining = 0
            then
              finish
                ~outcomes_rev
                ~runtime_requests_rev
                ~processed_events
                ~stop:Event_limit_reached
            else (
              let%bind drained =
                Moderator_manager.drain_internal_events_entries_bounded
                  ~max_events:1
                  moderator.manager
                  ~session_id:moderator.session_id
                  ~now_ms:(now_ms ())
                  ~history
                  ~available_tools
                  ~session_meta:moderator.session_meta
              in
              let outcomes_rev = List.rev_append drained.outcomes outcomes_rev in
              let new_requests =
                List.concat_map drained.outcomes ~f:(fun outcome ->
                  outcome.runtime_requests)
              in
              Option.iter on_runtime_request ~f:(fun on_runtime_request ->
                List.iter new_requests ~f:on_runtime_request);
              let runtime_requests_rev =
                List.rev_append new_requests runtime_requests_rev
              in
              let processed_events = processed_events + List.length drained.outcomes in
              match Runtime_semantics.should_end_session new_requests with
              | Some reason ->
                finish
                  ~outcomes_rev
                  ~runtime_requests_rev
                  ~processed_events
                  ~stop:(End_session_requested reason)
              | None -> loop ~outcomes_rev ~runtime_requests_rev ~processed_events)
          | Empty ->
            let%bind progress = Model_executor.await_session_progress executor cursor in
            (match progress with
             | Progressed -> loop ~outcomes_rev ~runtime_requests_rev ~processed_events
             | All_completed ->
               let%bind _settled =
                 Model_executor.settle_session executor ~session_id:moderator.session_id
               in
               let%bind after = observe () in
               (match after.stop with
                | Empty ->
                  finish
                    ~outcomes_rev
                    ~runtime_requests_rev
                    ~processed_events
                    ~stop:Quiescent
                | Halted ->
                  finish
                    ~outcomes_rev
                    ~runtime_requests_rev
                    ~processed_events
                    ~stop:Halted
                | Waiting_for_ui request ->
                  finish
                    ~outcomes_rev
                    ~runtime_requests_rev
                    ~processed_events
                    ~stop:(Waiting_for_ui request)
                | Limit_reached ->
                  loop ~outcomes_rev ~runtime_requests_rev ~processed_events))
          | Halted | Waiting_for_ui _ -> assert false)
    in
    loop ~outcomes_rev:[] ~runtime_requests_rev:[] ~processed_events:0)
;;
