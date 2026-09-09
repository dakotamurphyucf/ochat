open Core
module P = Agent_protocol
module M = Chat_response.Moderator_manager

type claim =
  snapshot:(unit -> (Session.Moderator_state.Identity_snapshot.t, P.Error.t) result)
  -> (executing:P.Moderator_execution.t
      -> event:Session.Snapshot.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:P.Invocation.follow_up
            -> (unit, P.Error.t) result)
      -> (unit, P.Error.t) result)
  -> (bool, P.Error.t) result

let failed message = P.Error.create Invalid_state ~message ~retryable:false ()

type event =
  | Queued
  | Ordinary of Chat_response.Moderation.Event.t

let run
      ~event
      ~claim
      ?script_tools
      ~manager
      ~history
      ~available_tools
      ~session_meta
      ~now
      ()
  =
  let open Result.Let_syntax in
  let%bind definition =
    match M.extension_definition manager with
    | Some definition -> Ok definition
    | None -> Error (failed "event execution requires an extensibility-v1 moderator")
  in
  let%bind before = M.identity_snapshot manager |> Result.map_error ~f:failed in
  match event, before.queued_internal_events with
  | Queued, [] -> Ok None
  | Queued, _ :: _ | Ordinary _, _ ->
    let outcome = ref None in
    let%bind claimed =
      claim
        ~snapshot:(fun () -> M.identity_snapshot manager |> Result.map_error ~f:failed)
        (fun ~executing ~event:selected ~execute ~commit ->
           let with_tools f =
             match script_tools with
             | Some script_tools ->
               Script_tool_calls.with_event script_tools ~definition ~execute ~executing f
             | None ->
               f (fun ~name:_ ~args:_ ->
                 Ok
                   (Chat_response.Moderation.Capabilities.Tool_error
                      "invocation.unavailable"))
           in
           with_tools (fun on_tool_call ->
             let session_id = P.Id.Session.to_string executing.context.session_id in
             let now_ms =
               P.Timestamp.to_time_ns (now ())
               |> Time_ns.to_int_ns_since_epoch
               |> fun n -> n / 1_000_000
             in
             let history = history () in
             let authorize ~event =
               match
                 Sexp.equal
                   (Session.Snapshot.sexp_of_t selected)
                   (Session.Snapshot.sexp_of_t event)
               with
               | true -> Ok ()
               | false -> Error "event no longer matches its actor claim"
             in
             let prepare_event
                   ~outcome:(prepared : Chat_response.Moderation.Outcome.t)
                   ~snapshot
               =
               let requests : P.Invocation.follow_up =
                 { request_turn =
                     Chat_response.Runtime_semantics.request_turn
                       prepared.runtime_requests
                 ; request_compaction =
                     Chat_response.Runtime_semantics.request_compaction
                       prepared.runtime_requests
                 ; end_session =
                     Chat_response.Runtime_semantics.should_end_session
                       prepared.runtime_requests
                 }
               in
               commit ~snapshot ~requests
               |> Result.map_error ~f:(fun error -> error.P.Error.message)
               |> Result.map ~f:(fun () -> fun () -> outcome := Some prepared)
             in
             (match event with
              | Queued ->
                M.handle_next_event_entries_transactional
                  manager
                  ~session_id
                  ~now_ms
                  ~history
                  ~available_tools
                  ~session_meta
                  ~authorize
                  ~on_tool_call
                  ~prepare_event
                |> Result.map ~f:ignore
              | Ordinary event ->
                let open Result.Let_syntax in
                let%bind captured =
                  Chat_response.Moderation.Event.to_value event
                  |> Session.Snapshot.of_value
                in
                M.handle_event_entries_transactional
                  manager
                  ~session_id
                  ~now_ms
                  ~history
                  ~available_tools
                  ~session_meta
                  ~event
                  ~authorize:(fun () -> authorize ~event:captured)
                  ~on_tool_call
                  ~prepare_event
                |> Result.map ~f:ignore)
             |> Result.map_error ~f:(fun message ->
               Chatml.Chatml_debug_log.emitf "event_failed: %s" message;
               failed "moderator event failed")))
    in
    (match claimed, !outcome with
     | false, None -> Ok None
     | true, Some outcome -> Ok (Some outcome)
     | _ -> Error (failed "event handoff returned inconsistent completion"))
;;

let run_queued_idle = run ~event:Queued
let run_ordinary ~event = run ~event:(Ordinary event)

module Lifecycle = struct
  type state =
    | Ready
    | Completed
    | Failed of P.Error.t

  type t =
    { manager : M.t
    ; event : Chat_response.Moderation.Event.t
    ; gate : Chat_response.Execution_gate.t
    ; state : state Atomic.t
    }

  type outcome =
    | Unavailable
    | Activated of Chat_response.Moderation.Outcome.t
    | Already_active

  let create ~manager ~resume =
    { manager
    ; event =
        (match resume with
         | true -> Session_resume
         | false -> Session_start)
    ; gate = Chat_response.Execution_gate.create ()
    ; state = Atomic.make Ready
    }
  ;;

  let pending t =
    match Atomic.get t.state with
    | Ready -> true
    | Completed | Failed _ -> false
  ;;

  let run t ~claim ?script_tools ~history ~available_tools ~session_meta ~now () =
    match
      Chat_response.Execution_gate.with_access t.gate (fun () ->
        match Atomic.get t.state with
        | Completed -> Ok Already_active
        | Failed error -> Error error
        | Ready ->
          (match
             let open Result.Let_syntax in
             let%bind halted = M.is_halted t.manager |> Result.map_error ~f:failed in
             match halted with
             | true -> Ok Already_active
             | false ->
               run_ordinary
                 ~event:t.event
                 ~claim:(claim ~event:t.event)
                 ?script_tools
                 ~manager:t.manager
                 ~history
                 ~available_tools
                 ~session_meta
                 ~now
                 ()
               |> Result.map ~f:(function
                 | None -> Unavailable
                 | Some outcome -> Activated outcome)
           with
           | Ok Unavailable -> Ok Unavailable
           | Ok ((Activated _ | Already_active) as outcome) ->
             Atomic.set t.state Completed;
             Ok outcome
           | Error error ->
             Atomic.set t.state (Failed error);
             Error error
           | exception exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             Atomic.set t.state (Failed (failed "moderator activation was interrupted"));
             Exn.raise_with_original_backtrace exn backtrace))
    with
    | Ok result -> result
    | Error error -> Error (failed (Chat_response.Execution_gate.error_message error))
  ;;
end

let foreground_outcome (outcome : Chat_response.Moderation.Outcome.t) =
  let compact =
    Chat_response.Runtime_semantics.request_compaction outcome.runtime_requests
  in
  { outcome with
    runtime_requests =
      List.filter outcome.runtime_requests ~f:(function
        | Request_compaction -> false
        | Request_turn -> not compact
        | End_session _ -> true)
  }
;;

let foreground_handlers ?script_tools ~capabilities ~manager ~session_meta ~now () =
  let open Result.Let_syntax in
  let%bind definition =
    M.extension_definition manager
    |> Result.of_option
         ~error:(failed "foreground handlers require an extensibility-v1 moderator")
  in
  let%bind snapshot = M.identity_snapshot manager |> Result.map_error ~f:failed in
  let observer : P.Invocation.observer =
    { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  let%bind () =
    capabilities.Operation_worker.Capabilities.manage_moderator_follow_up ~observer
  in
  let halted () =
    let%map snapshot = M.identity_snapshot manager |> Result.map_error ~f:failed in
    match snapshot.halted with
    | false -> None
    | true ->
      Some
        { Chat_response.Moderation.Outcome.empty with
          runtime_requests =
            [ End_session
                (Option.value snapshot.halted_reason ~default:"moderator halted")
            ]
        }
  in
  let handle ~history ~available_tools ~now_ms:_ ~event =
    (let%bind stopped = halted () in
     match stopped with
     | Some outcome -> Ok (Some outcome)
     | None ->
       let result =
         run_ordinary
           ~event
           ~claim:(capabilities.with_moderator_event ~event)
           ?script_tools
           ~manager
           ~history:(fun () -> history)
           ~available_tools
           ~session_meta
           ~now
           ()
       in
       (match result with
        | Ok (Some outcome) -> Ok (Some (foreground_outcome outcome))
        | Ok None -> Error (failed "foreground moderator event was not admitted")
        | Error error ->
          (* Another callback may commit a halt while this one waits for ownership.
             Preserve that terminal decision rather than fail the owning operation
             for a hook that must no longer execute. Failed uncommitted handlers
             leave the manager unhalted and retain their original failure. *)
          let%bind stopped = halted () in
          (match stopped with
           | Some outcome -> Ok (Some outcome)
           | None -> Error error)))
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
  in
  let drain ~history ~available_tools ~now_ms:_ ~max_events =
    let rec loop remaining outcomes =
      match remaining with
      | 0 -> Ok (List.rev outcomes)
      | _ ->
        let%bind stopped = halted () in
        (match stopped with
         | Some outcome -> Ok (List.rev (outcome :: outcomes))
         | None ->
           let drain_observation =
             match script_tools with
             | Some script_tools ->
               Moderator_observation.drain_foreground_with_tools
                 ~max_observations:1
                 ~script_tools
                 ~definition
                 ~capabilities
                 ~observer
             | None ->
               Moderator_observation.drain_idle
                 ~max_observations:1
                 ~claim:(capabilities.with_next_moderator_observation ~observer)
                 ~on_tool_call:(fun ~name:_ ~args:_ ->
                   Ok
                     (Chat_response.Moderation.Capabilities.Tool_error
                        "invocation.unavailable"))
           in
           let%bind observed =
             drain_observation
               ~manager
               ~history:(fun () -> history)
               ~available_tools
               ~session_meta
               ~now
               ()
           in
           let%bind next =
             match observed.outcomes with
             | [ outcome ] -> Ok (Some outcome)
             | [] ->
               run
                 ~event:Queued
                 ~claim:capabilities.with_queued_moderator_event
                 ?script_tools
                 ~manager
                 ~history:(fun () -> history)
                 ~available_tools
                 ~session_meta
                 ~now
                 ()
             | _ -> Error (failed "observation drain exceeded its event budget")
           in
           (match next with
            | None -> Ok (List.rev outcomes)
            | Some outcome ->
              let outcome = foreground_outcome outcome in
              (match
                 Chat_response.Runtime_semantics.should_end_session
                   outcome.runtime_requests
               with
               | Some _ -> Ok (List.rev (outcome :: outcomes))
               | None -> loop (remaining - 1) (outcome :: outcomes))))
    in
    loop (Int.clamp_exn max_events ~min:0 ~max:256) []
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
  in
  Ok
    Chat_response.In_memory_stream.
      { handle
      ; drain
      ; before_model_call =
          (fun () ->
            capabilities.admit_moderator_turn ()
            |> Result.map_error ~f:(fun error -> error.P.Error.message))
      }
;;
