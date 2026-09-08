open Core
module P = Agent_protocol
module M = Chat_response.Moderator_manager

type claim =
  snapshot:Session.Moderator_state.Identity_snapshot.t
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

let run_queued_idle
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
    | None ->
      Error (failed "queued event execution requires an extensibility-v1 moderator")
  in
  let%bind before = M.identity_snapshot manager |> Result.map_error ~f:failed in
  match before.queued_internal_events with
  | [] -> Ok None
  | _ :: _ ->
    let outcome = ref None in
    let%bind claimed =
      claim ~snapshot:before (fun ~executing ~event:selected ~execute ~commit ->
        let with_tools f =
          match script_tools with
          | Some script_tools ->
            Script_tool_calls.with_event script_tools ~definition ~execute ~executing f
          | None ->
            f (fun ~name:_ ~args:_ ->
              Ok
                (Chat_response.Moderation.Capabilities.Tool_error "invocation.unavailable"))
        in
        with_tools (fun on_tool_call ->
          M.handle_next_event_entries_transactional
            manager
            ~session_id:(P.Id.Session.to_string executing.context.session_id)
            ~now_ms:
              (P.Timestamp.to_time_ns (now ())
               |> Time_ns.to_int_ns_since_epoch
               |> fun n -> n / 1_000_000)
            ~history:(history ())
            ~available_tools
            ~session_meta
            ~authorize:(fun ~event ->
              match
                Sexp.equal
                  (Session.Snapshot.sexp_of_t selected)
                  (Session.Snapshot.sexp_of_t event)
              with
              | true -> Ok ()
              | false -> Error "queued event no longer matches its actor claim")
            ~on_tool_call
            ~prepare_event:(fun ~outcome:prepared ~snapshot ->
              let requests : P.Invocation.follow_up =
                { request_turn =
                    Chat_response.Runtime_semantics.request_turn prepared.runtime_requests
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
              |> Result.map ~f:(fun () -> fun () -> outcome := Some prepared))
          |> Result.map ~f:ignore
          |> Result.map_error ~f:(fun _ -> failed "queued moderator event failed")))
    in
    (match claimed, !outcome with
     | false, None -> Ok None
     | true, Some outcome -> Ok (Some outcome)
     | _ -> Error (failed "queued event handoff returned inconsistent completion"))
;;
