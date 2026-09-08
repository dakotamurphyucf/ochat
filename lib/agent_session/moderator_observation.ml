open Core
module P = Agent_protocol
module M = Chat_response.Moderator_manager

type result =
  { outcomes : Chat_response.Moderation.Outcome.t list
  ; budget_exhausted : bool
  }

let drain
      ?(max_observations = 32)
      ?on_tool_call
      ~capabilities
      ~observer
      ~manager
      ~history
      ~available_tools
      ~session_meta
      ~now
      ()
  =
  let open Result.Let_syntax in
  let%bind () =
    if max_observations < 1 || max_observations > 256
    then
      Error (P.Error.invalid_request "observation drain budget must be between 1 and 256")
    else Ok ()
  in
  let rec loop remaining outcomes =
    match remaining with
    | 0 -> Ok { outcomes = List.rev outcomes; budget_exhausted = true }
    | _ ->
      let outcome = ref None in
      let%bind claimed =
        capabilities.Operation_worker.Capabilities.with_next_moderator_observation
          ~observer
          (fun ~observing ~commit ->
             M.handle_observation_entries
               ?on_tool_call
               manager
               ~invocation:observing
               ~history:(history ())
               ~available_tools
               ~session_meta
               ~now_ms:
                 (P.Timestamp.to_time_ns (now ())
                  |> Time_ns.to_int_ns_since_epoch
                  |> fun n -> n / 1_000_000)
               ~prepare_observation:(fun ~observed ~outcome:prepared ~snapshot ->
                 commit ~resolved:observed ~snapshot
                 |> Result.map_error ~f:(fun e -> e.P.Error.message)
                 |> Result.map ~f:(fun () -> fun () -> outcome := Some prepared))
             |> Result.map ~f:(fun _ -> ())
             |> Result.map_error ~f:(fun _ ->
               P.Error.create
                 Invalid_state
                 ~message:"deferred tool observation failed"
                 ~retryable:false
                 ()))
      in
      (match claimed, !outcome with
       | false, None -> Ok { outcomes = List.rev outcomes; budget_exhausted = false }
       | true, Some outcome ->
         (match
            Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
          with
          | Some _ ->
            Ok { outcomes = List.rev (outcome :: outcomes); budget_exhausted = false }
          | None -> loop (remaining - 1) (outcome :: outcomes))
       | _ ->
         Error
           (P.Error.create
              Internal_error
              ~message:"observation handoff returned inconsistent completion"
              ~retryable:false
              ()))
  in
  loop max_observations []
;;
