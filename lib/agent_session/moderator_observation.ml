open Core
module P = Agent_protocol
module M = Chat_response.Moderator_manager

let failed message = P.Error.create Invalid_state ~message ~retryable:false ()

type result =
  { outcomes : Chat_response.Moderation.Outcome.t list
  ; budget_exhausted : bool
  }

let drain_with_claim
      ?(max_observations = 32)
      ~claim
      ~retain_follow_up
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
        claim (fun ~observing ~commit ~on_tool_call ~job_scope ->
          let jobs = Option.map job_scope ~f:Script_job_service.moderator_transaction in
          M.handle_observation_entries
            ?jobs
            ?on_tool_call
            ~retain_follow_up
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
              Ok
                { M.persist =
                    (fun () ->
                      commit ~resolved:observed ~snapshot
                      |> Result.map_error ~f:(fun e -> e.P.Error.message))
                ; install = (fun () -> outcome := Some prepared)
                })
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

let drain ?max_observations ?on_tool_call ~capabilities ~observer =
  drain_with_claim ?max_observations ~retain_follow_up:false ~claim:(fun handle ->
    capabilities.Operation_worker.Capabilities.with_next_moderator_observation
      ~observer
      (fun ~observing ~commit -> handle ~observing ~commit ~on_tool_call ~job_scope:None))
;;

let drain_idle ?max_observations ?on_tool_call ~claim =
  drain_with_claim ?max_observations ~retain_follow_up:true ~claim:(fun handle ->
    claim (fun ~observing ~commit ->
      handle ~observing ~commit ~on_tool_call ~job_scope:None))
;;

let drain_idle_with_tools ?max_observations ~script_tools ~definition ~claim =
  drain_with_claim ?max_observations ~retain_follow_up:true ~claim:(fun handle ->
    claim (fun ~(observing : P.Invocation.t) ~execute ~commit ->
      Script_tool_calls.with_job_scope
        script_tools
        ~owner:(P.Job.Invocation observing.context.id)
        ~selected:(Chat_response.Extension_compiler.definition_capabilities definition)
        ~error:failed
        (fun job_scope ->
           Script_tool_calls.with_observation
             script_tools
             ~definition
             ~execute
             ~observing
             (fun on_tool_call ->
                handle ~observing ~commit ~on_tool_call:(Some on_tool_call) ~job_scope))))
;;

let drain_foreground_with_tools
      ?max_observations
      ~script_tools
      ~definition
      ~capabilities
      ~observer
  =
  drain_with_claim ?max_observations ~retain_follow_up:true ~claim:(fun handle ->
    capabilities.Operation_worker.Capabilities.with_next_moderator_observation
      ~observer
      (fun ~observing ~commit ->
         Script_tool_calls.with_job_scope
           script_tools
           ~owner:(P.Job.Invocation observing.P.Invocation.context.id)
           ~selected:(Chat_response.Extension_compiler.definition_capabilities definition)
           ~error:failed
           (fun job_scope ->
              Script_tool_calls.with_observation
                script_tools
                ~definition
                ~execute:capabilities.with_invocation
                ~observing
                (fun on_tool_call ->
                   handle ~observing ~commit ~on_tool_call:(Some on_tool_call) ~job_scope))))
;;
