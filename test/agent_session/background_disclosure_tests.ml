open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module M = Chat_response.Moderator_manager
module Jobs = Agent_session.Script_job_service

let%expect_test
    "background event checks current job disclosure before the compiled handler can \
     observe its result"
  =
  List.iter [ false; true ] ~f:(fun revoke ->
    let root = invocation_fixture () in
    with_actor
      ~prepare_state:(fun state -> { state with invocations = [ root ] })
      (fun env _ actor _ _ ->
         let calls = ref 0 in
         let full = native_registry calls ~raises:false in
         let registry = ref full in
         let manager, _, _ =
           handoff_definition
             env
             ~capability_registry:full
             ~declare_tool:false
             ~events:
               {| | `Internal_event(payload) ->
            let ignored = state[0] <- state[0] + 1 in Task.pure(state)
            | _ -> Task.pure(state) |}
         in
         let before = M.identity_snapshot manager |> Result.ok_or_failwith in
         let source = M.invocation_observer manager |> Option.value_exn in
         let encode = Agent_session.Runtime_builder.encode_moderator_snapshot in
         A.change_moderator actor (Some (encode before)) |> protocol_ok |> ignore;
         let request =
           Background_execution_tests.capture_tool
             full
             Chat_response.One_off_request.default_policy
         in
         let job : P.Job.t =
           { id = P.Id.Job.create ()
           ; session_id
           ; generation = 0
           ; kind = Async_tool
           ; payload = Chat_response.Background_request.to_json request
           ; status = Succeeded
           ; retry_policy = Never
           ; attempt = 1
           ; created_at = timestamp
           ; started_at = Some timestamp
           ; next_run_at = None
           ; completed_at = Some timestamp
           ; result =
               Some
                 (P.Completion.to_json
                    (Succeeded (`String "PRIVATE-COMPLETION-SENTINEL")))
           ; delivery = Pending
           ; progress = None
           ; launch =
               Some
                 { owner = Invocation root.context.id
                 ; parent_job = None
                 ; nested_depth = 0
                 ; moderator_source = Some source
                 }
           }
         in
         A.add_job actor job |> protocol_ok |> ignore;
         let frame =
           Chat_response.Background_delivery.create ~source job
           |> Result.ok_or_failwith
           |> Chat_response.Background_delivery.capture
         in
         M.enqueue_internal_event_entries
           manager
           ~event:frame
           ~prepare:(fun ~before ~snapshot ->
             A.deliver_job
               ~expected:before
               ~expected_job:job
               actor
               ~job_id:job.id
               ~generation:0
               ~moderator_snapshot:(Some (encode snapshot))
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error -> error.P.Error.message))
         |> Result.ok_or_failwith
         |> ignore;
         let host : Jobs.host =
           { stage = (fun _ _ -> failwith "disclosure check started work")
           ; select = (fun owner ids -> A.select_background_jobs actor ~owner ~ids)
           ; abort =
               (fun owner id -> A.abort_background_job actor ~owner ~id |> protocol_ok)
           ; get = (fun owner id -> A.read_script_job actor ~owner ~id)
           ; materialize =
               (fun owner expected -> A.read_script_job_result actor ~owner ~expected)
           ; cancel = (fun owner id -> A.cancel_script_job actor ~owner ~id)
           }
         in
         let jobs =
           Jobs.create
             ~env
             ~policy:Chat_response.One_off_request.default_policy
             ~current_capabilities:(fun () -> !registry)
             ~host
         in
         let tools =
           Background_execution_tests.tools (fun () -> !registry)
           |> fun tools -> Agent_session.Script_tool_calls.with_job_service tools jobs
         in
         (match revoke with
          | false -> ()
          | true -> registry := C.select full ~names:[] |> Background_execution_tests.cap);
         let result =
           Agent_session.Moderator_event.run_queued_idle
             ~claim:(A.with_current_idle_queued_moderator_event_tools actor)
             ~script_tools:tools
             ~manager
             ~history:(fun () -> [])
             ~available_tools:[]
             ~session_meta:`Null
             ~now:(fun () -> timestamp)
             ()
         in
         let state = A.state actor |> protocol_ok in
         [%test_eq: int] 0 !calls;
         let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
         (match revoke, result with
          | false, Ok (Some _) ->
            [%test_eq: int] 0 (List.length snapshot.queued_internal_events);
            (match snapshot.current_state with
             | Array [ Int 1 ] -> ()
             | _ -> failwith "authorized handler did not run")
          | true, Error _ ->
            assert (
              Sexp.equal
                (Session.Snapshot.sexp_of_t before.current_state)
                (Session.Snapshot.sexp_of_t snapshot.current_state));
            [%test_eq: int] 1 (List.length snapshot.queued_internal_events);
            [%test_eq: int]
              1
              (List.count state.moderator_executions ~f:(fun event ->
                 match event.context.phase, event.status with
                 | Internal_event, Failed _ -> true
                 | _ -> false))
          | _ -> failwith "unexpected background disclosure result");
         let retained =
           List.find_exn state.jobs ~f:(fun current -> P.Id.Job.equal job.id current.id)
         in
         assert (Option.equal Jsonaf.exactly_equal retained.result job.result);
         print_s
           [%sexp
             (revoke : bool)
           , (if revoke
              then "withheld before handler"
              else "handler observed its owned completion"
              : string)]));
  [%expect
    {|
    (false "handler observed its owned completion")
    (true "withheld before handler")
    |}]
;;
