open Core
open Fixtures
open Job_fixtures
module B = Chat_response.Background_request
module Scheduler = Agent_server.Job_scheduler
module Owner = Agent_server.Runtime_owner
module Registry = Agent_server.Session_registry
module DR = Agent_server.Developer_root_owner
module DS = Agent_store.Developer_run_store
module SS = Agent_store.Session_store
module DT = Developer_root_owner_test_suite.Root_owner_tests
module DN = Developer_root_owner_test_suite.Native_executor_tests
module DC = Chat_response.Tool_capability
module DSelected = Developer_run_store_test_suite.Developer_run_selected_tests

let with_capacity_scheduler
      ?(retry_once = false)
      ?(on_conversation_call = ignore)
      ?capacity_limits
      ~reject_save
      f
  =
  with_actor ~reject_save (fun env sw actor _writer backend ->
    let calls = ref 0 in
    let capabilities =
      native_registry ~on_call:on_conversation_call calls ~raises:false
    in
    let capabilities =
      match retry_once with
      | false -> capabilities
      | true ->
        let native =
          C.find capabilities ~name:"read_file"
          |> Background_execution_tests.cap
          |> C.native_implementation
          |> Option.value_exn
        in
        let implementation =
          { native with
            run_with_progress =
              (fun ~invocation input ->
                native.run_with_progress ~invocation input |> ignore;
                let outcome =
                  match !calls with
                  | 1 ->
                    I.Fail
                      { code = "fixture.busy"
                      ; message = "try again"
                      ; retryable = true
                      ; details = `Null
                      }
                  | _ -> I.Complete `Null
                in
                Openai.Responses.Tool_output.Output.Text
                  (Jsonaf.to_string (I.outcome_to_json outcome)))
          }
        in
        C.create
          ~owner:"scheduler-fixture"
          ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
          ~result_contracts:[ "read_file", Invocation_v1 ]
          [ Chatmd_shell_spec.Source_ref.digest "structured-v1", implementation ]
        |> Background_execution_tests.cap
    in
    let script_tools = Background_execution_tests.tools (fun () -> capabilities) in
    let policy = Chat_response.One_off_request.default_policy in
    let executor : Agent_session.Runtime_builder.background_executor =
      { policy
      ; now = (fun () -> timestamp)
      ; run =
          (fun ~job
            ~deadline
            ~execute
            ~moderator_execute
            ~claim_event:_
            ~is_halted:_
            ~request ->
            Agent_session.Background_execution.run
              ~env
              ~job
              ~deadline
              ~execute
              ~moderator_execute
              ~request
              ~policy
              ~script_tools
              ~now:(fun () -> timestamp)
              ~moderate_tool:(fun _ _ -> Ok None)
              ~prepare_outcome:(fun _ -> Ok ())
              ())
      }
    in
    let runtime =
      { (Runtime_lease_tests.runtime ~script_tools ~close:ignore ()) with
        background_executor = Some executor
      }
    in
    let owner =
      Owner.create ~actor ~initial:(Some runtime) ~build:(fun () -> Ok runtime)
    in
    let registry = Registry.create () in
    let history_ids =
      Agent_session.History_id_source.create
        ~namespace:"scheduler"
        ~block_size:8
        ~reserve:(fun ~count:_ ->
          failwith "background completion must not allocate history")
      |> protocol_ok
    in
    let entry : Registry.entry =
      { actor
      ; history_ids
      ; runtime = owner
      ; durable_events =
          Agent_session.Durable_event_log.create ~capacity:128 [] |> protocol_ok
      ; capacity = None
      ; store_handle = None
      ; expire_permissions = (fun ~now:_ -> ())
      ; collect_results = (fun () -> Ok None)
      ; close = ignore
      }
    in
    Registry.add registry ~session_id entry |> protocol_ok;
    let default_limits : Agent_server.Config.Server.job_limits =
      { daemon_total = 1
      ; per_principal = 1
      ; per_prompt = 1
      ; per_workspace = 1
      ; per_session = 1
      ; per_kind = 1
      ; max_nested_depth = 1
      }
    in
    let capacity =
      Agent_server.Job_capacity.create
        ~limits:(Option.value capacity_limits ~default:default_limits)
    in
    let start () =
      Scheduler.start
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~registry
        ~developer_registry:None
        ~capacity
        ~model_job_inference:(fun _ _ -> failwith "unexpected model job inference")
    in
    let scheduler = start () in
    Exn.protect
      ~finally:(fun () ->
        Scheduler.close scheduler;
        Owner.close_and_wait owner)
      ~f:(fun () ->
        f
          env
          actor
          backend
          calls
          registry
          scheduler
          start
          capacity
          (Background_execution_tests.capture_tool capabilities policy)))
;;

let with_scheduler ?retry_once ~reject_save f =
  with_capacity_scheduler
    ?retry_once
    ~reject_save
    (fun env actor backend calls registry scheduler start _capacity request ->
       f env actor backend calls registry scheduler start request)
;;

let with_physical_developer
      ?store_limits
      ?(max_active_workers = 1)
      ~env
      ~capacity
      ~on_parent
      f
  =
  let path =
    Filename.concat
      (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
      ("ochat-mixed-scheduler." ^ Agent_protocol.Id.Transaction.(create () |> to_string))
  in
  let directory = Eio.Path.(Eio.Stdenv.fs env / path) in
  Eio.Path.mkdir ~perm:0o700 directory;
  Exn.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true directory)
    ~f:(fun () ->
      Eio.Switch.run (fun sw ->
        let session =
          SS.create
            ~env
            ~sw
            ~root:(Filename.concat path "data")
            ~server_id:DT.host_id
            ~process_start_identity:None
            ~lock_nonce:"mixed-scheduler-test"
          |> DT.ok
        in
        let store =
          DS.open_or_create
            session
            ~sw
            ~limits:
              (Option.value
                 store_limits
                 ~default:(DT.store_limits ~max_job_payload_bytes:4096 ()))
            ~checkpoint:ignore
          |> DT.ok
        in
        let capture = DT.capture () in
        let owner = DT.owner "dvr_scheduler_fairness" 0 in
        let handle, _ =
          DS.admit_epoch
            store
            ~owner
            ~capture_fingerprint:(Chatml_developer_capture.fingerprint capture)
            ~previous:None
          |> DT.ok
        in
        let child_calls = ref 0 in
        let selected = DN.pair_registry ~on_parent (ref 0) child_calls in
        let limits =
          DR.Limits.create
            ~max_active_runs:2
            ~max_staged_jobs:2
            ~max_native_occupancy:2
            ~max_active_workers
            ~max_quarantined_workers:2
          |> Or_error.ok_exn
        in
        let owners = DR.Registry.create ~sw ~store ~capacity ~now:DT.now ~limits in
        let root =
          DR.Registry.admit
            owners
            ~actor:
              (Operator_authorization.guarded
                 ~principal:DT.principal
                 ~is_current:(fun () -> true))
            ~capture
            ~registry:selected
            ~selected
            ~job_policy:Chat_response.One_off_request.default_policy
            ~selected_model:None
            ~handle
            ~workspace_conflict_domain:"mixed-scheduler-test"
          |> DT.ok
        in
        let data_root = Agent_store.Session_store.data_root session in
        let blobs =
          Agent_store.Blob_store.create
            ~env
            ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
            ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
            ~max_upload_bytes:65536L
          |> DT.ok
        in
        Agent_store.Blob_store.bind_developer_runs blobs ~session_store:session store
        |> DT.ok;
        let disclosure =
          Agent_server.Developer_result_disclosure.create
            ~runs:store
            ~blobs
            ~policy:
              (Agent_server.Developer_result_disclosure.Policy.create
                 ~authorize:(fun _ _ -> true))
            ~limits:
              (Agent_server.Developer_result_disclosure.Limits.create
                 ~max_result_bytes:65536
                 ~json:Document_schema.Limits.default
               |> DT.ok)
          |> DT.ok
        in
        let jobs =
          Agent_server.Developer_script_job_service.create
            ~root
            ~store
            ~disclosure
            ~env
            ~current_actor:(fun () -> DR.original_actor root)
            ~prepare_source:(fun ~binding:_ ~request:_ ~checkpoint ->
              checkpoint ();
              Ok None)
          |> DT.ok
        in
        let runner =
          DN.background_runner
            ~sw
            ~root
            ~store
            ~blobs
            ~env
            ~managed:None
            ~jobs
            ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
        in
        let result =
          f ~sw ~root ~handle ~selected ~owners ~runner ~store ~blobs ~disclosure
        in
        DR.close_and_wait root |> DT.ok;
        DR.Registry.close_and_wait owners |> DT.ok;
        DS.close_and_wait store |> DT.ok;
        SS.close session |> DT.ok;
        result))
;;

let new_job ?(retry_policy = J.Never) payload : J.t =
  { id = Agent_protocol.Id.Job.create ()
  ; session_id
  ; generation = 0
  ; kind = Async_tool
  ; launch = None
  ; progress = None
  ; payload
  ; status = Queued
  ; retry_policy
  ; attempt = 0
  ; created_at = timestamp
  ; started_at = None
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; delivery = Pending
  }
;;

let submit ?target ?retry_policy actor payload =
  let job = new_job ?retry_policy payload in
  Option.iter target ~f:(fun target -> target := Some job.id);
  A.add_job actor job |> protocol_ok |> ignore;
  job
;;

let current backend (job : J.t) =
  (Agent_session.Memory_backend.state backend).jobs
  |> List.find_exn ~f:(fun current -> Agent_protocol.Id.Job.equal current.id job.id)
;;

let rec until env predicate =
  match predicate () with
  | true -> ()
  | false ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.005;
    until env predicate
;;

let terminal backend job =
  match (current backend job).status with
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
;;

let%test_unit "shared capacity alternates conversation and developer owner turns" =
  let conversation_entered, conversation_entered_u = Eio.Promise.create () in
  let release_conversation, release_conversation_u = Eio.Promise.create () in
  let developer_entered, developer_entered_u = Eio.Promise.create () in
  let release_developer, release_developer_u = Eio.Promise.create () in
  let conversation_released = ref false in
  let developer_released = ref false in
  let unblock () =
    if not !conversation_released
    then (
      conversation_released := true;
      Eio.Promise.resolve release_conversation_u ());
    if not !developer_released
    then (
      developer_released := true;
      Eio.Promise.resolve release_developer_u ())
  in
  with_capacity_scheduler
    ~on_conversation_call:(fun () ->
      if not (Eio.Promise.is_resolved conversation_entered)
      then (
        Eio.Promise.resolve conversation_entered_u ();
        Eio.Promise.await release_conversation))
    ~reject_save:(fun _ -> false)
    (fun env actor backend calls registry initial _start capacity request ->
       Scheduler.close initial;
       with_physical_developer
         ~env
         ~capacity
         ~on_parent:(fun () ->
           Eio.Promise.resolve developer_entered_u ();
           Eio.Promise.await release_developer)
         (fun ~sw
           ~root
           ~handle
           ~selected
           ~owners
           ~runner
           ~store:_
           ~blobs:_
           ~disclosure:_ ->
            let parent = DC.find selected ~name:"parent_tool" |> DT.ok in
            let pinned = DC.select selected ~names:[ "parent_tool" ] |> DT.ok in
            let developer_request =
              B.tool
                ~capabilities:pinned
                ~reference:(DC.reference parent)
                ~input:(`Object [])
                ~policy:Chat_response.One_off_request.default_policy
              |> protocol_ok
            in
            let developer_id = DSelected.job_id "job_scheduler_capacity_one" in
            let original =
              DSelected.job
                (DR.owner root)
                (Chatml_developer_capture.fingerprint (DR.capture root))
                ~id:developer_id
                ~parent:Agent_protocol.Developer_job.Launch.Run_root
            in
            let queued =
              Agent_protocol.Developer_job.create
                ~owner:(DR.owner root)
                ~capture_fingerprint:
                  (Chatml_developer_capture.fingerprint (DR.capture root))
                ~id:developer_id
                ~launch:(Agent_protocol.Developer_job.launch original)
                ~kind:Async_tool
                ~payload:(B.to_json developer_request)
                ~retry_policy:J.Never
                ~created_at:(Agent_protocol.Developer_job.created_at original)
                ~state:(Agent_protocol.Developer_job.state original)
              |> protocol_ok
            in
            (* This durable queued row is admitted before this scheduler starts.
               It has no in-memory capacity reservation, so the scheduler's
               bounded scan and claim compete with conversation work. *)
            let prepared =
              DS.prepare_root_selected
                handle
                ~expected:(DS.observe handle |> DT.ok)
                ~phase:DS.Checkpoint.Active
                ~selected:[ queued ]
              |> DT.ok
            in
            (match
               DS.commit_and_settle
                 handle
                 ~admit:(fun () -> true)
                 prepared
                 (DS.Handoff.create ~settle:ignore)
               |> DT.ok
             with
             | DS.Settlement.Installed _ -> ()
             | Absent | Prewrite_conflict | Diverged ->
               failwith "durable queued developer fixture was not installed");
            let scheduler =
              Scheduler.start
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~registry
                ~developer_registry:(Some owners)
                ~capacity
                ~model_job_inference:(fun _ _ ->
                  Error (Agent_protocol.Error.invalid_request "unused model job"))
            in
            Exn.protect
              ~finally:(fun () ->
                unblock ();
                Scheduler.close scheduler;
                Scheduler.await_developer_scans_idle scheduler |> protocol_ok)
              ~f:(fun () ->
                let first = submit actor (B.to_json request) in
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                  Eio.Promise.await conversation_entered);
                Scheduler.attach_developer scheduler ~root ~runner |> protocol_ok;
                let second = submit actor (B.to_json request) in
                Scheduler.wake_developer scheduler ~root;
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                  let rec await_retry () =
                    if Scheduler.For_testing.developer_retry_pending scheduler ~root
                    then ()
                    else (
                      Eio.Fiber.yield ();
                      await_retry ())
                  in
                  await_retry ());
                conversation_released := true;
                Eio.Promise.resolve release_conversation_u ();
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                  Eio.Promise.await developer_entered);
                assert (Int.equal !calls 1);
                (match (current backend second).status with
                 | Queued -> ()
                 | Running
                 | Waiting_permission _
                 | Waiting_completion _
                 | Succeeded
                 | Failed _
                 | Cancelled
                 | Interrupted _ ->
                   failwith "conversation took capacity before developer turn");
                let developer =
                  DS.find_job (DS.observe handle |> DT.ok) developer_id
                  |> Option.value_exn
                in
                assert (Int.equal (Agent_protocol.Developer_job.state developer).attempt 1);
                developer_released := true;
                Eio.Promise.resolve release_developer_u ();
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                  until env (fun () ->
                    terminal backend first
                    && terminal backend second
                    &&
                    match DS.find_job (DS.observe handle |> DT.ok) developer_id with
                    | Some job ->
                      (match (Agent_protocol.Developer_job.state job).status with
                       | Succeeded -> true
                       | Queued
                       | Running
                       | Waiting_permission _
                       | Waiting_completion _
                       | Failed _
                       | Cancelled
                       | Interrupted _ -> false)
                    | None -> false));
                assert (Int.equal !calls 2))))
;;

let%test_unit "developer scan advances beyond an unsupported 64-job window" =
  let document =
    Document_schema.Limits.create
      ~max_bytes:262144
      ~max_depth:16
      ~max_fields:16384
      ~max_nodes:16384
    |> DT.ok
  in
  let store_limits =
    DS.Limits.create
      ~max_runs:3
      ~max_epochs_per_run:3
      ~max_jobs_per_run:65
      ~max_invocations_per_run:5
      ~max_inference_attempts_per_run:256
      ~max_inference_bytes_per_run:8192
      ~max_selected_per_commit:65
      ~max_job_payload_bytes:262144
      ~max_document_bytes:262144
      ~max_recovery_entries:16
      ~max_recovery_bytes:262144
      ~max_work_steps:200000
      ~document
    |> Or_error.ok_exn
  in
  let parent_calls = ref 0 in
  with_capacity_scheduler
    ~reject_save:(fun _ -> false)
    (fun env _actor _backend _calls registry initial _start capacity _request ->
       Scheduler.close initial;
       with_physical_developer
         ~store_limits
         ~env
         ~capacity
         ~on_parent:(fun () -> Int.incr parent_calls)
         (fun ~sw
           ~root
           ~handle
           ~selected
           ~owners
           ~runner
           ~store:_
           ~blobs:_
           ~disclosure:_ ->
            let parent = DC.find selected ~name:"parent_tool" |> DT.ok in
            let pinned = DC.select selected ~names:[ "parent_tool" ] |> DT.ok in
            let tool_request =
              B.tool
                ~capabilities:pinned
                ~reference:(DC.reference parent)
                ~input:(`Object [])
                ~policy:Chat_response.One_off_request.default_policy
              |> protocol_ok
            in
            let prepared_script =
              Chat_response.One_off_script.prepare_in_domain
                ~env
                ~capabilities:selected
                ~tools:[]
                ~source:"let main input = Task.pure(input)"
                ()
              |> Result.map_error ~f:(fun diagnostics ->
                Sexp.to_string_hum
                  [%sexp (diagnostics : Chatmd_shell_spec.Diagnostic.t list)])
              |> Result.ok_or_failwith
            in
            let script_request =
              B.script
                ~prepared:prepared_script
                ~input:`Null
                ~policy:Chat_response.One_off_request.default_policy
              |> protocol_ok
            in
            let make_job index kind payload =
              let id =
                DSelected.job_id
                  (if index < 64
                   then Printf.sprintf "job_window_%03d" index
                   else "job_window_zzz")
              in
              let original =
                DSelected.job
                  (DR.owner root)
                  (Chatml_developer_capture.fingerprint (DR.capture root))
                  ~id
                  ~parent:Agent_protocol.Developer_job.Launch.Run_root
              in
              Agent_protocol.Developer_job.create
                ~owner:(DR.owner root)
                ~capture_fingerprint:
                  (Chatml_developer_capture.fingerprint (DR.capture root))
                ~id
                ~launch:(Agent_protocol.Developer_job.launch original)
                ~kind
                ~payload:(B.to_json payload)
                ~retry_policy:J.Never
                ~created_at:(Agent_protocol.Developer_job.created_at original)
                ~state:(Agent_protocol.Developer_job.state original)
              |> protocol_ok
            in
            let queued =
              List.init 64 ~f:(fun index -> make_job index Shell_process tool_request)
              @ [ make_job 64 Async_tool script_request ]
            in
            let script_id = List.last_exn queued |> Agent_protocol.Developer_job.id in
            let prepared =
              DS.prepare_root_selected
                handle
                ~expected:(DS.observe handle |> DT.ok)
                ~phase:DS.Checkpoint.Active
                ~selected:queued
              |> Result.map_error ~f:(fun error ->
                Sexp.to_string_hum (DS.Error.sexp_of_t error))
              |> Result.ok_or_failwith
            in
            (match
               DS.commit_and_settle
                 handle
                 ~admit:(fun () -> true)
                 prepared
                 (DS.Handoff.create ~settle:ignore)
               |> DT.ok
             with
             | DS.Settlement.Installed _ -> ()
             | Absent | Prewrite_conflict | Diverged ->
               failwith "window fixture was not durably installed");
            let candidates =
              DR.Job_candidate.read_current root ~checkpoint:ignore |> protocol_ok
            in
            assert (Int.equal (List.length candidates) 65);
            assert (
              List.drop candidates 64
              |> List.hd_exn
              |> DR.Job_candidate.job
              |> Agent_protocol.Developer_job.id
              |> Agent_protocol.Id.Job.equal script_id);
            let scheduler =
              Scheduler.start
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~registry
                ~developer_registry:(Some owners)
                ~capacity
                ~model_job_inference:(fun _ _ ->
                  Error (Agent_protocol.Error.invalid_request "unused model job"))
            in
            Exn.protect
              ~finally:(fun () ->
                Scheduler.close scheduler;
                Scheduler.await_developer_scans_idle scheduler |> protocol_ok)
              ~f:(fun () ->
                Scheduler.attach_developer scheduler ~root ~runner |> protocol_ok;
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                  until env (fun () ->
                    match DS.find_job (DS.observe handle |> DT.ok) script_id with
                    | Some job ->
                      (match (Agent_protocol.Developer_job.state job).status with
                       | Succeeded -> true
                       | Queued
                       | Running
                       | Waiting_permission _
                       | Waiting_completion _
                       | Failed _
                       | Cancelled
                       | Interrupted _ -> false)
                    | None -> false));
                assert (Int.equal !parent_calls 0))))
;;

let%test_unit "a saturated developer root leaves another physical root runnable" =
  let capacity_limits : Agent_server.Config.Server.job_limits =
    { daemon_total = 2
    ; per_principal = 2
    ; per_prompt = 2
    ; per_workspace = 2
    ; per_session = 1
    ; per_kind = 2
    ; max_nested_depth = 1
    }
  in
  let first_entered, first_entered_u = Eio.Promise.create () in
  let release_first, release_first_u = Eio.Promise.create () in
  let second_entered, second_entered_u = Eio.Promise.create () in
  let entered = ref 0 in
  let first_exited = ref false in
  let first_released = ref false in
  with_capacity_scheduler
    ~capacity_limits
    ~reject_save:(fun _ -> false)
    (fun env _actor _backend _calls registry initial _start capacity _request ->
       Scheduler.close initial;
       with_physical_developer
         ~max_active_workers:2
         ~env
         ~capacity
         ~on_parent:(fun () ->
           Int.incr entered;
           if Int.equal !entered 1
           then (
             Eio.Promise.resolve first_entered_u ();
             Eio.Cancel.protect (fun () -> Eio.Promise.await release_first);
             first_exited := true)
           else if Int.equal !entered 2
           then Eio.Promise.resolve second_entered_u ())
         (fun ~sw ~root ~handle ~selected ~owners ~runner ~store ~blobs ~disclosure ->
            let other_owner = DT.owner "dvr_scheduler_other" 0 in
            let capture = DR.capture root in
            let other_handle, _ =
              DS.admit_epoch
                store
                ~owner:other_owner
                ~capture_fingerprint:(Chatml_developer_capture.fingerprint capture)
                ~previous:None
              |> DT.ok
            in
            let other_root =
              DR.Registry.admit
                owners
                ~actor:
                  (Operator_authorization.guarded
                     ~principal:DT.principal
                     ~is_current:(fun () -> true))
                ~capture
                ~registry:selected
                ~selected
                ~job_policy:Chat_response.One_off_request.default_policy
                ~selected_model:None
                ~handle:other_handle
                ~workspace_conflict_domain:"mixed-scheduler-other"
              |> DT.ok
            in
            Exn.protect
              ~finally:(fun () -> DR.close_and_wait other_root |> DT.ok)
              ~f:(fun () ->
                let other_jobs =
                  Agent_server.Developer_script_job_service.create
                    ~root:other_root
                    ~store
                    ~disclosure
                    ~env
                    ~current_actor:(fun () -> DR.original_actor other_root)
                    ~prepare_source:(fun ~binding:_ ~request:_ ~checkpoint ->
                      checkpoint ();
                      Ok None)
                  |> DT.ok
                in
                let other_runner =
                  DN.background_runner
                    ~sw
                    ~root:other_root
                    ~store
                    ~blobs
                    ~env
                    ~managed:None
                    ~jobs:other_jobs
                    ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
                in
                let parent = DC.find selected ~name:"parent_tool" |> DT.ok in
                let pinned = DC.select selected ~names:[ "parent_tool" ] |> DT.ok in
                let request =
                  B.tool
                    ~capabilities:pinned
                    ~reference:(DC.reference parent)
                    ~input:(`Object [])
                    ~policy:Chat_response.One_off_request.default_policy
                  |> protocol_ok
                in
                let add_queued run run_handle name =
                  let id = DSelected.job_id name in
                  let original =
                    DSelected.job
                      (DR.owner run)
                      (Chatml_developer_capture.fingerprint (DR.capture run))
                      ~id
                      ~parent:Agent_protocol.Developer_job.Launch.Run_root
                  in
                  let queued =
                    Agent_protocol.Developer_job.create
                      ~owner:(DR.owner run)
                      ~capture_fingerprint:
                        (Chatml_developer_capture.fingerprint (DR.capture run))
                      ~id
                      ~launch:(Agent_protocol.Developer_job.launch original)
                      ~kind:Async_tool
                      ~payload:(B.to_json request)
                      ~retry_policy:J.Never
                      ~created_at:(Agent_protocol.Developer_job.created_at original)
                      ~state:(Agent_protocol.Developer_job.state original)
                    |> protocol_ok
                  in
                  let prepared =
                    DS.prepare_root_selected
                      run_handle
                      ~expected:(DS.observe run_handle |> DT.ok)
                      ~phase:DS.Checkpoint.Active
                      ~selected:[ queued ]
                    |> DT.ok
                  in
                  (match
                     DS.commit_and_settle
                       run_handle
                       ~admit:(fun () -> true)
                       prepared
                       (DS.Handoff.create ~settle:ignore)
                     |> DT.ok
                   with
                   | DS.Settlement.Installed _ -> ()
                   | Absent | Prewrite_conflict | Diverged ->
                     failwith "other-root fixture was not durably installed");
                  id
                in
                let first = add_queued root handle "job_owner_first" in
                let blocked = add_queued root handle "job_owner_blocked" in
                let other = add_queued other_root other_handle "job_owner_other" in
                let scheduler =
                  Scheduler.start
                    ~sw
                    ~clock:(Eio.Stdenv.clock env)
                    ~registry
                    ~developer_registry:(Some owners)
                    ~capacity
                    ~model_job_inference:(fun _ _ ->
                      Error (Agent_protocol.Error.invalid_request "unused model job"))
                in
                Exn.protect
                  ~finally:(fun () ->
                    if not !first_released
                    then (
                      first_released := true;
                      Eio.Promise.resolve release_first_u ());
                    Scheduler.close scheduler;
                    Scheduler.await_developer_scans_idle scheduler |> protocol_ok)
                  ~f:(fun () ->
                    Scheduler.attach_developer scheduler ~root ~runner |> protocol_ok;
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      Eio.Promise.await first_entered);
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      let rec await_retry () =
                        if Scheduler.For_testing.developer_retry_pending scheduler ~root
                        then ()
                        else (
                          Eio.Fiber.yield ();
                          await_retry ())
                      in
                      await_retry ());
                    Scheduler.attach_developer
                      scheduler
                      ~root:other_root
                      ~runner:other_runner
                    |> protocol_ok;
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      Eio.Promise.await second_entered);
                    assert (Int.equal !entered 2);
                    let blocked_state =
                      DS.find_job (DS.observe handle |> DT.ok) blocked
                      |> Option.value_exn
                      |> Agent_protocol.Developer_job.state
                    in
                    (match blocked_state.status with
                     | Queued -> ()
                     | Running
                     | Waiting_permission _
                     | Waiting_completion _
                     | Succeeded
                     | Failed _
                     | Cancelled
                     | Interrupted _ -> failwith "saturated root claimed its second job");
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      until env (fun () ->
                        match DS.find_job (DS.observe other_handle |> DT.ok) other with
                        | Some job ->
                          (match (Agent_protocol.Developer_job.state job).status with
                           | Succeeded -> true
                           | Queued
                           | Running
                           | Waiting_permission _
                           | Waiting_completion _
                           | Failed _
                           | Cancelled
                           | Interrupted _ -> false)
                        | None -> false));
                    Eio.Fiber.fork ~sw (fun () ->
                      Eio.Fiber.yield ();
                      first_released := true;
                      Eio.Promise.resolve release_first_u ());
                    Scheduler.retire_developer scheduler ~root |> protocol_ok;
                    assert !first_exited;
                    assert (
                      not (Scheduler.For_testing.developer_retry_pending scheduler ~root));
                    Scheduler.wake_developer scheduler ~root;
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      until env (fun () ->
                        match DS.find_job (DS.observe handle |> DT.ok) first with
                        | Some job ->
                          (match (Agent_protocol.Developer_job.state job).status with
                           | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
                           | Queued
                           | Running
                           | Waiting_permission _
                           | Waiting_completion _ -> false)
                        | None -> false));
                    let blocked_after =
                      DS.find_job (DS.observe handle |> DT.ok) blocked
                      |> Option.value_exn
                      |> Agent_protocol.Developer_job.state
                    in
                    (match blocked_after.status with
                     | Queued -> assert (Int.equal blocked_after.attempt 0)
                     | Running
                     | Waiting_permission _
                     | Waiting_completion _
                     | Succeeded
                     | Failed _
                     | Cancelled
                     | Interrupted _ -> failwith "retired retry root claimed another job");
                    assert (Int.equal !entered 2)))))
;;

let pending_save target (transition : Agent_session.Session_transition.t) =
  List.exists transition.state.jobs ~f:(fun job ->
    Option.exists !target ~f:(Agent_protocol.Id.Job.equal job.id)
    &&
    match job.status with
    | Succeeded | Failed _ -> true
    | _ -> false)
;;

let%expect_test
    "scheduler retries only completion persistence and retains capacity until saved"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match !rejected < 3 && pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 1);
       let second = submit actor (B.to_json request) in
       until env (fun () -> !rejected = 3);
       [%test_eq: int] 1 !calls;
       [%test_eq: int] 1 (Scheduler.running_count scheduler);
       (match (current backend second).status with
        | Queued -> ()
        | _ -> failwith "capacity released before persistence");
       until env (fun () -> terminal backend first && terminal backend second);
       [%test_eq: int] 2 !calls;
       List.iter [ first; second ] ~f:(fun job ->
         let job = current backend job in
         [%test_eq: int] 1 job.attempt;
         print_s
           [%sexp
             (Agent_protocol.Completion.of_json (Option.value_exn job.result)
              |> protocol_ok
              : Agent_protocol.Completion.t)]));
  [%expect
    {|
    (Succeeded (String disclosed))
    (Succeeded (String disclosed))
    |}]
;;

let%expect_test
    "failed invocation cleanup is retried before recording background completion"
  =
  let rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match
        !rejected < 3
        && List.exists next.state.invocations ~f:(fun invocation ->
          match invocation.status with
          | Resolved _ | Published _ -> true
          | _ -> false)
      with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let job = submit actor (B.to_json request) in
       until env (fun () -> terminal backend job);
       until env (fun () -> Scheduler.running_count scheduler = 0);
       [%test_eq: int] 3 !rejected;
       [%test_eq: int] 1 !calls;
       (match (current backend job).status with
        | Failed _ -> ()
        | _ -> failwith "lost invocation result was not reported as failure");
       assert (
         List.for_all
           (Agent_session.Memory_backend.state backend).invocations
           ~f:(fun invocation ->
             match invocation.status with
             | Resolved _ | Published _ -> true
             | _ -> false));
       print_endline "one effect; failed cleanup persisted before terminal failure");
  [%expect {| one effect; failed cleanup persisted before terminal failure |}]
;;

let%expect_test "explicit execution retry waits for its failed completion to be saved" =
  let rejected = ref 0 in
  with_scheduler
    ~retry_once:true
    ~reject_save:(fun next ->
      match
        !rejected < 2
        && List.exists next.state.jobs ~f:(fun job ->
          match job.status, job.result with
          | Queued, Some _ -> true
          | _ -> false)
      with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry _scheduler _start request ->
       let job =
         submit
           ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 0 })
           actor
           (B.to_json request)
       in
       until env (fun () -> !rejected = 2);
       [%test_eq: int] 1 !calls;
       [%test_eq: int] 1 (current backend job).attempt;
       until env (fun () -> terminal backend job);
       [%test_eq: int] 2 !calls;
       [%test_eq: int] 2 (current backend job).attempt;
       (match (current backend job).status with
        | Succeeded -> ()
        | _ -> failwith "explicit retry did not finish");
       print_endline "two rejected saves; exactly two authorized execution attempts");
  [%expect {| two rejected saves; exactly two authorized execution attempts |}]
;;

let%expect_test
    "cancellation supersedes an unsaved completion and releases its worker capacity"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       let active = current backend first in
       Scheduler.cancel_conversation
         scheduler
         ~actor
         ~job:{ active with session_id = Agent_protocol.Id.Session.create () };
       Scheduler.cancel_conversation
         scheduler
         ~actor
         ~job:{ active with attempt = active.attempt + 1 };
       [%test_eq: int] 1 (Scheduler.running_count scheduler);
       let cancelled = A.cancel_job_internal actor ~job_id:first.id |> protocol_ok in
       Scheduler.cancel_conversation scheduler ~actor ~job:cancelled;
       until env (fun () -> Scheduler.running_count scheduler = 0);
       let second = submit actor (B.to_json request) in
       until env (fun () -> terminal backend second);
       [%test_eq: int] 2 !calls;
       print_s
         [%sexp
           (Agent_protocol.Completion.of_json
              (Option.value_exn (current backend first).result)
            |> protocol_ok
            : Agent_protocol.Completion.t)];
       match (current backend second).status with
       | Succeeded -> ()
       | _ -> failwith "next job did not acquire released capacity");
  [%expect {| (Cancelled "job cancelled") |}]
;;

let%expect_test
    "shutdown with an unsaved completion recovers as interrupted without repeating \
     effects"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls registry scheduler start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       Scheduler.close scheduler;
       until env (fun () -> Scheduler.running_count scheduler = 0);
       Scheduler.reconcile_recovered
         ~registry
         ~max_count:4096
         ~max_total_bytes:(64 * 1024 * 1024)
       |> protocol_ok;
       (match (current backend first).status with
        | Interrupted _ -> ()
        | _ -> failwith "lost completion was replayable");
       let restarted = start () in
       Exn.protect
         ~finally:(fun () -> Scheduler.close restarted)
         ~f:(fun () ->
           let second = submit actor (B.to_json request) in
           until env (fun () -> terminal backend second);
           [%test_eq: int] 2 !calls;
           print_endline "interrupted first attempt; only the new job executed"));
  [%expect {| interrupted first attempt; only the new job executed |}]
;;

let%expect_test
    "rejected admission completion retries without accumulating workers or blocking \
     cancellation"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (`Object [ "nested_depth", `String "invalid" ]) in
       until env (fun () -> !rejected >= 1);
       let second = submit actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       [%test_eq: int] 0 !calls;
       [%test_eq: int] 1 (Scheduler.running_count scheduler);
       (match (current backend second).status with
        | Queued -> ()
        | _ -> failwith "rejected completion lost its queue ownership");
       A.cancel_job_internal actor ~job_id:first.id |> protocol_ok |> ignore;
       until env (fun () -> terminal backend second);
       [%test_eq: int] 1 !calls;
       print_endline
         "one rejected worker; scheduler observed cancellation and resumed queue");
  [%expect {| one rejected worker; scheduler observed cancellation and resumed queue |}]
;;
