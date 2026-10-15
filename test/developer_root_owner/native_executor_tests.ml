open! Core
module P = Agent_protocol
module H = P.Host_invocation
module I = P.Invocation
module R = Agent_store.Developer_run_store
module O = Agent_server.Developer_root_owner
module X = Agent_server.Developer_native_executor
module D = Agent_session.Developer_invocation_owner.Selected
module C = Chat_response.Tool_capability
module T = Root_owner_tests
module Scope = Agent_session.Developer_job_scope
module Jobs = Agent_server.Developer_script_job_service
module Disclosure = Agent_server.Developer_result_disclosure
module Blobs = Agent_store.Blob_store
module F = Developer_run_store_test_suite.Developer_run_store_tests
module Selected = Developer_run_store_test_suite.Developer_run_selected_tests

let ok = T.ok

let registry ?unrelated ~custom calls =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "selected native fixture"
    let type_ = if custom then "custom" else "function"
    let parameters = `Object [ "type", `String (if custom then "string" else "object") ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         Int.incr calls;
         assert (String.equal input "{}");
         Openai.Responses.Tool_output.Output.Text "private result")
  in
  C.create
    ~owner:"selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "selected-resource")
    ((Chatmd_shell_spec.Source_ref.digest "selected-native-v1", implementation)
     :: Option.to_list unrelated)
  |> ok
;;

let pair_registry ~on_parent parent_calls child_calls =
  let module Parent = struct
    type input = string

    let name = "parent_tool"
    let description = Some "nested parent fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let module Child = struct
    type input = string

    let name = "child_tool"
    let description = Some "nested child fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let parent =
    Ochat_function.create_function
      (module Parent)
      (fun _ ->
         Int.incr parent_calls;
         on_parent ();
         Openai.Responses.Tool_output.Output.Text "parent")
  in
  let child =
    Ochat_function.create_function
      (module Child)
      (fun _ ->
         Int.incr child_calls;
         Openai.Responses.Tool_output.Output.Text "child")
  in
  C.create
    ~owner:"nested-selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "nested-resource")
    [ Chatmd_shell_spec.Source_ref.digest "parent-native-v1", parent
    ; Chatmd_shell_spec.Source_ref.digest "child-native-v1", child
    ]
  |> ok
;;

let invocation owner selected reference ~custom =
  H.create_developer
    { H.id = P.Id.Invocation.create ()
    ; owner = P.Execution_owner.Developer owner
    ; origin = Script
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = reference.C.name
    ; implementation_revision = reference.implementation_revision
    ; capability_fingerprint = C.fingerprint selected
    ; input = (if custom then `String "{}" else `Object [])
    ; created_at = T.now ()
    ; deadline = None
    }
  |> ok
;;

let stage_root_job
      ?(tools = [])
      ?(source = "let main input = Task.pure(input)")
      ?(input = `Null)
      ?(policy = Chat_response.One_off_request.default_policy)
      env
      root
      selected
  =
  let prepared =
    Chat_response.One_off_script.prepare_in_domain
      ~env
      ~capabilities:selected
      ~tools
      ~source
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      Sexp.to_string_hum [%sexp (diagnostics : Chatmd_shell_spec.Diagnostic.t list)])
    |> Result.ok_or_failwith
  in
  let request =
    Chat_response.Background_request.script ~prepared ~input ~policy
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  in
  O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
    let id =
      O.Action.stage_background action ~request ~checkpoint:ignore
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
      |> Result.ok_or_failwith
    in
    let selected =
      O.Action.seal action ~selected:[ id ]
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
      |> Result.ok_or_failwith
    in
    Ok (id, selected))
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let succeeded_claim claim =
  let state = P.Developer_job.state (R.Claim.job claim) in
  { state with
    status = P.Job.Succeeded
  ; completed_at = Some (T.now ())
  ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
  }
;;

let with_executor
      ?(checkpoint = ignore)
      ?(on_current = ignore)
      ?on_store
      ?on_capacity
      ?max_job_payload_bytes
      ?(per_session = 2)
      ?(wrap_env = Fn.id)
      ?(now = T.now)
      ~run_name
      ~make_selected
      f
  =
  T.with_store ~checkpoint ~wrap_env ?on_store ?max_job_payload_bytes (fun env sw store ->
    let captured = T.capture () in
    let owner = T.owner run_name 0 in
    let handle, _ =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
    in
    let selected = make_selected env in
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
      |> Or_error.ok_exn
    in
    let capacity = T.capacity ~per_session () in
    Option.iter on_capacity ~f:(fun notify -> notify capacity);
    let owners = O.Registry.create ~sw ~store ~capacity ~now ~limits in
    let root =
      O.Registry.admit
        owners
        ~actor:
          (Operator_authorization.guarded ~principal:T.principal ~is_current:(fun () ->
             on_current ();
             true))
        ~capture:captured
        ~registry:selected
        ~selected
        ~job_policy:Chat_response.One_off_request.default_policy
        ~selected_model:None
        ~handle
        ~workspace_conflict_domain:"selected-native-test"
      |> ok
    in
    let executor = X.create ~root |> ok in
    let result = f ~env ~root ~executor ~handle ~owner ~selected in
    O.close_and_wait root |> ok;
    O.Registry.close_and_wait owners |> ok;
    result)
;;

let with_job_executor
      ?(checkpoint = ignore)
      ?(current_actor = fun root -> O.original_actor root)
      ?(wrap_env = Fn.id)
      ?(on_store = fun _ _ _ -> ())
      ?on_capacity
      ?max_job_payload_bytes
      ?(per_session = 2)
      ?(now = T.now)
      ~run_name
      ~make_selected
      f
  =
  let resources = ref None in
  with_executor
    ~checkpoint
    ~wrap_env
    ?on_capacity
    ?max_job_payload_bytes
    ~per_session
    ~now
    ~run_name
    ~make_selected
    ~on_store:(fun env session store ->
      on_store env session store;
      let root = Agent_store.Session_store.data_root session in
      let blobs =
        Blobs.create
          ~env
          ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path root)
          ~durable_directory:(Agent_store.Data_root.durable_blobs_path root)
          ~max_upload_bytes:65536L
        |> ok
      in
      Blobs.bind_developer_runs blobs ~session_store:session store
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
      |> Result.ok_or_failwith;
      let limits =
        Disclosure.Limits.create
          ~max_result_bytes:65536
          ~json:Document_schema.Limits.default
        |> ok
      in
      let policy = Disclosure.Policy.create ~authorize:(fun _ _ -> true) in
      let disclosure = Disclosure.create ~runs:store ~blobs ~policy ~limits |> ok in
      resources := Some (store, disclosure, blobs))
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let store, disclosure, blobs = Option.value_exn !resources in
       let jobs =
         Jobs.create
           ~root
           ~store
           ~disclosure
           ~env
           ~current_actor:(fun () -> current_actor root)
           ~prepare_source:(fun ~binding:_ ~request:_ ~checkpoint ->
             checkpoint ();
             Ok None)
         |> Result.map_error ~f:(fun error -> error.P.Error.message)
         |> Result.ok_or_failwith
       in
       let executor = X.with_job_service executor jobs |> ok in
       f ~env ~root ~executor ~handle ~owner ~selected ~jobs ~store ~blobs)
;;

let background_runner ~sw ~root ~store ~blobs ~env ~managed ~jobs ~storage =
  let limits =
    Agent_store.Developer_result_publisher.Limits.create
      ~inline_bytes:1024
      ~max_result_bytes:65536
      ~max_pending:2
    |> ok
  in
  Agent_server.Developer_background_job_runner.create
    ~sw
    ~root
    ~store
    ~blobs
    ~publisher_limits:limits
    ~storage
    ~env
    ~managed
    ~jobs
    ~authorize:(fun _ _ -> Ok ())
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected output"))
  |> ok
;;

let%test_unit "claimed Tool target uses one physical native and result publisher" =
  List.iter
    [ Agent_store.Developer_result_publisher.Storage.Automatic; Artifact ]
    ~f:(fun storage ->
      let parent_calls = ref 0 in
      let child_calls = ref 0 in
      with_job_executor
        ~run_name:
          (match storage with
           | Automatic -> "dvr_claimed_tool_inline"
           | Artifact -> "dvr_claimed_tool_artifact")
        ~make_selected:(fun _ -> pair_registry ~on_parent:ignore parent_calls child_calls)
        (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
           let parent = C.find selected ~name:"parent_tool" |> ok in
           let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
           let request =
             Chat_response.Background_request.tool
               ~capabilities:pinned
               ~reference:(C.reference parent)
               ~input:(`Object [])
               ~policy:Chat_response.One_off_request.default_policy
             |> ok
           in
           let selected_id =
             O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
               let id =
                 O.Action.stage_background action ~request ~checkpoint:ignore |> ok
               in
               let selected = O.Action.seal action ~selected:[ id ] |> ok in
               Ok (id, selected))
             |> ok
           in
           Eio.Switch.run (fun sw ->
             let runner =
               background_runner ~sw ~root ~store ~blobs ~env ~managed:None ~jobs ~storage
             in
             let candidates =
               O.Job_candidate.read_current root ~checkpoint:ignore |> ok
             in
             assert (Int.equal (List.length candidates) 1);
             let candidate = List.hd_exn candidates in
             assert (
               P.Id.Job.equal
                 selected_id
                 (P.Developer_job.id (O.Job_candidate.job candidate)));
             let available =
               Agent_server.Developer_background_job_runner.availability runner candidate
               |> ok
             in
             assert (
               Agent_server.Developer_background_job_runner.Availability.equal
                 available
                 Runnable);
             let completed =
               Agent_server.Developer_background_job_runner.run_candidate runner candidate
               |> ok
               |> Option.value_exn
             in
             assert (Int.equal !parent_calls 1);
             assert (Int.equal !child_calls 0);
             assert (P.Id.Job.equal selected_id (P.Developer_job.id completed));
             assert (
               match (P.Developer_job.state completed).status with
               | P.Job.Succeeded -> true
               | _ -> false);
             assert (
               match storage, (P.Developer_job.state completed).result with
               | Automatic, Some (Inline (P.Completion.Succeeded (`String "parent"))) ->
                 true
               | Artifact, Some (Artifact { outcome = P.Stored_completion.Succeeded; _ })
                 -> true
               | _ -> false))))
;;

let%test_unit "claimed Script executes selected native and retains its lexical child" =
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_claimed_script_native_child"
    ~max_job_payload_bytes:4096
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let source =
         {|let main input =
  Task.bind(
    Task.catch(
      Task.bind(Job.start_tool("read_file", input), fun discarded -> Task.fail("discard")),
      fun ignored -> Task.pure(`Null)),
    fun ignored -> Task.bind(Tool.call("read_file", input), fun result ->
      match result with
      | `Ok(value) -> Task.bind(Job.start_tool("read_file", input), fun id ->
          match value with
          | `String(text) -> Task.pure(`String(id ++ ":" ++ text))
          | _ -> Task.fail("expected native text"))
      | `Error(message) -> Task.fail(message)))|}
       in
       let parent_id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         assert (Agent_server.Developer_background_job_runner.issued_by_root runner root);
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Agent_server.Developer_background_job_runner.Availability.equal
             (Agent_server.Developer_background_job_runner.availability runner candidate
              |> ok)
             Runnable);
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> Result.map_error ~f:(fun error -> error.P.Error.message)
           |> Result.ok_or_failwith
           |> Option.value_exn
         in
         assert (Int.equal !calls 1);
         assert (P.Id.Job.equal (P.Developer_job.id completed) parent_id);
         let child_id =
           match (P.Developer_job.state completed).result with
           | Some (Inline (P.Completion.Succeeded (`String text))) ->
             let id, native = String.lsplit2_exn text ~on:':' in
             assert (String.equal native "private result");
             P.Id.Job.of_string id |> ok
           | _ -> failwith "claimed Script did not return selected native result"
         in
         let child = R.find_job (R.observe handle |> ok) child_id |> Option.value_exn in
         let queued_ids =
           O.Job_candidate.read_current root ~checkpoint:ignore
           |> ok
           |> List.map ~f:(fun candidate ->
             P.Developer_job.id (O.Job_candidate.job candidate))
         in
         assert (List.equal P.Id.Job.equal queued_ids [ child_id ]);
         let launch = P.Developer_job.launch child in
         (match P.Developer_job.Launch.parent_job launch with
          | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1 ->
            ()
          | None | Some _ -> failwith "Script child lost claimed parent attempt");
         assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1)))
;;

let%test_unit "claimed Script cannot catch a physical admission error into another call" =
  let calls = ref 0 in
  let clock = ref (T.now ()) in
  with_job_executor
    ~run_name:"dvr_claimed_script_fatal_admission"
    ~max_job_payload_bytes:4096
    ~now:(fun () -> !clock)
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let parent_id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source:
             {|let main input = Task.bind(
  Task.catch(Tool.call("not_selected", input), fun ignored -> Tool.call("read_file", input)),
  fun ignored -> Task.pure(input))|}
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         assert (Int.equal !calls 0);
         let persisted =
           R.find_job (R.observe handle |> ok) parent_id |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state persisted).status with
           | P.Job.Failed _ -> true
           | _ -> false);
         let tiny_policy =
           { Chat_response.One_off_request.default_policy with max_output_bytes = 1 }
         in
         let expired_id = stage_root_job ~policy:tiny_policy env root selected in
         clock := P.Timestamp.of_string "2026-10-14T00:00:00Z" |> ok;
         let expired_candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         (match
            Agent_server.Developer_background_job_runner.run_candidate
              runner
              expired_candidate
          with
          | Error error ->
            assert (P.Error.equal_code error.code Resource_limit);
            assert (
              String.equal
                error.message
                "Script completion exceeds its saved output limit")
          | Ok _ -> failwith "undersized expired Script completion was accepted");
         assert (Int.equal !calls 0);
         let expired =
           R.find_job (R.observe handle |> ok) expired_id |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state expired).status with
           | P.Job.Failed _ -> true
           | _ -> false)))
;;

let%test_unit "malformed saved Async_tool enters one physical Failed attempt" =
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_claimed_malformed_async"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store ~blobs ->
       let capture_fingerprint = Chatml_developer_capture.fingerprint (O.capture root) in
       let id = Selected.job_id "job_claimed_malformed_async" in
       let original =
         Selected.job
           (O.owner root)
           capture_fingerprint
           ~id
           ~parent:P.Developer_job.Launch.Run_root
       in
       let malformed =
         P.Developer_job.create
           ~owner:(O.owner root)
           ~capture_fingerprint
           ~id
           ~launch:(P.Developer_job.launch original)
           ~kind:P.Job.Async_tool
           ~payload:(`Object [ "not_a_background_request", `True ])
           ~retry_policy:P.Job.Never
           ~created_at:(P.Developer_job.created_at original)
           ~state:(P.Developer_job.state original)
         |> ok
       in
       let prepared =
         R.prepare_root_selected
           handle
           ~expected:(R.observe handle |> ok)
           ~phase:R.Checkpoint.Active
           ~selected:[ malformed ]
         |> ok
       in
       (match
          R.commit_and_settle
            handle
            ~admit:(fun () -> true)
            prepared
            (R.Handoff.create ~settle:ignore)
          |> ok
        with
        | R.Settlement.Installed _ -> ()
        | Absent | Prewrite_conflict | Diverged ->
          failwith "malformed queued fixture was not installed");
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Agent_server.Developer_background_job_runner.Availability.equal
             (Agent_server.Developer_background_job_runner.availability runner candidate
              |> ok)
             Malformed_supported);
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         assert (Int.equal !calls 0);
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (Int.equal (P.Developer_job.state persisted).attempt 1);
         assert (
           match (P.Developer_job.state persisted).status with
           | P.Job.Failed _ -> true
           | _ -> false)))
;;

let%test_unit "claimed Tool keeps selected Artifact after uncertain terminal save" =
  let calls = ref 0 in
  let tracking = ref false in
  let writes = ref 0 in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  with_job_executor
    ~run_name:"dvr_claimed_tool_uncertain_artifact"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !tracking
          then (
            Int.incr writes;
            if Int.equal !writes 4 then rename_armed := true))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let binding = C.find selected ~name:"read_file" |> ok in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:selected
           ~reference:(C.reference binding)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selection))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Artifact
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         tracking := true;
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         tracking := false;
         assert (Int.equal !writes 4);
         assert (not !rename_armed);
         assert (Int.equal !calls 1);
         sync_blocked := false;
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (
           match (P.Developer_job.state persisted).result with
           | Some (Artifact { outcome = P.Stored_completion.Succeeded; _ }) -> true
           | None | Some (Inline _) | Some (Artifact _) -> false);
         (match
            Agent_server.Developer_background_job_runner.run_candidate runner candidate
          with
          | Ok None -> ()
          | Ok (Some _) | Error _ ->
            failwith "stale candidate restarted an uncertain claimed Tool");
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed Script does not replay native effect after uncertain result save" =
  let calls = ref 0 in
  let tracking = ref false in
  let writes = ref 0 in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  with_job_executor
    ~run_name:"dvr_claimed_script_uncertain_artifact"
    ~max_job_payload_bytes:4096
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !tracking
          then (
            Int.incr writes;
            if Int.equal !writes 4 then rename_armed := true))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source:
             {|let main input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(message) -> Task.fail(message))|}
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Artifact
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         tracking := true;
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         tracking := false;
         assert (Int.equal !calls 1);
         assert (not !rename_armed);
         sync_blocked := false;
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (
           match (P.Developer_job.state persisted).result with
           | Some (Artifact { outcome = P.Stored_completion.Succeeded; _ }) -> true
           | None | Some (Inline _) | Some (Artifact _) -> false);
         (match
            Agent_server.Developer_background_job_runner.run_candidate runner candidate
          with
          | Ok None -> ()
          | Ok (Some _) | Error _ ->
            failwith "stale candidate replayed an uncertain claimed Script");
         assert (Int.equal !calls 1)))
;;

let%test_unit "developer root Job scope selects only the returned physical start" =
  let calls = ref 0 in
  let data_root_path = ref None in
  let after_actor_read = ref ignore in
  let swap_during_read = ref false in
  let swapped = ref false in
  let other_principal =
    let id = P.Id.Principal.of_string "pri_other_developer_job_reader" |> ok in
    P.Principal.create
      ~id
      ~authentication_kind:"local.test"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> ok
  in
  let foreign_actor =
    Operator_authorization.guarded ~principal:other_principal ~is_current:(fun () -> true)
  in
  with_job_executor
    ~on_store:(fun _ session _ ->
      data_root_path
      := Some (Agent_store.Data_root.path (Agent_store.Session_store.data_root session)))
    ~checkpoint:(fun () -> !after_actor_read ())
    ~current_actor:(fun root ->
      if !swap_during_read
      then
        if !swapped
        then foreign_actor
        else (
          (after_actor_read := fun () -> swapped := true);
          O.original_actor root)
      else O.original_actor root)
    ~run_name:"dvr_script_root_job"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store:_ ~blobs:_ ->
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let persisted_job_ids () =
         let reader =
           Agent_store.Retention_reader.create
             ~env
             ~root:(Option.value_exn !data_root_path)
             ~max_entries:64
             ~max_bytes:65536
           |> ok
         in
         R.Result_root.rows_for_inventory
           (R.Result_root.of_handle handle |> ok)
           ~reader
           ~checkpoint:ignore
         |> ok
         |> R.Result_root.Inventory_rows.jobs
         |> List.map ~f:P.Developer_job.id
         |> List.sort ~compare:P.Id.Job.compare
       in
       let effects = ref [] in
       let compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:
             {|let main input =
  Task.bind(Task.catch(
    Task.bind(Job.start_tool("read_file", input), fun discarded -> Task.fail("discard")),
    fun ignored -> Task.pure("caught")), fun ignored ->
  Task.bind(Job.start_tool("read_file", input), fun id -> Task.pure(id)))|}
           ()
         |> Result.ok_or_failwith
       in
       let config : Runtime.runtime_config =
         { surface = Chatml.Chatml_extension_surface.one_off_v1
         ; operations = Runtime.default_operations ~handlers:Runtime.default_handlers ()
         }
       in
       let value, ordinary =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           let config = Jobs.Scope.install scope config in
           let value =
             Runtime.run_entrypoint
               ~prepare_result:(fun ~value:_ ~local_effects ->
                 effects := local_effects;
                 Ok ignore)
               config
               compiled
               ~entrypoint:"main"
               ~arguments:[ Value.import_json (`Object []) ]
               ()
             |> Result.map_error ~f:P.Error.invalid_request
           in
           Result.map value ~f:(fun value -> value, !effects))
         |> Result.map_error ~f:(fun error -> error.P.Error.message)
         |> Result.ok_or_failwith
       in
       assert (List.is_empty ordinary);
       let ids, _ =
         Chat_response.Background_job_operations.split_starts !effects
         |> Result.ok_or_failwith
       in
       let id = List.hd_exn ids in
       assert (
         match value with
         | Chatml.Chatml_lang.VString returned ->
           String.equal returned (P.Id.Job.to_string id)
         | _ -> false);
       let job = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
       assert (
         P.Execution_owner.equal_developer (P.Developer_job.owner job) (O.owner root));
       assert (Int.equal !calls 0);
       let script_compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:
             {|let main input = Job.start_script(`Object([
  {key="source"; value=`String("let main input = Task.pure(input)")},
  {key="tools"; value=`Array([])},
  {key="input"; value=`Null}
]))|}
           ()
         |> Result.ok_or_failwith
       in
       let script_effects = ref [] in
       ignore
         (Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
            let config = Jobs.Scope.install scope config in
            Runtime.run_entrypoint
              ~prepare_result:(fun ~value:_ ~local_effects ->
                script_effects := local_effects;
                Ok ignore)
              config
              script_compiled
              ~entrypoint:"main"
              ~arguments:[ Value.import_json `Null ]
              ()
            |> Result.map_error ~f:P.Error.invalid_request
            |> Result.map ~f:(fun value -> value, !script_effects))
          |> Result.map_error ~f:(fun error -> error.P.Error.message)
          |> Result.ok_or_failwith
          : Chatml.Chatml_lang.value * Chatml.Chatml_lang.eff list);
       let script_ids, _ =
         Chat_response.Background_job_operations.split_starts !script_effects
         |> Result.ok_or_failwith
       in
       assert (Int.equal (List.length script_ids) 1);
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (List.hd_exn script_ids)));
       let run_job_query source =
         let source =
           String.substr_replace_all
             source
             ~pattern:"JOB_ID"
             ~with_:(P.Id.Job.to_string id)
         in
         let compiled =
           Runtime.compile_script
             ~surface:Chatml.Chatml_extension_surface.one_off_v1
             ~source
             ()
           |> Result.ok_or_failwith
         in
         let value, ordinary =
           Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
             let config = Jobs.Scope.install scope config in
             let effects = ref [] in
             let value =
               Runtime.run_entrypoint
                 ~prepare_result:(fun ~value:_ ~local_effects ->
                   effects := local_effects;
                   Ok ignore)
                 config
                 compiled
                 ~entrypoint:"main"
                 ~arguments:[ Value.import_json (`Object []) ]
                 ()
               |> Result.map_error ~f:P.Error.invalid_request
             in
             Result.map value ~f:(fun value -> value, !effects))
           |> Result.map_error ~f:(fun error -> error.P.Error.message)
           |> Result.ok_or_failwith
         in
         assert (List.is_empty ordinary);
         Value.export_json value |> Result.ok_or_failwith
       in
       let pending = run_job_query {|let main input = Job.read_result("JOB_ID")|} in
       assert (
         match pending with
         | `Null -> true
         | _ -> false);
       let before = run_job_query {|let main input = Job.get("JOB_ID")|} in
       assert (
         match before with
         | `Object fields ->
           (match
              ( List.Assoc.find fields "status" ~equal:String.equal
              , List.Assoc.find fields "completion" ~equal:String.equal
              , List.Assoc.find fields "completion_kind" ~equal:String.equal )
            with
            | Some (`String "queued"), Some `Null, Some `Null -> true
            | _ -> false)
         | _ -> false);
       ignore
         (run_job_query
            {|let main input = Task.bind(Job.cancel("JOB_ID"), fun ignored -> Task.pure(`Null))|}
          : Jsonaf.t);
       let after = run_job_query {|let main input = Job.get("JOB_ID")|} in
       assert (
         match after with
         | `Object fields ->
           List.Assoc.find fields "status" ~equal:String.equal
           |> Option.value_map ~default:false ~f:(function
             | `String value -> String.equal value "cancelled"
             | _ -> false)
         | _ -> false);
       let model_compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.developer_tool_v1
           ~source:
             {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
  Task.pure(`Complete(input)))|}
           ()
         |> Result.ok_or_failwith
       in
       let before_model = persisted_job_ids () in
       let unavailable =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           let model_config : Runtime.runtime_config =
             { surface = Chatml.Chatml_extension_surface.developer_tool_v1
             ; operations = Runtime.default_operations ()
             }
           in
           let model_config = Jobs.Scope.install scope model_config in
           Runtime.run_entrypoint
             ~scoped_async_operation:Runtime.Model_spawn
             model_config
             model_compiled
             ~entrypoint:"run"
             ~arguments:
               [ Value.import_json `Null
               ; Value.import_json
                   (`Object [ "prompt", `String "p"; "input", `String "i" ])
               ]
             ()
           |> Result.map_error ~f:P.Error.invalid_request
           |> Result.map ~f:(fun value -> value, []))
       in
       assert (Result.is_error unavailable);
       assert (List.equal P.Id.Job.equal before_model (persisted_job_ids ()));
       swap_during_read := true;
       let denied =
         Or_error.try_with (fun () ->
           run_job_query {|let main input = Job.read_result("JOB_ID")|})
       in
       assert !swapped;
       assert (
         match denied with
         | Error error ->
           String.is_substring
             (Error.to_string_hum error)
             ~substring:"developer result is unauthorized"
         | Ok _ -> false))
;;

let managed_job_registry
      ?(tool_contract = Chatml_compilation.Tool_contract.Standard)
      ?(completion_schema = false)
      ?base_registry
      ?(source =
        "let run ctx input = Task.bind(Job.start_tool(\"read_file\", input), fun id ->\n\
        \  Task.pure(`Complete(input)))")
      env
      calls
  =
  let module M = Chat_response.Managed_tool_registry in
  let base =
    Option.value_or_thunk base_registry ~default:(fun () -> registry ~custom:false calls)
  in
  let dir = Eio.Stdenv.cwd env in
  let source_loader =
    Source_loader.captured_filesystem
      ~root:dir
      ~sources:
        [ "input.json", {|{"type":"object"}|}
        ; "output.json", {|{"type":"object"}|}
        ; "completion.json", {|{"type":"string"}|}
        ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader
      (Printf.sprintf
         {|<tool name="read_file"/>
<script id="owned" language="chatml" kind="tool">
%s
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"%s><uses tool="read_file"/></tool>|}
         source
         (if completion_schema then {| completion_schema="completion.json"|} else ""))
  in
  let managed =
    M.prepare
      ~env
      ~owner:"developer-managed-jobs"
      ~capabilities:base
      ~tool_contract
      elements
    |> Result.map_error ~f:(fun errors ->
      List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
    |> Result.ok_or_failwith
  in
  managed, M.capabilities managed
;;

let%test_unit "claimed managed Pending waits on selected owned Job and captured schema" =
  let calls = ref 0 in
  let prepared = ref None in
  with_job_executor
    ~run_name:"dvr_claimed_managed_pending"
    ~make_selected:(fun env ->
      let managed, selected = managed_job_registry ~completion_schema:true env calls in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let module M = Chat_response.Managed_tool_registry in
       let managed = Option.value_exn !prepared in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:selected
           ~reference
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selected = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selected))
         |> ok
       in
       let child_id = ref None in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution
               _borrowed
               ~effect_entry
               ~stage_background
               ~install_jobs:_
               continuation ->
               effect_entry () |> ok;
               let child_binding = C.find selected ~name:"read_file" |> ok in
               let child =
                 Chat_response.Background_request.tool
                   ~capabilities:selected
                   ~reference:(C.reference child_binding)
                   ~input:(`Object [])
                   ~policy:Chat_response.One_off_request.default_policy
                 |> ok
               in
               let id = stage_background ~request:child ~checkpoint:ignore |> ok in
               child_id := Some id;
               continuation
                 ~validate_work:(function
                   | I.Job actual when P.Id.Job.equal actual id -> Ok ()
                   | Job _ | Subscription _ -> Error "foreign managed child")
                 ~outcome:(I.Pending (I.Job id, `Object []))
                 ~selected_jobs:[ id ])
         }
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:(Some service)
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> ok
           |> Option.value_exn
         in
         assert (P.Id.Job.equal id (P.Developer_job.id completed));
         assert (Int.equal !calls 0);
         assert (
           Option.is_some
             (R.find_job (R.observe handle |> ok) (Option.value_exn !child_id)));
         assert (
           match (P.Developer_job.state completed).status with
           | P.Job.Waiting_completion dependency ->
             (match dependency.work with
              | I.Job waited ->
                P.Id.Job.equal waited (Option.value_exn !child_id)
                && Jsonaf.exactly_equal
                     (Option.value_exn dependency.completion_schema)
                     (`Object [ "type", `String "string" ])
              | Subscription _ -> false)
           | _ -> false)))
;;

let%test_unit "claimed managed Job.start keeps the request's pinned tool subset" =
  let read_calls = ref 0 in
  let extra_calls = ref 0 in
  let prepared = ref None in
  let module Extra = struct
    type input = string

    let name = "child_tool"
    let description = Some "unrelated current root tool"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let extra =
    Ochat_function.create_function
      (module Extra)
      (fun _ ->
         Int.incr extra_calls;
         Openai.Responses.Tool_output.Output.Text "extra")
  in
  with_job_executor
    ~run_name:"dvr_claimed_managed_subset"
    ~make_selected:(fun env ->
      let base =
        registry
          ~unrelated:(Chatmd_shell_spec.Source_ref.digest "unrelated-v1", extra)
          ~custom:false
          read_calls
      in
      let managed, selected = managed_job_registry ~base_registry:base env read_calls in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor:_ ~handle:_ ~owner:_ ~selected ~jobs ~store ~blobs ->
       let module M = Chat_response.Managed_tool_registry in
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let managed = Option.value_exn !prepared in
       let summary = C.find selected ~name:"summary" |> ok in
       let pinned = C.select selected ~names:[ "summary" ] |> ok in
       assert (Result.is_ok (C.find selected ~name:"child_tool"));
       let request =
         Chat_response.Background_request.tool
           ~capabilities:pinned
           ~reference:(C.reference summary)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selection))
         |> ok
       in
       let selected_child = ref None in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun execution
               _borrowed
               ~effect_entry
               ~stage_background
               ~install_jobs
               continuation ->
               effect_entry () |> ok;
               let unrelated = C.find selected ~name:"child_tool" |> ok in
               let unrelated_request =
                 Chat_response.Background_request.tool
                   ~capabilities:selected
                   ~reference:(C.reference unrelated)
                   ~input:(`Object [])
                   ~policy:Chat_response.One_off_request.default_policy
                 |> ok
               in
               assert (
                 Result.is_error
                   (stage_background ~request:unrelated_request ~checkpoint:ignore));
               let config : Runtime.runtime_config =
                 { surface = Chatml.Chatml_extension_surface.tool_v1
                 ; operations =
                     Runtime.default_operations ~handlers:Runtime.default_handlers ()
                 }
               in
               let installed = install_jobs config in
               let invoke_unrelated () =
                 let compiled =
                   Runtime.compile_script
                     ~surface:config.surface
                     ~source:{|let run ctx input = Job.start_tool("child_tool", input)|}
                     ()
                   |> Result.ok_or_failwith
                 in
                 Runtime.run_entrypoint
                   ?scoped_async_operation:installed.scoped_async_operation
                   ~prepare_result:(fun ~value:_ ~local_effects:_ -> Ok ignore)
                   installed.config
                   compiled
                   ~entrypoint:"run"
                   ~arguments:[ Value.import_json `Null; Value.import_json (`Object []) ]
                   ()
               in
               assert (Result.is_error (invoke_unrelated ()));
               let effects = ref [] in
               let actual = M.owned_prepared execution in
               ignore
                 (Chat_response.Extension_compiler.Program.run_in_scope
                    (Chat_response.Extension_compiler.program actual)
                    ~prepare_result:(fun ~value:_ ~local_effects ->
                      effects := local_effects;
                      Ok ignore)
                    ~control:None
                    ?scoped_async_operation:installed.scoped_async_operation
                    ~config:installed.config
                    ~entrypoint:"run"
                    ~arguments:[ Value.import_json `Null; Value.import_json (`Object []) ]
                    ()
                  |> Result.map_error ~f:(fun error -> error.Chatml_execution.message)
                  |> Result.ok_or_failwith
                  : Chatml.Chatml_lang.value);
               let child_id =
                 Chat_response.Background_job_operations.split_starts !effects
                 |> Result.ok_or_failwith
                 |> fst
                 |> List.hd_exn
               in
               selected_child := Some child_id;
               continuation
                 ~validate_work:(function
                   | I.Job actual when P.Id.Job.equal actual child_id -> Ok ()
                   | Job _ | Subscription _ -> Error "foreign managed child")
                 ~outcome:(I.Pending (I.Job child_id, `Object []))
                 ~selected_jobs:[ child_id ])
         }
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed:(Some service)
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> ok
           |> Option.value_exn
         in
         assert (P.Id.Job.equal id (P.Developer_job.id completed));
         assert (Int.equal !extra_calls 0);
         assert (Int.equal !read_calls 0);
         if
           not
             (match (P.Developer_job.state completed).status with
              | P.Job.Waiting_completion { work = I.Job actual; _ } ->
                P.Id.Job.equal actual (Option.value_exn !selected_child)
              | _ -> false)
         then
           raise_s
             [%sexp
               "unexpected claimed managed result"
             , ((P.Developer_job.state completed).status : P.Job.status)
             , (!selected_child : P.Id.Job.t option)]))
;;

let%test_unit
    "selected managed ChatML installs Job operations only in its live native scope"
  =
  let calls = ref 0 in
  let prepared = ref None in
  with_job_executor
    ~run_name:"dvr_script_managed_job"
    ~make_selected:(fun env ->
      let managed, selected = managed_job_registry env calls in
      prepared := Some managed;
      selected)
    (fun ~env:_ ~root:_ ~executor ~handle ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let module M = Chat_response.Managed_tool_registry in
       let module EC = Chat_response.Extension_compiler in
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let module Ops = Chat_response.Background_job_operations in
       let managed = Option.value_exn !prepared in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted =
         H.create_developer
           { (H.context (invocation owner selected reference ~custom:false)) with
             implementation_revision = M.execution_revision managed binding |> ok
           }
         |> ok
       in
       let escaped_install = ref None in
       let selected_ids = ref [] in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun execution
               _borrowed
               ~effect_entry
               ~stage_background:_
               ~install_jobs
               continuation ->
               effect_entry () |> ok;
               escaped_install := Some install_jobs;
               let prepared = M.owned_prepared execution in
               let config : Runtime.runtime_config =
                 { surface = Chatml.Chatml_extension_surface.tool_v1
                 ; operations =
                     Runtime.default_operations ~handlers:Runtime.default_handlers ()
                 }
               in
               let installed = install_jobs config in
               assert (Option.is_none installed.scoped_async_operation);
               let effects = ref [] in
               ignore
                 (EC.Program.run_in_scope
                    (EC.program prepared)
                    ~prepare_result:(fun ~value:_ ~local_effects ->
                      effects := local_effects;
                      Ok ignore)
                    ~control:None
                    ?scoped_async_operation:installed.scoped_async_operation
                    ~config:installed.config
                    ~entrypoint:"run"
                    ~arguments:[ Value.import_json `Null; Value.import_json (`Object []) ]
                    ()
                  |> Result.map_error ~f:(fun error -> error.Chatml_execution.message)
                  |> Result.ok_or_failwith
                  : Chatml.Chatml_lang.value);
               let ids, ordinary = Ops.split_starts !effects |> Result.ok_or_failwith in
               selected_ids := ids;
               assert (List.is_empty ordinary);
               continuation
                 ~validate_work:(fun work ->
                   match work with
                   | P.Invocation.Job id when List.mem ids id ~equal:P.Id.Job.equal ->
                     Ok ()
                   | Job _ | Subscription _ -> Error "foreign managed job")
                 ~outcome:(I.Complete (`Object []))
                 ~selected_jobs:ids)
         }
       in
       let terminal =
         X.run_root
           executor
           ~managed:(Some service)
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected output"))
         |> ok
       in
       assert (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))));
       assert (Int.equal (List.length !selected_ids) 1);
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (List.hd_exn !selected_ids)));
       let escaped = Option.value_exn !escaped_install in
       assert (
         Result.is_error
           (Or_error.try_with (fun () ->
              let config : Runtime.runtime_config =
                { surface = Chatml.Chatml_extension_surface.tool_v1
                ; operations =
                    Runtime.default_operations ~handlers:Runtime.default_handlers ()
                }
              in
              ignore
                (escaped config
                 : Agent_session.Native_tool_invocation.managed_script_install)))))
;;

let%test_unit
    "lexical root cleanup retains uncertain selected capacity for exact reconciliation"
  =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let capacity = ref None in
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_script_uncertain_root"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~per_session:1
    ~on_capacity:(fun value -> capacity := Some value)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env:_ ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store:_ ~blobs:_ ->
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let effects = ref [] in
       let invoked = ref 0 in
       let compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:{|let main input = Job.start_tool("read_file", input)|}
           ()
         |> Result.ok_or_failwith
       in
       let config : Runtime.runtime_config =
         { surface = Chatml.Chatml_extension_surface.one_off_v1
         ; operations = Runtime.default_operations ~handlers:Runtime.default_handlers ()
         }
       in
       let uncertain =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           Int.incr invoked;
           let config = Jobs.Scope.install scope config in
           let value =
             Runtime.run_entrypoint
               ~prepare_result:(fun ~value:_ ~local_effects ->
                 effects := local_effects;
                 Ok ignore)
               config
               compiled
               ~entrypoint:"main"
               ~arguments:[ Value.import_json (`Object []) ]
               ()
             |> Result.map_error ~f:P.Error.invalid_request
           in
           Result.map value ~f:(fun value ->
             rename_armed := true;
             value, !effects))
       in
       assert (Result.is_error uncertain);
       assert (Int.equal !invoked 1);
       let ids, _ =
         Chat_response.Background_job_operations.split_starts !effects
         |> Result.ok_or_failwith
       in
       let id = List.hd_exn ids in
       let project_source =
         let prepared = Chatml_developer_capture.prepared (O.capture root) in
         match Chatml_prepared_program.provenance prepared with
         | Project { declaration; lock = _ } -> declaration
         | Implicit -> Chatml_prepared_program.entry prepared
       in
       let key =
         Agent_server.Job_capacity.Key.developer
           ~owner:(O.owner root)
           ~project_source
           ~workspace_conflict_domain:"selected-native-test"
           ~kind:P.Job.Async_tool
           ~nested_depth:0
         |> ok
       in
       (match Agent_server.Job_capacity.try_acquire (Option.value_exn !capacity) key with
        | Ok None -> ()
        | Ok (Some _) | Error _ ->
          failwith "lexical cleanup released the uncertain selected reservation");
       assert (
         Result.is_error
           (Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope:_ ~sw:_ ->
              failwith "unknown physical handoff admitted another root callback")));
       sync_blocked := false;
       O.reconcile_pending root |> ok;
       assert (Option.is_some (R.find_job (R.observe handle |> ok) id));
       assert (Int.equal !invoked 1))
;;

let managed_registry env =
  let module Managed = Chat_response.Managed_tool_registry in
  let base =
    C.create
      ~owner:"selected-managed-test"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "managed-resource")
      []
    |> ok
  in
  let dir = Eio.Stdenv.cwd env in
  let source_loader =
    Source_loader.captured_filesystem
      ~root:dir
      ~sources:
        [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"object"}|} ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader
      {|<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
  in
  let managed =
    Managed.prepare ~env ~owner:"selected-managed-test" ~capabilities:base elements
    |> Result.map_error ~f:(fun diagnostics ->
      String.concat
        ~sep:"; "
        (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> Result.ok_or_failwith
  in
  managed, Managed.capabilities managed
;;

let%test_unit "selected native Function and Custom share durable dispatch policy" =
  List.iter [ false; true ] ~f:(fun custom ->
    let calls = ref 0 in
    with_executor
      ~run_name:(if custom then "dvr_selected_custom" else "dvr_selected_function")
      ~make_selected:(fun _env -> registry ~custom calls)
      (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom in
         let terminal =
           X.run_root
             executor
             ~managed:None
             ~reference
             ~invocation:admitted
             ~authorize:(fun _ _ -> Ok ())
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         assert (Int.equal !calls 1);
         assert (
           I.equal_status (H.status terminal) (Resolved (Complete (`String "disclosed"))));
         let observed = R.observe handle |> ok in
         let prior =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
         in
         assert (
           H.equal_context
             (H.context terminal)
             (H.context (R.Invocation_observation.value prior)));
         assert (
           I.equal_status
             (H.status terminal)
             (H.status (R.Invocation_observation.value prior)));
         let after_wait = invocation owner selected reference ~custom in
         let revoked =
           X.run_root
             executor
             ~managed:None
             ~reference
             ~invocation:after_wait
             ~authorize:(fun _ _ -> O.narrow_selection root ~names:[])
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         (match H.status revoked with
          | Resolved (Fail _) -> ()
          | _ -> failwith "replaced selected capability reached a native effect");
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed worker alone admits job child and inherited nested ancestry" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let invoke_nested = ref (fun () -> ()) in
  let retained_active = ref None in
  let nested_id = ref None in
  with_executor
    ~run_name:"dvr_claimed_native_parent"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !invoke_nested ()) parent_calls child_calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let parent_binding = C.find selected ~name:"parent_tool" |> ok in
       let parent_reference = C.reference parent_binding in
       let child_binding = C.find selected ~name:"child_tool" |> ok in
       let child_reference = C.reference child_binding in
       let parent_invocation =
         let admitted = invocation owner selected parent_reference ~custom:false in
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       (invoke_nested
        := fun () ->
             let admitted = invocation owner selected child_reference ~custom:false in
             let child =
               H.create_developer
                 { (H.context admitted) with
                   parent_invocation = Some (H.context parent_invocation).id
                 ; parent_job = Some job_id
                 }
               |> ok
             in
             nested_id := Some (H.context child).id;
             ignore
               (X.run_child
                  executor
                  ~managed:None
                  ~reference:child_reference
                  ~invocation:child
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "child"))
                : (H.t, P.Error.t) result));
       let expected = R.observe handle |> ok in
       let completed =
         O.with_claimed_job
           root
           ~expected
           ~lookup:(R.find_job_exact expected job_id)
           (fun ~active ~sw:_ ->
              retained_active := Some active;
              let ancestor = O.job_parent root active |> ok in
              assert (P.Id.Job.equal (Scope.Parent.job_id ancestor) job_id);
              assert (Int.equal (Scope.Parent.attempt ancestor) 1);
              assert (Int.equal (Scope.Parent.nested_depth ancestor) 0);
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner:(T.owner "dvr_foreign_claim" 0)
                     ~capture_fingerprint:
                       (Chatml_developer_capture.fingerprint (O.capture root))));
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner
                     ~capture_fingerprint:
                       (Chatml_source.Digest.of_contents
                          "foreign capture"
                          ~checkpoint:ignore)));
              let bad_context =
                H.create_developer
                  { (H.context parent_invocation) with
                    id = P.Id.Invocation.create ()
                  ; parent_job = Some (P.Id.Job.create ())
                  }
                |> ok
              in
              assert (
                Result.is_error
                  (X.run_job_child
                     executor
                     ~active
                     ~managed:None
                     ~reference:parent_reference
                     ~invocation:bad_context
                     ~authorize:(fun _ _ -> Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "parent"))));
              assert (Int.equal !parent_calls 0);
              let terminal =
                X.run_job_child
                  executor
                  ~active
                  ~managed:None
                  ~reference:parent_reference
                  ~invocation:parent_invocation
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "parent"))
                |> ok
              in
              assert (
                I.equal_status
                  (H.status terminal)
                  (Resolved (Complete (`String "parent"))));
              Ok (succeeded_claim (Scope.claim active)))
         |> ok
         |> Option.value_exn
       in
       assert (Int.equal !parent_calls 1);
       assert (Int.equal !child_calls 1);
       (match (P.Developer_job.state completed).status with
        | P.Job.Succeeded -> ()
        | _ -> failwith "claimed worker did not durably complete");
       let observed = R.observe handle |> ok in
       let child_id = Option.value_exn !nested_id in
       assert (
         Option.is_some (R.Invocation_lookup.prior (R.find_invocation observed child_id)));
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let pinned_job = stage_root_job env root selected in
       let pinned_parent =
         let admitted = invocation owner pinned parent_reference ~custom:false in
         H.create_developer { (H.context admitted) with parent_job = Some pinned_job }
         |> ok
       in
       (invoke_nested
        := fun () ->
             let child =
               H.create_developer
                 { (H.context (invocation owner pinned child_reference ~custom:false)) with
                   parent_invocation = Some (H.context pinned_parent).id
                 ; parent_job = Some pinned_job
                 }
               |> ok
             in
             let result =
               X.run_child
                 executor
                 ~managed:None
                 ~reference:child_reference
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "child"))
             in
             match result with
             | Ok terminal ->
               (match H.status terminal with
                | Resolved (Fail { code = "invocation.stale_binding"; _ }) -> ()
                | _ -> failwith "pinned child was not rejected by selected binding")
             | Error _ -> failwith "pinned child rejected before binding admission");
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected pinned_job)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   ~selected:pinned
                   executor
                   ~active
                   ~managed:None
                   ~reference:parent_reference
                   ~invocation:pinned_parent
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(fun _ -> Ok (`String "parent"))
                 |> ok
               in
               assert (
                 I.equal_status
                   (H.status terminal)
                   (Resolved (Complete (`String "parent"))));
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       assert (Int.equal !parent_calls 2);
       assert (Int.equal !child_calls 1);
       let expired = Option.value_exn !retained_active in
       let retry =
         H.create_developer
           { (H.context parent_invocation) with id = P.Id.Invocation.create () }
         |> ok
       in
       assert (
         Result.is_error
           (X.run_job_child
              executor
              ~active:expired
              ~managed:None
              ~reference:parent_reference
              ~invocation:retry
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "parent"))));
       assert (Int.equal !parent_calls 2))
;;

let%test_unit "claimed worker action atomically installs its selected child" =
  List.iter [ `Succeeded; `Queued; `Selection_changed ] ~f:(fun mode ->
    let calls = ref 0 in
    with_executor
      ~run_name:"dvr_claimed_selected_action"
      ~make_selected:(fun env -> registry ~custom:false calls)
      (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ->
         let parent_id = Selected.job_id "job_claimed_retry_selected_parent" in
         let capture_fingerprint =
           Chatml_developer_capture.fingerprint (O.capture root)
         in
         let original =
           Selected.job
             (O.owner root)
             capture_fingerprint
             ~id:parent_id
             ~parent:P.Developer_job.Launch.Run_root
         in
         let parent =
           P.Developer_job.create
             ~owner:(O.owner root)
             ~capture_fingerprint
             ~id:parent_id
             ~launch:(P.Developer_job.launch original)
             ~kind:(P.Developer_job.kind original)
             ~payload:(P.Developer_job.payload original)
             ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
             ~created_at:(P.Developer_job.created_at original)
             ~state:(P.Developer_job.state original)
           |> ok
         in
         let prepared_root =
           R.prepare_root_selected
             handle
             ~expected:(R.observe handle |> ok)
             ~phase:R.Checkpoint.Active
             ~selected:[ parent ]
           |> ok
         in
         ignore
           (R.commit_and_settle
              handle
              ~admit:(fun () -> true)
              prepared_root
              (R.Handoff.create ~settle:ignore)
            |> ok
            : R.Settlement.t);
         let prepared =
           Chat_response.One_off_script.prepare_in_domain
             ~env
             ~capabilities:selected
             ~tools:[]
             ~source:"let main input = Task.pure(input)"
             ()
           |> ok
         in
         let request =
           Chat_response.Background_request.script
             ~prepared
             ~input:`Null
             ~policy:Chat_response.One_off_request.default_policy
           |> ok
         in
         let candidates = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
         let candidate =
           List.find_exn candidates ~f:(fun candidate ->
             P.Id.Job.equal (P.Developer_job.id (O.Job_candidate.job candidate)) parent_id)
         in
         let selected_child = ref None in
         let dropped_child = ref None in
         let outcome =
           O.Job_candidate.with_claimed_selected
             root
             candidate
             (fun ~active ~action ~sw:_ ->
                incr calls;
                let dropped =
                  O.Worker_action.stage_background action ~request ~checkpoint:ignore
                  |> ok
                in
                dropped_child := Some dropped;
                O.Worker_action.rollback_staged action dropped |> ok;
                match mode with
                | `Selection_changed ->
                  O.narrow_selection root ~names:[] |> ok;
                  (match
                     O.Worker_action.stage_background action ~request ~checkpoint:ignore
                   with
                   | Error error ->
                     assert (
                       String.equal
                         error.P.Error.message
                         "developer worker selected registry changed")
                   | Ok _ -> failwith "changed Worker selection staged a child");
                  Error (P.Error.invalid_request "selected registry changed")
                | `Succeeded | `Queued ->
                  let child =
                    O.Worker_action.stage_background action ~request ~checkpoint:ignore
                    |> ok
                  in
                  selected_child := Some child;
                  let selection = O.Worker_action.seal action ~selected:[ child ] |> ok in
                  let after =
                    match mode with
                    | `Succeeded -> succeeded_claim (Scope.claim active)
                    | `Queued ->
                      { (P.Developer_job.state (R.Claim.job (Scope.claim active))) with
                        status = P.Job.Queued
                      ; started_at = None
                      ; next_run_at = Some (T.now ())
                      ; completed_at = None
                      ; result = None
                      ; retry_failure = Some (P.Error.invalid_request "retry")
                      }
                    | `Selection_changed -> assert false
                  in
                  Ok (after, selection))
         in
         let observed = R.observe handle |> ok in
         (match mode, outcome with
          | `Succeeded, Ok (Some (parent, disposition)) ->
            let child_id = Option.value_exn !selected_child in
            assert (P.Id.Job.equal (P.Developer_job.id parent) parent_id);
            (match disposition with
             | Agent_server.Developer_job_worker.Children.Settlement.Selected_installed ->
               ()
             | Selected_omitted_after_epoch_change ->
               failwith "active claimed selection was omitted");
            let child = R.find_job observed child_id |> Option.value_exn in
            let launch = P.Developer_job.launch child in
            (match P.Developer_job.Launch.parent launch with
             | Run_root -> ()
             | Invocation _ -> failwith "claimed worker child invented invocation parent");
            (match P.Developer_job.Launch.parent_job launch with
             | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1
               -> ()
             | None | Some _ -> failwith "claimed worker child lost exact parent attempt");
            assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1)
          | `Queued, Error error ->
            let child_id = Option.value_exn !selected_child in
            assert (
              String.equal
                error.P.Error.message
                "developer queued completion cannot select child jobs");
            assert (Option.is_none (R.find_job observed child_id));
            let parent = R.find_job observed parent_id |> Option.value_exn in
            (match (P.Developer_job.state parent).status with
             | P.Job.Failed _ -> ()
             | _ -> failwith "invalid selected parent did not fail durably");
            ignore (O.current root |> ok : O.Current.t)
          | `Selection_changed, Error _ ->
            assert (Option.is_none !selected_child);
            let parent = R.find_job observed parent_id |> Option.value_exn in
            (match (P.Developer_job.state parent).status with
             | P.Job.Failed _ -> ()
             | _ -> failwith "changed Worker selection did not fail durably")
          | (`Succeeded | `Queued | `Selection_changed), (Ok None | Ok (Some _) | Error _)
            -> failwith "claimed selected worker returned the wrong outcome");
         assert (Option.is_none (R.find_job observed (Option.value_exn !dropped_child)));
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed worker lexical jobs retain the saved tool ceiling" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_claimed_pinned_ceiling"
    ~make_selected:(fun _ -> pair_registry ~on_parent:ignore parent_calls child_calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store:_ ~blobs:_ ->
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let parent_id = stage_root_job ~tools:[ "parent_tool" ] env root pinned in
       let request capabilities name =
         let binding = C.find capabilities ~name |> ok in
         Chat_response.Background_request.tool
           ~capabilities
           ~reference:(C.reference binding)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let pinned_request = request pinned "parent_tool" in
       let wider_request = request selected "child_tool" in
       let unselected_child = ref None in
       let candidate =
         O.Job_candidate.read_current root ~checkpoint:ignore
         |> ok
         |> List.find_exn ~f:(fun candidate ->
           P.Id.Job.equal (P.Developer_job.id (O.Job_candidate.job candidate)) parent_id)
       in
       let completed =
         Jobs.with_claimed_worker jobs ~candidate (fun ~active ~scope ~sw:_ ->
           assert (
             Result.is_error
               (Jobs.Scope.stage_background
                  scope
                  ~request:wider_request
                  ~checkpoint:ignore));
           unselected_child
           := Some
                (Jobs.Scope.stage_background
                   scope
                   ~request:pinned_request
                   ~checkpoint:ignore
                 |> ok);
           Ok (succeeded_claim (Scope.claim active), []))
         |> ok
         |> Option.value_exn
       in
       assert (P.Id.Job.equal (P.Developer_job.id completed) parent_id);
       let observed = R.observe handle |> ok in
       assert (Option.is_none (R.find_job observed (Option.value_exn !unselected_child)));
       assert (Int.equal !parent_calls 0);
       assert (Int.equal !child_calls 0))
;;

let%test_unit "job child permission wait rechecks selection and real deadline" =
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  List.iter [ `Revoke; `Expire ] ~f:(fun mode ->
    let calls = ref 0 in
    let clock = ref (T.now ()) in
    with_executor
      ~now:(fun () -> !clock)
      ~run_name:
        (match mode with
         | `Revoke -> "dvr_job_revoke"
         | `Expire -> "dvr_job_expire")
      ~make_selected:(fun _env -> registry ~custom:false calls)
      (fun ~env ~root ~executor ~handle ~owner ~selected ->
         let job_id = stage_root_job env root selected in
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom:false in
         let admitted =
           H.create_developer
             { (H.context admitted) with
               parent_job = Some job_id
             ; deadline = Some deadline
             }
           |> ok
         in
         let expected = R.observe handle |> ok in
         ignore
           (O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw:_ ->
                 let terminal =
                   X.run_job_child
                     executor
                     ~active
                     ~managed:None
                     ~reference
                     ~invocation:admitted
                     ~authorize:(fun _ _ ->
                       Eio.Fiber.yield ();
                       match mode with
                       | `Revoke -> O.narrow_selection root ~names:[]
                       | `Expire ->
                         clock := expired_at;
                         Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "private"))
                   |> ok
                 in
                 (match H.status terminal with
                  | Resolved (Fail _) -> ()
                  | _ -> failwith "expired or revoked job parent reached native effect");
                 Ok (succeeded_claim (Scope.claim active)))
            |> ok
            : P.Developer_job.t option);
         assert (Int.equal !calls 0)))
;;

let%test_unit "cancelled claimed parent cannot start an effect while child joins" =
  let calls = ref 0 in
  let original = Failure "claimed parent cancelled before child joined" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  with_executor
    ~run_name:"dvr_cancelled_job_parent"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       let expected = R.observe handle |> ok in
       (match
          Eio.Cancel.sub (fun cancellation ->
            O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw ->
                 Eio.Fiber.fork ~sw (fun () ->
                   Eio.Cancel.protect (fun () ->
                     Eio.Promise.resolve ready_u ();
                     Eio.Promise.await release;
                     child_denied
                     := Result.is_error
                          (X.run_job_child
                             executor
                             ~active
                             ~managed:None
                             ~reference
                             ~invocation:admitted
                             ~authorize:(fun _ _ -> Ok ())
                             ~prepare_output:(fun _ -> Ok (`String "private")))));
                 Eio.Promise.await ready;
                 Eio.Cancel.protect (fun () ->
                   Eio.Cancel.cancel cancellation original;
                   Eio.Promise.resolve release_u ());
                 Ok (succeeded_claim (Scope.claim active))))
        with
        | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "claimed worker cancellation identity was lost");
       assert !child_denied;
       assert (Int.equal !calls 0))
;;

let%test_unit "cancelled native parent switch rejects protected nested child" =
  let original = Failure "native parent switch failed before child join" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  let effect_calls = ref 0 in
  with_executor
    ~run_name:"dvr_native_parent_switch"
    ~make_selected:(fun _env -> registry ~custom:false effect_calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let child =
         let initial = invocation owner selected reference ~custom:false in
         H.create_developer
           { (H.context initial) with parent_invocation = Some (H.context admitted).id }
         |> ok
       in
       let issuer =
         D.create
           ~owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint (O.capture root))
           ~ceiling:(O.native_ceiling root)
           ~selected:(fun () -> O.native_selected root)
           ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
             Result.map (O.current root) ~f:ignore)
           ~now:(fun () -> O.native_now root)
         |> ok
       in
       let scope, completion =
         O.with_native_registration root ~invocation:admitted (fun ~scope ->
           let dispatch : D.dispatch_storage =
             { read = O.Native_terminal.read_dispatch scope
             ; save_dispatch = O.Native_terminal.save_dispatch scope
             }
           in
           let completion =
             D.with_invocation
               issuer
               ~parent:D.Root
               ~invocation:admitted
               ~dispatch
               ~parent_live_now:(fun () -> O.Native_terminal.effect_parent_live_now scope)
               ~frozen:(fun () -> None)
               (fun ~active ~dispatched ->
                  O.with_native_callback scope ~dispatched (fun ~sw ->
                    Eio.Fiber.fork ~sw (fun () ->
                      Eio.Cancel.protect (fun () ->
                        Eio.Promise.resolve ready_u ();
                        Eio.Promise.await release;
                        child_denied
                        := Result.is_error
                             (D.check_parent_now issuer ~parent:(D.Child active) child);
                        if not !child_denied then Int.incr effect_calls));
                    Eio.Promise.await ready;
                    Eio.Cancel.protect (fun () ->
                      Eio.Switch.fail sw original;
                      Eio.Promise.resolve release_u ());
                    Ok (I.Complete `Null, None)))
             |> ok
           in
           O.Native_terminal.remember_failure
             scope
             ~terminal:(D.Completion.terminal completion)
           |> ok;
           Ok (scope, completion))
         |> ok
       in
       assert !child_denied;
       assert (Int.equal !effect_calls 0);
       (match D.Completion.dispatch completion with
        | Installed _ -> ()
        | Awaiting_reconciliation -> failwith "failed native parent lost durable dispatch");
       let terminal_revision = O.Native_terminal.resolve_remembered scope |> ok in
       match
         D.Completion.finish completion ~settlement:(Terminal_saved terminal_revision)
       with
       | exception actual -> assert (phys_equal actual original)
       | Ok _ | Error _ -> failwith "failed native parent lost exception identity")
;;

let%test_unit "job expiry during managed preparation cannot reserve child capacity" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let clock = ref (T.now ()) in
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  let denied = ref 0 in
  with_executor
    ~now:(fun () -> !clock)
    ~run_name:"dvr_job_prepare_expiry"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some job_id
           ; deadline = Some deadline
           }
         |> ok
       in
       let prepared =
         Chat_response.One_off_script.prepare_in_domain
           ~env
           ~capabilities:selected
           ~tools:[]
           ~source:"let main input = Task.pure(input)"
           ()
         |> ok
       in
       let request =
         Chat_response.Background_request.script
           ~prepared
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution
               _borrowed
               ~effect_entry
               ~stage_background
               ~install_jobs:_
               _continuation ->
               match effect_entry () with
               | Error _ -> failwith "managed job child expired before preparation"
               | Ok () ->
                 (match
                    stage_background ~request ~checkpoint:(fun () ->
                      if P.Timestamp.compare !clock deadline < 0
                      then (
                        Eio.Fiber.yield ();
                        clock := expired_at))
                  with
                  | Error _ ->
                    Int.incr denied;
                    Error
                      (I.Fail
                         { code = "expired"
                         ; message = ""
                         ; retryable = false
                         ; details = `Null
                         })
                  | Ok _ -> failwith "expired claimed parent reserved child work"))
         }
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected job_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~managed:(Some service)
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               (match H.status terminal with
                | Resolved (Fail _) -> ()
                | _ -> failwith "managed preparation ignored claimed-parent expiry");
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       assert (Int.equal !denied 1);
       ignore (stage_root_job env root selected : P.Id.Job.t))
;;

let%test_unit "managed job child stages exact claimed attempt and nesting depth" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let selected_child = ref None in
  with_executor
    ~run_name:"dvr_job_managed_ancestry"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let parent_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some parent_id
           }
         |> ok
       in
       let prepared =
         Chat_response.One_off_script.prepare_in_domain
           ~env
           ~capabilities:selected
           ~tools:[]
           ~source:"let main input = Task.pure(input)"
           ()
         |> ok
       in
       let request =
         Chat_response.Background_request.script
           ~prepared
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution
               _borrowed
               ~effect_entry
               ~stage_background
               ~install_jobs:_
               continuation ->
               effect_entry () |> ok;
               let id = stage_background ~request ~checkpoint:ignore |> ok in
               selected_child := Some id;
               continuation
                 ~validate_work:(fun _ -> Ok ())
                 ~outcome:(I.Complete (`Object []))
                 ~selected_jobs:[ id ])
         }
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected parent_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~managed:(Some service)
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               if
                 not
                   (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))))
               then failwith (Sexp.to_string_hum (I.sexp_of_status (H.status terminal)));
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       let child_id = Option.value_exn !selected_child in
       let child = R.find_job (R.observe handle |> ok) child_id |> Option.value_exn in
       let launch = P.Developer_job.launch child in
       (match P.Developer_job.Launch.parent launch with
        | Invocation id when P.Id.Invocation.equal id (H.context admitted).id -> ()
        | Run_root | Invocation _ -> failwith "child launch lost invocation parent");
       (match P.Developer_job.Launch.parent_job launch with
        | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1 -> ()
        | None | Some _ -> failwith "child launch lost claimed attempt");
       assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1))
;;

let%test_unit "managed selected IDs survive Complete and staging expires with callback" =
  let managed_definition = ref None in
  with_executor
    ~run_name:"dvr_selected_managed"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root:_ ~executor ~handle ~owner ~selected ->
       let module M = Chat_response.Managed_tool_registry in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let revision = M.execution_revision managed binding |> ok in
       let admitted =
         H.create_developer
           { (H.context (invocation owner selected reference ~custom:false)) with
             implementation_revision = revision
           }
         |> ok
       in
       let prepared =
         Chat_response.One_off_script.prepare_in_domain
           ~env
           ~capabilities:selected
           ~tools:[]
           ~source:"let main input = Task.pure(input)"
           ()
         |> ok
       in
       let request =
         Chat_response.Background_request.script
           ~prepared
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let stage_after = ref None in
       let selected_ids = ref [] in
       let fail_after_seal = ref false in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution
               _borrowed
               ~effect_entry
               ~stage_background
               ~install_jobs:_
               continuation ->
               stage_after := Some stage_background;
               match effect_entry () with
               | Error _ ->
                 Error
                   (I.Fail
                      { code = "expired"
                      ; message = ""
                      ; retryable = false
                      ; details = `Null
                      })
               | Ok () ->
                 let id = stage_background ~request ~checkpoint:ignore |> ok in
                 selected_ids := id :: !selected_ids;
                 let returned =
                   continuation
                     ~validate_work:(fun _ -> Ok ())
                     ~outcome:(I.Complete (`Object []))
                     ~selected_jobs:[ id ]
                 in
                 if !fail_after_seal
                 then
                   Error
                     (I.Fail
                        { code = "late managed cleanup"
                        ; message = "the sealed result remains canonical"
                        ; retryable = false
                        ; details = `Null
                        })
                 else returned)
         }
       in
       let run admitted =
         X.run_root
           executor
           ~managed:(Some service)
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected managed result"))
         |> ok
       in
       let terminal = run admitted in
       assert (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))));
       let id = List.hd_exn !selected_ids in
       assert (Option.is_some (R.find_job (R.observe handle |> ok) id));
       assert (
         Result.is_error ((Option.value_exn !stage_after) ~request ~checkpoint:ignore));
       fail_after_seal := true;
       let second =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       let retained = run second in
       assert (I.equal_status (H.status retained) (Resolved (Complete (`Object []))));
       assert (List.length !selected_ids = 2);
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (List.hd_exn !selected_ids))))
;;

let%test_unit "early selected read failure never enters handler and releases scope" =
  let armed = ref false in
  let fail_next_checkpoint = ref false in
  let failure = Failure "selected read failed before dispatch" in
  let checkpoint () =
    if !fail_next_checkpoint
    then (
      fail_next_checkpoint := false;
      raise failure)
  in
  let on_current () =
    if !armed
    then (
      armed := false;
      fail_next_checkpoint := true)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~on_current
    ~run_name:"dvr_selected_early_read"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let run () =
         X.run_root
           executor
           ~managed:None
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "disclosed"))
       in
       armed := true;
       (match run () with
        | exception actual -> assert (phys_equal actual failure)
        | Ok _ | Error _ -> failwith "early Store exception identity was lost");
       assert (Int.equal !calls 0);
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context admitted).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t);
       ignore (run () |> ok : H.t);
       assert (Int.equal !calls 1))
;;

let%test_unit "uncertain selected dispatch retains exact failure until owner recovery" =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let fail_before_rename = ref false in
  let fail_recovery_checkpoint = ref false in
  let original = Failure "original selected dispatch failure" in
  let later = Failure "later selected recovery failure" in
  let calls = ref 0 in
  with_executor
    ~checkpoint:(fun () ->
      if !fail_recovery_checkpoint
      then (
        fail_recovery_checkpoint := false;
        raise later))
    ~run_name:"dvr_selected_uncertain"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      Developer_run_store_test_suite.Developer_run_store_tests
      .fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !fail_before_rename
          then (
            fail_before_rename := false;
            fail_recovery_checkpoint := true;
            raise original))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       rename_armed := true;
       assert (
         Result.is_error
           (X.run_root
              executor
              ~managed:None
              ~reference
              ~invocation:admitted
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))));
       assert (not !rename_armed);
       assert (Int.equal !calls 0);
       sync_blocked := false;
       let id = (H.context admitted).id in
       let prior () =
         R.find_invocation (R.observe handle |> ok) id
         |> R.Invocation_lookup.prior
         |> Option.value_exn
         |> R.Invocation_observation.value
       in
       assert (I.equal_status (H.status (prior ())) Dispatching);
       assert (Result.is_error (O.current root));
       O.reconcile_pending root |> ok;
       (match H.status (prior ()) with
        | Resolved (Fail _) -> ()
        | _ -> failwith "retained native failure was not durably resolved");
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t);
       let second =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       fail_before_rename := true;
       (match
          X.run_root
            executor
            ~managed:None
            ~reference
            ~invocation:second
            ~authorize:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        with
        | exception actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "recovery replaced the original dispatch exception");
       assert (Int.equal !calls 0);
       O.reconcile_pending root |> ok;
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context second).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "absent native recovery cannot release another scope's dispatch writer" =
  let pause_first_recovery = ref false in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let checkpoint () =
    if !pause_first_recovery
    then (
      pause_first_recovery := false;
      Eio.Promise.resolve reached_u ();
      Eio.Promise.await release)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~run_name:"dvr_selected_distinct_dispatch_owner"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let first = invocation owner selected reference ~custom:false in
       let second = invocation owner selected reference ~custom:false in
       let first_scope =
         O.with_native_registration root ~invocation:first (fun ~scope -> Ok scope) |> ok
       in
       let dispatched = H.dispatch second |> ok in
       Eio.Switch.run (fun sw ->
         let ready, ready_u = Eio.Promise.create () in
         let go, go_u = Eio.Promise.create () in
         let finished, finished_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration root ~invocation:second (fun ~scope ->
               Eio.Promise.resolve ready_u ();
               Eio.Promise.await go;
               let revision =
                 O.Native_terminal.save_dispatch scope ~expected_revision:None ~dispatched
                 |> ok
               in
               let selected, terminal =
                 O.with_native_callback scope ~dispatched (fun ~sw:_ ->
                   let selected =
                     O.Native_terminal.seal scope ~outcome:(I.Complete `Null) ~selected:[]
                     |> ok
                   in
                   let terminal = H.resolve dispatched (I.Complete `Null) |> ok in
                   Ok (selected, terminal))
                 |> ok
               in
               ignore
                 (O.Native_terminal.submit scope ~terminal ~selected:(Some selected) |> ok
                  : int);
               Ok revision)
           in
           Eio.Promise.resolve finished_u result);
         Eio.Promise.await ready;
         let witness = O.current root |> ok in
         let first_finished, first_finished_u = Eio.Promise.create () in
         pause_first_recovery := true;
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve
             first_finished_u
             (O.Native_terminal.reconcile_dispatch first_scope));
         Eio.Promise.await reached;
         Eio.Promise.resolve go_u ();
         Eio.Fiber.yield ();
         assert (Result.is_error (O.check_effect_now root witness));
         Eio.Promise.resolve release_u ();
         (match Eio.Promise.await first_finished |> ok with
          | O.Native_terminal.Absent -> ()
          | Installed _ | Uncertain -> failwith "untouched scope acquired dispatch");
         ignore (Eio.Promise.await finished |> ok : int));
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "nested selected child uses live direct-parent proof and joins" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let on_parent = ref (fun () -> failwith "parent executor was not installed") in
  with_executor
    ~run_name:"dvr_selected_nested"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !on_parent ()) parent_calls child_calls)
    (fun ~env:_ ~root:_ ~executor ~handle ~owner ~selected ->
       let reference name = C.find selected ~name |> ok |> C.reference in
       let root = invocation owner selected (reference "parent_tool") ~custom:false in
       let child =
         H.create_developer
           { (H.context
                (invocation owner selected (reference "child_tool") ~custom:false))
             with
             parent_invocation = Some (H.context root).id
           }
         |> ok
       in
       (on_parent
        := fun () ->
             let terminal =
               X.run_child
                 executor
                 ~managed:None
                 ~reference:(reference "child_tool")
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "child disclosed"))
               |> ok
             in
             assert (
               I.equal_status
                 (H.status terminal)
                 (Resolved (Complete (`String "child disclosed")))));
       let parent =
         X.run_root
           executor
           ~managed:None
           ~reference:(reference "parent_tool")
           ~invocation:root
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "parent disclosed"))
         |> ok
       in
       assert (
         I.equal_status
           (H.status parent)
           (Resolved (Complete (`String "parent disclosed"))));
       assert (Int.equal !parent_calls 1 && Int.equal !child_calls 1);
       assert (
         Result.is_error
           (X.run_child
              executor
              ~managed:None
              ~reference:(reference "child_tool")
              ~invocation:
                (H.create_developer
                   { (H.context child) with id = P.Id.Invocation.create () }
                 |> ok)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "child disclosed"))));
       let observed = R.observe handle |> ok in
       List.iter [ root; child ] ~f:(fun admitted ->
         let durable =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
           |> R.Invocation_observation.value
         in
         match H.status durable with
         | Resolved (Complete _) -> ()
         | _ -> failwith "nested selected invocation did not finish durably"))
;;
