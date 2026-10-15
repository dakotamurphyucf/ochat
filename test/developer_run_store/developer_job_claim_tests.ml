open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module Scope = Agent_session.Developer_job_scope
module Worker = Agent_server.Developer_job_worker
module Publisher = Agent_store.Developer_result_publisher
module Blobs = Agent_store.Blob_store
module Disclosure = Agent_server.Developer_result_disclosure
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests

let timestamp = Selected.timestamp

let capacity ?(max_total = 2) ?(per_session = 1) () =
  let limits : Agent_server.Config.Server.job_limits =
    { daemon_total = max_total
    ; per_principal = max_total
    ; per_prompt = max_total
    ; per_workspace = max_total
    ; per_session
    ; per_kind = max_total
    ; max_nested_depth = 2
    }
  in
  Agent_server.Job_capacity.create ~limits
;;

let project_source =
  Chatml_source.Id.create ~package_namespace:"developer-test" ~relative_path:"main.chatml"
  |> Or_error.ok_exn
;;

let worker ?(now = fun () -> timestamp) ~sw ~store ~handle ~capacity () =
  Worker.create
    ~sw
    ~store
    ~initial:handle
    ~capacity
    ~project_source
    ~workspace_conflict_domain:"developer-test"
    ~now
    ~max_active:1
    ~max_quarantined:2
    ()
  |> F.ok
;;

let select_root ?(retry_policy = P.Job.Never) handle initial owner capture id =
  let original = Selected.job owner capture ~id ~parent:P.Developer_job.Launch.Run_root in
  let job =
    P.Developer_job.create
      ~owner
      ~capture_fingerprint:capture
      ~id
      ~launch:(P.Developer_job.launch original)
      ~kind:(P.Developer_job.kind original)
      ~payload:(P.Developer_job.payload original)
      ~retry_policy
      ~created_at:(P.Developer_job.created_at original)
      ~state:(P.Developer_job.state original)
    |> F.ok
  in
  let prepared =
    R.prepare_root_selected
      handle
      ~expected:initial
      ~phase:R.Checkpoint.Active
      ~selected:[ job ]
    |> F.run_ok
  in
  let observed =
    R.commit_and_settle handle ~admit:(fun () -> true) prepared (Selected.handoff ())
    |> F.run_ok
    |> Selected.installed
  in
  job, observed
;;

let terminal_state claim status =
  let state = P.Developer_job.state (R.Claim.job claim) in
  { state with
    status
  ; completed_at = Some timestamp
  ; result =
      (match status with
       | P.Job.Succeeded ->
         Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
       | Queued
       | Running
       | Waiting_permission _
       | Waiting_completion _
       | Failed _
       | Cancelled
       | Interrupted _ -> None)
  }
;;

let claimed_child ~owner ~capture ~parent ~parent_attempt ~depth ~id =
  let launch =
    P.Developer_job.Launch.create
      ~parent:P.Developer_job.Launch.Run_root
      ~parent_job:(Some (parent, parent_attempt))
      ~nested_depth:depth
    |> F.ok
  in
  let state : P.Developer_job.State.t =
    { status = P.Job.Queued
    ; attempt = 0
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; retry_failure = None
    }
  in
  P.Developer_job.create
    ~owner
    ~capture_fingerprint:capture
    ~id
    ~launch
    ~kind:P.Job.Async_tool
    ~payload:(`Object [ "selected_child", `True ])
    ~retry_policy:P.Job.Never
    ~created_at:timestamp
    ~state
  |> F.ok
;;

let%test_unit
    "claimed selected transition installs exact children or omits them after epoch change"
  =
  List.iter [ `Active; `Old_epoch ] ~f:(fun mode ->
    F.with_root (fun _env _sw _root _session store ->
      let owner = F.owner 0 in
      let capture = F.digest "atomic claimed selected children" in
      let handle, initial = F.admit store owner capture in
      let parent_id = Selected.job_id "job_claimed_selected_parent" in
      let _, observed = select_root handle initial owner capture parent_id in
      let claim_prepared =
        R.prepare_job_claim
          handle
          ~expected:observed
          ~lookup:(R.find_job_exact observed parent_id)
          ~now:timestamp
        |> F.run_ok
      in
      let claimed = R.commit_job_claim handle claim_prepared |> F.run_ok in
      let claim = R.Claimed.claim claimed in
      assert (
        R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
        |> F.run_ok);
      let child_id = Selected.job_id "job_claimed_selected_child" in
      let child =
        claimed_child
          ~owner
          ~capture
          ~parent:parent_id
          ~parent_attempt:(R.Claim.attempt claim)
          ~depth:1
          ~id:child_id
      in
      let terminal = terminal_state claim P.Job.Succeeded in
      let expected =
        match mode with
        | `Active -> R.Claimed.observation claimed
        | `Old_epoch ->
          let prior = R.observe handle |> F.run_ok in
          let _next, _ =
            R.admit_epoch
              store
              ~owner:(F.owner 1)
              ~capture_fingerprint:(F.digest "next selected graph")
              ~previous:(Some prior)
            |> F.run_ok
          in
          R.observe handle |> F.run_ok
      in
      let invalid_child =
        claimed_child
          ~owner
          ~capture
          ~parent:parent_id
          ~parent_attempt:(R.Claim.attempt claim + 1)
          ~depth:1
          ~id:(Selected.job_id "job_claimed_selected_invalid")
      in
      (match mode with
       | `Active ->
         assert (
           Result.is_error
             (R.prepare_claimed_selected
                handle
                ~expected
                ~claim
                ~after:terminal
                ~revoked_after:terminal
                ~selected:[ invalid_child ]))
       | `Old_epoch -> ());
      let prepared =
        R.prepare_claimed_selected
          handle
          ~expected
          ~claim
          ~after:terminal
          ~revoked_after:terminal
          ~selected:[ child ]
        |> F.run_ok
      in
      let installed = R.commit_claimed_transition handle prepared |> F.run_ok in
      let disposition =
        R.Claimed_selection.installed_disposition ~prepared ~installed |> F.run_ok
      in
      let actual = R.Installed.observation installed in
      let parent = R.find_job actual parent_id |> Option.value_exn in
      assert (
        match (P.Developer_job.state parent).status with
        | P.Job.Succeeded -> true
        | _ -> false);
      match mode, disposition, R.find_job actual child_id with
      | `Active, Selected_installed, Some actual_child ->
        assert (
          Jsonaf.exactly_equal
            (P.Developer_job.to_json child)
            (P.Developer_job.to_json actual_child))
      | `Old_epoch, Selected_omitted_after_epoch_change, None -> ()
      | _ -> failwith "claimed selected disposition lost exact child set"))
;;

let%test_unit "claimed children bound concurrent preparation before capacity reserve" =
  F.with_root (fun env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "bounded claimed children" in
    let handle, initial = F.admit store owner capture in
    let parent_id = Selected.job_id "job_bounded_claimed_parent" in
    let _, observed = select_root handle initial owner capture parent_id in
    let selected =
      Chat_response.Tool_capability.create
        ~owner:"bounded-worker-test"
        ~resource_fingerprint:
          (Chatml_source.Digest.to_hex (F.digest "bounded worker tools"))
        []
      |> F.ok
    in
    let compiled =
      Chat_response.One_off_script.prepare_in_domain
        ~env
        ~capabilities:selected
        ~tools:[]
        ~source:"let main input = Task.pure(input)"
        ()
      |> F.ok
    in
    let request =
      Chat_response.Background_request.script
        ~prepared:compiled
        ~input:`Null
        ~policy:Chat_response.One_off_request.default_policy
      |> F.ok
    in
    let worker =
      worker ~sw ~store ~handle ~capacity:(capacity ~max_total:3 ~per_session:3 ()) ()
    in
    let inner = ref None in
    let selected_child = ref None in
    let limit_seen = ref false in
    let result =
      Worker.with_claimed_selected_attempt
        worker
        ~expected:observed
        ~lookup:(R.find_job_exact observed parent_id)
        ~max_staged_jobs:2
        (fun ~active ~sw:_ ~children ->
           let claim = Scope.claim active in
           let prepare () =
             let launch =
               P.Developer_job.Launch.create
                 ~parent:P.Developer_job.Launch.Run_root
                 ~parent_job:(Some (parent_id, R.Claim.attempt claim))
                 ~nested_depth:1
               |> F.ok
             in
             Agent_session.Developer_root_job_admission.background
               ~owner
               ~capture_fingerprint:capture
               ~launch
               ~selected
               ~policy:Chat_response.One_off_request.default_policy
               ~request
               ~created_at:timestamp
               ~checkpoint:ignore
             |> F.ok
           in
           let entered = ref false in
           let third_prepared = ref false in
           let outer =
             Worker.Children.stage
               children
               ~prepare:(fun () -> Ok (prepare ()))
               ~checkpoint:(fun () ->
                 if not !entered
                 then (
                   entered := true;
                   let checked = ref false in
                   let id =
                     Worker.Children.stage
                       children
                       ~prepare:(fun () -> Ok (prepare ()))
                       ~checkpoint:(fun () ->
                         if not !checked
                         then (
                           checked := true;
                           match
                             Worker.Children.stage
                               children
                               ~prepare:(fun () ->
                                 third_prepared := true;
                                 Ok (prepare ()))
                               ~checkpoint:ignore
                           with
                           | Error error ->
                             assert (P.Error.equal_code error.code P.Error.Resource_limit);
                             limit_seen := true
                           | Ok _ -> failwith "third in-flight child was admitted"))
                     |> F.ok
                   in
                   inner := Some id))
             |> F.ok
           in
           assert (not !third_prepared);
           selected_child := Some outer;
           let selection = Worker.Children.seal children ~selected:[ outer ] |> F.ok in
           Ok (terminal_state claim P.Job.Succeeded, selection))
      |> F.ok
      |> Option.value_exn
    in
    let _, disposition = result in
    (match disposition with
     | Worker.Children.Settlement.Selected_installed -> ()
     | Selected_omitted_after_epoch_change -> failwith "active claimed child was omitted");
    assert !limit_seen;
    let actual = R.observe handle |> F.run_ok in
    assert (Option.is_some (R.find_job actual (Option.value_exn !selected_child)));
    assert (Option.is_none (R.find_job actual (Option.value_exn !inner)));
    Worker.close_and_wait worker |> F.ok)
;;

let selected_artifact ~claim ~proposed ~ensure_attempted ~acknowledge_installed =
  let completion =
    match proposed.P.Developer_job.State.result with
    | Some (P.Developer_job.Result_value.Inline completion) -> completion
    | Some (Artifact _) | None -> failwith "expected inline terminal completion"
  in
  let content = P.Completion.to_json completion |> Jsonaf.to_string in
  let blob =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.create ())
      ~kind:File
      ~media_type:"application/vnd.ochat.job-result+json"
      ~byte_length:(Int64.of_int (String.length content))
      ~digest:Digestif.SHA256.(digest_string content |> to_hex)
      ()
    |> F.ok
  in
  let after =
    { proposed with
      result =
        Some
          (P.Developer_job.Result_value.Artifact
             { outcome = P.Stored_completion.Succeeded; blob })
    }
  in
  Worker.Selected.create ~claim ~proposed ~after ~ensure_attempted ~acknowledge_installed
  |> F.ok
;;

let%test_unit "real developer result publisher selects one post-join terminal" =
  List.iter
    [ Publisher.Storage.Automatic, 1024, `Null, false, false
    ; Artifact, 0, `Null, true, true
    ; Automatic, 32768, `String (String.make 32700 'x'), true, false
    ]
    ~f:(fun (storage, inline_bytes, payload, expect_artifact, interrupted_stage) ->
      let on_checkpoint = ref (fun () -> ()) in
      let checkpoint () = !on_checkpoint () in
      let with_root = F.with_root ~checkpoint in
      with_root (fun env sw native_root session store ->
        let owner = F.owner 0 in
        let capture = F.digest "real developer result publisher" in
        let handle, initial = F.admit store owner capture in
        let root = R.Result_root.of_handle handle |> F.run_ok in
        let data_root = Agent_store.Session_store.data_root session in
        let blobs =
          Blobs.create
            ~env
            ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
            ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
            ~max_upload_bytes:65536L
          |> F.store_ok
        in
        Blobs.bind_developer_runs blobs ~session_store:session store |> F.store_ok;
        let limits =
          Publisher.Limits.create ~inline_bytes ~max_result_bytes:65536 ~max_pending:1
          |> F.ok
        in
        let publisher = Publisher.create ~sw ~runs:store ~root ~blobs ~limits |> F.ok in
        let id = Selected.job_id "job_real_developer_publisher" in
        let foreign =
          if Int.equal inline_bytes 1024
          then (
            let other_session =
              F.create_session
                env
                sw
                (Filename.concat (Filename.dirname native_root) "foreign-data")
            in
            let other_store = F.open_run other_session sw () in
            let other_handle, other_initial = F.admit other_store owner capture in
            let other_job =
              Selected.job owner capture ~id ~parent:P.Developer_job.Launch.Run_root
            in
            let prepared =
              R.prepare_root_selected
                other_handle
                ~expected:other_initial
                ~phase:R.Checkpoint.Active
                ~selected:[ other_job ]
              |> F.run_ok
            in
            let observed =
              R.commit_and_settle
                other_handle
                prepared
                ~admit:(fun () -> true)
                (Selected.handoff ())
              |> F.run_ok
              |> Selected.installed
            in
            let prepared_claim =
              R.prepare_job_claim
                other_handle
                ~expected:observed
                ~lookup:(R.find_job_exact observed id)
                ~now:timestamp
              |> F.run_ok
            in
            let claimed = R.commit_job_claim other_handle prepared_claim |> F.run_ok in
            Some (other_session, other_store, other_handle, claimed))
          else None
        in
        let _, selected = select_root handle initial owner capture id in
        let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
        let callback_count = ref 0 in
        let stage_count = ref 0 in
        let retained = ref None in
        let first =
          Worker.with_claimed_attempt
            worker
            ~expected:selected
            ~lookup:(R.find_job_exact selected id)
            ~select:(fun ~claim ~proposed ->
              let open Result.Let_syntax in
              if Int.equal inline_bytes 1024
              then (
                let stopped =
                  Eio.Switch.run (fun child_sw ->
                    let publisher =
                      Publisher.create ~sw:child_sw ~runs:store ~root ~blobs ~limits
                      |> F.ok
                    in
                    Publisher.select
                      publisher
                      ~storage
                      ~claim
                      ~proposed
                      ~selected_at:timestamp
                    |> F.ok)
                in
                assert (
                  match Publisher.Selected.ensure_attempted stopped with
                  | exception Invalid_argument _ -> true
                  | (exception _) | Ok () | Error _ -> false);
                let claim = Publisher.Selected.claim stopped in
                let intent =
                  Developer_intent_fixture.path
                    data_root
                    ~owner:(R.Claim.owner claim)
                    ~job:(P.Developer_job.id (R.Claim.job claim))
                    ~attempt:(R.Claim.attempt claim)
                in
                assert (not (Developer_intent_fixture.present env intent)));
              let%bind selection =
                Publisher.select
                  publisher
                  ~storage
                  ~claim
                  ~proposed
                  ~selected_at:timestamp
              in
              retained := Some selection;
              Option.iter foreign ~f:(fun (_, _, other_handle, claimed) ->
                let foreign_claim = R.Claimed.claim claimed in
                assert (
                  Result.is_error
                    (Publisher.select
                       publisher
                       ~storage:Publisher.Storage.Automatic
                       ~claim:foreign_claim
                       ~proposed
                       ~selected_at:timestamp));
                assert (
                  R.Claim.try_enter
                    foreign_claim
                    ~handle:other_handle
                    ~admit:(fun () -> true)
                    ~on_enter:(fun () -> ())
                  |> F.run_ok);
                Publisher.Selected.ensure_attempted selection |> F.ok;
                let after = Publisher.Selected.after selection in
                let prepared =
                  R.prepare_claimed_transition
                    other_handle
                    ~expected:(R.Claimed.observation claimed)
                    ~claim:foreign_claim
                    ~after
                    ~revoked_after:after
                  |> F.run_ok
                in
                let installed =
                  R.commit_claimed_transition other_handle prepared |> F.run_ok
                in
                assert (
                  Jsonaf.exactly_equal
                    (P.Developer_job.State.to_json
                       (P.Developer_job.state
                          (R.find_job (R.Installed.observation installed) id
                           |> Option.value_exn)))
                    (P.Developer_job.State.to_json after));
                assert (
                  Result.is_error
                    (Publisher.Selected.acknowledge_installed selection installed)));
              let opposite =
                match storage with
                | Publisher.Storage.Automatic -> Publisher.Storage.Artifact
                | Publisher.Storage.Artifact -> Publisher.Storage.Automatic
              in
              let%bind replay =
                Publisher.select
                  publisher
                  ~storage:opposite
                  ~claim
                  ~proposed
                  ~selected_at:timestamp
              in
              assert (phys_equal replay selection);
              let%map selected =
                Worker.Selected.create
                  ~claim:(Publisher.Selected.claim selection)
                  ~proposed:(Publisher.Selected.proposed selection)
                  ~after:(Publisher.Selected.after selection)
                  ~ensure_attempted:(fun () ->
                    let open Result.Let_syntax in
                    let%bind () = Publisher.Selected.ensure_attempted selection in
                    incr stage_count;
                    if interrupted_stage && Int.equal !stage_count 1
                    then
                      Error (P.Error.invalid_request "stage acknowledgement interrupted")
                    else Ok ())
                  ~acknowledge_installed:(fun installed ->
                    Publisher.Selected.acknowledge_installed selection installed)
              in
              Some selected)
            (fun ~active ~sw:_ ->
               incr callback_count;
               let state = terminal_state (Scope.claim active) P.Job.Succeeded in
               Ok
                 { state with
                   result =
                     Some
                       (P.Developer_job.Result_value.Inline
                          (P.Completion.Succeeded payload))
                 })
        in
        let completed =
          if interrupted_stage
          then (
            assert (Result.is_error first);
            Worker.reconcile_quarantined worker |> F.ok;
            R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn)
          else first |> F.ok |> Option.value_exn
        in
        let selection = Option.value_exn !retained in
        assert (Int.equal !callback_count 1);
        assert (Int.equal !stage_count (if interrupted_stage then 2 else 1));
        assert (Option.is_none (Publisher.pending publisher ~job:completed |> F.ok));
        (match
           ( (P.Developer_job.state completed).result
           , Publisher.Selected.reference selection
           , expect_artifact )
         with
         | Some (P.Developer_job.Result_value.Inline completion), None, false ->
           assert (P.Completion.equal completion (P.Completion.Succeeded payload))
         | Some (P.Developer_job.Result_value.Artifact result), Some reference, true ->
           assert (
             P.Id.Blob.equal
               result.blob.id
               (Agent_store.Developer_result_reference.blob reference).id);
           let read =
             Blobs.open_result blobs (Blobs.Result_root.developer root) result.blob.id
             |> F.store_ok
           in
           let content =
             Blobs.load_result_verified blobs ~sw read ~max_bytes:65536 |> F.store_ok
           in
           assert (
             String.equal
               content
               (P.Completion.to_json (P.Completion.Succeeded payload) |> Jsonaf.to_string))
         | _ -> failwith "publisher changed the selected result or storage mode");
        Worker.close_and_wait worker |> F.ok;
        let principal =
          P.Principal.create
            ~id:owner.principal_id
            ~authentication_kind:"local.test"
            ~scopes:P.Scope.Set.empty
            ~attributes:[]
          |> F.ok
        in
        let actor = Operator_authorization.trusted_local principal in
        let json =
          Document_schema.Limits.create
            ~max_bytes:65536
            ~max_depth:132
            ~max_fields:4096
            ~max_nodes:8192
          |> F.ok
        in
        let read_limits =
          Disclosure.Limits.create ~max_result_bytes:65536 ~json |> F.ok
        in
        let policy =
          Disclosure.Policy.create ~authorize:(fun actor selection ->
            P.Id.Principal.equal
              (Operator_authorization.principal actor).id
              (Disclosure.Selection.owner selection).principal_id)
        in
        let disclosure =
          Disclosure.create ~runs:store ~blobs ~policy ~limits:read_limits |> F.ok
        in
        let selection =
          Disclosure.Selection.create
            ~owner
            ~capture_fingerprint:capture
            ~job_id:id
            ~attempt:(P.Developer_job.state completed).attempt
          |> F.ok
        in
        let request =
          Disclosure.begin_request disclosure ~original_actor:actor selection |> F.ok
        in
        let expected = P.Completion.Succeeded payload in
        let read = Disclosure.read disclosure request ~sw ~current_actor:actor |> F.ok in
        (match read with
         | Disclosure.Result.Completion completion ->
           assert (P.Completion.equal completion expected)
         | Pending _ | Terminal_without_result _ ->
           failwith "installed result was not disclosed");
        let foreign_service =
          Disclosure.create ~runs:store ~blobs ~policy ~limits:read_limits |> F.ok
        in
        assert (
          Result.is_error
            (Disclosure.read foreign_service request ~sw ~current_actor:actor));
        if expect_artifact
        then (
          let revoked = ref false in
          let expiring =
            Operator_authorization.guarded ~principal ~is_current:(fun () -> not !revoked)
          in
          let request =
            Disclosure.begin_request disclosure ~original_actor:expiring selection |> F.ok
          in
          let entered, entered_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let finished, finished_u = Eio.Promise.create () in
          let first = ref true in
          (on_checkpoint
           := fun () ->
                if !first
                then (
                  first := false;
                  Eio.Promise.resolve entered_u ();
                  Eio.Promise.await release));
          let observed = ref None in
          Eio.Fiber.fork ~sw (fun () ->
            observed
            := Some (Disclosure.read disclosure request ~sw ~current_actor:expiring);
            Eio.Promise.resolve finished_u ());
          Eio.Promise.await entered;
          assert (Option.is_none !observed);
          revoked := true;
          Eio.Promise.resolve release_u ();
          Eio.Promise.await finished;
          (on_checkpoint := fun () -> ());
          match Option.value_exn !observed with
          | Error { P.Error.code = Unauthenticated; _ } -> ()
          | Error error ->
            raise_s [%sexp "wrong revoked read error", (error.code : P.Error.code)]
          | Ok _ -> failwith "revoked actor received developer result");
        let observed = R.observe handle |> F.run_ok in
        let _new_epoch =
          R.admit_epoch
            store
            ~owner:(F.owner 1)
            ~capture_fingerprint:(F.digest "new developer disclosure capture")
            ~previous:(Some observed)
          |> F.run_ok
        in
        (match Disclosure.read disclosure request ~sw ~current_actor:actor |> F.ok with
         | Disclosure.Result.Completion completion ->
           assert (P.Completion.equal completion expected)
         | Pending _ | Terminal_without_result _ ->
           failwith "historical result was not disclosed");
        let wrong_capture =
          Disclosure.Selection.create
            ~owner
            ~capture_fingerprint:(F.digest "wrong developer disclosure capture")
            ~job_id:id
            ~attempt:(P.Developer_job.state completed).attempt
          |> F.ok
        in
        let wrong_request =
          Disclosure.begin_request disclosure ~original_actor:actor wrong_capture |> F.ok
        in
        assert (
          Result.is_error
            (Disclosure.read disclosure wrong_request ~sw ~current_actor:actor));
        let wrong_attempt =
          Disclosure.Selection.create
            ~owner
            ~capture_fingerprint:capture
            ~job_id:id
            ~attempt:0
          |> F.ok
        in
        let wrong_request =
          Disclosure.begin_request disclosure ~original_actor:actor wrong_attempt |> F.ok
        in
        assert (
          Result.is_error
            (Disclosure.read disclosure wrong_request ~sw ~current_actor:actor));
        (match (P.Developer_job.state completed).result with
         | Some (P.Developer_job.Result_value.Artifact { blob; _ }) ->
           let data_root = Agent_store.Session_store.data_root session in
           let file =
             Filename.concat
               (Filename.concat
                  (Filename.concat
                     (Agent_store.Data_root.durable_blobs_path data_root)
                     "developer-runs")
                  (P.Id.Developer_run.to_string owner.run_id))
               (P.Id.Blob.to_string blob.id ^ ".blob")
           in
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(Eio.Stdenv.fs env / file)
             "corrupt-result";
           assert (
             Result.is_error (Disclosure.read disclosure request ~sw ~current_actor:actor))
         | Some (Inline _) | None -> ());
        Option.iter foreign ~f:(fun (other_session, other_store, _, _) ->
          R.close_and_wait other_store |> F.run_ok;
          Agent_store.Session_store.close other_session |> F.store_ok)))
;;

let%test_unit "developer disclosure distinguishes pending and terminal without result" =
  F.with_root (fun env sw _root session store ->
    let owner = F.owner 0 in
    let capture = F.digest "developer pending disclosure" in
    let handle, initial = F.admit store owner capture in
    let data_root = Agent_store.Session_store.data_root session in
    let blobs =
      Blobs.create
        ~env
        ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
        ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
        ~max_upload_bytes:4096L
      |> F.store_ok
    in
    Blobs.bind_developer_runs blobs ~session_store:session store |> F.store_ok;
    let principal =
      P.Principal.create
        ~id:owner.principal_id
        ~authentication_kind:"local.test"
        ~scopes:P.Scope.Set.empty
        ~attributes:[]
      |> F.ok
    in
    let actor = Operator_authorization.trusted_local principal in
    let policy =
      Disclosure.Policy.create ~authorize:(fun actor selection ->
        P.Id.Principal.equal
          (Operator_authorization.principal actor).id
          (Disclosure.Selection.owner selection).principal_id)
    in
    let json =
      Document_schema.Limits.create
        ~max_bytes:4096
        ~max_depth:32
        ~max_fields:512
        ~max_nodes:1024
      |> F.ok
    in
    let limits = Disclosure.Limits.create ~max_result_bytes:4096 ~json |> F.ok in
    let disclosure = Disclosure.create ~runs:store ~blobs ~policy ~limits |> F.ok in
    let id = Selected.job_id "job_pending_disclosure" in
    let _, queued = select_root handle initial owner capture id in
    let pending_selection =
      Disclosure.Selection.create
        ~owner
        ~capture_fingerprint:capture
        ~job_id:id
        ~attempt:0
      |> F.ok
    in
    let pending_request =
      Disclosure.begin_request disclosure ~original_actor:actor pending_selection |> F.ok
    in
    (match
       Disclosure.read disclosure pending_request ~sw ~current_actor:actor |> F.ok
     with
     | Disclosure.Result.Pending P.Job.Queued -> ()
     | Pending _ | Terminal_without_result _ | Completion _ ->
       failwith "new queued job did not report pending");
    let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
    let failed = P.Error.invalid_request "developer handler failed" in
    let completed =
      Worker.with_claimed_attempt
        worker
        ~expected:queued
        ~lookup:(R.find_job_exact queued id)
        (fun ~active ~sw:_ ->
           Ok (terminal_state (Scope.claim active) (P.Job.Failed failed)))
      |> F.ok
      |> Option.value_exn
    in
    Worker.close_and_wait worker |> F.ok;
    assert (
      Result.is_error
        (Disclosure.read disclosure pending_request ~sw ~current_actor:actor));
    let terminal_selection =
      Disclosure.Selection.create
        ~owner
        ~capture_fingerprint:capture
        ~job_id:id
        ~attempt:(P.Developer_job.state completed).attempt
      |> F.ok
    in
    let terminal_request =
      Disclosure.begin_request disclosure ~original_actor:actor terminal_selection |> F.ok
    in
    (match
       Disclosure.read disclosure terminal_request ~sw ~current_actor:actor |> F.ok
     with
     | Disclosure.Result.Terminal_without_result (P.Job.Failed _) -> ()
     | Pending _ | Terminal_without_result _ | Completion _ ->
       failwith "failed job without result did not report terminal absence");
    let denied = Disclosure.Policy.deny_all in
    let denied = Disclosure.create ~runs:store ~blobs ~policy:denied ~limits |> F.ok in
    assert (
      Result.is_error
        (Disclosure.begin_request denied ~original_actor:actor terminal_selection)))
;;

let%test_unit
    "owner-managed worker close retains uncertain selected capacity until settlement"
  =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "selected-capacity-close" in
    let handle, initial = F.admit store owner capture in
    let capacity = capacity () in
    let key =
      Agent_server.Job_capacity.Key.developer
        ~owner
        ~project_source
        ~workspace_conflict_domain:"developer-test"
        ~kind:P.Job.Async_tool
        ~nested_depth:0
      |> F.ok
    in
    let selected =
      Selected.job
        owner
        capture
        ~id:(Selected.job_id "job_uncertain_selected_capacity")
        ~parent:P.Developer_job.Launch.Run_root
    in
    let host_job = P.Host_job.of_developer selected |> F.ok in
    let reservation =
      Agent_server.Job_capacity.reserve_host_job capacity key ~job:host_job
      |> F.ok
      |> Option.value_exn
    in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ selected ]
      |> F.run_ok
    in
    let calls = ref 0 in
    let original = Failure "capacity handoff interrupted" in
    let handoff =
      R.Handoff.create ~settle:(function
        | R.Settlement.Installed _ ->
          incr calls;
          if Int.equal !calls 1
          then raise original
          else Agent_server.Job_capacity.publish reservation
        | Absent -> Agent_server.Job_capacity.abort reservation
        | Prewrite_conflict | Diverged -> ())
    in
    (match R.commit_and_settle handle prepared handoff ~admit:(fun () -> true) with
     | exception actual -> assert (phys_equal actual original)
     | Ok _ | Error _ -> failwith "handoff exception was erased");
    let worker =
      Worker.create
        ~close_policy:Retain_unclaimed
        ~sw
        ~store
        ~initial:handle
        ~capacity
        ~project_source
        ~workspace_conflict_domain:"developer-test"
        ~now:(fun () -> timestamp)
        ~max_active:1
        ~max_quarantined:2
        ()
      |> F.ok
    in
    Worker.close_and_wait worker |> F.ok;
    let competing =
      Selected.job
        owner
        capture
        ~id:(Selected.job_id "job_competing_selected_capacity")
        ~parent:P.Developer_job.Launch.Run_root
      |> P.Host_job.of_developer
      |> F.ok
    in
    (match Agent_server.Job_capacity.try_acquire_host_job capacity key ~job:competing with
     | Ok None -> ()
     | Ok (Some _) | Error _ ->
       failwith "closed worker released an unresolved root reservation");
    (match R.reconcile_and_settle handle prepared handoff |> F.run_ok with
     | R.Settlement.Installed _ -> ()
     | Absent | Prewrite_conflict | Diverged ->
       failwith "selected capacity did not settle as installed");
    assert (Int.equal !calls 2);
    Agent_server.Job_capacity.close_developer_run capacity ~owner;
    let lease =
      Agent_server.Job_capacity.try_acquire_host_job capacity key ~job:competing
      |> F.ok
      |> Option.value_exn
    in
    Agent_server.Job_capacity.release lease)
;;

let%test_unit "durable claim precedes one callback and cannot enter twice" =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "developer claim model" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_claim_model" in
    let _, selected = select_root handle initial owner capture id in
    let capacity = capacity () in
    let worker = worker ~sw ~store ~handle ~capacity () in
    let callback_count = ref 0 in
    let retained = ref None in
    let completed =
      Worker.with_claimed_attempt
        worker
        ~expected:selected
        ~lookup:(R.find_job_exact selected id)
        (fun ~active ~sw:_ ->
           incr callback_count;
           retained := Some active;
           let claimed = Scope.claim active in
           assert (Int.equal (R.Claim.attempt claimed) 1);
           assert (
             match R.find_job (R.observe handle |> F.run_ok) id with
             | Some value ->
               (match (P.Developer_job.state value).status with
                | P.Job.Running -> true
                | Queued
                | Waiting_permission _
                | Waiting_completion _
                | Succeeded
                | Failed _
                | Cancelled
                | Interrupted _ -> false)
             | None -> false);
           Scope.check_parent
             (Worker.issuer worker)
             active
             ~owner
             ~capture_fingerprint:capture
             ~job_id:id
             ~attempt:1
           |> F.ok;
           Ok (terminal_state claimed P.Job.Succeeded))
      |> F.ok
      |> Option.value_exn
    in
    assert (Int.equal !callback_count 1);
    assert (
      match (P.Developer_job.state completed).status with
      | P.Job.Succeeded -> true
      | _ -> false);
    let active = Option.value_exn !retained in
    assert (
      Result.is_error
        (Scope.check_parent
           (Worker.issuer worker)
           active
           ~owner
           ~capture_fingerprint:capture
           ~job_id:id
           ~attempt:1));
    assert (
      Result.is_error
        (Worker.with_claimed_attempt
           worker
           ~expected:selected
           ~lookup:(R.find_job_exact selected id)
           (fun ~active:_ ~sw:_ ->
              incr callback_count;
              Ok (P.Developer_job.state completed))));
    assert (Int.equal !callback_count 1);
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit "selected result retains one claim until exact preparation and Store settle"
  =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "worker selected result handoff" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_selected_result_handoff" in
    let _, selected = select_root handle initial owner capture id in
    let capacity = capacity () in
    let worker = worker ~sw ~store ~handle ~capacity () in
    let callback_count = ref 0 in
    let selection_count = ref 0 in
    let prepare_count = ref 0 in
    let acknowledge_count = ref 0 in
    let cleanup_sentinel = Failure "selected cleanup after recovery" in
    let first =
      Worker.with_claimed_attempt
        worker
        ~expected:selected
        ~lookup:(R.find_job_exact selected id)
        ~select:(fun ~claim ~proposed ->
          incr selection_count;
          Ok
            (Some
               (selected_artifact
                  ~claim
                  ~proposed
                  ~ensure_attempted:(fun () ->
                    incr prepare_count;
                    if Int.equal !prepare_count 1
                    then Error (P.Error.invalid_request "stage not ready")
                    else Ok ())
                  ~acknowledge_installed:(fun _ ->
                    incr acknowledge_count;
                    raise cleanup_sentinel))))
        (fun ~active ~sw:_ ->
           incr callback_count;
           Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
    in
    assert (Result.is_error first);
    assert (Int.equal !callback_count 1);
    assert (Int.equal !selection_count 1);
    assert (Int.equal !prepare_count 1);
    assert (Int.equal !acknowledge_count 0);
    let running = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
    assert (
      match (P.Developer_job.state running).status with
      | P.Job.Running -> true
      | _ -> false);
    let key =
      Agent_server.Job_capacity.Key.developer
        ~owner
        ~project_source
        ~workspace_conflict_domain:"developer-test"
        ~kind:P.Job.Async_tool
        ~nested_depth:0
      |> F.ok
    in
    assert (Option.is_none (Agent_server.Job_capacity.try_acquire capacity key |> F.ok));
    let observed_cleanup =
      match Worker.reconcile_quarantined worker with
      | exception exn -> phys_equal exn cleanup_sentinel
      | Ok () | Error _ -> false
    in
    assert observed_cleanup;
    Worker.reconcile_quarantined worker |> F.ok;
    assert (Int.equal !callback_count 1);
    assert (Int.equal !selection_count 1);
    assert (Int.equal !prepare_count 2);
    assert (Int.equal !acknowledge_count 1);
    let persisted = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
    assert (
      match
        (P.Developer_job.state persisted).status, (P.Developer_job.state persisted).result
      with
      | P.Job.Succeeded, Some (P.Developer_job.Result_value.Artifact _) -> true
      | _ -> false);
    let released =
      Agent_server.Job_capacity.try_acquire capacity key |> F.ok |> Option.value_exn
    in
    Agent_server.Job_capacity.release released;
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit
    "installed selected cleanup exception preserves terminal and retires capacity"
  =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "worker selected cleanup exception" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_selected_cleanup_exception" in
    let _, selected = select_root handle initial owner capture id in
    let capacity = capacity () in
    let worker = worker ~sw ~store ~handle ~capacity () in
    let cleanup_sentinel = Failure "selected cleanup after install" in
    let callback_count = ref 0 in
    let acknowledge_count = ref 0 in
    let observed_cleanup =
      match
        Worker.with_claimed_attempt
          worker
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~select:(fun ~claim ~proposed ->
            Ok
              (Some
                 (selected_artifact
                    ~claim
                    ~proposed
                    ~ensure_attempted:(fun () -> Ok ())
                    ~acknowledge_installed:(fun _ ->
                      incr acknowledge_count;
                      raise cleanup_sentinel))))
          (fun ~active ~sw:_ ->
             incr callback_count;
             Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
      with
      | exception exn -> phys_equal exn cleanup_sentinel
      | Ok _ | Error _ -> false
    in
    assert observed_cleanup;
    assert (Int.equal !callback_count 1);
    assert (Int.equal !acknowledge_count 1);
    let persisted = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
    assert (
      match
        (P.Developer_job.state persisted).status, (P.Developer_job.state persisted).result
      with
      | P.Job.Succeeded, Some (P.Developer_job.Result_value.Artifact _) -> true
      | _ -> false);
    Worker.reconcile_quarantined worker |> F.ok;
    assert (Int.equal !acknowledge_count 1);
    let key =
      Agent_server.Job_capacity.Key.developer
        ~owner
        ~project_source
        ~workspace_conflict_domain:"developer-test"
        ~kind:P.Job.Async_tool
        ~nested_depth:0
      |> F.ok
    in
    let released =
      Agent_server.Job_capacity.try_acquire capacity key |> F.ok |> Option.value_exn
    in
    Agent_server.Job_capacity.release released;
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit "child-switch failure cannot select a successful result" =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "worker result child join" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_result_child_join" in
    let _, selected = select_root handle initial owner capture id in
    let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
    let callback_count = ref 0 in
    let selection_count = ref 0 in
    let sentinel = Failure "child failed before terminal selection" in
    let propagated =
      match
        Worker.with_claimed_attempt
          worker
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~select:(fun ~claim:_ ~proposed:_ ->
            incr selection_count;
            Ok None)
          (fun ~active ~sw ->
             incr callback_count;
             Eio.Fiber.fork ~sw (fun () ->
               Eio.Fiber.yield ();
               raise sentinel);
             Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
      with
      | exception actual -> phys_equal actual sentinel
      | Ok _ | Error _ -> false
    in
    assert propagated;
    assert (Int.equal !callback_count 1);
    assert (Int.equal !selection_count 0);
    let persisted = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
    assert (
      match (P.Developer_job.state persisted).status with
      | P.Job.Failed _ -> true
      | _ -> false);
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit "selected handoff rejects another physical claim" =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "worker result claim affinity" in
    let handle, initial = F.admit store owner capture in
    let first_id = Selected.job_id "job_result_claim_first" in
    let second_id = Selected.job_id "job_result_claim_second" in
    let root_job id =
      Selected.job owner capture ~id ~parent:P.Developer_job.Launch.Run_root
    in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ root_job first_id; root_job second_id ]
      |> F.run_ok
    in
    let observed =
      R.commit_and_settle handle ~admit:(fun () -> true) prepared (Selected.handoff ())
      |> F.run_ok
      |> Selected.installed
    in
    let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
    let prior_claim = ref None in
    ignore
      (Worker.with_claimed_attempt
         worker
         ~expected:observed
         ~lookup:(R.find_job_exact observed first_id)
         (fun ~active ~sw:_ ->
            let claim = Scope.claim active in
            prior_claim := Some claim;
            Ok (terminal_state claim P.Job.Succeeded))
       |> F.ok);
    let current = R.observe handle |> F.run_ok in
    let selection_count = ref 0 in
    let result =
      Worker.with_claimed_attempt
        worker
        ~expected:current
        ~lookup:(R.find_job_exact current second_id)
        ~select:(fun ~claim:_ ~proposed ->
          incr selection_count;
          Ok
            (Some
               (selected_artifact
                  ~claim:(Option.value_exn !prior_claim)
                  ~proposed
                  ~ensure_attempted:(fun () -> failwith "foreign selection staged")
                  ~acknowledge_installed:(fun _ ->
                    failwith "foreign selection acknowledged"))))
        (fun ~active ~sw:_ -> Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
    in
    assert (Result.is_error result);
    assert (Int.equal !selection_count 1);
    let second =
      R.find_job (R.observe handle |> F.run_ok) second_id |> Option.value_exn
    in
    assert (
      match (P.Developer_job.state second).status with
      | P.Job.Failed error -> P.Error.equal_code error.code P.Error.Invalid_request
      | _ -> false);
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit "scheduled claim and ended waits cancel only their exact attempt" =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "scheduled developer claim" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_scheduled_claim" in
    let due = P.Timestamp.add_ms timestamp 1_000 |> F.ok in
    let _, selected =
      select_root
        ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
        handle
        initial
        owner
        capture
        id
    in
    let now = ref timestamp in
    let capacity = capacity () in
    let worker = worker ~now:(fun () -> !now) ~sw ~store ~handle ~capacity () in
    let called = ref 0 in
    let first ~active ~sw:_ =
      incr called;
      let claimed = Scope.claim active in
      let state = P.Developer_job.state (R.Claim.job claimed) in
      Ok
        { state with
          status = P.Job.Queued
        ; started_at = None
        ; next_run_at = Some due
        ; retry_failure = Some (P.Error.invalid_request "retry")
        }
    in
    let first_job =
      Worker.with_claimed_attempt
        worker
        ~expected:selected
        ~lookup:(R.find_job_exact selected id)
        first
      |> F.ok
      |> Option.value_exn
    in
    assert (Int.equal (P.Developer_job.state first_job).attempt 1);
    let retry_observed = R.observe handle |> F.run_ok in
    let callback ~active ~sw:_ =
      incr called;
      Ok (terminal_state (Scope.claim active) P.Job.Succeeded)
    in
    assert (
      Option.is_none
        (Worker.with_claimed_attempt
           worker
           ~expected:retry_observed
           ~lookup:(R.find_job_exact retry_observed id)
           callback
         |> F.ok));
    assert (Int.equal !called 1);
    now := due;
    assert (
      Option.is_some
        (Worker.with_claimed_attempt
           worker
           ~expected:retry_observed
           ~lookup:(R.find_job_exact retry_observed id)
           callback
         |> F.ok));
    assert (Int.equal !called 2);
    let retry_id = Selected.job_id "job_scheduled_retry_cancel" in
    let _, retry_selected =
      select_root
        ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
        handle
        (R.observe handle |> F.run_ok)
        owner
        capture
        retry_id
    in
    now := timestamp;
    let retry_job =
      Worker.with_claimed_attempt
        worker
        ~expected:retry_selected
        ~lookup:(R.find_job_exact retry_selected retry_id)
        first
      |> F.ok
      |> Option.value_exn
    in
    assert (Int.equal (P.Developer_job.state retry_job).attempt 1);
    Worker.join_ended_attempt worker ~job:retry_job |> F.ok;
    let retry_observed = R.observe handle |> F.run_ok in
    let cancelled =
      R.prepare_inactive_cancel
        handle
        ~expected:retry_observed
        ~lookup:(R.find_job_exact retry_observed retry_id)
        ~now:timestamp
      |> F.run_ok
      |> R.commit_inactive_cancel handle
      |> F.run_ok
      |> R.Installed.observation
      |> fun observed -> R.find_job observed retry_id |> Option.value_exn
    in
    let cancelled_state = P.Developer_job.state cancelled in
    assert (Int.equal cancelled_state.attempt 1);
    assert (Option.is_none cancelled_state.started_at);
    assert (Option.is_none cancelled_state.next_run_at);
    assert (
      match cancelled_state.status with
      | P.Job.Cancelled -> true
      | _ -> false);
    Worker.close_and_wait worker |> F.ok);
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "ended dependency cancellation" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_waiting_cancel" in
    let _, selected = select_root handle initial owner capture id in
    let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
    let dependency : P.Job.dependency =
      { invocation_id = P.Id.Invocation.of_string "inv_waiting_cancel" |> F.ok
      ; work = P.Invocation.Job (Selected.job_id "job_dependency")
      ; deadline = timestamp
      ; completion_schema = None
      ; max_output_bytes = 128
      ; max_output_depth = 4
      }
    in
    let ended =
      Worker.with_claimed_attempt
        worker
        ~expected:selected
        ~lookup:(R.find_job_exact selected id)
        (fun ~active ~sw:_ ->
           let state = P.Developer_job.state (R.Claim.job (Scope.claim active)) in
           Ok { state with status = P.Job.Waiting_completion dependency })
      |> F.ok
      |> Option.value_exn
    in
    Worker.join_ended_attempt worker ~job:ended |> F.ok;
    let current = R.observe handle |> F.run_ok in
    let cancelled =
      R.prepare_inactive_cancel
        handle
        ~expected:current
        ~lookup:(R.find_job_exact current id)
        ~now:timestamp
      |> F.run_ok
      |> R.commit_inactive_cancel handle
      |> F.run_ok
      |> R.Installed.observation
      |> fun observed -> R.find_job observed id |> Option.value_exn
    in
    let state = P.Developer_job.state cancelled in
    assert (Int.equal state.attempt 1);
    assert (Option.is_some state.started_at);
    assert (
      match state.status with
      | P.Job.Cancelled -> true
      | _ -> false);
    Worker.close_and_wait worker |> F.ok)
;;

let%test_unit "claimed worker self-cancel requests without joining itself" =
  F.with_root (fun _env sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "worker self cancellation" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_self_cancel" in
    let _, selected = select_root handle initial owner capture id in
    let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
    let called = ref 0 in
    (match
       Worker.with_claimed_attempt
         worker
         ~expected:selected
         ~lookup:(R.find_job_exact selected id)
         (fun ~active ~sw:_ ->
            incr called;
            let claim = Scope.claim active in
            let claimed = R.Claim.job claim in
            assert (
              match Worker.cancel_claimed worker ~job:claimed |> F.ok with
              | Worker.Cancellation.Requested_self -> true
              | No_live_claim | Joined -> false);
            Ok (terminal_state claim P.Job.Cancelled))
     with
     | Ok (Some job) ->
       assert (
         match (P.Developer_job.state job).status with
         | P.Job.Cancelled -> true
         | _ -> false)
     | Ok None | Error _ -> failwith "self-cancel did not settle its own attempt"
     | exception Eio.Cancel.Cancelled _ -> ());
    assert (Int.equal !called 1);
    Worker.reconcile_quarantined worker |> F.ok;
    let observed = R.observe handle |> F.run_ok in
    assert (
      match
        R.find_job observed id
        |> Option.value_exn
        |> P.Developer_job.state
        |> fun (state : P.Developer_job.State.t) -> state.status
      with
      | P.Job.Cancelled -> true
      | _ ->
        raise_s
          [%sexp
            "self-cancel stored unexpected status"
          , (P.Job.status_to_json
               (P.Developer_job.state (R.find_job observed id |> Option.value_exn)).status
             : Jsonaf.t)]);
    match Worker.close_and_wait worker with
    | Ok () -> ()
    | Error error -> raise_s [%sexp "self-cancel close", (error : P.Error.t)])
;;

let%test_unit
    "unattempted claim and finish conflicts release only their own pending token"
  =
  F.with_root (fun _env _sw _root _session store ->
    let owner = F.owner 0 in
    let capture = F.digest "developer prewrite conflict" in
    let handle, initial = F.admit store owner capture in
    let id = Selected.job_id "job_prewrite_conflict" in
    let _, selected = select_root handle initial owner capture id in
    let first =
      R.prepare_job_claim
        handle
        ~expected:selected
        ~lookup:(R.find_job_exact selected id)
        ~now:timestamp
      |> F.run_ok
    in
    let unrelated expected =
      let prepared =
        R.prepare_root_selected handle ~expected ~phase:R.Checkpoint.Active ~selected:[]
        |> F.run_ok
      in
      R.commit_and_settle handle ~admit:(fun () -> true) prepared (Selected.handoff ())
      |> F.run_ok
      |> Selected.installed
    in
    let after_unrelated = unrelated selected in
    assert (
      match R.commit_job_claim handle first with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      match R.reconcile_job_claim handle first with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    let second =
      R.prepare_job_claim
        handle
        ~expected:after_unrelated
        ~lookup:(R.find_job_exact after_unrelated id)
        ~now:timestamp
      |> F.run_ok
    in
    let same_bytes_other_token =
      R.prepare_job_claim
        handle
        ~expected:after_unrelated
        ~lookup:(R.find_job_exact after_unrelated id)
        ~now:timestamp
      |> F.run_ok
    in
    let claimed = R.commit_job_claim handle second |> F.run_ok in
    assert (
      match R.commit_job_claim handle same_bytes_other_token with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      match R.reconcile_job_claim handle same_bytes_other_token with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      match R.reconcile_job_claim handle second with
      | Ok (R.Claimed repeated) ->
        phys_equal (R.Claimed.claim claimed) (R.Claimed.claim repeated)
      | Ok (R.Claim_absent | R.Claim_diverged) | Error _ -> false);
    let claim = R.Claimed.claim claimed in
    assert (
      R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
      |> F.run_ok);
    let running = R.Claimed.observation claimed in
    let ended = terminal_state claim P.Job.Succeeded in
    let first_finish =
      R.prepare_claimed_transition
        handle
        ~expected:running
        ~claim
        ~after:ended
        ~revoked_after:ended
      |> F.run_ok
    in
    let after_second_unrelated = unrelated running in
    assert (
      match R.commit_claimed_transition handle first_finish with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      match R.reconcile_claimed_transition handle first_finish with
      | Error R.Error.Prewrite_conflict -> true
      | Ok _ | Error _ -> false);
    let second_finish =
      R.prepare_claimed_transition
        handle
        ~expected:after_second_unrelated
        ~claim
        ~after:ended
        ~revoked_after:ended
      |> F.run_ok
    in
    let installed = R.commit_claimed_transition handle second_finish |> F.run_ok in
    let persisted =
      R.find_job (R.Installed.observation installed) id |> Option.value_exn
    in
    assert (
      match (P.Developer_job.state persisted).status with
      | P.Job.Succeeded -> true
      | _ -> false))
;;

let%test_unit "reset preserves terminal outcome but interrupts queued retry" =
  List.iteri [ `Retry; `Terminal ] ~f:(fun index result_kind ->
    F.with_root (fun _env sw _root _session store ->
      let owner = F.owner 0 in
      let capture = F.digest "developer old epoch claim" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id ("job_old_epoch_claim_" ^ Int.to_string index) in
      let _, selected =
        select_root
          ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
          handle
          initial
          owner
          capture
          id
      in
      let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
      let entered, signal_entered = Eio.Promise.create () in
      let resume, signal_resume = Eio.Promise.create () in
      let completed =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Worker.with_claimed_attempt
            worker
            ~expected:selected
            ~lookup:(R.find_job_exact selected id)
            (fun ~active ~sw:_ ->
               Eio.Promise.resolve signal_entered ();
               Eio.Promise.await resume;
               assert (
                 Result.is_error
                   (Scope.check_parent
                      (Worker.issuer worker)
                      active
                      ~owner
                      ~capture_fingerprint:capture
                      ~job_id:id
                      ~attempt:1));
               let claimed = Scope.claim active in
               let state = P.Developer_job.state (R.Claim.job claimed) in
               Ok
                 (match result_kind with
                  | `Terminal -> terminal_state claimed P.Job.Succeeded
                  | `Retry ->
                    { state with
                      status = P.Job.Queued
                    ; started_at = None
                    ; next_run_at = Some timestamp
                    ; retry_failure = Some (P.Error.invalid_request "retry")
                    })))
      in
      Eio.Promise.await entered;
      let next_owner = F.owner 1 in
      let prior = R.observe handle |> F.run_ok in
      let next_handle, _ =
        R.admit_epoch
          store
          ~owner:next_owner
          ~capture_fingerprint:(F.digest "next graph")
          ~previous:(Some prior)
        |> F.run_ok
      in
      Worker.advance_epoch worker ~new_current:next_handle |> F.ok;
      Eio.Promise.resolve signal_resume ();
      let old = Eio.Promise.await_exn completed |> F.ok |> Option.value_exn in
      assert (
        match result_kind, (P.Developer_job.state old).status with
        | `Retry, P.Job.Interrupted "epoch changed" | `Terminal, P.Job.Succeeded -> true
        | _ -> false);
      Worker.close_and_wait worker |> F.ok))
;;

let%test_unit "invalid callback attempts and start times become durable failure" =
  List.iteri [ `Attempt; `Start_time ] ~f:(fun index invalid_field ->
    F.with_root (fun _env sw _root _session store ->
      let owner = F.owner 0 in
      let capture = F.digest "invalid developer callback" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id ("job_invalid_callback_" ^ Int.to_string index) in
      let _, selected = select_root handle initial owner capture id in
      let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
      let called = ref 0 in
      let selected_count = ref 0 in
      let result =
        Worker.with_claimed_attempt
          worker
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~select:(fun ~claim:_ ~proposed:_ ->
            incr selected_count;
            Ok None)
          (fun ~active ~sw:_ ->
             incr called;
             let proposed = terminal_state (Scope.claim active) P.Job.Succeeded in
             Ok
               (match invalid_field with
                | `Attempt -> { proposed with attempt = proposed.attempt + 1 }
                | `Start_time ->
                  { proposed with
                    started_at = Some (P.Timestamp.of_time_ns Time_ns.epoch)
                  }))
      in
      assert (Result.is_error result);
      let persisted = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
      assert (
        match (P.Developer_job.state persisted).status with
        | P.Job.Failed error -> P.Error.equal_code error.code P.Error.Invalid_state
        | _ -> false);
      assert (Int.equal !called 1);
      assert (Int.equal !selected_count 0);
      Worker.close_and_wait worker |> F.ok))
;;

let%test_unit "post-rename uncertain claim retains capacity without callback replay" =
  F.with_temp_directory "ochat-developer-claim-uncertain" (fun original temporary ->
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let env =
        F.fault_after_authority_rename_env
          original
          ~rename_armed:armed
          ~sync_blocked
          ~parent_sync_blocked
          ~after_rename:(fun () -> sync_blocked := true)
      in
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let store = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "uncertain claim" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id "job_uncertain_claim" in
      let _, selected = select_root handle initial owner capture id in
      let competing_claim =
        R.prepare_job_claim
          handle
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~now:timestamp
        |> F.run_ok
      in
      let capacity = capacity () in
      let worker = worker ~sw ~store ~handle ~capacity () in
      let callback_count = ref 0 in
      armed := true;
      assert (
        Result.is_error
          (Worker.with_claimed_attempt
             worker
             ~expected:selected
             ~lookup:(R.find_job_exact selected id)
             (fun ~active:_ ~sw:_ ->
                incr callback_count;
                Ok
                  (terminal_state
                     (failwith "callback ran before durable claim")
                     P.Job.Succeeded))));
      assert (Int.equal !callback_count 0);
      assert (
        match R.commit_job_claim handle competing_claim with
        | Error R.Error.Conflict -> true
        | Ok _ | Error _ -> false);
      let key =
        Agent_server.Job_capacity.Key.developer
          ~owner
          ~project_source
          ~workspace_conflict_domain:"developer-test"
          ~kind:P.Job.Async_tool
          ~nested_depth:0
        |> F.ok
      in
      assert (Option.is_none (Agent_server.Job_capacity.try_acquire capacity key |> F.ok));
      sync_blocked := false;
      assert (Result.is_error (Worker.reconcile_quarantined worker));
      Worker.reconcile_quarantined worker |> F.ok;
      assert (Int.equal !callback_count 0);
      assert (
        match R.reconcile_job_claim handle competing_claim with
        | Error R.Error.Prewrite_conflict -> true
        | Ok _ | Error _ -> false);
      let lease =
        Agent_server.Job_capacity.try_acquire capacity key |> F.ok |> Option.value_exn
      in
      Agent_server.Job_capacity.release lease;
      Worker.close_and_wait worker |> F.ok;
      R.close_and_wait store |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session)))
;;

let%test_unit "scope close joins a yielding admission without consuming its callback" =
  F.with_temp_directory "ochat-developer-claim-admission" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let arrived, signal_arrived = Eio.Promise.create () in
      let gate, _signal_gate = Eio.Promise.create () in
      let checkpoint () =
        if !armed
        then (
          armed := false;
          Eio.Promise.resolve signal_arrived ();
          Eio.Promise.await gate)
      in
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let store = F.open_run session sw ~checkpoint () in
      let owner = F.owner 0 in
      let capture = F.digest "admission close" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id "job_admission_close" in
      let _, selected = select_root handle initial owner capture id in
      let prepared =
        R.prepare_job_claim
          handle
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~now:timestamp
        |> F.run_ok
      in
      let claim = R.commit_job_claim handle prepared |> F.run_ok |> R.Claimed.claim in
      let scope = Scope.create ~store ~initial:handle ~max_active:1 |> F.ok in
      let callback_count = ref 0 in
      armed := true;
      let attempt =
        Eio.Fiber.fork_promise ~sw (fun () ->
          try
            Scope.with_claim scope ~handle ~claim (fun ~active:_ ~sw:_ ->
              incr callback_count)
          with
          | Eio.Cancel.Cancelled _ ->
            Error (P.Error.invalid_request "cancelled admission"))
      in
      Eio.Promise.await arrived;
      assert (
        Result.is_error
          (Scope.with_claim scope ~handle ~claim (fun ~active:_ ~sw:_ ->
             incr callback_count)));
      Scope.close_and_wait scope |> F.ok;
      assert (Result.is_error (Eio.Promise.await_exn attempt));
      assert (Int.equal !callback_count 0);
      let observed = R.observe handle |> F.run_ok in
      let cleanup =
        R.prepare_unentered_claim_cleanup
          handle
          ~expected:observed
          ~claim
          ~status:(`Interrupted "scope closed")
          ~now:timestamp
        |> F.run_ok
      in
      ignore (R.commit_claimed_transition handle cleanup |> F.run_ok);
      R.close_and_wait store |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session)))
;;

let%test_unit "terminal rename exception keeps exact Prepared, child and callback" =
  F.with_temp_directory "ochat-developer-terminal-exception" (fun original temporary ->
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let sentinel = Failure "terminal persistence was interrupted" in
      let env =
        F.fault_after_authority_rename_env
          original
          ~rename_armed:armed
          ~sync_blocked
          ~parent_sync_blocked
          ~after_rename:(fun () -> raise sentinel)
      in
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let store = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "terminal exception graph" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id "job_terminal_exception" in
      let _, selected = select_root handle initial owner capture id in
      let capacity = capacity ~per_session:2 () in
      let worker = worker ~sw ~store ~handle ~capacity () in
      let capabilities =
        Chat_response.Tool_capability.create
          ~owner:"uncertain-child-test"
          ~resource_fingerprint:
            (Chatml_source.Digest.to_hex (F.digest "uncertain child tools"))
          []
        |> F.ok
      in
      let script =
        Chat_response.One_off_script.prepare_in_domain
          ~env
          ~capabilities
          ~tools:[]
          ~source:"let main input = Task.pure(input)"
          ()
        |> F.ok
      in
      let request =
        Chat_response.Background_request.script
          ~prepared:script
          ~input:`Null
          ~policy:Chat_response.One_off_request.default_policy
        |> F.ok
      in
      let child_id = ref None in
      let callback_count = ref 0 in
      let selection_count = ref 0 in
      let prepare_count = ref 0 in
      let acknowledge_count = ref 0 in
      let observed_exception =
        try
          ignore
            (Worker.with_claimed_selected_attempt
               worker
               ~expected:selected
               ~lookup:(R.find_job_exact selected id)
               ~max_staged_jobs:2
               ~select:(fun ~claim ~proposed ->
                 incr selection_count;
                 Ok
                   (Some
                      (selected_artifact
                         ~claim
                         ~proposed
                         ~ensure_attempted:(fun () ->
                           incr prepare_count;
                           Ok ())
                         ~acknowledge_installed:(fun _ ->
                           incr acknowledge_count;
                           Ok ()))))
               (fun ~active ~sw:_ ~children ->
                  incr callback_count;
                  let claim = Scope.claim active in
                  let launch =
                    P.Developer_job.Launch.create
                      ~parent:P.Developer_job.Launch.Run_root
                      ~parent_job:(Some (id, R.Claim.attempt claim))
                      ~nested_depth:1
                    |> F.ok
                  in
                  let prepared =
                    Agent_session.Developer_root_job_admission.background
                      ~owner
                      ~capture_fingerprint:capture
                      ~launch
                      ~selected:capabilities
                      ~policy:Chat_response.One_off_request.default_policy
                      ~request
                      ~created_at:timestamp
                      ~checkpoint:ignore
                    |> F.ok
                  in
                  let child =
                    Worker.Children.stage
                      children
                      ~prepare:(fun () -> Ok prepared)
                      ~checkpoint:ignore
                    |> F.ok
                  in
                  child_id := Some child;
                  let selected =
                    Worker.Children.seal children ~selected:[ child ] |> F.ok
                  in
                  armed := true;
                  Ok (terminal_state claim P.Job.Succeeded, selected))
             : ( (P.Developer_job.t * Worker.Children.Settlement.t) option
                 , P.Error.t )
                 result);
          false
        with
        | exn -> phys_equal exn sentinel
      in
      assert observed_exception;
      assert (Int.equal !callback_count 1);
      assert (Int.equal !selection_count 1);
      assert (Int.equal !prepare_count 1);
      assert (Int.equal !acknowledge_count 0);
      let current = R.observe handle |> F.run_ok in
      assert (Option.is_some (R.find_job current (Option.value_exn !child_id)));
      assert (
        match R.find_job current id with
        | Some job ->
          (match (P.Developer_job.state job).status with
           | P.Job.Succeeded -> true
           | _ -> false)
        | None -> false);
      Worker.reconcile_quarantined worker |> F.ok;
      assert (
        Option.is_some
          (R.find_job (R.observe handle |> F.run_ok) (Option.value_exn !child_id)));
      assert (Int.equal !callback_count 1);
      assert (Int.equal !selection_count 1);
      assert (Int.equal !acknowledge_count 1);
      Worker.close_and_wait worker |> F.ok;
      R.close_and_wait store |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session)))
;;

let%test_unit "terminal rename cancellation retains exact Prepared and original cause" =
  F.with_temp_directory "ochat-developer-terminal-cancel" (fun original temporary ->
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let cancellation = ref None in
      let env =
        F.fault_after_authority_rename_env
          original
          ~rename_armed:armed
          ~sync_blocked
          ~parent_sync_blocked
          ~after_rename:(fun () ->
            Option.iter !cancellation ~f:(fun context -> Eio.Cancel.cancel context Exit);
            Eio.Fiber.check ())
      in
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let store = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "terminal cancellation graph" in
      let handle, initial = F.admit store owner capture in
      let id = Selected.job_id "job_terminal_cancel" in
      let _, selected = select_root handle initial owner capture id in
      let worker = worker ~sw ~store ~handle ~capacity:(capacity ()) () in
      let callback_count = ref 0 in
      let selection_count = ref 0 in
      let acknowledge_count = ref 0 in
      let propagated =
        try
          Eio.Cancel.sub (fun context ->
            cancellation := Some context;
            ignore
              (Worker.with_claimed_attempt
                 worker
                 ~expected:selected
                 ~lookup:(R.find_job_exact selected id)
                 ~select:(fun ~claim ~proposed ->
                   incr selection_count;
                   Ok
                     (Some
                        (selected_artifact
                           ~claim
                           ~proposed
                           ~ensure_attempted:(fun () -> Ok ())
                           ~acknowledge_installed:(fun _ ->
                             incr acknowledge_count;
                             Ok ()))))
                 (fun ~active ~sw:_ ->
                    incr callback_count;
                    armed := true;
                    Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
               : (P.Developer_job.t option, P.Error.t) result));
          false
        with
        | Eio.Cancel.Cancelled _ -> true
      in
      assert propagated;
      assert (Int.equal !callback_count 1);
      assert (Int.equal !selection_count 1);
      Worker.reconcile_quarantined worker |> F.ok;
      assert (Int.equal !acknowledge_count 1);
      let persisted = R.find_job (R.observe handle |> F.run_ok) id |> Option.value_exn in
      assert (
        match (P.Developer_job.state persisted).status with
        | P.Job.Succeeded -> true
        | _ -> false);
      Worker.close_and_wait worker |> F.ok;
      R.close_and_wait store |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session)))
;;
