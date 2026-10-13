open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module Scope = Agent_session.Developer_job_scope
module Worker = Agent_server.Developer_job_worker
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests

let timestamp = Selected.timestamp

let capacity () =
  let limits : Agent_server.Config.Server.job_limits =
    { daemon_total = 2
    ; per_principal = 2
    ; per_prompt = 2
    ; per_workspace = 2
    ; per_session = 1
    ; per_kind = 2
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

let%test_unit "scheduled claim waits without capacity or callback until due" =
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
    Worker.close_and_wait worker |> F.ok)
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

let%test_unit "terminal rename exception keeps exact Prepared and callback runs once" =
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
      let capacity = capacity () in
      let worker = worker ~sw ~store ~handle ~capacity () in
      let callback_count = ref 0 in
      let selection_count = ref 0 in
      let prepare_count = ref 0 in
      let acknowledge_count = ref 0 in
      let observed_exception =
        try
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
                         ~ensure_attempted:(fun () ->
                           incr prepare_count;
                           Ok ())
                         ~acknowledge_installed:(fun _ ->
                           incr acknowledge_count;
                           Ok ()))))
               (fun ~active ~sw:_ ->
                  incr callback_count;
                  armed := true;
                  Ok (terminal_state (Scope.claim active) P.Job.Succeeded))
             : (P.Developer_job.t option, P.Error.t) result);
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
      assert (
        match R.find_job current id with
        | Some job ->
          (match (P.Developer_job.state job).status with
           | P.Job.Succeeded -> true
           | _ -> false)
        | None -> false);
      Worker.reconcile_quarantined worker |> F.ok;
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
