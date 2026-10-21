open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests

let limits ~max_bytes =
  Document_schema.Limits.create ~max_bytes ~max_depth:16 ~max_fields:256 ~max_nodes:512
  |> F.ok
;;

let generous = limits ~max_bytes:4096

let target =
  Inference.Request.Target.create
    ~adapter:"fixture"
    ~profile:"selected"
    ~profile_revision:(Some "revision-one")
    ~account:None
    ~endpoint:"local-fixture"
    ~model:"fixture-model"
    ~settings:[]
    ~limits:generous
  |> F.ok
;;

let request =
  `Object
    [ "prompt", `String "guide"
    ; "input", `String "hello"
    ; "future", `Object [ "nested", `String "preserved" ]
    ]
;;

let call request = P.Developer_model_call.create ~request ~target ~limits:generous |> F.ok

let model_job
      ?(parent_job = None)
      ?(nested_depth = 0)
      owner
      capture
      ~name
      ~parent
      ~payload
  =
  let launch = P.Developer_job.Launch.create ~parent ~parent_job ~nested_depth |> F.ok in
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
    ~id:(Selected.job_id name)
    ~launch
    ~kind:P.Job.Model_call
    ~payload
    ~retry_policy:P.Job.Never
    ~created_at:Selected.timestamp
    ~state
  |> F.ok
;;

let installed = function
  | R.Settlement.Installed value -> R.Installed.observation value
  | Absent | Prewrite_conflict | Diverged -> failwith "external model write not installed"
;;

let save handle prepared =
  R.commit_and_settle
    handle
    ~admit:(fun () -> true)
    prepared
    (R.Handoff.create ~settle:(fun _ -> ()))
  |> F.run_ok
  |> installed
;;

let%test_unit "developer model envelope is one bounded lossless recipe" =
  let admitted = call request in
  let encoded = P.Developer_model_call.to_json admitted in
  let decoded = P.Developer_model_call.of_json encoded ~limits:generous |> F.ok in
  assert (Jsonaf.exactly_equal encoded (P.Developer_model_call.to_json decoded));
  assert (Jsonaf.exactly_equal request (P.Developer_model_call.request decoded));
  let duplicate =
    `Object
      [ "prompt", `String "first"; "prompt", `String "second"; "input", `String "x" ]
  in
  assert (Result.is_ok (P.Model_spawn_request.decode duplicate));
  assert (
    Result.is_error
      (P.Developer_model_call.create ~request:duplicate ~target ~limits:generous));
  let measured json =
    Document_schema.Json.validate_and_measure ~limits:generous json |> F.ok
  in
  let envelope_bytes = measured encoded in
  let subtree_bytes =
    Int.max (measured request) (measured (Inference.Request.Target.to_json target))
  in
  assert (envelope_bytes > subtree_bytes);
  let tight = limits ~max_bytes:subtree_bytes in
  assert (
    match P.Developer_model_call.create ~request ~target ~limits:tight with
    | Error { code = P.Protocol_error.Resource_limit; _ } -> true
    | Ok _ | Error _ -> false);
  let unsupported =
    F.replace_json_field encoded ~name:"recipe" ~f:(fun _ -> `String "future_recipe")
  in
  assert (Result.is_error (P.Developer_model_call.of_json unsupported ~limits:generous))
;;

let%test_unit "external model append has one parent path and survives parent resolution" =
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "external model parent graph" in
    let handle, initial = F.admit run owner capture in
    let payload = P.Developer_model_call.to_json (call request) in
    let root_job =
      model_job owner capture ~name:"job_model_root" ~parent:Run_root ~payload
    in
    assert (
      Result.is_error
        (R.prepare_root_selected
           handle
           ~expected:initial
           ~phase:R.Checkpoint.Active
           ~selected:[ root_job ]));
    let root_prepared =
      R.prepare_external_model
        handle
        ~expected:initial
        ~parent:R.External_model_parent.Run_root
        ~job:root_job
      |> F.run_ok
    in
    assert (Result.is_error (R.commit handle root_prepared));
    let after_root = save handle root_prepared in
    let stored_root =
      R.find_job after_root (P.Developer_job.id root_job) |> Option.value_exn
    in
    assert (P.Developer_job.same_immutable root_job stored_root);
    let malformed_roots =
      [ model_job
          ~nested_depth:1
          owner
          capture
          ~name:"job_model_root_depth"
          ~parent:Run_root
          ~payload
      ; model_job
          ~parent_job:(Some (P.Developer_job.id root_job, 0))
          owner
          capture
          ~name:"job_model_root_ancestry"
          ~parent:Run_root
          ~payload
      ]
    in
    List.iter malformed_roots ~f:(fun candidate ->
      assert (
        match
          R.prepare_external_model
            handle
            ~expected:after_root
            ~parent:R.External_model_parent.Run_root
            ~job:candidate
        with
        | Error R.Error.Conflict -> true
        | Ok _ | Error _ -> false));
    let oversized_request =
      `Object [ "prompt", `String "guide"; "input", `String (String.make 1200 'x') ]
    in
    let oversized =
      model_job
        owner
        capture
        ~name:"job_model_oversized"
        ~parent:Run_root
        ~payload:(P.Developer_model_call.to_json (call oversized_request))
    in
    assert (
      match
        R.prepare_external_model
          handle
          ~expected:after_root
          ~parent:R.External_model_parent.Run_root
          ~job:oversized
      with
      | Error R.Error.Limit_exceeded -> true
      | Ok _ | Error _ -> false);
    let invalid =
      model_job
        owner
        capture
        ~name:"job_model_invalid_recipe"
        ~parent:Run_root
        ~payload:(F.replace_json_field payload ~name:"recipe" ~f:(fun _ -> `String "v2"))
    in
    assert (
      match
        R.prepare_external_model
          handle
          ~expected:after_root
          ~parent:R.External_model_parent.Run_root
          ~job:invalid
      with
      | Error R.Error.Conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:after_root
           ~parent:R.External_model_parent.Run_root
           ~job:root_job));
    let admitted = Selected.admitted owner in
    let dispatched = P.Host_invocation.dispatch admitted |> F.ok in
    let absent = R.find_invocation after_root Selected.invocation_id in
    let dispatch_prepared =
      R.prepare_dispatch handle ~expected:after_root ~lookup:absent ~dispatched
      |> F.run_ok
    in
    let after_dispatch = save handle dispatch_prepared in
    let lookup = R.find_invocation after_dispatch Selected.invocation_id in
    let child =
      model_job
        owner
        capture
        ~name:"job_model_child"
        ~parent:(Invocation Selected.invocation_id)
        ~payload
    in
    let malformed_child =
      model_job
        ~parent_job:(Some (P.Developer_job.id root_job, 0))
        owner
        capture
        ~name:"job_model_child_wrong_parent"
        ~parent:(Invocation Selected.invocation_id)
        ~payload
    in
    assert (
      match
        R.prepare_external_model
          handle
          ~expected:after_dispatch
          ~parent:(R.External_model_parent.Dispatching lookup)
          ~job:malformed_child
      with
      | Error R.Error.Conflict -> true
      | Ok _ | Error _ -> false);
    assert (
      Result.is_error
        (R.prepare_terminal_selected
           handle
           ~expected:after_dispatch
           ~lookup
           ~terminal:
             (P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> F.ok)
           ~selected:[ child ]));
    let child_prepared =
      R.prepare_external_model
        handle
        ~expected:after_dispatch
        ~parent:(R.External_model_parent.Dispatching lookup)
        ~job:child
      |> F.run_ok
    in
    let after_child = save handle child_prepared in
    let terminal =
      P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> F.ok
    in
    let lookup = R.find_invocation after_child Selected.invocation_id in
    let terminal_prepared =
      R.prepare_terminal_selected
        handle
        ~expected:after_child
        ~lookup
        ~terminal
        ~selected:[]
      |> F.run_ok
    in
    let after_terminal = save handle terminal_prepared in
    assert (Option.is_some (R.find_job after_terminal (P.Developer_job.id child)));
    let later =
      model_job
        owner
        capture
        ~name:"job_model_late"
        ~parent:(Invocation Selected.invocation_id)
        ~payload
    in
    let terminal_lookup = R.find_invocation after_terminal Selected.invocation_id in
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:after_terminal
           ~parent:(R.External_model_parent.Dispatching terminal_lookup)
           ~job:later));
    R.close_and_wait run |> F.run_ok)
;;

let%test_unit "claimed model children retain exact attempt ancestry after parent settles" =
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "claimed model children" in
    let handle, initial = F.admit run owner capture in
    let payload = P.Developer_model_call.to_json (call request) in
    let parent =
      model_job owner capture ~name:"job_model_claimed_parent" ~parent:Run_root ~payload
    in
    let queued =
      R.prepare_external_model
        handle
        ~expected:initial
        ~parent:R.External_model_parent.Run_root
        ~job:parent
      |> F.run_ok
      |> save handle
    in
    let claimed =
      R.prepare_job_claim
        handle
        ~expected:queued
        ~lookup:(R.find_job_exact queued (P.Developer_job.id parent))
        ~now:Selected.timestamp
      |> F.run_ok
      |> R.commit_job_claim handle
      |> F.run_ok
    in
    let claim = R.Claimed.claim claimed in
    assert (
      R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
      |> F.run_ok);
    let running = R.Claimed.observation claimed in
    let child name =
      model_job
        ~parent_job:(Some (P.Developer_job.id parent, R.Claim.attempt claim))
        ~nested_depth:1
        owner
        capture
        ~name
        ~parent:Run_root
        ~payload
    in
    let first = child "job_model_claimed_first" in
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:running
           ~parent:R.External_model_parent.Run_root
           ~job:first));
    let append observed child =
      R.prepare_external_model
        handle
        ~expected:observed
        ~parent:
          (R.External_model_parent.Claimed_model
             (claim, R.find_job_exact observed (P.Developer_job.id parent)))
        ~job:child
      |> F.run_ok
      |> save handle
    in
    let after_first = append running first in
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:after_first
           ~parent:
             (R.External_model_parent.Claimed_model
                (claim, R.find_job_exact running (P.Developer_job.id parent)))
           ~job:(child "job_model_stale_lookup")));
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:after_first
           ~parent:
             (R.External_model_parent.Claimed_model
                (claim, R.find_job_exact after_first (P.Developer_job.id parent)))
           ~job:first));
    let second = child "job_model_claimed_second" in
    let after_second = append after_first second in
    let running_job = R.Claim.job claim in
    let terminal : P.Developer_job.State.t =
      { (P.Developer_job.state running_job) with
        status = P.Job.Succeeded
      ; completed_at = Some Selected.timestamp
      ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
      }
    in
    let settled =
      R.prepare_claimed_transition
        handle
        ~expected:after_second
        ~claim
        ~after:terminal
        ~revoked_after:terminal
      |> F.run_ok
      |> R.commit_claimed_transition handle
      |> F.run_ok
      |> R.Installed.observation
    in
    assert (Option.is_some (R.find_job settled (P.Developer_job.id first)));
    assert (Option.is_some (R.find_job settled (P.Developer_job.id second)));
    assert (
      Result.is_error
        (R.prepare_external_model
           handle
           ~expected:settled
           ~parent:
             (R.External_model_parent.Claimed_model
                (claim, R.find_job_exact settled (P.Developer_job.id parent)))
           ~job:(child "job_model_after_parent_terminal")));
    let guard_owner =
      F.owner ~run_id:(P.Id.Developer_run.of_string "dvr_model_claim_guard" |> F.ok) 0
    in
    let guard_handle, guard_initial = F.admit run guard_owner capture in
    let guard_parent =
      model_job
        guard_owner
        capture
        ~name:"job_model_precommit_guard"
        ~parent:Run_root
        ~payload
    in
    let guard_queued =
      R.prepare_external_model
        guard_handle
        ~expected:guard_initial
        ~parent:R.External_model_parent.Run_root
        ~job:guard_parent
      |> F.run_ok
      |> save guard_handle
    in
    let guard_claimed =
      R.prepare_job_claim
        guard_handle
        ~expected:guard_queued
        ~lookup:(R.find_job_exact guard_queued (P.Developer_job.id guard_parent))
        ~now:Selected.timestamp
      |> F.run_ok
      |> R.commit_job_claim guard_handle
      |> F.run_ok
    in
    let guard_claim = R.Claimed.claim guard_claimed in
    assert (
      R.Claim.try_enter
        guard_claim
        ~handle:guard_handle
        ~admit:(fun () -> true)
        ~on_enter:(fun () -> ())
      |> F.run_ok);
    let guard_running = R.Claimed.observation guard_claimed in
    let guarded_child =
      model_job
        ~parent_job:(Some (P.Developer_job.id guard_parent, R.Claim.attempt guard_claim))
        ~nested_depth:1
        guard_owner
        capture
        ~name:"job_model_guarded_child"
        ~parent:Run_root
        ~payload
    in
    let guarded_prepared =
      R.prepare_external_model
        guard_handle
        ~expected:guard_running
        ~parent:
          (R.External_model_parent.Claimed_model
             ( guard_claim
             , R.find_job_exact guard_running (P.Developer_job.id guard_parent) ))
        ~job:guarded_child
      |> F.run_ok
    in
    (* Trusted cleanup revokes the entered Claim before the caller can commit.
       Even an always-true host admit callback cannot publish this child. *)
    R.Claim.abandon_before_callback guard_claim ~handle:guard_handle |> F.run_ok;
    assert (
      match
        R.commit_and_settle
          guard_handle
          guarded_prepared
          (R.Handoff.create ~settle:(fun _ -> ()))
          ~admit:(fun () -> true)
      with
      | Error R.Error.Foreign_or_stale -> true
      | Ok _ | Error _ -> false);
    let guard_after = R.observe guard_handle |> F.run_ok in
    assert (Option.is_none (R.find_job guard_after (P.Developer_job.id guarded_child)));
    R.close_and_wait run |> F.run_ok)
;;

let%test_unit "reopen validates the persisted model recipe before issuing a run" =
  F.with_temp_directory "ochat-developer-model-recovery" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let run = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "model recovery" in
      let handle, initial = F.admit run owner capture in
      let job =
        model_job
          owner
          capture
          ~name:"job_model_recovery"
          ~parent:Run_root
          ~payload:(P.Developer_model_call.to_json (call request))
      in
      let prepared =
        R.prepare_external_model
          handle
          ~expected:initial
          ~parent:R.External_model_parent.Run_root
          ~job
        |> F.run_ok
      in
      ignore (save handle prepared : R.Observation.t);
      R.close_and_wait run |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session);
      let authority =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "developer-runs"
          / ("run_" ^ P.Id.Developer_run.to_string F.run_id ^ ".json"))
      in
      let original = Eio.Path.load authority in
      let json = Jsonaf.parse original |> Or_error.ok_exn in
      let mutate_job f =
        F.replace_json_field json ~name:"payload" ~f:(fun payload ->
          F.replace_json_field payload ~name:"jobs" ~f:(function
            | `Array [ job ] -> `Array [ f job ]
            | _ -> failwith "expected one persisted model job"))
      in
      let mutate_payload f =
        mutate_job (fun job -> F.replace_json_field job ~name:"payload" ~f)
      in
      let invalid =
        [ mutate_payload (fun envelope ->
            F.replace_json_field envelope ~name:"recipe" ~f:(fun _ ->
              `String "unsupported"))
        ; mutate_payload (fun envelope ->
            F.replace_json_field envelope ~name:"request" ~f:(fun _ ->
              `Object
                [ "prompt", `String "first"
                ; "prompt", `String "second"
                ; "input", `String "hello"
                ]))
        ; mutate_job (fun job ->
            F.replace_json_field job ~name:"launch" ~f:(fun launch ->
              F.replace_json_field launch ~name:"nested_depth" ~f:(fun _ -> `Number "1")))
        ]
      in
      List.iteri invalid ~f:(fun index mutation ->
        Eio.Path.save ~create:(`Or_truncate 0o600) authority (Jsonaf.to_string mutation);
        let reopened =
          Agent_store.Session_store.open_existing
            ~env
            ~sw
            ~root
            ~process_start_identity:None
            ~lock_nonce:("model-recovery-" ^ Int.to_string index)
          |> F.store_ok
        in
        assert (
          Result.is_error
            (R.open_or_create reopened ~sw ~limits:(F.limits ()) ~checkpoint:(fun () ->
               ())));
        F.store_ok (Agent_store.Session_store.close reopened));
      Eio.Path.save ~create:(`Or_truncate 0o600) authority original;
      let reopened =
        Agent_store.Session_store.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"model-recovery-valid"
        |> F.store_ok
      in
      let max_document_bytes = String.length original + 16 in
      let document = limits ~max_bytes:max_document_bytes in
      let bounded =
        R.Limits.create
          ~max_runs:3
          ~max_epochs_per_run:4
          ~max_jobs_per_run:4
          ~max_invocations_per_run:4
          ~max_inference_attempts_per_run:256
          ~commands:(Agent_store.Developer_command_limits.default ~document)
          ~max_inference_bytes_per_run:max_document_bytes
          ~max_selected_per_commit:2
          ~max_job_payload_bytes:1024
          ~max_document_bytes
          ~max_recovery_entries:16
          ~max_recovery_bytes:16384
          ~max_work_steps:200
          ~document
        |> Or_error.ok_exn
      in
      let recovered =
        R.open_or_create reopened ~sw ~limits:bounded ~checkpoint:(fun () -> ())
        |> F.run_ok
      in
      let handle, observed = F.admit recovered owner capture in
      let next =
        model_job
          owner
          capture
          ~name:"job_model_document_limit"
          ~parent:Run_root
          ~payload:(P.Developer_model_call.to_json (call request))
      in
      let payload_bytes candidate =
        Document_schema.Json.validate_and_measure
          ~limits:generous
          (P.Developer_job.payload candidate)
        |> F.ok
      in
      assert (payload_bytes job + payload_bytes next < 1024);
      let before_rejection = Eio.Path.load authority in
      assert (
        match
          R.prepare_external_model
            handle
            ~expected:observed
            ~parent:R.External_model_parent.Run_root
            ~job:next
        with
        | Error R.Error.Limit_exceeded -> true
        | Ok _ | Error _ -> false);
      assert (String.equal before_rejection (Eio.Path.load authority));
      assert (
        Option.is_none
          (R.find_job (R.observe handle |> F.run_ok) (P.Developer_job.id next)));
      R.close_and_wait recovered |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close reopened)))
;;
