open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module F = Developer_run_store_tests

let timestamp = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> F.ok
let invocation_id = P.Id.Invocation.of_string "inv_developer_selected" |> F.ok
let job_id name = P.Id.Job.of_string name |> F.ok

let admitted ?(id = invocation_id) owner =
  P.Host_invocation.create_developer
    { id
    ; owner = P.Execution_owner.Developer owner
    ; origin = P.Invocation.Script
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = "selected.test"
    ; implementation_revision = "v1"
    ; capability_fingerprint = "test-capability"
    ; input = `Object []
    ; created_at = timestamp
    ; deadline = None
    }
  |> F.ok
;;

let job owner capture ~id ~parent =
  let launch =
    P.Developer_job.Launch.create ~parent ~parent_job:None ~nested_depth:0 |> F.ok
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
    ~payload:(`Object [ "job", `String (P.Id.Job.to_string id) ])
    ~retry_policy:P.Job.Never
    ~created_at:timestamp
    ~state
  |> F.ok
;;

let installed = function
  | R.Settlement.Installed value -> R.Installed.observation value
  | Absent | Diverged -> failwith "expected durable installed transaction"
;;

let handoff ?(on_settle = fun _ -> ()) () = R.Handoff.create ~settle:on_settle

let active_job owner capture ~id ~parent ~parent_job ~attempt ~retry_policy =
  let launch =
    P.Developer_job.Launch.create ~parent ~parent_job ~nested_depth:0 |> F.ok
  in
  let state : P.Developer_job.State.t =
    { status = P.Job.Running
    ; attempt
    ; started_at = Some timestamp
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
    ~payload:(`Object [])
    ~retry_policy
    ~created_at:timestamp
    ~state
  |> F.ok
;;

let with_jobs payload jobs =
  F.replace_json_field payload ~name:"jobs" ~f:(fun _ ->
    `Array (List.map jobs ~f:P.Developer_job.to_json))
;;

let with_invocations payload invocations =
  F.replace_json_field payload ~name:"invocations" ~f:(fun _ ->
    `Array
      (List.map invocations ~f:(fun invocation ->
         `Object
           [ "revision", `String "1"
           ; "value", P.Host_invocation.developer_to_json invocation |> F.ok
           ])))
;;

let%test_unit "developer-only codecs roundtrip validated states and reject wrappers" =
  let owner = F.owner 0 in
  let capture = F.digest "developer codec states" in
  let invocation = admitted owner in
  let dispatched = P.Host_invocation.dispatch invocation |> F.ok in
  let resolved =
    P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> F.ok
  in
  List.iter [ invocation; dispatched; resolved ] ~f:(fun value ->
    let json = P.Host_invocation.developer_to_json value |> F.ok in
    let decoded = P.Host_invocation.developer_of_json json |> F.ok in
    assert (Jsonaf.exactly_equal json (P.Host_invocation.developer_to_json decoded |> F.ok));
    assert (Result.is_error (P.Host_invocation.developer_of_json `Null));
    let unknown =
      match json with
      | `Object fields -> `Object (("unexpected", `Null) :: fields)
      | _ -> failwith "developer invocation codec returned nonobject"
    in
    assert (Result.is_error (P.Host_invocation.developer_of_json unknown)));
  let permission_id = P.Id.Permission.of_string "per_developer_codec" |> F.ok in
  let dependency : P.Job.dependency =
    { invocation_id = P.Id.Invocation.of_string "inv_codec_dependency" |> F.ok
    ; work = P.Invocation.Job (job_id "job_codec_dependency")
    ; deadline = timestamp
    ; completion_schema = None
    ; max_output_bytes = 128
    ; max_output_depth = 4
    }
  in
  let failure = P.Error.invalid_request "codec failure" in
  let artifact =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.create ())
      ~kind:P.Blob.File
      ~media_type:"application/json"
      ~byte_length:2L
      ~digest:Digestif.SHA256.(digest_string "{}" |> to_hex)
      ()
    |> F.ok
  in
  let states : P.Developer_job.State.t list =
    [ { status = P.Job.Queued
      ; attempt = 0
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Queued
      ; attempt = 1
      ; started_at = None
      ; next_run_at = Some timestamp
      ; completed_at = None
      ; result = None
      ; retry_failure = Some failure
      }
    ; { status = P.Job.Running
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Waiting_permission permission_id
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Waiting_completion dependency
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Succeeded
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = Some timestamp
      ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
      ; retry_failure = None
      }
    ; { status = P.Job.Succeeded
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = Some timestamp
      ; result =
          Some
            (P.Developer_job.Result_value.Artifact
               { outcome = P.Stored_completion.Succeeded; blob = artifact })
      ; retry_failure = None
      }
    ; { status = P.Job.Failed failure
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = Some timestamp
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Cancelled
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = Some timestamp
      ; result = None
      ; retry_failure = None
      }
    ; { status = P.Job.Interrupted "restart"
      ; attempt = 1
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = Some timestamp
      ; result = None
      ; retry_failure = None
      }
    ]
  in
  let launch =
    P.Developer_job.Launch.create
      ~parent:P.Developer_job.Launch.Run_root
      ~parent_job:None
      ~nested_depth:0
    |> F.ok
  in
  List.iter states ~f:(fun state ->
    let value =
      P.Developer_job.create
        ~owner
        ~capture_fingerprint:capture
        ~id:(job_id "job_codec_states")
        ~launch
        ~kind:P.Job.Async_tool
        ~payload:(`Object [ "input", `String "codec" ])
        ~retry_policy:(P.Job.Safe_retry { max_attempts = 3; backoff_ms = 0 })
        ~created_at:timestamp
        ~state
      |> F.ok
    in
    let json = P.Developer_job.to_json value in
    let decoded = P.Developer_job.of_json json |> F.ok in
    assert (Jsonaf.exactly_equal json (P.Developer_job.to_json decoded));
    assert (Result.is_error (P.Developer_job.of_json `Null));
    let unknown =
      match json with
      | `Object fields -> `Object (("unexpected", `Null) :: fields)
      | _ -> failwith "developer job codec returned nonobject"
    in
    assert (Result.is_error (P.Developer_job.of_json unknown)))
;;

let%test_unit "recovery validates member ancestry without equating historical attempts" =
  F.with_temp_directory "ochat-developer-member-ancestry" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let run = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "member ancestry" in
      let handle, initial = F.admit run owner capture in
      let first_id = job_id "job_ancestry_first" in
      let second_id = job_id "job_ancestry_second" in
      let first =
        job owner capture ~id:first_id ~parent:P.Developer_job.Launch.Run_root
      in
      let second =
        job owner capture ~id:second_id ~parent:P.Developer_job.Launch.Run_root
      in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:initial
          ~phase:R.Checkpoint.Active
          ~selected:[ first; second ]
        |> F.run_ok
      in
      ignore (R.commit_and_settle handle prepared (handoff ()) |> F.run_ok |> installed);
      R.close_and_wait run |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session);
      let authority =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "developer-runs"
          / ("run_" ^ P.Id.Developer_run.to_string F.run_id ^ ".json"))
      in
      let document_limits =
        Document_schema.Limits.create
          ~max_bytes:4096
          ~max_depth:16
          ~max_fields:256
          ~max_nodes:512
        |> F.ok
      in
      let original =
        Document_schema.Document.decode ~limits:document_limits (Eio.Path.load authority)
        |> F.ok
      in
      let pending_job ~id ~parent_job ~attempt ~retry_policy =
        active_job
          owner
          capture
          ~id
          ~parent:P.Developer_job.Launch.Run_root
          ~parent_job
          ~attempt
          ~retry_policy
      in
      let first_running =
        pending_job
          ~id:first_id
          ~parent_job:None
          ~attempt:2
          ~retry_policy:(P.Job.Safe_retry { max_attempts = 3; backoff_ms = 0 })
      in
      let parent_id = P.Id.Invocation.of_string "inv_ancestry_parent" |> F.ok in
      let child_id = P.Id.Invocation.of_string "inv_ancestry_child" |> F.ok in
      let second_child =
        job owner capture ~id:second_id ~parent:P.Developer_job.Launch.Run_root
      in
      let second_child =
        P.Developer_job.create
          ~owner
          ~capture_fingerprint:capture
          ~id:second_id
          ~launch:
            (P.Developer_job.Launch.create
               ~parent:(P.Developer_job.Launch.Invocation parent_id)
               ~parent_job:(Some (first_id, 1))
               ~nested_depth:0
             |> F.ok)
          ~kind:(P.Developer_job.kind second_child)
          ~payload:(P.Developer_job.payload second_child)
          ~retry_policy:P.Job.Never
          ~created_at:timestamp
          ~state:(P.Developer_job.state second_child)
        |> F.ok
      in
      let missing_job_id = job_id "job_ancestry_missing" in
      let missing_parent =
        pending_job
          ~id:first_id
          ~parent_job:(Some (missing_job_id, 1))
          ~attempt:1
          ~retry_policy:P.Job.Never
      in
      let cycle_first =
        pending_job
          ~id:first_id
          ~parent_job:(Some (second_id, 1))
          ~attempt:1
          ~retry_policy:P.Job.Never
      in
      let cycle_second =
        pending_job
          ~id:second_id
          ~parent_job:(Some (first_id, 1))
          ~attempt:1
          ~retry_policy:P.Job.Never
      in
      let missing_invocation =
        job
          owner
          capture
          ~id:first_id
          ~parent:
            (P.Developer_job.Launch.Invocation
               (P.Id.Invocation.of_string "inv_ancestry_missing" |> F.ok))
      in
      let invocation_with_parent ?(parent_job = None) id parent_invocation =
        P.Host_invocation.create_developer
          { id
          ; owner = P.Execution_owner.Developer owner
          ; origin = P.Invocation.Script
          ; parent_invocation
          ; parent_job
          ; tool_name = "ancestry.test"
          ; implementation_revision = "v1"
          ; capability_fingerprint = "ancestry-capability"
          ; input = `Object []
          ; created_at = timestamp
          ; deadline = None
          }
        |> F.ok
        |> P.Host_invocation.dispatch
        |> F.ok
      in
      let dispatching_parent = invocation_with_parent parent_id None in
      let terminal_parent =
        invocation_with_parent ~parent_job:(Some first_id) parent_id None
        |> fun value ->
        P.Host_invocation.resolve value (P.Invocation.Complete `Null) |> F.ok
      in
      let unrelated_terminal =
        P.Host_invocation.resolve dispatching_parent (P.Invocation.Complete `Null) |> F.ok
      in
      let child_of_dispatching =
        job
          owner
          capture
          ~id:first_id
          ~parent:(P.Developer_job.Launch.Invocation parent_id)
      in
      let cases =
        [ ( "historical parent attempt"
          , [ first_running; second_child ]
          , [ terminal_parent ]
          , true )
        ; "missing parent job", [ missing_parent; second ], [], false
        ; "missing parent invocation", [ missing_invocation; second ], [], false
        ; ( "nonterminal invocation parent"
          , [ child_of_dispatching; second ]
          , [ dispatching_parent ]
          , false )
        ; ( "mismatched invocation parent job"
          , [ first_running; second_child ]
          , [ unrelated_terminal ]
          , false )
        ; ( "invocation ancestry cycle"
          , [ first; second ]
          , [ invocation_with_parent parent_id (Some child_id)
            ; invocation_with_parent child_id (Some parent_id)
            ]
          , false )
        ; "job ancestry cycle", [ cycle_first; cycle_second ], [], false
        ]
      in
      List.iter cases ~f:(fun (label, jobs, invocations, accepted) ->
        let payload =
          with_jobs (Document_schema.Document.payload original) jobs
          |> fun payload -> with_invocations payload invocations
        in
        let updated =
          Document_schema.Document.create
            ~limits:document_limits
            ~kind:(Document_schema.Document.kind original)
            ~version:(Document_schema.Document.version original)
            ~payload
          |> F.ok
        in
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          authority
          (Document_schema.Document.to_string updated);
        let reopened =
          Agent_store.Session_store.open_existing
            ~env
            ~sw
            ~root
            ~process_start_identity:None
            ~lock_nonce:("ancestry-" ^ label)
          |> F.store_ok
        in
        let result =
          R.open_or_create reopened ~sw ~limits:(F.limits ()) ~checkpoint:(fun () -> ())
        in
        if Bool.( <> ) (Result.is_ok result) accepted
        then failwith ("incorrect ancestry recovery: " ^ label);
        Option.iter (Result.ok result) ~f:(fun run -> R.close_and_wait run |> F.run_ok);
        F.store_ok (Agent_store.Session_store.close reopened))))
;;

let%test_unit "selected jobs appear with exactly their parent transaction" =
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "selected transaction graph" in
    let handle, initial = F.admit run owner capture in
    let dispatched = P.Host_invocation.dispatch (admitted owner) |> F.ok in
    let absent = R.find_invocation initial invocation_id in
    assert (Option.is_none (R.Invocation_lookup.prior absent));
    let dispatch =
      R.prepare_dispatch handle ~expected:initial ~lookup:absent ~dispatched |> F.run_ok
    in
    assert (Result.is_error (R.commit handle dispatch));
    assert (Result.is_error (R.reconcile handle dispatch));
    assert (
      Option.is_none (R.Invocation_lookup.prior (R.find_invocation initial invocation_id)));
    let dispatch_observed =
      R.commit_and_settle handle dispatch (handoff ()) |> F.run_ok |> installed
    in
    let lookup = R.find_invocation dispatch_observed invocation_id in
    let prior = R.Invocation_lookup.prior lookup |> Option.value_exn in
    assert (Int.equal (R.Invocation_observation.revision prior) 1);
    assert (
      P.Invocation.equal_status
        (P.Host_invocation.status (R.Invocation_observation.value prior))
        Dispatching);
    let child_id = job_id "job_selected_child" in
    let child =
      job
        owner
        capture
        ~id:child_id
        ~parent:(P.Developer_job.Launch.Invocation invocation_id)
    in
    let terminal =
      P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> F.ok
    in
    let prepared =
      R.prepare_terminal_selected
        handle
        ~expected:dispatch_observed
        ~lookup
        ~terminal
        ~selected:[ child ]
      |> F.run_ok
    in
    assert (Result.is_error (R.commit handle prepared));
    assert (Option.is_none (R.find_job dispatch_observed child_id));
    let terminal_observed =
      R.commit_and_settle handle prepared (handoff ()) |> F.run_ok |> installed
    in
    assert (Option.is_some (R.find_job terminal_observed child_id));
    let terminal_entry =
      R.find_invocation terminal_observed invocation_id
      |> R.Invocation_lookup.prior
      |> Option.value_exn
    in
    assert (Int.equal (R.Invocation_observation.revision terminal_entry) 2);
    assert (
      P.Invocation.equal_status
        (P.Host_invocation.status (R.Invocation_observation.value terminal_entry))
        (P.Invocation.Resolved (P.Invocation.Complete `Null)));
    let rejected =
      [ job
          (F.owner ~principal_id:F.other_principal 0)
          capture
          ~id:(job_id "job_foreign_principal")
          ~parent:P.Developer_job.Launch.Run_root
      ; job
          owner
          (F.digest "different capture")
          ~id:(job_id "job_foreign_capture")
          ~parent:P.Developer_job.Launch.Run_root
      ; job owner capture ~id:child_id ~parent:P.Developer_job.Launch.Run_root
      ]
    in
    List.iter rejected ~f:(fun candidate ->
      assert (
        Result.is_error
          (R.prepare_root_selected
             handle
             ~expected:terminal_observed
             ~phase:R.Checkpoint.Active
             ~selected:[ candidate ])));
    let too_many =
      List.map [ "job_cap_a"; "job_cap_b"; "job_cap_c" ] ~f:(fun name ->
        job owner capture ~id:(job_id name) ~parent:P.Developer_job.Launch.Run_root)
    in
    assert (
      match
        R.prepare_root_selected
          handle
          ~expected:terminal_observed
          ~phase:R.Checkpoint.Active
          ~selected:too_many
      with
      | Error R.Error.Limit_exceeded -> true
      | Ok _ | Error _ -> false);
    let root_id = job_id "job_selected_root" in
    let root_job =
      job owner capture ~id:root_id ~parent:P.Developer_job.Launch.Run_root
    in
    let root_prepared =
      R.prepare_root_selected
        handle
        ~expected:terminal_observed
        ~phase:R.Checkpoint.Active
        ~selected:[ root_job ]
      |> F.run_ok
    in
    assert (Result.is_error (R.commit handle root_prepared));
    assert (Option.is_none (R.find_job terminal_observed root_id));
    let final =
      R.commit_and_settle handle root_prepared (handoff ()) |> F.run_ok |> installed
    in
    assert (Option.is_some (R.find_job final root_id));
    assert (Option.is_some (R.find_job final child_id));
    assert (
      Result.is_error
        (R.prepare_dispatch handle ~expected:final ~lookup:absent ~dispatched));
    let remaining = [ "inv_selected_2"; "inv_selected_3"; "inv_selected_4" ] in
    List.iter remaining ~f:(fun text ->
      let id = P.Id.Invocation.of_string text |> F.ok in
      let observed = R.observe handle |> F.run_ok in
      let dispatched = P.Host_invocation.dispatch (admitted ~id owner) |> F.ok in
      let lookup = R.find_invocation observed id in
      let prepared =
        R.prepare_dispatch handle ~expected:observed ~lookup ~dispatched |> F.run_ok
      in
      ignore (R.commit_and_settle handle prepared (handoff ()) |> F.run_ok |> installed));
    let id = P.Id.Invocation.of_string "inv_selected_over_quota" |> F.ok in
    let observed = R.observe handle |> F.run_ok in
    let dispatched = P.Host_invocation.dispatch (admitted ~id owner) |> F.ok in
    let lookup = R.find_invocation observed id in
    assert (
      match R.prepare_dispatch handle ~expected:observed ~lookup ~dispatched with
      | Error R.Error.Limit_exceeded -> true
      | Ok _ | Error _ -> false);
    let next_owner = F.owner 1 in
    let next_handle, next_observation =
      R.admit_epoch
        run
        ~owner:next_owner
        ~capture_fingerprint:capture
        ~previous:(Some observed)
      |> F.run_ok
    in
    let old_dispatch =
      P.Host_invocation.dispatch
        (admitted ~id:(P.Id.Invocation.of_string "inv_stale_epoch" |> F.ok) owner)
      |> F.ok
    in
    let old_lookup =
      R.find_invocation next_observation (P.Host_invocation.context old_dispatch).id
    in
    assert (
      match
        R.prepare_dispatch
          next_handle
          ~expected:next_observation
          ~lookup:old_lookup
          ~dispatched:old_dispatch
      with
      | Error R.Error.Conflict -> true
      | Ok _ | Error _ -> false);
    let old_id = P.Id.Invocation.of_string "inv_selected_2" |> F.ok in
    let old_lookup = R.find_invocation next_observation old_id in
    let old_terminal =
      P.Host_invocation.resolve
        (P.Host_invocation.dispatch (admitted ~id:old_id owner) |> F.ok)
        (P.Invocation.Complete `Null)
      |> F.ok
    in
    assert (
      match
        R.prepare_terminal_selected
          next_handle
          ~expected:next_observation
          ~lookup:old_lookup
          ~terminal:old_terminal
          ~selected:[]
      with
      | Error R.Error.Conflict -> true
      | Ok _ | Error _ -> false);
    R.close_and_wait run |> F.run_ok)
;;

let%test_unit "bounded selection agrees with an independent set model" =
  let candidates = [ job_id "job_model_a"; job_id "job_model_b"; job_id "job_model_c" ] in
  let selections = [ []; [ 0 ]; [ 1 ]; [ 0; 2 ]; [ 1; 2 ] ] in
  List.iter selections ~f:(fun indexes ->
    F.with_root (fun _env _sw _root _session run ->
      let owner = F.owner 0 in
      let capture = F.digest "finite independent selection model" in
      let handle, before = F.admit run owner capture in
      let selected =
        List.filter_mapi candidates ~f:(fun index id ->
          if List.mem indexes index ~equal:Int.equal
          then Some (job owner capture ~id ~parent:P.Developer_job.Launch.Run_root)
          else None)
      in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:before
          ~phase:R.Checkpoint.Active
          ~selected
        |> F.run_ok
      in
      List.iter candidates ~f:(fun id -> assert (Option.is_none (R.find_job before id)));
      let after =
        R.commit_and_settle handle prepared (handoff ()) |> F.run_ok |> installed
      in
      List.iteri candidates ~f:(fun index id ->
        assert (
          Bool.equal
            (Option.is_some (R.find_job after id))
            (List.mem indexes index ~equal:Int.equal)));
      assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint after)) 2L);
      R.close_and_wait run |> F.run_ok))
;;

let%test_unit "handoff settlement is one-way across absent, diverged and callback failure"
  =
  let selected owner capture text =
    job owner capture ~id:(job_id text) ~parent:P.Developer_job.Launch.Run_root
  in
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "absent settlement" in
    let handle, initial = F.admit run owner capture in
    let chosen = selected owner capture "job_absent_settlement" in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ chosen ]
      |> F.run_ok
    in
    let calls = ref 0 in
    let token = handoff ~on_settle:(fun _ -> incr calls) () in
    (match R.reconcile_and_settle handle prepared token |> F.run_ok with
     | R.Settlement.Absent -> ()
     | Installed _ | Diverged -> failwith "new transaction was not absent");
    (match R.commit_and_settle handle prepared token |> F.run_ok with
     | R.Settlement.Absent -> ()
     | Installed _ | Diverged -> failwith "settled absence was reversed");
    assert (Int.equal !calls 1);
    let observed = R.observe handle |> F.run_ok in
    assert (Option.is_none (R.find_job observed (P.Developer_job.id chosen)));
    assert (
      Int64.equal
        (R.Checkpoint.revision (R.Observation.checkpoint observed))
        (R.Checkpoint.revision (R.Observation.checkpoint initial)));
    R.close_and_wait run |> F.run_ok);
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "diverged settlement" in
    let handle, initial = F.admit run owner capture in
    let prepare text =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ selected owner capture text ]
      |> F.run_ok
    in
    let stale = prepare "job_diverged_stale" in
    let winner = prepare "job_diverged_winner" in
    ignore (R.commit_and_settle handle winner (handoff ()) |> F.run_ok |> installed);
    let calls = ref 0 in
    let token = handoff ~on_settle:(fun _ -> incr calls) () in
    (match R.reconcile_and_settle handle stale token |> F.run_ok with
     | R.Settlement.Diverged -> ()
     | Installed _ | Absent -> failwith "divergent transaction was settled as valid");
    (match R.commit_and_settle handle stale token |> F.run_ok with
     | R.Settlement.Diverged -> ()
     | Installed _ | Absent -> failwith "divergent handoff was reversed");
    assert (Int.equal !calls 1);
    let current = R.observe handle |> F.run_ok in
    (match
       R.admit_epoch
         run
         ~owner:(F.owner 1)
         ~capture_fingerprint:capture
         ~previous:(Some current)
     with
     | Error R.Error.Conflict -> ()
     | _ -> failwith "divergent handoff did not block a fresh epoch mutation");
    R.close_and_wait run |> F.run_ok);
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "callback failure settlement" in
    let handle, initial = F.admit run owner capture in
    let chosen = selected owner capture "job_callback_failure" in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ chosen ]
      |> F.run_ok
    in
    let calls = ref 0 in
    let original = Failure "capacity settlement failed once" in
    let token =
      handoff
        ~on_settle:(fun _ ->
          incr calls;
          if Int.equal !calls 1 then raise original)
        ()
    in
    (match R.commit_and_settle handle prepared token with
     | exception error -> assert (phys_equal error original)
     | Ok _ | Error _ -> failwith "callback exception was lost");
    assert (Int.equal !calls 1);
    let current = R.observe handle |> F.run_ok in
    (match
       R.admit_epoch
         run
         ~owner:(F.owner 1)
         ~capture_fingerprint:capture
         ~previous:(Some current)
     with
     | Error R.Error.Conflict -> ()
     | _ -> failwith "failed settlement did not block a fresh epoch mutation");
    let observed =
      R.reconcile_and_settle handle prepared token |> F.run_ok |> installed
    in
    assert (Option.is_some (R.find_job observed (P.Developer_job.id chosen)));
    ignore (R.commit_and_settle handle prepared token |> F.run_ok |> installed);
    assert (Int.equal !calls 2);
    R.close_and_wait run |> F.run_ok);
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "cancelled settlement caller" in
    let handle, initial = F.admit run owner capture in
    let chosen = selected owner capture "job_cancelled_settlement" in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ chosen ]
      |> F.run_ok
    in
    let cancellation = ref None in
    let calls = ref 0 in
    let reason = Failure "cancel caller during capacity settlement" in
    let token =
      handoff
        ~on_settle:(fun _ ->
          incr calls;
          Eio.Cancel.cancel (Option.value_exn !cancellation) reason)
        ()
    in
    (match
       Eio.Cancel.sub (fun context ->
         cancellation := Some context;
         ignore (R.commit_and_settle handle prepared token |> F.run_ok);
         Eio.Fiber.check ())
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
     | _ -> failwith "caller cancellation was lost");
    let observed =
      R.reconcile_and_settle handle prepared token |> F.run_ok |> installed
    in
    assert (Option.is_some (R.find_job observed (P.Developer_job.id chosen)));
    assert (Int.equal !calls 1);
    R.close_and_wait run |> F.run_ok)
;;

let%test_unit "uncertain selected commit retains barrier and settles exactly" =
  F.with_temp_directory "ochat-developer-selected-uncertain" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let rename_armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let env =
        F.fault_after_authority_rename_env
          ~after_rename:(fun () -> sync_blocked := true)
          env
          ~rename_armed
          ~sync_blocked
          ~parent_sync_blocked
      in
      let root = Filename.concat temporary "data" in
      let session = F.create_session env sw root in
      let run = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "uncertain selected graph" in
      let handle, initial = F.admit run owner capture in
      let selected_id = job_id "job_selected_uncertain" in
      let selected =
        job owner capture ~id:selected_id ~parent:P.Developer_job.Launch.Run_root
      in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:initial
          ~phase:R.Checkpoint.Active
          ~selected:[ selected ]
        |> F.run_ok
      in
      let settlement_calls = ref 0 in
      let token =
        handoff
          ~on_settle:(function
            | R.Settlement.Installed _ -> incr settlement_calls
            | Absent | Diverged -> failwith "unexpected selected settlement")
          ()
      in
      rename_armed := true;
      assert (Result.is_error (R.commit_and_settle handle prepared token));
      assert (not !rename_armed);
      assert (Int.equal !settlement_calls 0);
      sync_blocked := false;
      let current = R.observe handle |> F.run_ok in
      (match
         R.admit_epoch
           run
           ~owner:(F.owner 1)
           ~capture_fingerprint:capture
           ~previous:(Some current)
       with
       | Error R.Error.Conflict -> ()
       | _ -> failwith "uncertain selected commit did not block a fresh mutation");
      let final = R.reconcile_and_settle handle prepared token |> F.run_ok |> installed in
      assert (Option.is_some (R.find_job final selected_id));
      assert (Int.equal !settlement_calls 1);
      ignore (R.reconcile_and_settle handle prepared token |> F.run_ok |> installed);
      assert (Int.equal !settlement_calls 1);
      let later =
        R.prepare_root_selected
          handle
          ~expected:final
          ~phase:R.Checkpoint.Active
          ~selected:[]
        |> F.run_ok
      in
      let later_observed =
        R.commit_and_settle handle later (handoff ()) |> F.run_ok |> installed
      in
      ignore (R.commit_and_settle handle prepared token |> F.run_ok |> installed);
      assert (Int.equal !settlement_calls 1);
      assert (
        Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint later_observed)) 3L);
      R.close_and_wait run |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session);
      let reopened =
        Agent_store.Session_store.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"selected-restart"
        |> F.store_ok
      in
      let run = F.open_run reopened sw () in
      let _handle, recovered = F.admit run owner capture in
      assert (Option.is_some (R.find_job recovered selected_id));
      R.close_and_wait run |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close reopened)))
;;
