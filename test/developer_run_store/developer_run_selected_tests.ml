open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module F = Developer_run_store_tests

module Held_domain_manager = struct
  type t =
    { manager : Eio.Domain_manager.ty Eio.Resource.t
    ; armed : bool Atomic.t
    ; skip : int Atomic.t
    ; entered : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    ; finished : bool Atomic.t
    }

  let run t f =
    let (Eio.Resource.T (data, ops)) = t.manager in
    let module Native = (val Eio.Resource.get ops Eio.Domain_manager.Pi.Mgr) in
    Native.run data (fun ~cancelled ->
      if Atomic.get t.armed && Atomic.get t.skip > 0
      then (
        Atomic.set t.skip 0;
        f ~cancelled)
      else if Atomic.exchange t.armed false
      then
        Exn.protect
          ~finally:(fun () -> Atomic.set t.finished true)
          ~f:(fun () ->
            (* Run the real canonical codec, then hold its return while the
               Domain_manager call still owns the Store lease. *)
            let _decoded = f ~cancelled in
            Eio.Promise.resolve t.entered ();
            Eio.Cancel.protect (fun () -> Eio.Promise.await t.release);
            raise (Eio.Promise.await cancelled))
      else f ~cancelled)
  ;;

  let run_raw t f = Eio.Domain_manager.run_raw t.manager f
end

let with_domain_manager env manager : Eio_unix.Stdenv.base =
  object
    method domain_mgr = manager
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method fs = env#fs
    method cwd = env#cwd
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

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
  | Absent | Prewrite_conflict | Diverged ->
    failwith "expected durable installed transaction"
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
  assert (Result.is_error (P.Host_invocation.developer_of_json `Null));
  List.iter [ invocation; dispatched; resolved ] ~f:(fun value ->
    let json = P.Host_invocation.developer_to_json value |> F.ok in
    let decoded = P.Host_invocation.developer_of_json json |> F.ok in
    assert (Jsonaf.exactly_equal json (P.Host_invocation.developer_to_json decoded |> F.ok));
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
    ; { status = P.Job.Cancelled
      ; attempt = 1
      ; started_at = None
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
  assert (Result.is_error (P.Developer_job.of_json `Null));
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
      ignore
        (R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
         |> F.run_ok
         |> installed);
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
      R.commit_and_settle handle ~admit:(fun () -> true) dispatch (handoff ())
      |> F.run_ok
      |> installed
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
      R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
      |> F.run_ok
      |> installed
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
      R.commit_and_settle handle ~admit:(fun () -> true) root_prepared (handoff ())
      |> F.run_ok
      |> installed
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
      ignore
        (R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
         |> F.run_ok
         |> installed));
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
        R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
        |> F.run_ok
        |> installed
      in
      List.iteri candidates ~f:(fun index id ->
        assert (
          Bool.equal
            (Option.is_some (R.find_job after id))
            (List.mem indexes index ~equal:Int.equal)));
      assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint after)) 2L);
      R.close_and_wait run |> F.run_ok))
;;

let%test_unit "inactive cancellation is exact-attempt CAS across stale and absent writes" =
  F.with_root (fun _env _sw _root _session run ->
    let owner = F.owner 0 in
    let capture = F.digest "inactive cancellation CAS" in
    let handle, initial = F.admit run owner capture in
    let id = job_id "job_inactive_cancel" in
    let chosen = job owner capture ~id ~parent:P.Developer_job.Launch.Run_root in
    let selected =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ chosen ]
      |> F.run_ok
      |> fun prepared ->
      R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
      |> F.run_ok
      |> installed
    in
    let lookup = R.find_job_exact selected id in
    let stale =
      R.prepare_inactive_cancel handle ~expected:selected ~lookup ~now:timestamp
      |> F.run_ok
    in
    let unrelated =
      R.prepare_root_selected
        handle
        ~expected:selected
        ~phase:R.Checkpoint.Active
        ~selected:[]
      |> F.run_ok
    in
    ignore
      (R.commit_and_settle handle ~admit:(fun () -> true) unrelated (handoff ())
       |> F.run_ok
       |> installed
       : R.Observation.t);
    (match R.commit_inactive_cancel handle stale with
     | Error R.Error.Prewrite_conflict -> ()
     | Ok _ | Error _ -> failwith "stale cancellation overwrote another revision");
    (match R.reconcile_inactive_cancel handle stale with
     | Error R.Error.Prewrite_conflict -> ()
     | Ok _ | Error _ -> failwith "stale cancellation was adopted");
    let current = R.observe handle |> F.run_ok in
    let exact =
      R.prepare_inactive_cancel
        handle
        ~expected:current
        ~lookup:(R.find_job_exact current id)
        ~now:timestamp
      |> F.run_ok
    in
    (match R.reconcile_inactive_cancel handle exact |> F.run_ok with
     | R.Absent -> ()
     | Installed _ | Diverged -> failwith "unattempted cancellation was not absent");
    let installed = R.commit_inactive_cancel handle exact |> F.run_ok in
    let cancelled =
      R.find_job (R.Installed.observation installed) id |> Option.value_exn
    in
    assert (
      match (P.Developer_job.state cancelled).status with
      | P.Job.Cancelled -> true
      | _ -> false);
    assert (Int.equal (P.Developer_job.state cancelled).attempt 0);
    (match R.reconcile_inactive_cancel handle exact |> F.run_ok with
     | R.Installed _ -> ()
     | Absent | Diverged -> failwith "installed cancellation lost its durable result");
    let terminal = R.observe handle |> F.run_ok in
    assert (
      Result.is_error
        (R.prepare_inactive_cancel
           handle
           ~expected:terminal
           ~lookup:(R.find_job_exact terminal id)
           ~now:timestamp));
    R.close_and_wait run |> F.run_ok)
;;

let%test_unit "inactive cancellation reconciles a post-rename uncertain write" =
  F.with_temp_directory "ochat-inactive-cancel-uncertain" (fun original temporary ->
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
      in
      let session = F.create_session env sw (Filename.concat temporary "data") in
      let run = F.open_run session sw () in
      let owner = F.owner 0 in
      let capture = F.digest "uncertain inactive cancellation" in
      let handle, initial = F.admit run owner capture in
      let id = job_id "job_uncertain_inactive_cancel" in
      let chosen = job owner capture ~id ~parent:P.Developer_job.Launch.Run_root in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:initial
          ~phase:R.Checkpoint.Active
          ~selected:[ chosen ]
        |> F.run_ok
      in
      let selected =
        R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
        |> F.run_ok
        |> installed
      in
      let cancellation =
        R.prepare_inactive_cancel
          handle
          ~expected:selected
          ~lookup:(R.find_job_exact selected id)
          ~now:timestamp
        |> F.run_ok
      in
      armed := true;
      assert (Result.is_error (R.commit_inactive_cancel handle cancellation));
      (match R.reconcile_inactive_cancel handle cancellation |> F.run_ok with
       | R.Installed installed ->
         let cancelled =
           R.find_job (R.Installed.observation installed) id |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state cancelled).status with
           | P.Job.Cancelled -> true
           | _ -> false)
       | Absent | Diverged ->
         failwith "uncertain cancellation lost its exact installed write");
      R.close_and_wait run |> F.run_ok;
      F.store_ok (Agent_store.Session_store.close session)))
;;

let%test_unit "handoff settlement is one-way across absent, prewrite and callback failure"
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
     | Installed _ | Prewrite_conflict | Diverged ->
       failwith "new transaction was not absent");
    (match
       R.commit_and_settle handle ~admit:(fun () -> true) prepared token |> F.run_ok
     with
     | R.Settlement.Absent -> ()
     | Installed _ | Prewrite_conflict | Diverged ->
       failwith "settled absence was reversed");
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
    let capture = F.digest "prewrite settlement" in
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
    let identical_unwritten = prepare "job_diverged_winner" in
    ignore
      (R.commit_and_settle handle ~admit:(fun () -> true) winner (handoff ())
       |> F.run_ok
       |> installed);
    let calls = ref 0 in
    let token = handoff ~on_settle:(fun _ -> incr calls) () in
    (match R.reconcile_and_settle handle stale token |> F.run_ok with
     | R.Settlement.Prewrite_conflict -> ()
     | Installed _ | Absent | Diverged ->
       failwith "unattempted transaction was settled as valid");
    (match R.commit_and_settle handle ~admit:(fun () -> true) stale token |> F.run_ok with
     | R.Settlement.Prewrite_conflict -> ()
     | Installed _ | Absent | Diverged -> failwith "prewrite handoff was reversed");
    assert (Int.equal !calls 1);
    let identical_calls = ref 0 in
    let identical_token = handoff ~on_settle:(fun _ -> incr identical_calls) () in
    (match
       R.commit_and_settle
         handle
         ~admit:(fun () -> true)
         identical_unwritten
         identical_token
       |> F.run_ok
     with
     | R.Settlement.Prewrite_conflict -> ()
     | Installed _ | Absent | Diverged ->
       failwith "identical after bytes gave a second publication right");
    assert (Int.equal !identical_calls 1);
    let current = R.observe handle |> F.run_ok in
    let _, next =
      R.admit_epoch
        run
        ~owner:(F.owner 1)
        ~capture_fingerprint:capture
        ~previous:(Some current)
      |> F.run_ok
    in
    assert (Int.equal (R.Checkpoint.owner (R.Observation.checkpoint next)).epoch 1);
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
    (match R.commit_and_settle handle ~admit:(fun () -> true) prepared token with
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
    ignore
      (R.commit_and_settle handle ~admit:(fun () -> true) prepared token
       |> F.run_ok
       |> installed);
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
         ignore
           (R.commit_and_settle handle ~admit:(fun () -> true) prepared token |> F.run_ok);
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
            | Absent | Prewrite_conflict | Diverged ->
              failwith "unexpected selected settlement")
          ()
      in
      rename_armed := true;
      assert (
        Result.is_error
          (R.commit_and_settle handle ~admit:(fun () -> true) prepared token));
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
        R.commit_and_settle handle ~admit:(fun () -> true) later (handoff ())
        |> F.run_ok
        |> installed
      in
      ignore
        (R.commit_and_settle handle ~admit:(fun () -> true) prepared token
         |> F.run_ok
         |> installed);
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

let%test_unit "selected rename admission rejects after staging without publishing" =
  let run_case ~admit ~expected_error =
    F.with_root (fun _env _sw _root _session run ->
      let owner = F.owner 0 in
      let capture = F.digest "guarded selected graph" in
      let handle, initial = F.admit run owner capture in
      let selected_id = job_id "job_guarded_selected" in
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
      let calls = ref 0 in
      let token = handoff ~on_settle:(fun _ -> incr calls) () in
      expected_error (fun () -> R.commit_and_settle handle prepared token ~admit);
      assert (Int.equal !calls 1);
      (match R.reconcile_and_settle handle prepared token |> F.run_ok with
       | R.Settlement.Absent -> ()
       | Installed _ | Prewrite_conflict | Diverged ->
         failwith "rejected selected write was published");
      assert (Int.equal !calls 1);
      let current = R.observe handle |> F.run_ok in
      assert (Option.is_none (R.find_job current selected_id));
      assert (
        Int64.equal
          (R.Checkpoint.revision (R.Observation.checkpoint current))
          (R.Checkpoint.revision (R.Observation.checkpoint initial)));
      let _next_handle, _next =
        R.admit_epoch
          run
          ~owner:(F.owner 1)
          ~capture_fingerprint:capture
          ~previous:(Some current)
        |> F.run_ok
      in
      R.close_and_wait run |> F.run_ok)
  in
  let checked = ref 0 in
  run_case
    ~admit:(fun () ->
      incr checked;
      false)
    ~expected_error:(fun commit ->
      match commit () with
      | Error R.Error.Foreign_or_stale -> ()
      | Ok _ | Error _ -> failwith "rejected pre-rename admission was acknowledged");
  assert (Int.equal !checked 1);
  let original = Core_unix.Unix_error (EIO, "injected admission I/O", "selected") in
  run_case
    ~admit:(fun () -> raise original)
    ~expected_error:(fun commit ->
      match commit () with
      | exception actual -> assert (phys_equal actual original)
      | Ok _ | Error _ -> failwith "pre-rename exception identity was lost")
;;

let%test_unit "cancelled large claim preparation joins its codec before close and retry" =
  F.with_temp_directory "ochat-developer-large-cpu" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let finished = Atomic.make false in
      let armed = Atomic.make false in
      (* Claim preparation first decodes the old document, then encodes the
         proposed Running state. Hold that second pure CPU operation. *)
      let skip = Atomic.make 1 in
      let observed =
        Held_domain_manager.
          { manager = Eio.Stdenv.domain_mgr env
          ; armed
          ; skip
          ; entered = entered_u
          ; release
          ; finished
          }
      in
      let manager =
        Eio.Resource.T (observed, Eio.Domain_manager.Pi.mgr (module Held_domain_manager))
      in
      let wrapped = with_domain_manager env manager in
      let session = F.create_session wrapped sw (Filename.concat temporary "data") in
      let max_bytes = 512 * 1024 in
      let document =
        Document_schema.Limits.create
          ~max_bytes
          ~max_depth:16
          ~max_fields:1_000
          ~max_nodes:2_000
        |> F.ok
      in
      let limits =
        R.Limits.create
          ~max_runs:1
          ~max_epochs_per_run:1
          ~max_jobs_per_run:1
          ~max_invocations_per_run:1
          ~max_inference_attempts_per_run:256
          ~commands:(Agent_store.Developer_command_limits.default ~document)
          ~max_inference_bytes_per_run:max_bytes
          ~max_selected_per_commit:1
          ~max_job_payload_bytes:max_bytes
          ~max_document_bytes:max_bytes
          ~max_recovery_entries:16
          ~max_recovery_bytes:max_bytes
          ~max_work_steps:10_000
          ~document
        |> Or_error.ok_exn
      in
      let store = R.open_or_create session ~sw ~limits ~checkpoint:ignore |> F.run_ok in
      let owner = F.owner 0 in
      let capture = F.digest "large developer CPU document" in
      let handle, initial = F.admit store owner capture in
      let id = job_id "job_large_cpu_document" in
      let launch =
        P.Developer_job.Launch.create
          ~parent:P.Developer_job.Launch.Run_root
          ~parent_job:None
          ~nested_depth:0
        |> F.ok
      in
      let payload = `Object [ "text", `String (String.make 128_000 'x') ] in
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
      let selected =
        P.Developer_job.create
          ~owner
          ~capture_fingerprint:capture
          ~id
          ~launch
          ~kind:P.Job.Async_tool
          ~payload
          ~retry_policy:P.Job.Never
          ~created_at:timestamp
          ~state
        |> F.ok
      in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:initial
          ~phase:R.Checkpoint.Active
          ~selected:[ selected ]
        |> F.run_ok
      in
      ignore
        (R.commit_and_settle handle ~admit:(fun () -> true) prepared (handoff ())
         |> F.run_ok
         |> installed
         : R.Observation.t);
      let authority =
        Eio.Path.(
          Eio.Stdenv.fs env
          / Filename.concat temporary "data"
          / "developer-runs"
          / ("run_" ^ P.Id.Developer_run.to_string F.run_id ^ ".json"))
      in
      let before = Eio.Path.load authority in
      assert (String.length before > 64 * 1024);
      let first_candidate =
        R.scheduler_candidates handle ~checkpoint:ignore |> F.run_ok |> List.hd_exn
      in
      let reason = Failure "large claim preparation cancelled" in
      let cancel_ready, cancel_ready_u = Eio.Promise.create () in
      let scan_done, scan_done_u = Eio.Promise.create () in
      Atomic.set armed true;
      Eio.Fiber.fork ~sw (fun () ->
        let outcome =
          try
            Eio.Cancel.sub (fun cancel ->
              Eio.Promise.resolve cancel_ready_u cancel;
              ignore
                (R.prepare_job_claim
                   handle
                   ~expected:(R.Scheduler_candidate.observation first_candidate)
                   ~lookup:(R.Scheduler_candidate.lookup first_candidate)
                   ~now:timestamp
                 : _ result));
            `Unexpected_success
          with
          | Eio.Cancel.Cancelled actual -> `Cancelled actual
        in
        Eio.Promise.resolve scan_done_u outcome);
      let cancel = Eio.Promise.await cancel_ready in
      Eio.Promise.await entered;
      let close_done, close_done_u = Eio.Promise.create () in
      Exn.protect
        ~finally:(fun () -> Eio.Promise.resolve release_u ())
        ~f:(fun () ->
          Eio.Cancel.cancel cancel reason;
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve close_done_u (R.close_and_wait store));
          Eio.Fiber.yield ();
          assert (R.is_closed store);
          assert (not (Eio.Promise.is_resolved close_done));
          assert (not (Atomic.get finished)));
      (match Eio.Promise.await scan_done with
       | `Cancelled actual -> assert (phys_equal actual reason)
       | `Unexpected_success -> failwith "claim preparation cancellation was lost");
      assert (Atomic.get finished);
      Eio.Promise.await close_done |> F.run_ok;
      assert (String.equal before (Eio.Path.load authority));
      let reopened =
        R.open_or_create session ~sw ~limits ~checkpoint:ignore |> F.run_ok
      in
      let handle, _ = F.admit reopened owner capture in
      let candidate =
        R.scheduler_candidates handle ~checkpoint:ignore |> F.run_ok |> List.hd_exn
      in
      assert (
        Jsonaf.exactly_equal
          payload
          (P.Developer_job.payload (R.Scheduler_candidate.job candidate)));
      let claimed =
        R.prepare_job_claim
          handle
          ~expected:(R.Scheduler_candidate.observation candidate)
          ~lookup:(R.Scheduler_candidate.lookup candidate)
          ~now:timestamp
        |> F.run_ok
        |> R.commit_job_claim handle
        |> F.run_ok
      in
      R.Claim.abandon_before_callback (R.Claimed.claim claimed) ~handle |> F.run_ok;
      R.close_and_wait reopened |> F.run_ok;
      Agent_store.Session_store.close session |> F.store_ok))
;;
