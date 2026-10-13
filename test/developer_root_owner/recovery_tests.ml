open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module S = Agent_store.Session_store
module O = Agent_server.Developer_root_owner
module C = Chatml_developer_capture
module F = Developer_run_store_test_suite.Developer_run_store_tests
module Selected = Developer_run_store_test_suite.Developer_run_selected_tests
module Base = Root_owner_tests

let seed_running env root captured owner id =
  Eio.Switch.run (fun sw ->
    let session =
      S.create
        ~env
        ~sw
        ~root
        ~server_id:Base.host_id
        ~process_start_identity:None
        ~lock_nonce:"root-recovery-seed"
      |> Base.ok
    in
    let store =
      R.open_or_create session ~sw ~limits:(Base.store_limits ()) ~checkpoint:ignore
      |> Base.ok
    in
    let handle, initial =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:None
      |> Base.ok
    in
    let job =
      Selected.job
        owner
        (C.fingerprint captured)
        ~id
        ~parent:P.Developer_job.Launch.Run_root
    in
    let selected =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ job ]
      |> Base.ok
    in
    let observed =
      R.commit_and_settle handle selected (Selected.handoff ()) ~admit:(fun () -> true)
      |> Base.ok
      |> Selected.installed
    in
    let prepared =
      R.prepare_job_claim
        handle
        ~expected:observed
        ~lookup:(R.find_job_exact observed id)
        ~now:(Base.now ())
      |> Base.ok
    in
    ignore (R.commit_job_claim handle prepared |> Base.ok : R.Claimed.t);
    R.close_and_wait store |> Base.ok;
    S.close session |> Base.ok)
;;

let%test_unit "startup retains exact terminal selection and blocks revoked success" =
  F.with_temp_directory "ochat-root-recovery" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let captured = Base.capture () in
    let owner = Base.owner "dvr_root_recovery" 0 in
    let id = Selected.job_id "job_root_recovery" in
    seed_running env root captured owner id;
    Eio.Switch.run (fun sw ->
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"root-recovery-open"
        |> Base.ok
      in
      let store =
        R.open_or_create session ~sw ~limits:(Base.store_limits ()) ~checkpoint:ignore
        |> Base.ok
      in
      let handle, _ =
        R.admit_epoch
          store
          ~owner
          ~capture_fingerprint:(C.fingerprint captured)
          ~previous:None
        |> Base.ok
      in
      let actor_current = ref true in
      let actor =
        Operator_authorization.guarded ~principal:Base.principal ~is_current:(fun () ->
          !actor_current)
      in
      let registry =
        Chat_response.Tool_capability.create
          ~owner:"root-recovery"
          ~resource_fingerprint:Digestif.SHA256.(digest_string "recovery" |> to_hex)
          []
        |> Base.ok
      in
      let limits =
        O.Limits.create
          ~max_active_runs:1
          ~max_staged_jobs:2
          ~max_native_occupancy:2
          ~max_active_workers:1
          ~max_quarantined_workers:2
        |> Base.or_error
      in
      let owners =
        O.Registry.create ~sw ~store ~capacity:(Base.capacity ()) ~now:Base.now ~limits
      in
      let common ~recover =
        O.Registry.admit_with_recovery
          owners
          ~actor
          ~capture:captured
          ~registry
          ~selected:registry
          ~job_policy:Chat_response.One_off_request.default_policy
          ~handle
          ~workspace_conflict_domain:"root-recovery"
          ~recover
      in
      assert (
        Result.is_error
          (O.Registry.admit
             owners
             ~actor
             ~capture:captured
             ~registry
             ~selected:registry
             ~job_policy:Chat_response.One_off_request.default_policy
             ~handle
             ~workspace_conflict_domain:"root-recovery"));
      let released = R.Run_claim.acquire ~store ~handle |> Base.ok in
      R.Run_claim.release released |> Base.ok;
      let original = P.Error.invalid_request "retain selected terminal" in
      assert (
        Result.is_error
          (common ~recover:(fun recovery ->
             let view = R.Recovery.observe recovery |> Base.ok in
             let candidate = R.Recovery.View.candidates view |> List.hd_exn in
             let job = R.Recovery.Candidate.job candidate in
             let state = P.Developer_job.state job in
             let after : P.Developer_job.State.t =
               { state with
                 status = P.Job.Succeeded
               ; completed_at = Some (Base.now ())
               ; result =
                   Some
                     (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
               ; retry_failure = None
               }
             in
             ignore
               (R.Recovery.prepare_terminal recovery ~view ~candidate ~after |> Base.ok
                : R.Recovery.Pending.t);
             Error original)));
      assert (Result.is_error (R.Run_claim.acquire ~store ~handle));
      actor_current := false;
      assert (
        Result.is_error
          (O.Registry.resume_recovery owners ~actor ~handle ~recover:(fun recovery ->
             ignore (recovery : R.Recovery.t);
             failwith "revoked actor must not enter recovery callback")));
      let observed = R.observe handle |> Base.ok in
      let job = R.find_job observed id |> Option.value_exn in
      (match (P.Developer_job.state job).status with
       | Running -> ()
       | _ -> failwith "revoked actor changed selected outcome");
      actor_current := true;
      let run =
        O.Registry.resume_recovery owners ~actor ~handle ~recover:(fun recovery ->
          let pending = R.Recovery.pending recovery |> Option.value_exn in
          ignore
            (R.Recovery.commit_terminal recovery pending ~admit:(fun () ->
               Operator_authorization.is_current actor)
             |> Base.ok
             : R.Installed.t);
          R.Recovery.finish recovery |> Result.map_error ~f:(fun _ -> original))
        |> Base.ok
      in
      O.close_and_wait run |> Base.ok;
      O.Registry.close_and_wait owners |> Base.ok;
      R.close_and_wait store |> Base.ok;
      S.close session |> Base.ok))
;;
