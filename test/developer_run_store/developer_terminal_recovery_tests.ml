open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module S = Agent_store.Session_store
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests

let interrupted job =
  let state = P.Developer_job.state job in
  { state with
    status = P.Job.Interrupted "recovered after restart"
  ; completed_at = Some Selected.timestamp
  ; result = None
  ; retry_failure = None
  }
;;

let authority env root (owner : P.Execution_owner.developer) =
  Eio.Path.(
    Eio.Stdenv.fs env
    / root
    / "developer-runs"
    / ("run_" ^ P.Id.Developer_run.to_string owner.run_id ^ ".json"))
;;

let seed_running ?(alternate = false) ?(extra_jobs = 0) ~env ~root ~owner ~capture ~id () =
  Eio.Switch.run (fun sw ->
    let session = F.create_session env sw root in
    let store = F.open_run session sw ~max_jobs_per_run:(Int.max 4 (extra_jobs + 1)) () in
    let handle, initial = F.admit store owner capture in
    let job = Selected.job owner capture ~id ~parent:P.Developer_job.Launch.Run_root in
    let prepared =
      R.prepare_root_selected
        handle
        ~expected:initial
        ~phase:R.Checkpoint.Active
        ~selected:[ job ]
      |> F.run_ok
    in
    let observed =
      R.commit_and_settle handle prepared (Selected.handoff ()) ~admit:(fun () -> true)
      |> F.run_ok
      |> Selected.installed
    in
    let lookup = R.find_job_exact observed id in
    let prepared =
      R.prepare_job_claim handle ~expected:observed ~lookup ~now:Selected.timestamp
      |> F.run_ok
    in
    ignore (R.commit_job_claim handle prepared |> F.run_ok : R.Claimed.t);
    List.iter (List.range 0 extra_jobs) ~f:(fun index ->
      let observed = R.observe handle |> F.run_ok in
      let extra =
        Selected.job
          owner
          capture
          ~id:(Selected.job_id ("job_recovery_extra_" ^ Int.to_string index))
          ~parent:P.Developer_job.Launch.Run_root
      in
      let prepared =
        R.prepare_root_selected
          handle
          ~expected:observed
          ~phase:R.Checkpoint.Active
          ~selected:[ extra ]
        |> F.run_ok
      in
      ignore
        (R.commit_and_settle handle prepared (Selected.handoff ()) ~admit:(fun () -> true)
         |> F.run_ok
         |> Selected.installed
         : R.Observation.t));
    let alternate_bytes =
      if alternate
      then (
        let file = authority env root owner in
        let original = Eio.Path.load file in
        let observed = R.observe handle |> F.run_ok in
        let prepared =
          R.prepare_root_selected
            handle
            ~expected:observed
            ~phase:R.Checkpoint.Active
            ~selected:[]
          |> F.run_ok
        in
        ignore
          (R.commit_and_settle handle prepared (Selected.handoff ()) ~admit:(fun () ->
             true)
           |> F.run_ok
           |> Selected.installed
           : R.Observation.t);
        let newer = Eio.Path.load file in
        Eio.Path.save ~create:(`Or_truncate 0o600) file original;
        Some newer)
      else None
    in
    R.close_and_wait store |> F.run_ok;
    S.close session |> F.store_ok;
    alternate_bytes)
;;

let recover_one ~historical =
  F.with_temp_directory "ochat-developer-terminal-recovery" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "terminal recovery original capture" in
    let id = Selected.job_id "job_terminal_recovery" in
    ignore (seed_running ~env ~root ~owner ~capture ~id () : string option);
    Eio.Switch.run (fun sw ->
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"developer-foundation-reopened"
        |> F.store_ok
      in
      let store = F.open_run session sw () in
      let original, observed = F.admit store owner capture in
      let handle =
        if historical
        then (
          let handle, _ =
            R.admit_epoch
              store
              ~owner:(F.owner 1)
              ~capture_fingerprint:(F.digest "new epoch capture")
              ~previous:(Some observed)
            |> F.run_ok
          in
          handle)
        else original
      in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let view = R.Recovery.observe recovery |> F.run_ok in
      let candidates = R.Recovery.View.candidates view in
      assert (Int.equal (List.length candidates) 1);
      let candidate = List.hd_exn candidates in
      let job = R.Recovery.Candidate.job candidate in
      assert (P.Execution_owner.equal_developer (P.Developer_job.owner job) owner);
      let bad =
        let after = interrupted job in
        { after with attempt = after.attempt + 1 }
      in
      assert (
        Result.is_error (R.Recovery.prepare_terminal recovery ~view ~candidate ~after:bad));
      assert (
        Result.is_error
          (R.prepare_checkpoint
             handle
             ~expected:(R.Recovery.View.observation view)
             ~phase:R.Checkpoint.Active));
      let pending =
        R.Recovery.prepare_terminal recovery ~view ~candidate ~after:(interrupted job)
        |> F.run_ok
      in
      assert (Result.is_error (R.Run_claim.release claim));
      (match R.Recovery.reconcile_terminal recovery pending |> F.run_ok with
       | R.Absent -> ()
       | Installed _ | Diverged -> failwith "unwritten terminal was not absent");
      let pending = R.Recovery.reprepare_same recovery pending |> F.run_ok in
      ignore
        (R.Recovery.commit_terminal recovery pending ~admit:(fun () -> true) |> F.run_ok
         : R.Installed.t);
      R.Recovery.finish recovery |> F.run_ok;
      assert (R.Recovery.is_finished recovery);
      let after = R.observe handle |> F.run_ok in
      let job = R.find_job after id |> Option.value_exn in
      (match (P.Developer_job.state job).status with
       | Interrupted _ -> ()
       | Queued
       | Running
       | Waiting_permission _
       | Waiting_completion _
       | Succeeded
       | Failed _
       | Cancelled -> failwith "recovered job did not retain terminal state");
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok))
;;

let%expect_test
    "terminal recovery keeps original attempt across current and historical epochs"
  =
  List.iter [ false; true ] ~f:(fun historical ->
    recover_one ~historical;
    print_s [%sexp (historical : bool), "exact terminal recovered"]);
  [%expect
    {|
    (false "exact terminal recovered")
    (true "exact terminal recovered")
    |}]
;;

let%test_unit "recovery reprepare follows exact selected terminal after prewrite conflict"
  =
  F.with_temp_directory "ochat-developer-recovery-prewrite" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "recovery prewrite capture" in
    let id = Selected.job_id "job_recovery_prewrite" in
    let newer =
      seed_running ~alternate:true ~env ~root ~owner ~capture ~id () |> Option.value_exn
    in
    Eio.Switch.run (fun sw ->
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"recovery-prewrite-open"
        |> F.store_ok
      in
      let store = F.open_run session sw () in
      let handle, _ = F.admit store owner capture in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let view = R.Recovery.observe recovery |> F.run_ok in
      let candidate = R.Recovery.View.candidates view |> List.hd_exn in
      let job = R.Recovery.Candidate.job candidate in
      let pending =
        R.Recovery.prepare_terminal recovery ~view ~candidate ~after:(interrupted job)
        |> F.run_ok
      in
      (* A competing external writer installs another valid Store-produced
         revision after our exact before-bytes were captured. *)
      Eio.Path.save ~create:(`Or_truncate 0o600) (authority env root owner) newer;
      assert (
        match R.Recovery.commit_terminal recovery pending ~admit:(fun () -> true) with
        | Error R.Error.Prewrite_conflict -> true
        | Ok _ | Error _ -> false);
      assert (
        match R.Recovery.reconcile_terminal recovery pending with
        | Error R.Error.Prewrite_conflict -> true
        | Ok _ | Error _ -> false);
      let same = R.Recovery.reprepare_same recovery pending |> F.run_ok in
      assert (
        phys_equal (R.Recovery.Pending.after pending) (R.Recovery.Pending.after same));
      ignore
        (R.Recovery.commit_terminal recovery same ~admit:(fun () -> true) |> F.run_ok
         : R.Installed.t);
      R.Recovery.finish recovery |> F.run_ok;
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok))
;;

let%test_unit "attempted recovery rename retains exact pending and exception" =
  F.with_temp_directory "ochat-developer-recovery-attempted" (fun original temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "recovery attempted capture" in
    let id = Selected.job_id "job_recovery_attempted" in
    ignore (seed_running ~env:original ~root ~owner ~capture ~id () : string option);
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let sentinel = Failure "recovery rename interrupted" in
      let env =
        F.fault_after_authority_rename_env
          original
          ~rename_armed:armed
          ~sync_blocked
          ~parent_sync_blocked
          ~after_rename:(fun () -> raise sentinel)
      in
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"recovery-attempted-open"
        |> F.store_ok
      in
      let store = F.open_run session sw () in
      let handle, _ = F.admit store owner capture in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let view = R.Recovery.observe recovery |> F.run_ok in
      let candidate = R.Recovery.View.candidates view |> List.hd_exn in
      let pending =
        R.Recovery.prepare_terminal
          recovery
          ~view
          ~candidate
          ~after:(interrupted (R.Recovery.Candidate.job candidate))
        |> F.run_ok
      in
      armed := true;
      assert (
        match R.Recovery.commit_terminal recovery pending ~admit:(fun () -> true) with
        | exception exn -> phys_equal exn sentinel
        | Ok _ | Error _ -> false);
      assert (
        Option.value_map (R.Recovery.pending recovery) ~default:false ~f:(fun current ->
          phys_equal current pending));
      assert (Result.is_error (R.Run_claim.release claim));
      (match R.Recovery.reconcile_terminal recovery pending |> F.run_ok with
       | Installed _ -> ()
       | Absent | Diverged -> failwith "attempted terminal lost installed selection");
      R.Recovery.finish recovery |> F.run_ok;
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok))
;;

let%test_unit "recovery work limit and callback exception stay distinct" =
  F.with_temp_directory "ochat-developer-recovery-budget" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "recovery budget capture" in
    let id = Selected.job_id "job_recovery_budget" in
    ignore (seed_running ~extra_jobs:5 ~env ~root ~owner ~capture ~id () : string option);
    let ticks = ref 0 in
    let checkpoint () = incr ticks in
    let observed_work =
      Eio.Switch.run (fun sw ->
        let session =
          S.open_existing
            ~env
            ~sw
            ~root
            ~process_start_identity:None
            ~lock_nonce:"recovery-budget-probe"
          |> F.store_ok
        in
        let store = F.open_run session sw ~checkpoint ~max_jobs_per_run:6 () in
        let opening_work = !ticks in
        let handle, _ = F.admit store owner capture in
        let admission_work = !ticks - opening_work in
        let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
        let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
        let before = !ticks in
        let view = R.Recovery.observe recovery |> F.run_ok in
        let observed_work = !ticks - before in
        (* Opening now includes the independent abandoned-Live inventory scan;
           compare recovery against admission work, which this limit probes. *)
        if observed_work <= admission_work
        then
          failwithf
            "recovery work thresholds: opening=%d admission=%d observed=%d"
            opening_work
            admission_work
            observed_work
            ();
        let observed = R.Recovery.View.observation view in
        let extra = Selected.job_id "job_recovery_extra_0" in
        assert (
          match
            R.prepare_job_claim
              handle
              ~expected:observed
              ~lookup:(R.find_job_exact observed extra)
              ~now:Selected.timestamp
          with
          | Error R.Error.Conflict -> true
          | Ok _ | Error _ -> false);
        assert (Result.is_error (R.Run_claim.release claim));
        R.Recovery.abort_clean recovery |> F.run_ok;
        R.Run_claim.release claim |> F.run_ok;
        R.close_and_wait store |> F.run_ok;
        S.close session |> F.store_ok;
        observed_work)
    in
    Eio.Switch.run (fun sw ->
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"recovery-budget-limited"
        |> F.store_ok
      in
      let store =
        F.open_run session sw ~max_jobs_per_run:6 ~max_work_steps:(observed_work - 1) ()
      in
      let handle, _ = F.admit store owner capture in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let limited = function
        | Error R.Error.Limit_exceeded -> true
        | Ok _ | Error _ -> false
      in
      assert (limited (R.Recovery.observe recovery));
      assert (limited (R.Recovery.finish recovery));
      R.Recovery.abort_clean recovery |> F.run_ok;
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok);
    Eio.Switch.run (fun sw ->
      ticks := 0;
      let trigger = ref None in
      let sentinel = Failure "reprepare callback interrupted" in
      let checkpoint () =
        incr ticks;
        if Option.value_map !trigger ~default:false ~f:(Int.equal !ticks)
        then raise sentinel
      in
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"recovery-budget-sentinel"
        |> F.store_ok
      in
      let store = F.open_run session sw ~checkpoint ~max_jobs_per_run:6 () in
      let handle, _ = F.admit store owner capture in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let view = R.Recovery.observe recovery |> F.run_ok in
      let candidate = R.Recovery.View.candidates view |> List.hd_exn in
      let pending =
        R.Recovery.prepare_terminal
          recovery
          ~view
          ~candidate
          ~after:(interrupted (R.Recovery.Candidate.job candidate))
        |> F.run_ok
      in
      (match R.Recovery.reconcile_terminal recovery pending |> F.run_ok with
       | Absent -> ()
       | Installed _ | Diverged -> failwith "unwritten terminal was not absent");
      ticks := 0;
      trigger := Some (observed_work + 1);
      assert (
        match R.Recovery.reprepare_same recovery pending with
        | exception exn -> phys_equal exn sentinel
        | Ok _ | Error _ -> false);
      assert (
        Option.value_map (R.Recovery.pending recovery) ~default:false ~f:(fun current ->
          phys_equal current pending));
      trigger := None;
      let same = R.Recovery.reprepare_same recovery pending |> F.run_ok in
      ignore
        (R.Recovery.commit_terminal recovery same ~admit:(fun () -> true) |> F.run_ok
         : R.Installed.t);
      R.Recovery.finish recovery |> F.run_ok;
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok))
;;
