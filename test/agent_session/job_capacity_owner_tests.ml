open Core
open Fixtures
module P = Agent_protocol
module D = P.Developer_job
module H = P.Host_job
module C = Agent_server.Job_capacity

let limits ?(daemon_total = 1) ?(per_session = 1) () =
  Agent_server.Config.Server.
    { daemon_total
    ; per_principal = 4
    ; per_prompt = 4
    ; per_workspace = 4
    ; per_session
    ; per_kind = 4
    ; max_nested_depth = 4
    }
;;

let owner ?(epoch = 0) ?run_id () =
  let run_id = Option.value run_id ~default:(P.Id.Developer_run.create ()) in
  match
    P.Execution_owner.developer
      ~host_id:(P.Id.Server.of_string "srv_capacity_test" |> protocol_ok)
      ~run_id
      ~epoch
      ~principal_id
    |> protocol_ok
  with
  | Developer owner -> owner
  | Conversation _ -> assert false
;;

let source_id =
  Chatml_source.Id.create ~package_namespace:"capacity_test" ~relative_path:"main.chatml"
  |> Or_error.ok_exn
;;

let fingerprint = Chatml_source.Digest.of_contents "capture" ~checkpoint:ignore

let queued =
  D.State.
    { status = Queued
    ; attempt = 0
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; retry_failure = None
    }
;;

let developer_job ?id owner state =
  let id = Option.value id ~default:(P.Id.Job.create ()) in
  let launch =
    D.Launch.create ~parent:D.Launch.Run_root ~parent_job:None ~nested_depth:0
    |> protocol_ok
  in
  D.create
    ~owner
    ~capture_fingerprint:fingerprint
    ~id
    ~launch
    ~kind:P.Job.Async_tool
    ~payload:`Null
    ~retry_policy:(P.Job.Safe_retry { max_attempts = 3; backoff_ms = 0 })
    ~created_at:timestamp
    ~state
  |> protocol_ok
;;

let host job = H.of_developer job |> protocol_ok

let key owner =
  C.Key.developer
    ~owner
    ~project_source:source_id
    ~workspace_conflict_domain:"capacity-test"
    ~kind:P.Job.Async_tool
    ~nested_depth:0
  |> protocol_ok
;;

let reserve capacity key job =
  C.reserve_host_job capacity key ~job:(host job) |> protocol_ok |> Option.value_exn
;;

let claim capacity key job =
  C.try_acquire_host_job capacity key ~job:(host job) |> protocol_ok
;;

let available capacity key =
  match C.try_acquire capacity key |> protocol_ok with
  | None -> false
  | Some lease ->
    C.release lease;
    true
;;

let%test_unit "developer state preserves current-attempt retry convention" =
  let owner = owner () in
  let job = developer_job owner queued in
  let retry =
    D.State.
      { queued with
        attempt = 1
      ; next_run_at = Some timestamp
      ; retry_failure = Some (P.Error.invalid_request "previous attempt failed")
      }
  in
  assert (Result.is_ok (D.with_state job retry));
  assert (Result.is_error (D.with_state job { retry with started_at = Some timestamp }));
  assert (Result.is_error (D.with_state job { retry with next_run_at = None }));
  assert (Result.is_error (D.with_state job { retry with retry_failure = None }));
  assert (
    Result.is_error
      (D.with_state job { queued with status = Succeeded; completed_at = Some timestamp }));
  assert (
    Result.is_ok
      (D.with_state job { queued with status = Cancelled; completed_at = Some timestamp }));
  assert (
    Result.is_error
      (D.with_state
         job
         { queued with
           status = Cancelled
         ; started_at = Some timestamp
         ; completed_at = Some timestamp
         }));
  let bad_permission = P.Id.Permission.t_of_sexp (Sexp.Atom "invalid") in
  let active = { queued with attempt = 1; started_at = Some timestamp } in
  assert (
    Result.is_error
      (D.with_state job { active with status = Waiting_permission bad_permission }));
  let bad_subscription = P.Id.Subscription.t_of_sexp (Sexp.Atom "invalid") in
  let dependency =
    P.Job.
      { invocation_id = P.Id.Invocation.create ()
      ; work = P.Invocation.Subscription bad_subscription
      ; deadline = timestamp
      ; completion_schema = None
      ; max_output_bytes = 0
      ; max_output_depth = 0
      }
  in
  assert (
    Result.is_error
      (D.with_state job { active with status = Waiting_completion dependency }));
  ()
;;

let%test_unit "owner-qualified reservation and stable run capacity" =
  Eio_main.run (fun _ ->
    let capacity = C.create ~limits:(limits ~daemon_total:2 ()) in
    let run_id = P.Id.Developer_run.create () in
    let first = owner ~run_id () in
    let second = owner () in
    let same_id = P.Id.Job.create () in
    let first_job = developer_job ~id:same_id first queued in
    let second_job = developer_job ~id:same_id second queued in
    let first_key = key first
    and second_key = key second in
    let first_reservation = reserve capacity first_key first_job in
    let second_reservation = reserve capacity second_key second_job in
    C.publish first_reservation;
    C.publish second_reservation;
    let first_lease = claim capacity first_key first_job |> Option.value_exn in
    let second_lease = claim capacity second_key second_job |> Option.value_exn in
    let next_epoch = owner ~run_id ~epoch:1 () in
    C.retire_previous_epochs capacity ~current_owner:next_epoch;
    assert (not (available capacity (key next_epoch)));
    C.release second_lease;
    assert (not (available capacity (key next_epoch)));
    C.release first_lease;
    assert (available capacity (key next_epoch)))
;;

let%test_unit "queued retry waits for its claimed previous attempt" =
  Eio_main.run (fun _ ->
    let owner = owner () in
    let key = key owner in
    let capacity = C.create ~limits:(limits ()) in
    let job = developer_job owner queued in
    let reservation = reserve capacity key job in
    C.publish reservation;
    let old_lease = claim capacity key job |> Option.value_exn in
    let retry =
      D.with_state
        job
        { queued with
          attempt = 1
        ; next_run_at = Some timestamp
        ; retry_failure = Some (P.Error.invalid_request "retry")
        }
      |> protocol_ok
    in
    assert (Option.is_none (claim capacity key retry));
    C.release old_lease;
    let new_lease = claim capacity key retry |> Option.value_exn in
    C.release new_lease)
;;

let%test_unit "capacity transitions agree with a four-state charge model" =
  let trials = ref 0 in
  Quickcheck.test
    (Quickcheck.Generator.list (Int.gen_incl 0 3))
    ~seed:(`Deterministic "developer-capacity-lifecycle")
    ~trials:120
    ~f:(fun actions ->
      Eio_main.run (fun _ ->
        let capacity = C.create ~limits:(limits ()) in
        let owner = owner () in
        let key = key owner in
        let job = developer_job owner queued in
        let reservation = reserve capacity key job in
        let terminal =
          D.with_state
            job
            { queued with status = Cancelled; completed_at = Some timestamp }
          |> protocol_ok
        in
        let phase = ref `Staged in
        let lease = ref None in
        List.take actions 24
        |> List.iter ~f:(fun action ->
          (match action with
           | 0 ->
             C.publish reservation;
             (match !phase with
              | `Staged -> phase := `Ready
              | `Ready | `Claimed | `Free -> ())
           | 1 ->
             C.abort reservation;
             (match !phase with
              | `Staged -> phase := `Free
              | `Ready | `Claimed | `Free -> ())
           | 2 ->
             let acquired = claim capacity key job in
             (match !phase, acquired with
              | (`Ready | `Free), Some current ->
                phase := `Claimed;
                lease := Some current
              | (`Staged | `Claimed), None -> ()
              | _ -> failwith "capacity claim disagrees with reference phase")
           | _ ->
             C.retire_host_job capacity (host terminal);
             (match !phase with
              | `Staged | `Ready -> phase := `Free
              | `Claimed | `Free -> ()));
          let charged =
            match !phase with
            | `Free -> false
            | `Staged | `Ready | `Claimed -> true
          in
          assert (Bool.equal (available capacity key) (not charged)));
        (match !phase with
         | `Staged -> C.abort reservation
         | `Ready -> C.retire_host_job capacity (host terminal)
         | `Claimed -> Option.iter !lease ~f:C.release
         | `Free -> ());
        assert (available capacity key);
        Int.incr trials));
  [%test_eq: int] 120 !trials
;;

let%expect_test "conversation and developer jobs share only the intended capacity buckets"
  =
  Eio_main.run (fun _ ->
    let owner = owner () in
    let developer = developer_job owner queued |> host in
    let conversation_job = Background_scheduler_tests.new_job `Null in
    let conversation = H.of_conversation conversation_job |> protocol_ok in
    let original = H.to_conversation conversation |> Option.value_exn in
    [%test_eq: Sexp.t] (P.Job.sexp_of_t conversation_job) (P.Job.sexp_of_t original);
    let developer_key = key owner in
    let conversation_key =
      C.Key.create
        ~principal_id:(Some principal_id)
        ~prompt:"capacity-test"
        ~workspace_conflict_domain:"capacity-test"
        ~session_id
        ~kind:P.Job.Async_tool
        ~nested_depth:0
    in
    let base = limits ~daemon_total:2 ~per_session:2 () in
    let cases =
      Agent_server.Config.Server.
        [ "global", { base with daemon_total = 1 }, false
        ; "principal", { base with per_principal = 1 }, false
        ; "workspace", { base with per_workspace = 1 }, false
        ; "kind", { base with per_kind = 1 }, false
        ; "workload", { base with per_prompt = 1 }, true
        ; "owner", { base with per_session = 1 }, true
        ]
    in
    let trial limits first_key first_job second_key second_job =
      let capacity = C.create ~limits in
      let first =
        C.reserve_host_job capacity first_key ~job:first_job
        |> protocol_ok
        |> Option.value_exn
      in
      let accepted =
        match C.reserve_host_job capacity second_key ~job:second_job |> protocol_ok with
        | None -> false
        | Some reservation ->
          C.abort reservation;
          true
      in
      C.abort first;
      if not accepted
      then (
        let after_release =
          C.reserve_host_job capacity second_key ~job:second_job
          |> protocol_ok
          |> Option.value_exn
        in
        C.abort after_release);
      accepted
    in
    List.iter cases ~f:(fun (label, limits, expected) ->
      let conversation_first =
        trial limits conversation_key conversation developer_key developer
      in
      let developer_first =
        trial limits developer_key developer conversation_key conversation
      in
      [%test_eq: bool] expected conversation_first;
      [%test_eq: bool] expected developer_first;
      printf
        "%s: conversation-first=%b developer-first=%b\n"
        label
        conversation_first
        developer_first));
  [%expect
    {|
    global: conversation-first=false developer-first=false
    principal: conversation-first=false developer-first=false
    workspace: conversation-first=false developer-first=false
    kind: conversation-first=false developer-first=false
    workload: conversation-first=true developer-first=true
    owner: conversation-first=true developer-first=true |}]
;;
