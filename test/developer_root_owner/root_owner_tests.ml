open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module R = Agent_store.Developer_run_store
module O = Agent_server.Developer_root_owner
module C = Chatml_developer_capture
module B = Chatml_project_build
module F = Developer_run_store_test_suite.Developer_run_store_tests

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error _ -> failwith "fixture rejected"
;;

let or_error = Or_error.ok_exn
let host_id = P.Id.Server.of_string "srv_developer_root_owner" |> ok
let principal_id = P.Id.Principal.of_string "pri_developer_root_owner" |> ok

let principal =
  P.Principal.create
    ~id:principal_id
    ~authentication_kind:"local.test"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> ok
;;

let owner run_id epoch =
  let run_id = P.Id.Developer_run.of_string run_id |> ok in
  match P.Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id |> ok with
  | Developer owner -> owner
  | Conversation _ -> failwith "wrong owner kind"
;;

let capture () =
  let fixture = Fixtures.make ~library:true () in
  let entry = Support.capture ~namespace:"root" "src/developer.chatml" "let main = 0" in
  let build = Fixtures.compose fixture ~entry:(B.Entry.Captured entry) |> Support.ok in
  let graph = B.prepared build |> Chatml_prepared_program.fingerprint in
  let origin = C.Origin.local_saved ~graph ~source_revision:"saved" |> or_error in
  let json_bounds =
    C.Json_bounds.create ~max_bytes:4096 ~max_depth:32 ~max_nodes:256 |> or_error
  in
  C.create
    ~build
    ~origin
    ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
    ~entrypoint:"main"
    ~input:`Null
    ~fixtures:None
    ~json_bounds
    ~checkpoint:ignore
  |> ok
;;

let store_limits () =
  R.Limits.create
    ~max_runs:3
    ~max_epochs_per_run:3
    ~max_jobs_per_run:4
    ~max_invocations_per_run:4
    ~max_selected_per_commit:2
    ~max_job_payload_bytes:1024
    ~max_document_bytes:4096
    ~max_recovery_entries:16
    ~max_recovery_bytes:16384
    ~max_work_steps:200
    ~document:
      (Document_schema.Limits.create
         ~max_bytes:4096
         ~max_depth:16
         ~max_fields:256
         ~max_nodes:512
       |> ok)
  |> or_error
;;

let capacity ?(per_session = 2) () =
  let limits : Agent_server.Config.Server.job_limits =
    { daemon_total = 2
    ; per_principal = 2
    ; per_prompt = 2
    ; per_workspace = 2
    ; per_session
    ; per_kind = 2
    ; max_nested_depth = 2
    }
  in
  Agent_server.Job_capacity.create ~limits
;;

let with_store ?(checkpoint = ignore) ?(wrap_env = Fn.id) f =
  Eio_main.run (fun env ->
    let path =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        ("ochat-root-owner." ^ P.Id.Transaction.(create () |> to_string))
    in
    let directory = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 directory;
    Exn.protect
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let env = wrap_env env in
          let session =
            S.create
              ~env
              ~sw
              ~root:(Filename.concat path "data")
              ~server_id:host_id
              ~process_start_identity:None
              ~lock_nonce:"root-owner-test"
            |> ok
          in
          let store =
            R.open_or_create session ~sw ~limits:(store_limits ()) ~checkpoint |> ok
          in
          let result = f env sw store in
          R.close_and_wait store |> ok;
          S.close session |> ok;
          result))
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true directory))
;;

let now () = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> ok

let%test_unit
    "one physical root owner seals a checkpoint and excludes duplicate admission"
  =
  with_store (fun env sw store ->
    let captured = capture () in
    let run_owner = owner "dvr_root_owner" 0 in
    let handle, _ =
      R.admit_epoch
        store
        ~owner:run_owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:None
      |> ok
    in
    let actor_current = ref true in
    let actor =
      Operator_authorization.guarded ~principal ~is_current:(fun () -> !actor_current)
    in
    let registry =
      Chat_response.Tool_capability.create
        ~owner:"root-owner-test"
        ~resource_fingerprint:Digestif.SHA256.(digest_string "fixture" |> to_hex)
        []
      |> ok
    in
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
      |> or_error
    in
    let owner_registry =
      O.Registry.create ~sw ~store ~capacity:(capacity ()) ~now ~limits
    in
    let admit () =
      O.Registry.admit
        owner_registry
        ~actor
        ~capture:captured
        ~registry
        ~selected:registry
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle
        ~workspace_conflict_domain:"root-owner-test"
    in
    let run = admit () |> ok in
    assert (Result.is_error (admit ()));
    assert (
      Result.is_error
        (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
           O.reconcile_pending run)));
    let other_owner = owner "dvr_other_root_owner" 0 in
    let other_handle, _ =
      R.admit_epoch
        store
        ~owner:other_owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:None
      |> ok
    in
    let other_run =
      O.Registry.admit
        owner_registry
        ~actor:(Operator_authorization.trusted_local principal)
        ~capture:captured
        ~registry
        ~selected:registry
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle:other_handle
        ~workspace_conflict_domain:"root-owner-test"
      |> ok
    in
    let witness = O.current run |> ok in
    O.narrow_selection run ~names:[] |> ok;
    assert (Result.is_error (O.check_effect_now run witness));
    let prepared =
      Chat_response.One_off_script.prepare_in_domain
        ~env
        ~capabilities:registry
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
    let checked = ref 0 in
    let staged_ids = ref None in
    let value =
      O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
        incr checked;
        assert (Result.is_error (O.Registry.close_and_wait owner_registry));
        assert (Result.is_error (O.reconcile_pending run));
        let chosen = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
        let unselected =
          O.Action.stage_background action ~request ~checkpoint:ignore |> ok
        in
        assert (not (P.Id.Job.equal chosen unselected));
        staged_ids := Some (chosen, unselected);
        assert (
          Result.is_error
            (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
               O.Action.seal action ~selected:[ chosen ])));
        let selection = O.Action.seal action ~selected:[ chosen ] |> ok in
        Ok ("complete", selection))
      |> ok
    in
    assert (String.equal value "complete");
    assert (Int.equal !checked 1);
    let observed = R.observe handle |> ok in
    assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint observed)) 2L);
    let chosen, unselected = Option.value_exn !staged_ids in
    assert (Option.is_some (R.find_job observed chosen));
    assert (Option.is_none (R.find_job observed unselected));
    let effects = ref 0 in
    let cancelled_job = ref None in
    let reason = Failure "cancel root after one effect" in
    (match
       Eio.Cancel.sub (fun cancel ->
         ignore
           (O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
              incr effects;
              let id =
                O.Action.stage_background action ~request ~checkpoint:ignore |> ok
              in
              cancelled_job := Some id;
              let selection = O.Action.seal action ~selected:[ id ] |> ok in
              Eio.Cancel.cancel cancel reason;
              Eio.Fiber.check ();
              Ok ((), selection))
            : (unit, P.Error.t) result))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
     | () -> failwith "root cancellation identity was lost");
    assert (Int.equal !effects 1);
    let current = R.observe handle |> ok in
    assert (Option.is_none (R.find_job current (Option.value_exn !cancelled_job)));
    assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint current)) 2L);
    let revoked_job = ref None in
    (match
       O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
         let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
         revoked_job := Some id;
         let selection = O.Action.seal action ~selected:[ id ] |> ok in
         actor_current := false;
         Ok ((), selection))
     with
     | Error _ -> ()
     | Ok () -> failwith "revoked actor published a selected job");
    let current = R.observe handle |> ok in
    assert (Option.is_none (R.find_job current (Option.value_exn !revoked_job)));
    assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint current)) 2L);
    O.close_and_wait run |> ok;
    ignore
      (O.with_root_action other_run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
         let selection = O.Action.seal action ~selected:[] |> ok in
         Ok ((), selection))
       |> ok
       : unit);
    let next_owner = owner "dvr_root_owner" 1 in
    let next_handle, _ =
      R.admit_epoch
        store
        ~owner:next_owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:(Some current)
      |> ok
    in
    let next_run =
      O.Registry.admit
        owner_registry
        ~actor:(Operator_authorization.trusted_local principal)
        ~capture:captured
        ~registry
        ~selected:registry
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle:next_handle
        ~workspace_conflict_domain:"root-owner-test"
      |> ok
    in
    O.close_and_wait next_run |> ok;
    O.close_and_wait other_run |> ok;
    O.Registry.close_and_wait owner_registry |> ok)
;;

let%test_unit
    "registry joins an admission stalled inside the Store and releases its run claim"
  =
  let armed = ref false in
  let reached = ref None in
  let blocked = ref None in
  let checkpoint () =
    if !armed
    then (
      armed := false;
      let _, signal = Option.value_exn !reached in
      Eio.Promise.resolve signal ();
      let wait, _ = Option.value_exn !blocked in
      Eio.Promise.await wait)
  in
  with_store ~checkpoint (fun _env sw store ->
    let captured = capture () in
    let owner = owner "dvr_root_admission" 0 in
    let handle, _ =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:None
      |> ok
    in
    let actor = Operator_authorization.trusted_local principal in
    let selected =
      Chat_response.Tool_capability.create
        ~owner:"root-owner-test"
        ~resource_fingerprint:Digestif.SHA256.(digest_string "fixture" |> to_hex)
        []
      |> ok
    in
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
      |> or_error
    in
    let registry = O.Registry.create ~sw ~store ~capacity:(capacity ()) ~now ~limits in
    let reached_pair = Eio.Promise.create () in
    let blocked_pair = Eio.Promise.create () in
    reached := Some reached_pair;
    blocked := Some blocked_pair;
    let answer, answer_u = Eio.Promise.create () in
    armed := true;
    Eio.Fiber.fork ~sw (fun () ->
      let result =
        match
          O.Registry.admit
            registry
            ~actor
            ~capture:captured
            ~registry:selected
            ~selected
            ~job_policy:Chat_response.One_off_request.default_policy
            ~handle
            ~workspace_conflict_domain:"root-owner-test"
        with
        | Ok _ -> `Admitted
        | Error _ -> `Rejected
        | exception Eio.Cancel.Cancelled _ -> `Cancelled
      in
      Eio.Promise.resolve answer_u result);
    Eio.Promise.await (fst reached_pair);
    O.Registry.close_and_wait registry |> ok;
    (match Eio.Promise.await answer with
     | `Rejected | `Cancelled -> ()
     | `Admitted -> failwith "closed registry admitted a stalled owner");
    let second = O.Registry.create ~sw ~store ~capacity:(capacity ()) ~now ~limits in
    let replacement =
      O.Registry.admit
        second
        ~actor
        ~capture:captured
        ~registry:selected
        ~selected
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle
        ~workspace_conflict_domain:"root-owner-test"
      |> ok
    in
    O.close_and_wait replacement |> ok;
    O.Registry.close_and_wait second |> ok)
;;

let%test_unit "uncertain root handoff retains capacity through owner close" =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let reconcile_armed = ref false in
  let reconcile_gate = ref None in
  let checkpoint () =
    if !reconcile_armed
    then (
      reconcile_armed := false;
      let reached, release = Option.value_exn !reconcile_gate in
      Eio.Promise.resolve reached ();
      Eio.Promise.await release)
  in
  with_store
    ~checkpoint
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun env sw store ->
       let captured = capture () in
       let run_owner = owner "dvr_root_uncertain" 0 in
       let handle, _ =
         R.admit_epoch
           store
           ~owner:run_owner
           ~capture_fingerprint:(C.fingerprint captured)
           ~previous:None
         |> ok
       in
       let selected =
         Chat_response.Tool_capability.create
           ~owner:"root-owner-test"
           ~resource_fingerprint:Digestif.SHA256.(digest_string "fixture" |> to_hex)
           []
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
       let limits =
         O.Limits.create
           ~max_active_runs:1
           ~max_staged_jobs:1
           ~max_active_workers:1
           ~max_quarantined_workers:2
         |> or_error
       in
       let capacity = capacity ~per_session:1 () in
       let registry = O.Registry.create ~sw ~store ~capacity ~now ~limits in
       let run =
         O.Registry.admit
           registry
           ~actor:(Operator_authorization.trusted_local principal)
           ~capture:captured
           ~registry:selected
           ~selected
           ~job_policy:Chat_response.One_off_request.default_policy
           ~handle
           ~workspace_conflict_domain:"root-owner-test"
         |> ok
       in
       let selected_id = ref None in
       let effects = ref 0 in
       (match
          O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
            incr effects;
            let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
            selected_id := Some id;
            let selection = O.Action.seal action ~selected:[ id ] |> ok in
            rename_armed := true;
            Ok ((), selection))
        with
        | Error _ -> ()
        | Ok () -> failwith "uncertain rename was acknowledged");
       assert (Int.equal !effects 1);
       assert (not !rename_armed);
       sync_blocked := false;
       let reached, reached_u = Eio.Promise.create () in
       let release, release_u = Eio.Promise.create () in
       let result, result_u = Eio.Promise.create () in
       reconcile_gate := Some (reached_u, release);
       reconcile_armed := true;
       Eio.Fiber.fork ~sw (fun () ->
         let answer = O.reconcile_pending run in
         Eio.Promise.resolve result_u answer);
       Eio.Promise.await reached;
       (match O.reconcile_pending run with
        | Error error when P.Error.equal_code error.code P.Error.Conflict -> ()
        | Ok () | Error _ -> failwith "a second reconciler entered stale work");
       assert (
         Result.is_error
           (O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
              let selection = O.Action.seal action ~selected:[] |> ok in
              Ok ((), selection))));
       Eio.Promise.resolve release_u ();
       Eio.Promise.await result |> ok;
       ignore
         (O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
            let selection = O.Action.seal action ~selected:[] |> ok in
            Ok ((), selection))
          |> ok
          : unit);
       rename_armed := true;
       (match
          O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
            let selection = O.Action.seal action ~selected:[] |> ok in
            Ok ((), selection))
        with
        | Error _ -> ()
        | Ok () -> failwith "second uncertain rename was acknowledged");
       (match O.close_and_wait run with
        | Error _ -> ()
        | Ok () -> failwith "unknown selected handoff released the run claim");
       let project_source =
         let prepared = C.prepared captured in
         match Chatml_prepared_program.provenance prepared with
         | Project { declaration; lock = _ } -> declaration
         | Implicit -> Chatml_prepared_program.entry prepared
       in
       let key =
         Agent_server.Job_capacity.Key.developer
           ~owner:run_owner
           ~project_source
           ~workspace_conflict_domain:"root-owner-test"
           ~kind:P.Job.Async_tool
           ~nested_depth:0
         |> ok
       in
       (match Agent_server.Job_capacity.try_acquire capacity key with
        | Ok None -> ()
        | Ok (Some _) | Error _ ->
          failwith "owner close released an uncertain selected reservation");
       sync_blocked := false;
       O.reconcile_pending run |> ok;
       let current = R.observe handle |> ok in
       assert (Option.is_some (R.find_job current (Option.value_exn !selected_id)));
       O.close_and_wait run |> ok;
       let released =
         Agent_server.Job_capacity.try_acquire capacity key |> ok |> Option.value_exn
       in
       Agent_server.Job_capacity.release released;
       O.Registry.close_and_wait registry |> ok)
;;
