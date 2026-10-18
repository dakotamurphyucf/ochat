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

let store_limits
      ?(max_runs = 3)
      ?(max_job_payload_bytes = 1024)
      ?(max_jobs_per_run = 4)
      ?(max_invocations_per_run = 5)
      ?(max_document_bytes = 8192)
      ?(max_inference_bytes_per_run = 8192)
      ?(max_work_steps = 200)
      ?max_command_terminal_bytes
      ?max_document_fields
      ?max_document_nodes
      ()
  =
  let document =
    Document_schema.Limits.create
      ~max_bytes:max_document_bytes
      ~max_depth:16
      ~max_fields:
        (Option.value
           max_document_fields
           ~default:(if max_document_bytes <= 8192 then 256 else 1024))
      ~max_nodes:
        (Option.value
           max_document_nodes
           ~default:(if max_document_bytes <= 8192 then 512 else 2048))
    |> ok
  in
  let command_default = Agent_store.Developer_command_limits.default ~document in
  let commands =
    match max_command_terminal_bytes with
    | None -> command_default
    | Some max_terminal_bytes ->
      Agent_store.Developer_command_limits.create
        ~max_commands:(Agent_store.Developer_command_limits.max_commands command_default)
        ~max_bytes:(Agent_store.Developer_command_limits.max_bytes command_default)
        ~max_terminal_bytes
      |> or_error
  in
  R.Limits.create
    ~max_runs
    ~max_epochs_per_run:3
    ~max_jobs_per_run
    ~max_invocations_per_run
    ~max_inference_attempts_per_run:256
    ~commands
    ~max_inference_bytes_per_run
    ~max_selected_per_commit:2
    ~max_job_payload_bytes
    ~max_document_bytes
    ~max_recovery_entries:16
    ~max_recovery_bytes:(Int.max 16384 max_document_bytes)
    ~max_work_steps
    ~document
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

let with_store
      ?(checkpoint = ignore)
      ?(wrap_env = Fn.id)
      ?(on_store = fun _ _ _ -> ())
      ?(max_job_payload_bytes = 1024)
      ?(max_invocations_per_run = 5)
      ?max_command_terminal_bytes
      f
  =
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
            R.open_or_create
              session
              ~sw
              ~limits:
                (store_limits
                   ~max_job_payload_bytes
                   ~max_invocations_per_run
                   ?max_command_terminal_bytes
                   ())
              ~checkpoint
            |> ok
          in
          on_store env session store;
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
    let checks_after_callback = ref None in
    let actor =
      Operator_authorization.guarded ~principal ~is_current:(fun () ->
        !actor_current
        &&
        match !checks_after_callback with
        | None -> true
        | Some 0 -> false
        | Some remaining ->
          checks_after_callback := Some (remaining - 1);
          true)
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
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
        ~max_active_inference_attempts:8
        ~max_admitted_call_bindings:32
      |> or_error
    in
    let capacity = capacity () in
    let owner_registry = O.Registry.create ~sw ~store ~capacity ~now ~limits in
    let admit () =
      O.Registry.admit
        owner_registry
        ~actor
        ~capture:captured
        ~registry
        ~selected:registry
        ~job_policy:Chat_response.One_off_request.default_policy
        ~selected_model:None
        ~handle
        ~workspace_conflict_domain:"root-owner-test"
    in
    let run = admit () |> ok in
    assert (Result.is_error (admit ()));
    assert (
      Result.is_error
        (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
           O.reconcile_pending run)));
    assert (
      Result.is_error
        (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
           O.current_after_mutations run)));
    let other_owner = owner "dvr_other_root_owner" 0 in
    let other_handle, _ =
      R.admit_epoch
        store
        ~owner:other_owner
        ~capture_fingerprint:(C.fingerprint captured)
        ~previous:None
      |> ok
    in
    let other_actor = Operator_authorization.trusted_local principal in
    let full_checks = ref 0 in
    let final_checks = ref 0 in
    let final_admission ~actor:admitted_actor ~capture ~checkpoint =
      assert (phys_equal admitted_actor other_actor && phys_equal capture captured);
      checkpoint ();
      incr full_checks;
      Ok
        (fun () ->
          incr final_checks;
          Error (P.Error.invalid_request "current developer source revoked"))
    in
    assert (
      Result.is_error
        (O.Registry.admit
           ~final_admission
           ~checkpoint:ignore
           owner_registry
           ~actor:other_actor
           ~capture:captured
           ~registry
           ~selected:registry
           ~job_policy:Chat_response.One_off_request.default_policy
           ~selected_model:None
           ~handle:other_handle
           ~workspace_conflict_domain:"root-owner-test"));
    [%test_eq: int] !full_checks 1;
    [%test_eq: int] !final_checks 1;
    [%test_eq: int] (List.length (O.Registry.active_for_scheduler owner_registry |> ok)) 1;
    let other_run =
      O.Registry.admit
        owner_registry
        ~actor:other_actor
        ~capture:captured
        ~registry
        ~selected:registry
        ~job_policy:Chat_response.One_off_request.default_policy
        ~selected_model:None
        ~handle:other_handle
        ~workspace_conflict_domain:"root-owner-test"
      |> ok
    in
    let witness = O.current run |> ok in
    O.narrow_selection run ~names:[] |> ok;
    assert (Result.is_error (O.check_effect_now run witness));
    let empty_before = O.current_after_mutations run |> ok in
    let unchanged = O.current_after_mutations run |> ok in
    assert (O.Current.same_selection empty_before unchanged);
    O.narrow_selection run ~names:[] |> ok;
    let empty_after = O.current_after_mutations run |> ok in
    assert (phys_equal (O.Current.selected empty_before) (O.Current.selected empty_after));
    assert (
      String.equal
        (Chat_response.Tool_capability.fingerprint (O.Current.selected empty_before))
        (Chat_response.Tool_capability.fingerprint (O.Current.selected empty_after)));
    assert (not (O.Current.same_selection empty_before empty_after));
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
    let issuer =
      let digest value = Chatml_source.Digest.of_contents value ~checkpoint:ignore in
      let artifact =
        P.Developer_agent_artifact.create
          ~revision_id:(P.Id.Prompt_revision.create ())
          ~manifest_sha256:(digest "copied selected artifact")
          ~root_relative_path:
            (Source.Portable_path.create "children/copied.chatmd" |> or_error)
      in
      Chat_response.Background_request.Issuer_source.create
        ~artifact
        ~prompt:(P.Id.Prompt_definition.create ())
        ~workspace:(P.Id.Workspace_definition.create ())
        ~tool_contract:Chatml_compilation.Tool_contract.Standard
        ~source_policy:(digest "copied source policy")
        ~runtime_policy:(digest "copied runtime policy")
    in
    let preattached =
      Chat_response.Background_request.with_issuer_source request issuer |> ok
    in
    let checked = ref 0 in
    let staged_ids = ref None in
    let value =
      O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
        incr checked;
        assert (Result.is_error (O.Registry.close_and_wait owner_registry));
        assert (Result.is_error (O.reconcile_pending run));
        (match
           O.Action.stage_background action ~request:preattached ~checkpoint:ignore
         with
         | Error error when P.Error.equal_code error.code P.Error.Invalid_request -> ()
         | Error _ | Ok _ -> failwith "root accepted a caller-preattached queued issuer");
        let chosen = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
        let unselected =
          O.Action.stage_background action ~request ~checkpoint:ignore |> ok
        in
        assert (not (P.Id.Job.equal chosen unselected));
        O.Action.rollback_staged action unselected |> ok;
        O.Action.rollback_staged action unselected |> ok;
        assert (
          Result.is_error (O.Action.stage_background action ~request ~checkpoint:ignore));
        staged_ids := Some (chosen, unselected);
        assert (
          Result.is_error
            (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
               O.Action.seal action ~selected:[ chosen ])));
        let selection = O.Action.seal action ~selected:[ chosen ] |> ok in
        assert (Result.is_error (O.Action.rollback_staged action chosen));
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
    [%test_eq: int]
      (List.length (O.Job_candidate.read_current run ~checkpoint:ignore |> ok))
      1;
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
    actor_current := true;
    let absent_job = ref None in
    let callback_count = ref 0 in
    assert (
      Result.is_error
        (O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           incr callback_count;
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           absent_job := Some id;
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           checks_after_callback := Some 1;
           Ok ((), selection))));
    assert (Int.equal !callback_count 1);
    let absent_job = Option.value_exn !absent_job in
    assert (Option.is_none (R.find_job (R.observe handle |> ok) absent_job));
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
       failwith "absent selected root intent released staged capacity");
    checks_after_callback := None;
    O.reconcile_pending run |> ok;
    assert (Option.is_some (R.find_job (R.observe handle |> ok) absent_job));
    assert (Int.equal !callback_count 1);
    let before_next = R.observe handle |> ok in
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
        ~previous:(Some before_next)
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
        ~selected_model:None
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
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
        ~max_active_inference_attempts:8
        ~max_admitted_call_bindings:32
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
            ~selected_model:None
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
        ~selected_model:None
        ~handle
        ~workspace_conflict_domain:"root-owner-test"
      |> ok
    in
    O.close_and_wait replacement |> ok;
    O.Registry.close_and_wait second |> ok)
;;

let%test_unit "physical root job control preserves exact inactive attempts" =
  with_store (fun env sw store ->
    let captured = capture () in
    let run_owner = owner "dvr_root_job_control" 0 in
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
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
        ~max_active_inference_attempts:8
        ~max_admitted_call_bindings:32
      |> or_error
    in
    let registry = O.Registry.create ~sw ~store ~capacity:(capacity ()) ~now ~limits in
    let run =
      O.Registry.admit
        registry
        ~actor:(Operator_authorization.trusted_local principal)
        ~capture:captured
        ~registry:selected
        ~selected
        ~job_policy:Chat_response.One_off_request.default_policy
        ~selected_model:None
        ~handle
        ~workspace_conflict_domain:"root-owner-test"
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
    let stage () =
      O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
        let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
        let selection = O.Action.seal action ~selected:[ id ] |> ok in
        Ok (id, selection))
      |> function
      | Ok id -> id
      | Error error -> raise_s [%sexp "stage failed", (error : P.Error.t)]
    in
    let id = stage () in
    let initial_candidates = O.Job_candidate.read_current run ~checkpoint:ignore |> ok in
    assert (Int.equal (List.length initial_candidates) 1);
    assert (
      P.Id.Job.equal
        (P.Developer_job.id (O.Job_candidate.job (List.hd_exn initial_candidates)))
        id);
    let physical = O.Job_candidate.host_job (List.hd_exn initial_candidates) in
    let conversation_job : P.Job.t =
      { id
      ; session_id = P.Id.Session.create ()
      ; generation = 0
      ; kind = Async_tool
      ; launch = None
      ; progress = None
      ; payload = `Object []
      ; status = Queued
      ; retry_policy = Never
      ; attempt = 0
      ; created_at = now ()
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Pending
      }
    in
    let conversation = P.Host_job.of_conversation conversation_job |> ok in
    assert (P.Id.Job.equal (P.Host_job.id physical) (P.Host_job.id conversation));
    assert (
      not
        (P.Host_job.Identity.equal
           (P.Host_job.identity physical)
           (P.Host_job.identity conversation)));
    let groups =
      [ P.Host_job.owner physical, [ physical ]
      ; P.Host_job.owner conversation, [ conversation ]
      ]
    in
    let idle_owner =
      P.Execution_owner.conversation ~session_id:(P.Id.Session.create ()) ~generation:0
      |> ok
    in
    let other_job = { conversation_job with session_id = P.Id.Session.create () } in
    let other_owner = P.Host_job.of_conversation other_job |> ok |> P.Host_job.owner in
    let queued_rows =
      [ P.Host_job.owner conversation, [ conversation_job ]
      ; idle_owner, []
      ; other_owner, [ other_job ]
      ]
    in
    let ready_first, ready_after =
      Agent_server.Job_scheduler.For_testing.fair_conversation_order
        ~after:None
        queued_rows
    in
    let ready_second, ready_after =
      Agent_server.Job_scheduler.For_testing.fair_conversation_order
        ~after:ready_after
        queued_rows
    in
    let ready_third, _ =
      Agent_server.Job_scheduler.For_testing.fair_conversation_order
        ~after:ready_after
        queued_rows
    in
    assert (Int.equal (List.length ready_first) 2);
    assert (Int.equal (List.length ready_second) 2);
    assert (List.equal P.Execution_owner.equal ready_first ready_third);
    assert (
      not (P.Execution_owner.equal (List.hd_exn ready_first) (List.hd_exn ready_second)));
    let groups = (idle_owner, []) :: groups in
    let first, after =
      Agent_server.Job_scheduler.For_testing.fair_order ~after:None groups
    in
    let second, after_second =
      Agent_server.Job_scheduler.For_testing.fair_order ~after groups
    in
    let third, _ =
      Agent_server.Job_scheduler.For_testing.fair_order ~after:after_second groups
    in
    assert (Int.equal (List.length first) 2);
    assert (Int.equal (List.length second) 2);
    assert (
      List.equal P.Execution_owner.equal (List.map first ~f:fst) (List.map third ~f:fst));
    assert (
      not (P.Execution_owner.equal (fst (List.hd_exn first)) (fst (List.hd_exn second))));
    let surviving = List.tl_exn first in
    let after_removal, _ =
      Agent_server.Job_scheduler.For_testing.fair_order ~after surviving
    in
    assert (
      P.Execution_owner.equal
        (fst (List.hd_exn surviving))
        (fst (List.hd_exn after_removal)));
    let snapshot = O.Job_control.find_current run id |> ok |> Option.value_exn in
    assert (
      match O.Job_control.cancel_current run snapshot |> ok with
      | O.Job_control.Durable_cancelled -> true
      | Already_terminal | Claimed_cancel_requested -> false);
    assert (
      match O.Job_control.cancel_current run snapshot |> ok with
      | O.Job_control.Already_terminal -> true
      | Durable_cancelled | Claimed_cancel_requested -> false);
    let cancelled = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
    assert (
      match (P.Developer_job.state cancelled).status with
      | P.Job.Cancelled -> true
      | _ -> false);
    let claimed_id = stage () in
    let queued_snapshot =
      O.Job_control.find_current run claimed_id |> ok |> Option.value_exn
    in
    let candidate =
      match O.Job_candidate.read_current run ~checkpoint:ignore |> ok with
      | [ candidate ] -> candidate
      | _ -> failwith "scheduler did not project exactly the queued physical job"
    in
    let started, started_u = Eio.Promise.create () in
    let never, _never_u = Eio.Promise.create () in
    let finished, finished_u = Eio.Promise.create () in
    let effects = ref 0 in
    Eio.Fiber.fork ~sw (fun () ->
      let outcome =
        match
          O.Job_candidate.with_claimed run candidate (fun ~active:_ ~sw:_ ->
            incr effects;
            assert (
              List.is_empty (O.Job_candidate.read_current run ~checkpoint:ignore |> ok));
            Eio.Promise.resolve started_u ();
            Eio.Promise.await never;
            failwith "cancelled job resumed its callback")
        with
        | exception Eio.Cancel.Cancelled _ -> `Cancelled
        | Ok _ -> `Returned
        | Error _ -> `Rejected
      in
      Eio.Promise.resolve finished_u outcome);
    Eio.Promise.await started;
    assert (Result.is_error (O.Job_control.cancel_current run queued_snapshot));
    let claimed_snapshot =
      O.Job_control.find_current run claimed_id |> ok |> Option.value_exn
    in
    assert (
      match O.Job_control.cancel_current run claimed_snapshot |> ok with
      | O.Job_control.Claimed_cancel_requested -> true
      | Already_terminal | Durable_cancelled -> false);
    assert (Int.equal !effects 1);
    assert (
      match Eio.Promise.await finished with
      | `Cancelled -> true
      | `Returned | `Rejected -> false);
    assert (
      match
        (P.Developer_job.state
           (R.find_job (R.observe handle |> ok) claimed_id |> Option.value_exn))
          .status
      with
      | P.Job.Cancelled -> true
      | _ -> false);
    O.close_and_wait run |> ok;
    O.Registry.close_and_wait registry |> ok)
;;

let%test_unit "uncertain job cancellation retains owner and capacity until recovery" =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  with_store
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun env sw store ->
       let captured = capture () in
       let run_owner = owner "dvr_cancel_unknown" 0 in
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
       let limits =
         O.Limits.create
           ~max_active_runs:1
           ~max_staged_jobs:1
           ~max_native_occupancy:1
           ~max_active_workers:1
           ~max_quarantined_workers:2
           ~max_active_inference_attempts:8
           ~max_admitted_call_bindings:32
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
           ~selected_model:None
           ~handle
           ~workspace_conflict_domain:"root-owner-test"
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
       let id =
         O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selection))
         |> ok
       in
       let snapshot = O.Job_control.find_current run id |> ok |> Option.value_exn in
       rename_armed := true;
       assert (Result.is_error (O.Job_control.cancel_current run snapshot));
       assert !sync_blocked;
       assert (
         Result.is_error
           (O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
              let selection = O.Action.seal action ~selected:[] |> ok in
              Ok ((), selection))));
       assert (Result.is_error (O.close_and_wait run));
       let project_source =
         match Chatml_prepared_program.provenance (C.prepared captured) with
         | Project { declaration; lock = _ } -> declaration
         | Implicit -> Chatml_prepared_program.entry (C.prepared captured)
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
          failwith "uncertain cancellation released selected capacity");
       sync_blocked := false;
       O.reconcile_pending run |> ok;
       let cancelled = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
       assert (
         match (P.Developer_job.state cancelled).status with
         | P.Job.Cancelled -> true
         | _ -> false);
       O.close_and_wait run |> ok;
       let released =
         Agent_server.Job_capacity.try_acquire capacity key |> ok |> Option.value_exn
       in
       Agent_server.Job_capacity.release released;
       O.Registry.close_and_wait registry |> ok)
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
           ~max_native_occupancy:2
           ~max_active_workers:1
           ~max_quarantined_workers:2
           ~max_active_inference_attempts:8
           ~max_admitted_call_bindings:32
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
           ~selected_model:None
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
       assert (Result.is_error (O.current_after_mutations run));
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

let%test_unit "nested native terminal settles before its live root resumes" =
  let pause_next_read = ref false in
  let fail_next_read = ref false in
  let fail_before_native_rename = ref false in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let read_failure = Failure "transient native cleanup observation failed" in
  let read_reached = ref None in
  let read_release = ref None in
  let checkpoint () =
    if !fail_next_read
    then (
      fail_next_read := false;
      raise read_failure)
    else if !pause_next_read
    then (
      pause_next_read := false;
      Eio.Promise.resolve (Option.value_exn !read_reached) ();
      Eio.Promise.await (Option.value_exn !read_release))
  in
  with_store
    ~checkpoint
    ~max_invocations_per_run:7
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~clear_sync_block_on_failure:true
        ~before_rename:(fun () ->
          if !fail_before_native_rename
          then
            raise
              (Core_unix.Unix_error (EIO, "injected pre-rename failure", "native terminal")))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun env sw store ->
       let captured = capture () in
       let run_owner = owner "dvr_nested_native" 0 in
       let handle, initial =
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
       let selected =
         Chat_response.Tool_capability.create
           ~owner:"root-owner-test"
           ~resource_fingerprint:Digestif.SHA256.(digest_string "fixture" |> to_hex)
           []
         |> ok
       in
       let prepared_script =
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
           ~prepared:prepared_script
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let limits =
         O.Limits.create
           ~max_active_runs:1
           ~max_staged_jobs:2
           ~max_native_occupancy:2
           ~max_active_workers:1
           ~max_quarantined_workers:2
           ~max_active_inference_attempts:8
           ~max_admitted_call_bindings:32
         |> or_error
       in
       let owner_registry =
         O.Registry.create ~sw ~store ~capacity:(capacity ()) ~now ~limits
       in
       let run =
         O.Registry.admit
           owner_registry
           ~actor
           ~capture:captured
           ~registry:selected
           ~selected
           ~job_policy:Chat_response.One_off_request.default_policy
           ~selected_model:None
           ~handle
           ~workspace_conflict_domain:"root-owner-test"
         |> ok
       in
       let admitted name =
         let id = P.Id.Invocation.of_string name |> ok in
         P.Host_invocation.create_developer
           { id
           ; owner = P.Execution_owner.Developer run_owner
           ; origin = P.Invocation.Script
           ; parent_invocation = None
           ; parent_job = None
           ; tool_name = "nested.test"
           ; implementation_revision = "v1"
           ; capability_fingerprint = "test-capability"
           ; input = `Null
           ; created_at = now ()
           ; deadline = None
           }
         |> ok
       in
       let admit_dispatch name =
         let invocation = admitted name in
         let id = (P.Host_invocation.context invocation).id in
         let dispatched = P.Host_invocation.dispatch invocation |> ok in
         let expected = R.observe handle |> ok in
         let lookup = R.find_invocation expected id in
         let dispatch = R.prepare_dispatch handle ~expected ~lookup ~dispatched |> ok in
         let installed =
           R.commit_and_settle
             handle
             dispatch
             (R.Handoff.create ~settle:(fun _ -> ()))
             ~admit:(fun () -> true)
           |> ok
         in
         (match installed with
          | R.Settlement.Installed _ -> ()
          | Absent | Prewrite_conflict | Diverged -> failwith "dispatch did not install");
         invocation, dispatched
       in
       ignore (initial : R.Observation.t);
       let invocation, dispatched = admit_dispatch "inv_nested_native" in
       let child = ref None in
       let callbacks = ref 0 in
       O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
         let old = O.current run |> ok in
         O.with_native_registration run ~invocation (fun ~scope ->
           assert (
             Result.is_error
               (O.with_native_registration run ~invocation (fun ~scope:_ -> Ok ())));
           let selection =
             O.with_native_callback scope ~dispatched (fun ~sw:_ ->
               incr callbacks;
               let id =
                 O.Native_terminal.stage_background
                   scope
                   ~request
                   ~checkpoint:ignore
                   ~check_parent_now:(fun () -> Ok ())
                 |> ok
               in
               child := Some id;
               O.Native_terminal.seal
                 scope
                 ~outcome:(P.Invocation.Complete `Null)
                 ~selected:[ id ])
             |> ok
           in
           let terminal =
             P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> ok
           in
           assert (
             Result.is_error (O.Native_terminal.submit scope ~terminal ~selected:None));
           let revision =
             O.Native_terminal.submit scope ~terminal ~selected:(Some selection) |> ok
           in
           assert (Int.equal revision 2);
           Ok ())
         |> ok;
         assert (Result.is_error (O.check_effect_now run old));
         ignore (O.current run |> ok : O.Current.t);
         let selection = O.Action.seal action ~selected:[] |> ok in
         Ok ((), selection))
       |> ok;
       assert (Int.equal !callbacks 1);
       let observed = R.observe handle |> ok in
       assert (Option.is_some (R.find_job observed (Option.value_exn !child)));
       let first, first_dispatched = admit_dispatch "inv_terminal_first" in
       let second, second_dispatched = admit_dispatch "inv_terminal_second" in
       let terminal invocation dispatched =
         let selection =
           O.with_native_callback invocation ~dispatched (fun ~sw:_ ->
             incr callbacks;
             O.Native_terminal.seal
               invocation
               ~outcome:(P.Invocation.Complete `Null)
               ~selected:[])
           |> ok
         in
         let resolved =
           P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null) |> ok
         in
         selection, resolved
       in
       O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw ->
         let second_ready, second_ready_u = Eio.Promise.create () in
         let second_go, second_go_u = Eio.Promise.create () in
         let second_done, second_done_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration run ~invocation:second (fun ~scope ->
               let selected, resolved = terminal scope second_dispatched in
               Eio.Promise.resolve second_ready_u ();
               Eio.Promise.await second_go;
               let submitted =
                 O.Native_terminal.submit
                   scope
                   ~terminal:resolved
                   ~selected:(Some selected)
               in
               Eio.Promise.resolve second_done_u submitted;
               Ok ())
           in
           assert (Result.is_ok result));
         Eio.Promise.await second_ready;
         let first_done, first_done_u = Eio.Promise.create () in
         let reached, reached_u = Eio.Promise.create () in
         let release, release_u = Eio.Promise.create () in
         read_reached := Some reached_u;
         read_release := Some release;
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration run ~invocation:first (fun ~scope ->
               let selected, resolved = terminal scope first_dispatched in
               pause_next_read := true;
               let submitted =
                 O.Native_terminal.submit
                   scope
                   ~terminal:resolved
                   ~selected:(Some selected)
               in
               Eio.Promise.resolve first_done_u submitted;
               Ok ())
           in
           assert (Result.is_ok result));
         Eio.Promise.await reached;
         Eio.Promise.resolve second_go_u ();
         (* The second completed callback joins the same physical drain while
            the first Store read is paused; it must await its own settlement. *)
         Eio.Fiber.yield ();
         Eio.Promise.resolve release_u ();
         ignore (Eio.Promise.await first_done |> ok : int);
         ignore (Eio.Promise.await second_done |> ok : int);
         let selection = O.Action.seal action ~selected:[] |> ok in
         Ok ((), selection))
       |> ok;
       assert (Int.equal !callbacks 3);
       let observed = R.observe handle |> ok in
       List.iter [ first; second ] ~f:(fun admitted ->
         let id = (P.Host_invocation.context admitted).id in
         let prior =
           R.find_invocation observed id |> R.Invocation_lookup.prior |> Option.value_exn
         in
         assert (
           P.Invocation.equal_status
             (P.Host_invocation.status (R.Invocation_observation.value prior))
             (Resolved (Complete `Null))));
       let completed, completed_dispatched =
         admit_dispatch "inv_before_dispatch_writer"
       in
       let writing = admitted "inv_transient_dispatch_writer" in
       let writing_dispatched = P.Host_invocation.dispatch writing |> ok in
       O.with_root_action run ~phase:R.Checkpoint.Active (fun ~action ~sw ->
         let terminal_ready, terminal_ready_u = Eio.Promise.create () in
         let terminal_go, terminal_go_u = Eio.Promise.create () in
         let terminal_done, terminal_done_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration run ~invocation:completed (fun ~scope ->
               let open Result.Let_syntax in
               let selected, resolved = terminal scope completed_dispatched in
               Eio.Promise.resolve terminal_ready_u ();
               Eio.Promise.await terminal_go;
               let%bind revision =
                 O.Native_terminal.submit
                   scope
                   ~terminal:resolved
                   ~selected:(Some selected)
               in
               Eio.Promise.resolve terminal_done_u revision;
               Ok ())
           in
           assert (Result.is_ok result));
         Eio.Promise.await terminal_ready;
         let writer_reached, writer_reached_u = Eio.Promise.create () in
         let writer_release, writer_release_u = Eio.Promise.create () in
         let writer_reconciled, writer_reconciled_u = Eio.Promise.create () in
         let writer_done, writer_done_u = Eio.Promise.create () in
         read_reached := Some writer_reached_u;
         read_release := Some writer_release;
         Eio.Fiber.fork ~sw (fun () ->
           let retained_scope = ref None in
           let failed =
             P.Invocation.Fail
               { code = "native.dispatch_store_error"
               ; message = "dispatch write failed after rename"
               ; retryable = false
               ; details = `Null
               }
           in
           let failed_terminal =
             P.Host_invocation.resolve writing_dispatched failed |> ok
           in
           let result =
             O.with_native_registration run ~invocation:writing (fun ~scope ->
               retained_scope := Some scope;
               pause_next_read := true;
               rename_armed := true;
               assert (
                 Result.is_error
                   (O.Native_terminal.save_dispatch
                      scope
                      ~expected_revision:None
                      ~dispatched:writing_dispatched));
               assert (not !rename_armed);
               assert (not !sync_blocked);
               let installed =
                 R.find_invocation
                   (R.observe handle |> ok)
                   (P.Host_invocation.context writing).id
                 |> R.Invocation_lookup.prior
                 |> Option.value_exn
               in
               assert (
                 P.Invocation.equal_status
                   (P.Host_invocation.status (R.Invocation_observation.value installed))
                   Dispatching);
               Eio.Promise.resolve writer_reconciled_u ();
               ignore (Eio.Promise.await terminal_done : int);
               O.Native_terminal.remember_failure scope ~terminal:failed_terminal |> ok;
               Ok ())
           in
           assert (Result.is_ok result);
           let retained_scope = Option.value_exn !retained_scope in
           ignore
             (O.Native_terminal.resolve_retained retained_scope ~terminal:failed_terminal
              |> ok
              : int);
           Eio.Promise.resolve writer_done_u ());
         Eio.Promise.await writer_reached;
         Eio.Promise.resolve terminal_go_u ();
         (* The already-completed callback waits for the sibling's transient
            dispatch writer; it cannot turn that temporary owner into Failed. *)
         Eio.Fiber.yield ();
         Eio.Promise.resolve writer_release_u ();
         Eio.Promise.await writer_reconciled;
         ignore (Eio.Promise.await terminal_done : int);
         Eio.Promise.await writer_done;
         let selection = O.Action.seal action ~selected:[] |> ok in
         Ok ((), selection))
       |> ok;
       let observed = R.observe handle |> ok in
       List.iter [ completed; writing ] ~f:(fun admitted ->
         let prior =
           R.find_invocation observed (P.Host_invocation.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
         in
         let actual = P.Host_invocation.status (R.Invocation_observation.value prior) in
         if
           P.Id.Invocation.equal
             (P.Host_invocation.context admitted).id
             (P.Host_invocation.context writing).id
         then
           assert (
             match actual with
             | Resolved (Fail _) -> true
             | _ -> false)
         else assert (P.Invocation.equal_status actual (Resolved (Complete `Null))));
       let sealed_invocation, sealed_dispatch = admit_dispatch "inv_retained_sealed" in
       let sealed_scope = ref None in
       let failure = Failure "interrupted after sealing" in
       (match
          O.with_native_registration run ~invocation:sealed_invocation (fun ~scope ->
            sealed_scope := Some scope;
            ignore
              (O.with_native_callback scope ~dispatched:sealed_dispatch (fun ~sw:_ ->
                 let id =
                   O.Native_terminal.stage_background
                     scope
                     ~request
                     ~checkpoint:ignore
                     ~check_parent_now:(fun () -> Ok ())
                   |> ok
                 in
                 child := Some id;
                 O.Native_terminal.seal
                   scope
                   ~outcome:(P.Invocation.Complete `Null)
                   ~selected:[ id ])
               |> ok
               : O.Native_terminal.Selection.t);
            raise failure)
        with
        | exception actual -> assert (phys_equal actual failure)
        | Ok _ | Error _ -> failwith "native exception identity was lost");
       let sealed_scope = Option.value_exn !sealed_scope in
       let cancelled =
         P.Host_invocation.cancel sealed_dispatch ~reason:"interrupted" |> ok
       in
       assert (
         Result.is_error
           (O.Native_terminal.resolve_retained sealed_scope ~terminal:cancelled));
       let complete =
         P.Host_invocation.resolve sealed_dispatch (P.Invocation.Complete `Null) |> ok
       in
       actor_current := false;
       assert (
         Result.is_error
           (O.Native_terminal.resolve_retained sealed_scope ~terminal:complete));
       assert (
         Option.is_none (R.find_job (R.observe handle |> ok) (Option.value_exn !child)));
       actor_current := true;
       O.reconcile_pending run |> ok;
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (Option.value_exn !child)));
       ignore (O.current run |> ok : O.Current.t);
       let entered_without_scope = ref false in
       fail_next_read := true;
       (match
          O.with_native_registration
            run
            ~invocation:(admitted "inv_no_scope")
            (fun ~scope:_ ->
               entered_without_scope := true;
               Ok ())
        with
        | exception exn when phys_equal exn read_failure -> ()
        | Ok () | Error _ -> failwith "initial native read failure was lost");
       assert (not !entered_without_scope);
       O.with_native_registration
         run
         ~invocation:(admitted "inv_no_scope")
         (fun ~scope:_ -> Ok ())
       |> ok;
       ignore (O.current run |> ok : O.Current.t);
       let absent_scope = ref None in
       assert (
         Result.is_error
           (O.with_native_registration
              run
              ~invocation:(admitted "inv_retained_absent")
              (fun ~scope ->
                 absent_scope := Some scope;
                 fail_next_read := true;
                 Error (P.Error.invalid_request "abort before dispatch"))));
       assert (Result.is_error (O.current run));
       O.Native_terminal.abandon_retained_without_dispatch
         (Option.value_exn !absent_scope)
       |> ok;
       ignore (O.current run |> ok : O.Current.t);
       let unsealed_invocation, unsealed_dispatch =
         admit_dispatch "inv_retained_unsealed"
       in
       let unsealed_scope = ref None in
       let reason = Failure "cancel before native callback" in
       (match
          Eio.Cancel.sub (fun cancel ->
            O.with_native_registration run ~invocation:unsealed_invocation (fun ~scope ->
              unsealed_scope := Some scope;
              Eio.Cancel.cancel cancel reason;
              Eio.Fiber.check ();
              Ok ()))
        with
        | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
        | Ok () | Error _ -> failwith "native cancellation identity was lost");
       let unsealed_scope = Option.value_exn !unsealed_scope in
       assert (Result.is_error (O.current run));
       assert (
         Result.is_error
           (O.Native_terminal.abandon_retained_without_dispatch unsealed_scope));
       assert (Result.is_error (O.close_and_wait run));
       let cancelled =
         P.Host_invocation.cancel unsealed_dispatch ~reason:"interrupted" |> ok
       in
       actor_current := false;
       fail_before_native_rename := true;
       assert (
         Result.is_error
           (O.Native_terminal.resolve_retained unsealed_scope ~terminal:cancelled));
       let observed = R.observe handle |> ok in
       let prior =
         R.find_invocation observed (P.Host_invocation.context cancelled).id
         |> R.Invocation_lookup.prior
         |> Option.value_exn
       in
       assert (
         P.Invocation.equal_status
           (P.Host_invocation.status (R.Invocation_observation.value prior))
           Dispatching);
       assert (Result.is_error (O.close_and_wait run));
       fail_before_native_rename := false;
       O.reconcile_pending run |> ok;
       let observed = R.observe handle |> ok in
       let prior =
         R.find_invocation observed (P.Host_invocation.context cancelled).id
         |> R.Invocation_lookup.prior
         |> Option.value_exn
       in
       assert (
         P.Invocation.equal_status
           (P.Host_invocation.status (R.Invocation_observation.value prior))
           (P.Host_invocation.status cancelled));
       O.close_and_wait run |> ok;
       O.Registry.close_and_wait owner_registry |> ok)
;;
