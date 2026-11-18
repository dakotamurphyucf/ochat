open! Core
open Fixtures
module A = Agent_session.Session_actor
module S = Agent_session.Session_state
module L = Agent_session.Inference_ledger
module O = Inference.Observation
module R = Inference_runtime
module C = Inference_client
module G = Agent_server.Graph_tracking

let observation_ok result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (O.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let with_tracking_actor ?(map_initial = Fn.id) f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let reject = ref false in
      let commits = ref 0 in
      let actor =
        A.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
             |> map_initial)
          ~persistence:
            { archive_reference
            ; commit =
                (fun ?observation:_ ~command_audit:_ ~previous:_ _ ->
                  if !reject
                  then
                    Error
                      (Agent_protocol.Error.create
                         Persistence_error
                         ~message:"injected tracking write failure"
                         ~retryable:false
                         ())
                  else (
                    incr commits;
                    Ok ()))
            }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "tracking-token")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; goal_descendant_profile
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~f:(fun () -> f sw actor reject commits)
        ~finally:(fun () -> A.shutdown actor)))
;;

let prepared ?(post_stream = fun ~sw:_ ~inputs:_ -> Seq.empty) () =
  let fixture =
    Inference_fixture.create ~namespace:"tracked" ~default_model:"fixture" ~post_stream
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default
    |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (R.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let context =
    Inference_fixture.resolve fixture target
    |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (R.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let request =
    Inference.Request.create
      ~target
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun e ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let prepared =
    R.Context.prepare context ~preparation_id:"prepared-tracking" request
    |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (R.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  context, request, prepared
;;

let source name = Transcript.Source_id.of_string name |> Result.ok_or_failwith

let upstream on_admitted =
  G.Upstream.
    { new_preparation_id = (fun () -> "prepared-tracking")
    ; on_admitted
    ; on_attempt = ignore
    ; on_observation = ignore
    ; on_completion = ignore
    }
;;

let rows actor = (A.state actor |> protocol_ok).inference_ledger |> L.rows
let only_handle actor = List.hd_exn (rows actor) |> L.Row.handle

let usage handle revision tokens =
  let actual = O.Count.create (Actual tokens) |> observation_ok in
  let unknown = O.Count.create (Unknown Not_reported) |> observation_ok in
  let counts =
    O.Usage.create
      ~counts:
        { input = actual
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> observation_ok
  in
  O.create
    ~scope:(L.Handle.scope handle)
    ~id:(L.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage counts)
    ~limits:O.Admission.observation
  |> observation_ok
;;

exception Upstream_failed

let%expect_test "cancelled graph acquisition returns acknowledged cleanup ownership" =
  with_tracking_actor (fun _ actor _ commits ->
    let source = source "cancelled-graph-acquisition" in
    let acquired = ref false in
    let upstream = upstream (fun ~scope:_ ~accounting_id:_ -> assert false) in
    let cancelled =
      try
        Eio.Cancel.sub (fun context ->
          Eio.Cancel.cancel context Exit;
          let graph = G.create actor ~source ~upstream |> protocol_ok in
          acquired := true;
          Exn.protect ~f:Eio.Fiber.check ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              G.seal graph |> protocol_ok;
              G.finish graph |> protocol_ok)));
        false
      with
      | Eio.Cancel.Cancelled Exit -> true
    in
    assert (!acquired && cancelled);
    let replacement = G.create actor ~source ~upstream |> protocol_ok in
    G.seal replacement |> protocol_ok;
    G.finish replacement |> protocol_ok;
    assert (List.is_empty (rows actor) && !commits = 0);
    print_endline
      "ACK returned before cancellation; source released without attempt charge");
  [%expect {| ACK returned before cancellation; source released without attempt charge |}]
;;

let%expect_test
    "graph allocation cleans routing and unsubmitted Prepared on strict failure"
  =
  with_tracking_actor (fun _ actor reject _ ->
    let _, _, prepared = prepared () in
    let admitted = ref false in
    let graph =
      G.create
        actor
        ~source:(source "graph-failure")
        ~upstream:
          (upstream (fun ~scope:_ ~accounting_id:_ ->
             admitted := true;
             reject := true;
             raise Upstream_failed))
      |> protocol_ok
    in
    reject := true;
    let failed_write =
      try
        (G.identity graph).with_attempt
          prepared
          ~relation:Root
          ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ -> ());
        false
      with
      | G.Rejected _ -> true
    in
    assert (failed_write && (not !admitted) && List.is_empty (rows actor));
    reject := false;
    let original =
      try
        (G.identity graph).with_attempt
          prepared
          ~relation:Root
          ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ -> ());
        false
      with
      | Upstream_failed -> true
    in
    assert original;
    (match O.Attempt_record.state (L.Row.record (List.hd_exn (rows actor))) with
     | Prepared -> ()
     | _ -> assert false);
    reject := false;
    G.seal graph |> protocol_ok;
    G.finish graph |> protocol_ok;
    (match O.Attempt_record.state (L.Row.record (List.hd_exn (rows actor))) with
     | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted } ->
       ()
     | _ -> assert false);
    G.finish graph |> protocol_ok;
    print_endline "failed ACK unpublished; primary failure preserved; graph joined");
  [%expect {| failed ACK unpublished; primary failure preserved; graph joined |}]
;;

let%expect_test
    "late retained usage revises once without reopening or manufacturing a turn"
  =
  with_tracking_actor (fun sw actor _ commits ->
    let context, request, _ = prepared () in
    let graph =
      G.create
        actor
        ~source:(source "graph-late")
        ~upstream:(upstream (fun ~scope:_ ~accounting_id:_ -> ()))
      |> protocol_ok
    in
    C.run
      context
      ~sw
      ~identity:(G.identity graph)
      ~relation:Root
      ~request
      ~before_dispatch:ignore
      ~on_attempt:(G.on_attempt graph)
      ~on_observation:(G.on_observation graph)
      ~on_completion:(G.on_completion graph)
      ~on_event:ignore
    |> Result.map_error ~f:(fun _ -> "selected inference failed")
    |> Result.ok_or_failwith
    |> ignore;
    let handle = only_handle actor in
    let incoming = usage handle 1L 7L in
    G.on_observation graph incoming;
    let after = !commits in
    G.on_observation graph incoming;
    assert (after = !commits);
    let before = A.state actor |> protocol_ok in
    assert (
      Int64.equal
        (Agent_protocol.Inference_query.Summary.turns (L.summary before.inference_ledger))
          .completed
        0L);
    let future_scope =
      Transcript.Scope.create
        ~source:(Transcript.Scope.key (L.Handle.scope handle)).source
        ~attempt:
          (Transcript.Attempt_id.of_string "future-unallocated" |> Result.ok_or_failwith)
        ~relation:Root
      |> Result.ok_or_failwith
    in
    let future =
      O.create
        ~scope:future_scope
        ~id:(L.Handle.accounting_id handle)
        ~revision:2L
        ~payload:(O.payload incoming)
        ~limits:O.Admission.observation
      |> observation_ok
    in
    let foreign_owner =
      A.open_inference_owner actor ~source:(source "foreign") |> protocol_ok
    in
    assert (
      Result.is_error (A.observe_owned_inference actor ~owner:foreign_owner incoming));
    A.seal_inference_owner actor ~owner:foreign_owner |> protocol_ok;
    A.finish_inference_owner actor ~owner:foreign_owner |> protocol_ok;
    let key = Transcript.Scope.key (L.Handle.scope handle) in
    let parent_scope =
      Transcript.Scope.Key.{ source = source "unrelated-parent"; attempt = key.attempt }
    in
    let contradictory =
      Transcript.Scope.create
        ~source:key.source
        ~attempt:key.attempt
        ~relation:
          (Nested
             { scope = parent_scope
             ; call_entry_id = None
             ; call_alias = Some "same-alias"
             })
      |> Result.ok_or_failwith
    in
    let contradiction =
      O.create
        ~scope:contradictory
        ~id:(L.Handle.accounting_id handle)
        ~revision:2L
        ~payload:(O.payload incoming)
        ~limits:O.Admission.observation
      |> observation_ok
    in
    let rejected_parent =
      try
        G.on_observation graph contradiction;
        false
      with
      | G.Rejected _ -> true
    in
    assert rejected_parent;
    let rejected =
      try
        G.on_observation graph future;
        false
      with
      | G.Rejected _ -> true
    in
    assert rejected;
    G.seal graph |> protocol_ok;
    G.finish graph |> protocol_ok;
    let closed =
      try
        G.on_observation graph incoming;
        false
      with
      | G.Rejected _ -> true
    in
    assert closed;
    print_endline
      "late replacement/duplicate exact; unknown and closed owners rejected; no turn \
       inferred");
  [%expect
    {| late replacement/duplicate exact; unknown and closed owners rejected; no turn inferred |}]
;;

let%expect_test
    "administration accepts only acknowledged tracking drift and preserves interruption"
  =
  with_tracking_actor (fun _ actor _ commits ->
    let _, _, prepared = prepared () in
    let owner =
      A.open_inference_owner actor ~source:(source "graph-admin") |> protocol_ok
    in
    let handle =
      A.admit_inference
        actor
        ~owner
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration:(R.Prepared.configuration prepared)
      |> protocol_ok
    in
    let attachment, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
    let expected = A.state actor |> protocol_ok in
    let options =
      Agent_session.Administration.
        { keep_history = true
        ; keep_tasks = true
        ; keep_grants = true
        ; keep_labels = true
        ; workspace_instance = None
        }
    in
    let candidate =
      Agent_session.Administration.plan_reset expected options |> protocol_ok
    in
    A.validate_administration_basis actor ~attachment_id:attachment.id ~expected
    |> protocol_ok;
    A.seal_inference_owner actor ~owner |> protocol_ok;
    A.finish_inference_owner actor ~owner |> protocol_ok;
    let joined = A.state actor |> protocol_ok in
    let joined_commits = !commits in
    (* Retry the genuine installed finish receipt, without a second interruption
       write or a new execution grant. *)
    A.finish_inference_owner actor ~owner |> protocol_ok;
    assert (phys_equal joined (A.state actor |> protocol_ok));
    [%test_eq: int] joined_commits !commits;
    A.commit_reconciled_administration
      actor
      ~command_audit:None
      ~attachment_id:attachment.id
      ~expected
      ~kind:Reset
      candidate
    |> protocol_ok
    |> ignore;
    let actual = A.state actor |> protocol_ok in
    assert (actual.identity.generation = 1);
    let row =
      L.find actual.inference_ledger ~ordinal:(L.Handle.ordinal handle)
      |> Option.value_exn
    in
    (match O.Attempt_record.state (L.Row.record row) with
     | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted } ->
       ()
     | _ -> assert false);
    assert (
      Result.is_error
        (A.validate_administration_basis actor ~attachment_id:attachment.id ~expected));
    print_endline
      "original CAS fenced; reconciled generation retains actual interrupted admission");
  [%expect
    {| original CAS fenced; reconciled generation retains actual interrupted admission |}]
;;

let%expect_test
    "trusted administration refuses unrelated history changes after original admission"
  =
  with_tracking_actor (fun _ actor _ _ ->
    let attachment, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
    let expected = A.state actor |> protocol_ok in
    let candidate =
      Agent_session.Administration.plan_reset
        expected
        { keep_history = true
        ; keep_tasks = true
        ; keep_grants = true
        ; keep_labels = true
        ; workspace_instance = None
        }
      |> protocol_ok
    in
    A.validate_administration_basis actor ~attachment_id:attachment.id ~expected
    |> protocol_ok;
    A.append_history actor ~attachment_id:attachment.id [ actor_entry ]
    |> protocol_ok
    |> ignore;
    assert (
      Result.is_error
        (A.commit_reconciled_administration
           actor
           ~command_audit:None
           ~attachment_id:attachment.id
           ~expected
           ~kind:Reset
           candidate));
    let actual = A.state actor |> protocol_ok in
    assert (actual.identity.generation = 0);
    assert (List.length actual.conversation.canonical_history = 1);
    print_endline
      "unrelated acknowledged history conflicts; original generation/history retained");
  [%expect
    {| unrelated acknowledged history conflicts; original generation/history retained |}]
;;

let ledger_ok result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (L.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let%expect_test "recovery keeps Prepared unsubmitted and Running submission uncertain" =
  let _, _, prepared = prepared () in
  let map_initial state =
    let admit ledger =
      L.admit
        ledger
        ~source:(source "lost-graph")
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration:(R.Prepared.configuration prepared)
      |> ledger_ok
    in
    let ledger, _, _ = admit state.S.inference_ledger in
    let ledger, running, _ = admit ledger in
    let ledger = L.set_state ledger running Running |> ledger_ok in
    { state with inference_ledger = ledger }
  in
  with_tracking_actor ~map_initial (fun _ actor _ commits ->
    A.reconcile_inference_recovery actor |> protocol_ok;
    let deliveries =
      List.map (rows actor) ~f:(fun row ->
        match O.Attempt_record.state (L.Row.record row) with
        | Interrupted { reason = Host_interrupted; delivery } -> delivery
        | _ -> assert false)
    in
    assert (
      List.equal
        Inference.Event.Terminal.equal_delivery
        deliveries
        [ Definitely_not_submitted; Possibly_submitted ]);
    let after = !commits in
    A.reconcile_inference_recovery actor |> protocol_ok;
    assert (after = !commits);
    print_endline
      "Prepared definitely unsubmitted; Running possibly submitted; recovery repeat no-op");
  [%expect
    {| Prepared definitely unsubmitted; Running possibly submitted; recovery repeat no-op |}]
;;

let%expect_test "late scope-only callbacks cannot reconstruct a retired admission" =
  let map_initial state =
    let limits =
      L.Limits.create
        ~max_attempts:1
        ~max_turns:256
        ~max_retained_bytes:(4 * 1024 * 1024)
        ~document_limits:O.Admission.observation
      |> ledger_ok
    in
    let inference_ledger =
      L.create
        ~session_id:state.S.identity.session_id
        ~generation:state.identity.generation
        ~before_tracking_unknown:false
        ~limits
      |> ledger_ok
    in
    { state with inference_ledger }
  in
  with_tracking_actor ~map_initial (fun sw actor reject commits ->
    let context, request, prepared = prepared () in
    let graph =
      G.create
        actor
        ~source:(source "graph-retired")
        ~upstream:(upstream (fun ~scope:_ ~accounting_id:_ -> ()))
      |> protocol_ok
    in
    let dispatch () =
      C.run
        context
        ~sw
        ~identity:(G.identity graph)
        ~relation:Root
        ~request
        ~before_dispatch:ignore
        ~on_attempt:(G.on_attempt graph)
        ~on_observation:(G.on_observation graph)
        ~on_completion:(G.on_completion graph)
        ~on_event:ignore
      |> Result.map_error ~f:(fun _ -> "selected inference failed")
      |> Result.ok_or_failwith
      |> ignore
    in
    dispatch ();
    let actual_handle = only_handle actor in
    let module Host = Agent_session.Inference_graph_owner.Host in
    let module Registration = Agent_session.Inference_graph_owner.Registration in
    (* A second actual graph exposes its public read-only lifetime projection;
       no registration or Handle is manufactured by the test. *)
    let joined_owner =
      A.open_inference_owner actor ~source:(source "joined-prune-owner") |> protocol_ok
    in
    let before_joined = !commits in
    let joined_handle =
      A.admit_inference
        actor
        ~owner:joined_owner
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration:(R.Prepared.configuration prepared)
      |> protocol_ok
    in
    A.release_inference actor ~owner:joined_owner ~handle:joined_handle |> protocol_ok;
    (* These are two real successful canonical writes: Prepared admission and
       truthful unsubmitted interruption. Existing later no-write oracles retain
       their exact baseline rather than assuming the original total. *)
    [%test_eq: int] (before_joined + 2) !commits;
    assert (
      Option.is_none
        (Host.registration joined_owner ~ordinal:(L.Handle.ordinal joined_handle)));
    let joined =
      Host.retained_registration joined_owner joined_handle |> Option.value_exn
    in
    assert (L.Handle.equal joined_handle (Registration.handle joined));
    assert (not (Registration.policy_is_current joined));
    let before_rejected = A.state actor |> protocol_ok in
    let commits_before_rejected = !commits in
    (* This fixture rejects before storage; it does not model retry of an
       uncertain physical Commit_writer. A rejected candidate must not prune. *)
    let publication_rejected =
      Exn.protect
        ~finally:(fun () -> reject := false)
        ~f:(fun () ->
          reject := true;
          try
            dispatch ();
            false
          with
          | G.Rejected error ->
            [%test_eq: Agent_protocol.Error.code] Persistence_error error.code;
            true)
    in
    assert publication_rejected;
    assert (phys_equal before_rejected (A.state actor |> protocol_ok));
    [%test_eq: int] commits_before_rejected !commits;
    assert (
      Option.exists
        (Host.retained_registration joined_owner joined_handle)
        ~f:(phys_equal joined));
    assert (L.Handle.equal joined_handle (only_handle actor));
    dispatch ();
    (* Check before graph finish: otherwise finish itself could mask a missing
       Joined-pruning step in the positive State installation path. *)
    assert (
      Option.is_none
        (L.find
           (A.state actor |> protocol_ok).inference_ledger
           ~ordinal:(L.Handle.ordinal joined_handle)));
    assert (Option.is_none (Host.retained_registration joined_owner joined_handle));
    A.seal_inference_owner actor ~owner:joined_owner |> protocol_ok;
    let before_empty_finish = A.state actor |> protocol_ok in
    let empty_finish_commits = !commits in
    A.finish_inference_owner actor ~owner:joined_owner |> protocol_ok;
    A.finish_inference_owner actor ~owner:joined_owner |> protocol_ok;
    assert (phys_equal before_empty_finish (A.state actor |> protocol_ok));
    [%test_eq: int] empty_finish_commits !commits;
    let module Fold = Agent_session.Canonical_usage_fold in
    let marks =
      Fold.Marks.of_ledger (A.state actor |> protocol_ok).inference_ledger
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Fold.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let expected_marks : Fold.Marks.t =
      { attempt = 3L; turn = 0L; untracked_attempts = 0L; untracked_turns = 0L }
    in
    if not (Fold.Marks.equal expected_marks marks)
    then
      raise_s
        [%sexp
          "unexpected canonical admission marks"
        , (expected_marks : Fold.Marks.t)
        , (marks : Fold.Marks.t)];
    let incoming = usage actual_handle 1L 9L in
    let after = !commits in
    let rejected =
      try
        G.on_observation graph incoming;
        false
      with
      | G.Rejected _ -> true
    in
    assert rejected;
    A.observe_inference actor ~handle:actual_handle incoming |> protocol_ok;
    assert (after = !commits);
    assert (List.length (rows actor) = 1);
    G.seal graph |> protocol_ok;
    G.finish graph |> protocol_ok;
    print_endline
      "scope-only retired callback rejects; actual held admission ignores with no new \
       charge; Joined membership prunes only after positive publication");
  [%expect
    {| scope-only retired callback rejects; actual held admission ignores with no new charge; Joined membership prunes only after positive publication |}]
;;

let%expect_test "actual worker allocation requires ACK and expires before terminal save" =
  (* This is ordinary unbound operation execution, not a Goal controller fixture.
     The associated Goal runtime gate remains a separate composed qualification. *)
  List.iter [ false; true ] ~f:(fun fail_terminal ->
    let phase = ref `Accept in
    let terminal_attempt, terminal_attempt_u = Eio.Promise.create () in
    let dispatches = ref 0 in
    let context, request, _ =
      prepared
        ~post_stream:(fun ~sw:_ ~inputs:_ ->
          incr dispatches;
          Seq.empty)
        ()
    in
    Job_fixtures.with_actor
      ~reject_save:(fun _ ->
        match !phase with
        | `Accept -> false
        | `Allocation -> true
        | `Terminal ->
          ignore (Eio.Promise.try_resolve terminal_attempt_u ());
          true)
      (fun _env sw actor writer _backend ->
         let graph =
           G.create
             actor
             ~source:(source "actual-operation")
             ~upstream:(upstream (fun ~scope:_ ~accounting_id:_ -> ()))
           |> protocol_ok
         in
         let retained = ref None in
         let checkpoint, checkpoint_u = Eio.Promise.create () in
         let dispatch identity =
           C.run
             context
             ~sw
             ~identity
             ~relation:Root
             ~request
             ~before_dispatch:ignore
             ~on_attempt:(G.on_attempt graph)
             ~on_observation:(G.on_observation graph)
             ~on_completion:(G.on_completion graph)
             ~on_event:ignore
           |> Result.map_error ~f:(fun _ -> "fixture inference preparation failed")
           |> Result.ok_or_failwith
           |> ignore
         in
         let worker =
           Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
             let result =
               try
                 let identity =
                   G.bind_identity graph ~allocate:capabilities.allocate_inference
                 in
                 retained := Some identity;
                 phase := `Allocation;
                 let rejected =
                   Exn.protect
                     ~finally:(fun () -> phase := `Accept)
                     ~f:(fun () ->
                       try
                         dispatch identity;
                         false
                       with
                       | G.Rejected _ -> true)
                 in
                 assert rejected;
                 assert (Int.equal !dispatches 0);
                 assert (List.is_empty (rows actor));
                 dispatch identity;
                 assert (Int.equal !dispatches 1);
                 assert (Int.equal (List.length (rows actor)) 1);
                 if fail_terminal then phase := `Terminal;
                 Ok ()
               with
               | exn -> Error exn
             in
             Eio.Promise.resolve checkpoint_u result;
             match result with
             | Ok () ->
               Agent_session.Operation_worker.Completed
                 { final_history = input.history
                 ; moderator_snapshot = None
                 ; runtime_requests = []
                 }
             | Error _ ->
               Agent_session.Operation_worker.Failed
                 (Agent_protocol.Error.invalid_request "worker allocation fixture failed"))
         in
         A.set_operation_worker actor (Some worker) |> protocol_ok;
         A.submit_message
           actor
           ~submitting_principal:principal_id
           ~attachment_id:writer.id
           actor_entry
         |> protocol_ok
         |> ignore;
         (match Eio.Promise.await checkpoint with
          | Ok () -> ()
          | Error exn -> raise exn);
         if fail_terminal
         then Eio.Promise.await terminal_attempt
         else ignore (await_idle actor : S.t);
         let before = A.state actor |> protocol_ok in
         assert (Bool.equal (Option.is_some before.active_operation) fail_terminal);
         let rejected =
           try
             dispatch (Option.value_exn !retained);
             false
           with
           | G.Rejected _ -> true
         in
         assert rejected;
         assert (Int.equal !dispatches 1);
         let after = A.state actor |> protocol_ok in
         [%test_eq: int64] after.counters.revision before.counters.revision;
         phase := `Accept;
         G.seal graph |> protocol_ok;
         G.finish graph |> protocol_ok));
  print_endline
    "failed allocation has no dispatch/charge; exact live worker admits; returned port \
     expires before terminal ACK";
  [%expect
    {| failed allocation has no dispatch/charge; exact live worker admits; returned port expires before terminal ACK |}]
;;

let%expect_test "actual Run worker refreshes before a nonyielding final authority check" =
  (* A genuine headless User_submission owns the real Run/operation. This is
     ordinary Run execution, not a fabricated Goal episode or controller. *)
  let forbidden_authorization = ref false in
  let graph_ref = ref None in
  let expire = ref (fun () -> ()) in
  let checkpoint, checkpoint_u = Eio.Promise.create () in
  let await_checkpoint = ref (fun () -> failwith "fixture clock is unavailable") in
  let await_terminal : (A.t -> S.t) ref =
    ref (fun _ -> failwith "fixture clock is unavailable")
  in
  let capture_before_worker : (unit -> S.t) ref =
    ref (fun () -> failwith "fixture Actor is unavailable")
  in
  let before_worker = ref None in
  let dispatches = ref 0 in
  let refreshes = ref 0 in
  let final_checks = ref 0 in
  let context, request, _ =
    prepared
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr dispatches;
        Seq.empty)
      ()
  in
  let worker =
    Agent_session.Operation_worker.create ~run:(fun ~sw ~input capabilities ->
      before_worker := Some (!capture_before_worker ());
      let result =
        Result.try_with (fun () ->
          let graph = Option.value_exn !graph_ref in
          let identity =
            G.bind_identity graph ~allocate:capabilities.allocate_inference
          in
          let dispatch ~revoke =
            forbidden_authorization := false;
            let guard =
              R.Submission_guard.create ~refresh:(fun () ->
                Int.incr refreshes;
                forbidden_authorization := false;
                Eio.Fiber.yield ();
                if revoke then !expire ();
                fun () ->
                  Int.incr final_checks;
                  forbidden_authorization := true)
            in
            C.run
              context
              ~sw
              ~identity
              ~submission_guard:guard
              ~relation:Root
              ~request
              ~before_dispatch:ignore
              ~on_attempt:(G.on_attempt graph)
              ~on_observation:(G.on_observation graph)
              ~on_completion:(G.on_completion graph)
              ~on_event:ignore
            |> Result.map_error ~f:(fun _ -> "fixture inference preparation failed")
            |> Result.ok_or_failwith
            |> ignore
          in
          dispatch ~revoke:false;
          assert (Int.equal !dispatches 1);
          assert (Int.equal !refreshes 1 && Int.equal !final_checks 1);
          let rejected =
            try
              dispatch ~revoke:true;
              false
            with
            | Agent_session.Inference_allocation.Rejected _ -> true
          in
          assert rejected;
          assert (Int.equal !dispatches 1);
          assert (Int.equal !refreshes 2 && Int.equal !final_checks 1))
      in
      forbidden_authorization := false;
      Eio.Promise.resolve checkpoint_u result;
      match result with
      | Ok () ->
        Agent_session.Operation_worker.Completed
          { final_history = input.history
          ; moderator_snapshot = None
          ; runtime_requests = []
          }
      | Error _ ->
        Agent_session.Operation_worker.Failed
          (Agent_protocol.Error.invalid_request "Run final-check fixture failed"))
  in
  Headless_run_tests.with_service
    ~prepare:(fun env ->
      (await_checkpoint
       := fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
              Eio.Promise.await checkpoint));
      await_terminal
      := fun actor ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
             let rec loop () =
               let state = A.state actor |> protocol_ok in
               match state.active_operation with
               | None -> state
               | Some _ ->
                 Eio.Fiber.yield ();
                 loop ()
             in
             loop ()))
    ~worker
    ~authorize_hook:(fun () ->
      if !forbidden_authorization then failwith "final check invoked host authorization")
    (fun fixture ->
       let graph =
         G.create
           fixture.actor
           ~source:(source "actual-run-final-check")
           ~upstream:(upstream (fun ~scope:_ ~accounting_id:_ -> ()))
         |> protocol_ok
       in
       graph_ref := Some graph;
       (capture_before_worker := fun () -> A.state fixture.actor |> protocol_ok);
       (expire := fun () -> fixture.auth_now := fixture.expires_at);
       (* Initialize through the real startup, whose no-work Single_turn
          completes. User input is then admitted from actual Running/Idle. *)
       fixture.start (fixture.request ~mode:Single_turn "initialize-final-check")
       |> Headless_run_tests.admitted
       |> ignore;
       assert (fixture.activate () |> protocol_ok);
       fixture.start
         (fixture.request
            ~input:(Headless_run_tests.input "run inference")
            "owned-final-check")
       |> Headless_run_tests.admitted
       |> ignore;
       (match !await_checkpoint () with
        | Ok () -> ()
        | Error exn -> raise exn);
       G.seal graph |> protocol_ok;
       G.finish graph |> protocol_ok;
       let settled = !await_terminal fixture.actor in
       let before = Option.value_exn !before_worker in
       assert (Option.is_some before.moderator);
       assert (Option.equal Jsonaf.exactly_equal before.moderator settled.moderator);
       let before_index = Option.value_exn before.run_state in
       let settled_index = Option.value_exn settled.run_state in
       let installation = Agent_session.Run_state.installation before_index in
       assert (Option.is_some installation.source);
       assert (
         Agent_session.Run_source_installation.equal
           installation
           (Agent_session.Run_state.installation settled_index));
       let operation = Option.value_exn before.active_operation in
       assert (
         List.exists
           (Agent_session.Run_state.runs settled_index)
           ~f:(fun (run : Agent_protocol.Run.t) ->
             List.exists
               run.terminal_work
               ~f:(fun (proof : Agent_protocol.Run_work.Terminal.t) ->
                 Agent_protocol.Run_work.Terminal.equal_outcome proof.outcome Succeeded
                 &&
                 match proof.work.key with
                 | Operation id -> Agent_protocol.Id.Operation.equal id operation.id
                 | Retained _ -> false))));
  print_endline
    "actual provider admitted once; later composed revocation rejects; final check calls \
     no host policy";
  [%expect
    {| actual provider admitted once; later composed revocation rejects; final check calls no host policy |}]
;;

module Terminal_failure_order = struct
  type t =
    | Before_retirement
    | During_retirement
    | After_committed_cancel
end

let%expect_test "actual terminal errors survive worker exit and later retirement" =
  let module Owner = Agent_server.Runtime_owner in
  let module Retirement = A.Runtime_retirement in
  List.iter
    Terminal_failure_order.
      [ Before_retirement; During_retirement; After_committed_cancel ]
    ~f:(fun order ->
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let attempted, attempted_u = Eio.Promise.create () in
      let expected_operation = ref None in
      let before_terminal = ref None in
      let worker_calls = ref 0
      and worker_exits = ref 0
      and terminal_calls = ref 0 in
      let failure =
        Agent_protocol.Error.create
          Persistence_error
          ~message:"actual terminal prewriter rejection"
          ~retryable:true
          ()
      in
      let assert_failure actual =
        [%test_eq: Sexp.t]
          (Agent_protocol.Error.sexp_of_t failure)
          (Agent_protocol.Error.sexp_of_t actual)
      in
      let worker =
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _capabilities ->
          Int.incr worker_calls;
          (* A finite exit handoff makes the live-worker assertion deterministic.
             Both genuine Cancel and retirement can mark its real switch while
             it is here. The unconditional release below always lets it leave. *)
          Exn.protect
            ~finally:(fun () -> Int.incr worker_exits)
            ~f:(fun () ->
              Eio.Cancel.protect (fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await release);
              Agent_session.Operation_worker.Completed
                { final_history = input.history
                ; moderator_snapshot = None
                ; runtime_requests = []
                }))
      in
      let make_persistence _env _sw _initial =
        { A.archive_reference
        ; commit =
            (fun ?observation:_
              ~command_audit:_
              ~(previous : S.t)
              (transition : Agent_session.Session_transition.t) ->
              match previous.active_operation, !expected_operation with
              | Some operation, Some expected
                when Agent_protocol.Id.Operation.equal operation.id expected
                     && Option.is_none transition.state.active_operation ->
                Int.incr terminal_calls;
                before_terminal := Some previous;
                if Option.is_none (Eio.Promise.peek attempted)
                then Eio.Promise.resolve attempted_u ();
                Error failure
              | (Some _ | None), (Some _ | None) -> Ok ())
        }
      in
      let release_worker () =
        if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
      in
      let with_deadline =
        ref (fun _ -> failwith "terminal fixture clock is unavailable")
      in
      let body_completed = ref false in
      let body_failure = ref None in
      let run_body f =
        match f () with
        | () -> body_completed := true
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          body_failure := Some (exn, backtrace);
          Exn.raise_with_original_backtrace exn backtrace
      in
      let caught_cleanup =
        try
          Headless_run_tests.with_service
            ~make_persistence
            ~worker
            ~keep_writer:true
            ~prepare:(fun env ->
              with_deadline := Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5.)
            (fun fixture ->
               run_body (fun () ->
                 Exn.protect ~finally:release_worker ~f:(fun () ->
                   !with_deadline (fun () ->
                     fixture.start
                       (fixture.request ~mode:Single_turn "initialize-terminal")
                     |> Headless_run_tests.admitted
                     |> ignore;
                     assert (fixture.activate () |> protocol_ok);
                     fixture.start
                       (fixture.request
                          ~input:(Headless_run_tests.input "terminal outcome")
                          "actual-terminal")
                     |> Headless_run_tests.admitted
                     |> ignore;
                     Eio.Promise.await entered;
                     let state = A.state fixture.actor |> protocol_ok in
                     let operation = Option.value_exn state.active_operation in
                     expected_operation := Some operation.id;
                     let retirement =
                       match order with
                       | Before_retirement -> None
                       | During_retirement ->
                         let retirement =
                           A.retire_runtime_worker fixture.actor ~closing:true
                           |> protocol_ok
                         in
                         assert (not (Retirement.is_finished retirement));
                         assert (Int.equal !worker_exits 0);
                         Some retirement
                       | After_committed_cancel ->
                         A.cancel_operation
                           fixture.actor
                           ~attachment_id:fixture.attachment_id
                           ~operation_id:operation.id
                         |> protocol_ok
                         |> ignore;
                         let cancelled = A.state fixture.actor |> protocol_ok in
                         let active = Option.value_exn cancelled.active_operation in
                         assert (Agent_protocol.Id.Operation.equal active.id operation.id);
                         (match active.state with
                          | Cancelling -> ()
                          | Starting
                          | Running
                          | Completed
                          | Failed _
                          | Cancelled
                          | Interrupted _ ->
                            failwith "actual Cancel did not commit Cancelling");
                         None
                     in
                     release_worker ();
                     Eio.Promise.await attempted;
                     (* This serialized read occurs after the real failing terminal
                     request has left the Actor wrapper, before late retirement. *)
                     let after = A.state fixture.actor |> protocol_ok in
                     assert (phys_equal (Option.value_exn !before_terminal) after);
                     assert (Int.equal !worker_calls 1 && Int.equal !worker_exits 1);
                     assert (Int.equal !terminal_calls 1);
                     let retirement =
                       match retirement with
                       | Some retirement -> retirement
                       | None ->
                         A.retire_runtime_worker fixture.actor ~closing:true
                         |> protocol_ok
                     in
                     (match Retirement.await retirement with
                      | Error actual -> assert_failure actual
                      | Ok () -> failwith "failed terminal was reported as acknowledged");
                     let repeated =
                       A.retire_runtime_worker fixture.actor ~closing:true |> protocol_ok
                     in
                     assert (phys_equal retirement repeated);
                     (match Retirement.await repeated with
                      | Error actual -> assert_failure actual
                      | Ok () -> failwith "retained terminal failure changed");
                     assert (phys_equal after (A.state fixture.actor |> protocol_ok));
                     assert (Int.equal !terminal_calls 1);
                     (* The real Owner must report this same failure rather than
                     await a nonexistent second callback. It retains its runtime;
                     this is not a successful raw-resource close assertion. *)
                     (match Owner.close_and_wait fixture.runtime_owner with
                      | () -> failwith "Owner close hid the failed terminal"
                      | exception Owner.Cleanup_failed actual -> assert_failure actual);
                     assert (Owner.is_loaded fixture.runtime_owner);
                     assert (Int.equal !terminal_calls 1)))));
          false
        with
        | Owner.Cleanup_failed actual ->
          (* The fixture's unconditional strong-close finalizer is truthful too. *)
          assert_failure actual;
          true
      in
      Option.iter !body_failure ~f:(fun (exn, backtrace) ->
        Exn.raise_with_original_backtrace exn backtrace);
      assert !body_completed;
      assert caught_cleanup;
      match order with
      | Before_retirement -> print_endline "terminal error before retirement: retained"
      | During_retirement ->
        print_endline "live worker then retirement: exact error joined"
      | After_committed_cancel ->
        print_endline "committed Cancel then terminal error: retained");
  [%expect
    {|
    terminal error before retirement: retained
    live worker then retirement: exact error joined
    committed Cancel then terminal error: retained |}]
;;

let%expect_test "failed factual release retains the actual registration for retry" =
  with_tracking_actor (fun _ actor reject commits ->
    let _, _, prepared = prepared () in
    let source = source "retained-release-owner" in
    let owner = A.open_inference_owner actor ~source |> protocol_ok in
    let handle =
      A.admit_inference
        actor
        ~owner
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration:(R.Prepared.configuration prepared)
      |> protocol_ok
    in
    let confirmed = A.state actor |> protocol_ok in
    (* This fixture rejects before touching storage. It does not model an
       unavailable physical Commit_writer or permit retrying its uncertain head. *)
    reject := true;
    let failed = A.release_inference actor ~owner ~handle |> Result.is_error in
    assert failed;
    A.seal_inference_owner actor ~owner |> protocol_ok;
    assert (Result.is_error (A.finish_inference_owner actor ~owner));
    assert (phys_equal confirmed (A.state actor |> protocol_ok));
    assert (Result.is_error (A.open_inference_owner actor ~source));
    [%test_eq: int] 1 !commits;
    reject := false;
    (* SAME opaque owner and handle must remain usable. Removing the registration
       in a failed release's finalizer makes this retry fail before persistence. *)
    A.release_inference actor ~owner ~handle |> protocol_ok;
    let saved = List.hd_exn (rows actor) in
    assert (L.Handle.equal handle (L.Row.handle saved));
    (match O.Attempt_record.state (L.Row.record saved) with
     | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted } ->
       ()
     | Prepared | Running | Terminal _ | Interrupted _ -> assert false);
    A.finish_inference_owner actor ~owner |> protocol_ok;
    assert (Result.is_error (A.release_inference actor ~owner ~handle));
    let replacement = A.open_inference_owner actor ~source |> protocol_ok in
    let replacement_handle =
      A.admit_inference
        actor
        ~owner:replacement
        ~relation:Root
        ~operation_id:None
        ~invocation_id:None
        ~configuration:(R.Prepared.configuration prepared)
      |> protocol_ok
    in
    [%test_eq: int] (L.Handle.generation handle) (L.Handle.generation replacement_handle);
    assert (
      Transcript.Source_id.equal
        (Transcript.Scope.key (L.Handle.scope handle)).source
        (Transcript.Scope.key (L.Handle.scope replacement_handle)).source);
    let before_wrong_owner = A.state actor |> protocol_ok in
    (* Source and generation are deliberately identical. The new actual graph
       must not inherit old joined membership from those metadata fields. *)
    assert (
      Result.is_error
        (A.observe_owned_inference actor ~owner:replacement (usage handle 1L 7L)));
    assert (Result.is_error (A.release_inference actor ~owner:replacement ~handle));
    assert (phys_equal before_wrong_owner (A.state actor |> protocol_ok));
    A.release_inference actor ~owner:replacement ~handle:replacement_handle |> protocol_ok;
    A.seal_inference_owner actor ~owner:replacement |> protocol_ok;
    A.finish_inference_owner actor ~owner:replacement |> protocol_ok;
    [%test_eq: int] 4 !commits;
    [%test_eq: int] 2 (List.length (rows actor));
    print_endline
      "failed release retains custody; exact retry settles once; replacement graph \
       cannot inherit old facts");
  [%expect
    {| failed release retains custody; exact retry settles once; replacement graph cannot inherit old facts |}]
;;

let%expect_test "factual graph duties elect once and retain cancelled or newer frontiers" =
  let module F = Agent_session.Inference_graph_owner.Factual_usage in
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      let initial_calls = ref 0 in
      let correction_calls = ref 0 in
      let initial_entered, initial_entered_u = Eio.Promise.create () in
      let initial_release, initial_release_u = Eio.Promise.create () in
      let correction_entered, correction_entered_u = Eio.Promise.create () in
      let correction_release, correction_release_u = Eio.Promise.create () in
      let failure =
        Agent_protocol.Error.create
          Persistence_error
          ~message:"retained factual procedure failure"
          ~retryable:false
          ()
      in
      (* Fixed core procedures are state-machine stimuli, not native ACKs or
         execution permission. Real Actor/registration pruning stays above. *)
      let facts =
        F.Host.create
          ~confirm_initial:(fun () ->
            incr initial_calls;
            if !initial_calls = 1
            then (
              Eio.Promise.resolve initial_entered_u ();
              Eio.Promise.await initial_release);
            Ok ())
          ~correct_usage:(fun () ->
            incr correction_calls;
            match !correction_calls with
            | 1 ->
              Eio.Promise.resolve correction_entered_u ();
              Eio.Promise.await correction_release;
              Ok F.Correction_outcome.Settled
            | 2 -> Error failure
            | 3 -> Ok Awaiting_initial_confirmation
            | _ -> Ok Settled)
      in
      F.Host.invalidate facts ~ledger_revision:1L;
      (match F.settle_correction facts |> protocol_ok with
       | Deferred -> ()
       | Settled -> failwith "correction bypassed actual initial confirmation");
      assert (!correction_calls = 0);
      let cancellation, cancellation_u = Eio.Promise.create () in
      let leader_done, leader_done_u = Eio.Promise.create () in
      let follower_entered, follower_entered_u = Eio.Promise.create () in
      let follower_done, follower_done_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        let cancelled =
          try
            Eio.Cancel.sub (fun context ->
              Eio.Promise.resolve cancellation_u context;
              F.confirm_initial facts |> protocol_ok);
            false
          with
          | Eio.Cancel.Cancelled Exit -> true
        in
        Eio.Promise.resolve leader_done_u cancelled);
      Eio.Promise.await initial_entered;
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve follower_entered_u ();
        let cancelled =
          try
            F.confirm_initial facts |> protocol_ok;
            false
          with
          | Eio.Cancel.Cancelled Exit -> true
        in
        Eio.Promise.resolve follower_done_u cancelled);
      Eio.Promise.await follower_entered;
      assert (!initial_calls = 1);
      Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
      assert (Eio.Promise.await leader_done);
      assert (Eio.Promise.await follower_done);
      Eio.Promise.resolve initial_release_u ();
      assert (not (F.Host.initial_confirmed facts |> protocol_ok));
      assert (F.Host.pending facts |> protocol_ok);
      F.confirm_initial facts |> protocol_ok;
      assert (!initial_calls = 2);
      assert (F.Host.initial_confirmed facts |> protocol_ok);
      let corrected, corrected_u = Eio.Promise.create () in
      let joined, joined_u = Eio.Promise.create () in
      let joining, joining_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve corrected_u (F.settle_correction facts |> protocol_ok));
      Eio.Promise.await correction_entered;
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve joining_u ();
        Eio.Promise.resolve joined_u (F.settle_correction facts |> protocol_ok));
      Eio.Promise.await joining;
      assert (!correction_calls = 1);
      F.Host.invalidate facts ~ledger_revision:2L;
      Eio.Promise.resolve correction_release_u ();
      List.iter
        [ Eio.Promise.await corrected; Eio.Promise.await joined ]
        ~f:(function
          | F.Drain_outcome.Deferred -> ()
          | Settled -> failwith "old correction cleared newer canonical facts");
      assert (F.Host.pending facts |> protocol_ok);
      (match F.settle_correction facts with
       | Error actual -> assert (phys_equal actual failure)
       | Ok _ -> failwith "factual procedure lost its original Error");
      assert (F.Host.pending facts |> protocol_ok);
      (match F.settle_correction facts |> protocol_ok with
       | Deferred -> ()
       | Settled -> failwith "awaiting initial confirmation became settled");
      assert (F.Host.pending facts |> protocol_ok);
      (match F.settle_correction facts |> protocol_ok with
       | Settled -> ()
       | Deferred -> failwith "current factual procedure did not settle");
      assert (not (F.Host.pending facts |> protocol_ok));
      F.Host.invalidate facts ~ledger_revision:1L;
      F.confirm_initial facts |> protocol_ok;
      (match F.settle_correction facts |> protocol_ok with
       | Settled -> ()
       | Deferred -> failwith "older invalidation regressed the clean frontier");
      assert (!initial_calls = 2 && !correction_calls = 4);
      print_endline
        "one initial/correction election; cancellation and Error retain duty; newer and \
         awaiting frontiers remain pending"));
  [%expect
    {| one initial/correction election; cancellation and Error retain duty; newer and awaiting frontiers remain pending |}]
;;
