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
                (fun ~command_audit:_ ~previous:_ _ ->
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
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~f:(fun () -> f sw actor reject commits)
        ~finally:(fun () -> A.shutdown actor)))
;;

let prepared () =
  let fixture =
    Inference_fixture.create
      ~namespace:"tracked"
      ~default_model:"fixture"
      ~post_stream:(fun ~sw:_ ~inputs:_ -> Seq.empty)
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
          ~f:(fun ~scope:_ ~accounting_id:_ -> ());
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
          ~f:(fun ~scope:_ ~accounting_id:_ -> ());
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
  with_tracking_actor (fun _ actor _ _ ->
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
    let candidate = Agent_session.Administration.reset expected options |> protocol_ok in
    A.validate_administration_basis actor ~attachment_id:attachment.id ~expected
    |> protocol_ok;
    A.seal_inference_owner actor ~owner |> protocol_ok;
    A.finish_inference_owner actor ~owner |> protocol_ok;
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
      Agent_session.Administration.reset
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
  with_tracking_actor ~map_initial (fun sw actor _ commits ->
    let context, request, _ = prepared () in
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
    dispatch ();
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
       charge");
  [%expect
    {| scope-only retired callback rejects; actual held admission ignores with no new charge |}]
;;
