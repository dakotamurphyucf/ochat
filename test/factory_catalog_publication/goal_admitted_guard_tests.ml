open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module Ledger = A.Inference_ledger
module Observation = Inference.Observation
module Source = Deferred_source_support

module Cut = struct
  type t =
    | Automatic_hold
    | Explicit_same_hold
    | Explicit_hold_before_sync
    | Source_revoked
    | Source_revoked_settlement_failure
  [@@deriving sexp_of]
end

let prompt =
  {|<developer>Actual Goal rich-input allocation owner.</developer>
<tool name="guard_input" agent="guard-input.chatmd" local/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let command_request entry ~key mutation =
  let current = state entry in
  A.Goal_command.create
    ~session_id:current.identity.session_id
    ~generation:current.identity.generation
    ~source:
      (if A.Goal_command.Mutation.needs_source mutation
       then Some (G.source (Goal.goal current))
       else None)
    ~expected_catalog_revision:
      (A.Goal_catalog.revision (Option.value_exn current.goal_catalog))
    ~expected_goal_revision:(Some (G.revision (Goal.goal current)))
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
;;

let send_command daemon entry request =
  let current = state entry in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~session_id:current.identity.session_id
  in
  Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
;;

let command daemon entry ~key mutation =
  send_command daemon entry (command_request entry ~key mutation)
;;

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

module Fault = struct
  type t =
    { mutable selected : A.Goal_command.t option
    ; mutable candidate : A.Goal_catalog.t option
    ; mutable hits : int
    ; mutable settlement : (Transcript.Scope.t * Observation.Observation_id.t) option
    ; mutable settlement_candidate : Ledger.t option
    }

  let create () =
    { selected = None
    ; candidate = None
    ; hits = 0
    ; settlement = None
    ; settlement_candidate = None
    }
  ;;

  let receipt catalog request =
    A.Goal_catalog.receipt
      catalog
      ~principal_id:principal.id
      ~key:request.A.Goal_command.key
      ~request_sha256:(A.Goal_command.digest request |> protocol_ok)
    |> protocol_ok
  ;;

  let settlement_reached t env path =
    match t.settlement with
    | None -> ()
    | Some (scope, accounting_id) ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         let delta =
           A.Session_delta_document.decode
             ~limits:Document_schema.Limits.default
             transaction.delta
           |> document_ok
           |> A.Session_delta_document.value
         in
         List.iter (atoms delta) ~f:(function
           | Inference_ledger_changed ledger
           | Delegated_inference_ledger_changed (ledger, _) ->
             (match
                List.find (Ledger.rows ledger) ~f:(fun row ->
                  let handle = Ledger.Row.handle row in
                  Transcript.Scope.equal scope (Ledger.Handle.scope handle)
                  && Observation.Observation_id.equal
                       accounting_id
                       (Ledger.Handle.accounting_id handle))
              with
              | Some row ->
                (match Observation.Attempt_record.state (Ledger.Row.record row) with
                 | Interrupted { delivery = Possibly_submitted; _ } ->
                   t.settlement <- None;
                   t.settlement_candidate <- Some ledger;
                   t.hits <- t.hits + 1;
                   raise (Core_unix.Unix_error (EIO, "566 factual settlement sync", path))
                 | Prepared | Running | Terminal _ | Interrupted _ -> ())
              | None -> ())
           | _ -> ())
       | Some _ | None -> ())
  ;;

  let reached t env path =
    match t.selected with
    | None -> ()
    | Some request ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id request.session_id
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           match atoms delta with
           | [ Goal_catalog_changed catalog ]
             when Option.is_some (receipt catalog request) ->
             t.selected <- None;
             t.candidate <- Some catalog;
             t.hits <- t.hits + 1;
             raise (Core_unix.Unix_error (EIO, "542 explicit Hold sync", path))
           | _ -> ())
       | Some _ | None -> ())
  ;;
end

let assert_metadata_same (before : Agent_store.Session_metadata.t) after =
  [%test_eq: int] before.schema_version after.Agent_store.Session_metadata.schema_version;
  [%test_eq: int] before.data_schema_version after.data_schema_version;
  [%test_eq: string] before.prompt_artifact after.prompt_artifact;
  [%test_eq: string] before.workspace_identity after.workspace_identity;
  assert (
    Jsonaf.exactly_equal
      (P.Session.to_json before.session)
      (P.Session.to_json after.session))
;;

exception Observed_explicit_hold_failure of P.Error.t
exception Observed_settlement_failure

let row current scope accounting_id =
  Ledger.rows current.A.Session_state.inference_ledger
  |> List.find_exn ~f:(fun row ->
    let handle = Ledger.Row.handle row in
    Transcript.Scope.equal scope (Ledger.Handle.scope handle)
    && Observation.Observation_id.equal accounting_id (Ledger.Handle.accounting_id handle))
;;

let run (cut : Cut.t) =
  let fault = Fault.create () in
  let primary = ref None in
  let source_authorized = ref true in
  let authorize current =
    if !source_authorized
    then authorize current
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"566 original source revoked"
           ~retryable:false
           ())
  in
  let providers = ref 0 in
  let admitted_hook = ref (fun ~scope:_ ~accounting_id:_ -> ()) in
  let attempt_hook = ref (fun (_ : Inference_runtime.Attempt.t) -> ()) in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      let transcript =
        `Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      if not (String.is_substring transcript ~substring:"542_ADMITTED_INPUT")
      then raise_s [%sexp "unexpected fixture provider transcript", (transcript : string)];
      Int.incr providers;
      completed_response "542 admitted input complete")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Result.map (base.runtime_inference_ports actor) ~f:(fun upstream ->
            { upstream with
              on_admitted =
                (fun ~scope ~accounting_id ->
                  upstream.on_admitted ~scope ~accounting_id;
                  !admitted_hook ~scope ~accounting_id)
            ; on_attempt =
                (fun attempt ->
                  upstream.on_attempt attempt;
                  !attempt_hook attempt)
            }))
    }
  in
  (match
     Creation_tests.with_daemon
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary:Before_sync
           ~matches:(fun path ->
             String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(fun path ->
             Fault.reached fault env path;
             Fault.settlement_reached fault env path))
       ~prompt_text:prompt
       ~sources:
         [ ( "guard-input.chatmd"
           , "<developer>Return the selected input result.</developer>" )
         ]
       ~qualify_chatml_extensions:true
       ~policy
       (fun env sw _root daemon ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
            Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
              let session, _ = Goal.create_goal daemon entry in
              let budget =
                G.Budget.create
                  ~admitted_turns:None
                  ~admitted_attempts:(Some 1L)
                  ~observed_tokens:None
                  ~deadline:None
                |> protocol_ok
              in
              command
                daemon
                entry
                ~key:"goal-guard-budget"
                (Change_budget (Goal.goal_id, budget))
              |> protocol_ok
              |> ignore;
              let before = state entry in
              let request =
                A.Run_admission_request.Headless.create
                  ~session_id:before.identity.session_id
                  ~generation:before.identity.generation
                  ~expected_revision:before.counters.revision
                  ~mode:Workflow
                  ~input:
                    (User_submission
                       { kind = Chatmd
                       ; text =
                           {|<user>before <agent src="guard-input.chatmd" local>542_ADMITTED_INPUT</agent> after</user>|}
                       ; attachments = []
                       })
                  ~key:(P.Idempotency_key.of_string "goal-guard-episode" |> protocol_ok)
                |> protocol_ok
              in
              let intent =
                A.Goal_episode_admission.intent
                  ~goal_id:Goal.goal_id
                  ~expected_goal_revision:(G.revision (Goal.goal before))
                  ~expected_catalog_revision:
                    (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
                |> protocol_ok
              in
              let input_scope = ref None in
              let admission = ref None in
              let admitted, admitted_u = Eio.Promise.create () in
              let release, release_u = Eio.Promise.create () in
              let release_first () =
                if Option.is_none (Eio.Promise.peek release)
                then Eio.Promise.resolve release_u ()
              in
              (admitted_hook
               := fun ~scope ~accounting_id ->
                    if Option.is_some (Eio.Promise.peek admitted)
                    then failwith "input admitted a second actual inference";
                    Eio.Promise.resolve admitted_u (scope, accounting_id);
                    Eio.Promise.await release);
              protect_cleanup ~finally:release_first (fun () ->
                let work =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    Factory.with_effect_deferred_run_source
                      (Daemon.factory daemon)
                      entry
                      ~actor:original
                      ~authorize
                      ~request
                      (fun _ prepared ->
                         let open Result.Let_syntax in
                         let%bind selected =
                           Actor.admit_held_goal_run
                             entry.actor
                             ~binding:(Owner.Prepared.binding prepared)
                             ~intent
                             ~session
                         in
                         match selected with
                         | Admitted activation ->
                           let held = Actor.Goal_activation.held activation in
                           admission := Some (Actor.Held_run.admission held);
                           Owner.with_held_input_runtime
                             entry.runtime
                             ~prepared
                             ~held
                             ~checked:(Actor.Goal_activation.checked activation)
                             (fun owner ->
                                Owner.Held_input_runtime.submit
                                  owner
                                  ~prepare:(fun ~runtime ~scope ->
                                    input_scope := Some scope;
                                    Factory.prepare_held_input
                                      (Daemon.factory daemon)
                                      ~entry
                                      ~runtime
                                      ~scope))
                         | Rejected error
                         | Uncertain error
                         | Activation_unavailable { error; _ } -> Error error
                         | Retained _ ->
                           failwith "fresh Goal guard fixture returned retained DATA"))
                in
                let scope, accounting_id =
                  Eio.Fiber.first
                    (fun () -> Eio.Promise.await admitted)
                    (fun () ->
                       Eio.Promise.await_exn work |> protocol_ok |> ignore;
                       failwith "actual rich parser ended before allocation admission")
                in
                let selected_scope = Option.value_exn !input_scope in
                let execution =
                  Actor.Held_input_scope.execution selected_scope |> protocol_ok
                in
                execution.checkpoint () |> protocol_ok;
                let charged = state entry in
                let first = row charged scope accounting_id in
                let handle = Ledger.Row.handle first in
                [%test_eq: int64]
                  1L
                  (A.Goal_accounting.admitted_attempts (Goal.accounting charged));
                [%test_eq: int] 0 !providers;
                let probe =
                  Actor.Input_graph.open_owner
                    entry.actor
                    ~scope:selected_scope
                    ~source:
                      (Transcript.Source_id.of_string "goal-guard-probe"
                       |> Result.ok_or_failwith)
                  |> protocol_ok
                in
                let denied =
                  execution.allocate_inference
                    ~graph:probe
                    ~relation:Root
                    ~configuration:
                      (Observation.Attempt_record.configuration (Ledger.Row.record first))
                  |> rejected
                in
                [%test_eq: P.Error.code] Permission_denied denied.code;
                let held = state entry in
                assert (
                  G.State.equal
                    (Open (Held (Budget_gate Admitted_attempts)))
                    (G.state (Goal.goal held)));
                let held_revision = held.counters.revision in
                [%test_eq: int64]
                  1L
                  (A.Goal_accounting.admitted_attempts (Goal.accounting held));
                [%test_eq: P.Error.code]
                  Permission_denied
                  (execution.checkpoint () |> rejected).code;
                Actor.Input_graph.seal_inference_owner
                  entry.actor
                  ~scope:selected_scope
                  ~owner:probe
                |> protocol_ok;
                Actor.Input_graph.finish_inference_owner
                  entry.actor
                  ~scope:selected_scope
                  ~owner:probe
                |> protocol_ok;
                let faulted =
                  match cut with
                  | Automatic_hold ->
                    (attempt_hook
                     := fun _ ->
                          let running = state entry in
                          assert (Int64.(running.counters.revision > held_revision));
                          assert (
                            G.State.equal
                              (G.state (Goal.goal held))
                              (G.state (Goal.goal running)));
                          let actual = row running scope accounting_id in
                          assert (Ledger.Handle.equal handle (Ledger.Row.handle actual));
                          match
                            Observation.Attempt_record.state (Ledger.Row.record actual)
                          with
                          | Running -> ()
                          | Prepared | Terminal _ | Interrupted _ ->
                            failwith "actual attempt ACK did not retain Running");
                    None
                  | Source_revoked | Source_revoked_settlement_failure ->
                    source_authorized := false;
                    [%test_eq: P.Error.code]
                      Permission_denied
                      (execution.checkpoint () |> rejected).code;
                    (match cut with
                     | Source_revoked_settlement_failure ->
                       fault.settlement <- Some (scope, accounting_id)
                     | Source_revoked -> ()
                     | Automatic_hold | Explicit_same_hold | Explicit_hold_before_sync ->
                       assert false);
                    None
                  | Explicit_same_hold ->
                    command
                      daemon
                      entry
                      ~key:"goal-guard-explicit-hold"
                      (Hold (Goal.goal_id, Budget_gate Admitted_attempts))
                    |> protocol_ok
                    |> ignore;
                    assert (
                      G.State.equal
                        (G.state (Goal.goal held))
                        (G.state (Goal.goal (state entry))));
                    None
                  | Explicit_hold_before_sync ->
                    let before = state entry in
                    let handle = Option.value_exn entry.store_handle in
                    let metadata = Agent_store.Session_store.Handle.metadata handle in
                    let request =
                      command_request
                        entry
                        ~key:"goal-guard-explicit-hold-fault"
                        (Hold (Goal.goal_id, Budget_gate Admitted_attempts))
                    in
                    let catalog = Option.value_exn before.goal_catalog in
                    assert (Option.is_none (Fault.receipt catalog request));
                    fault.selected <- Some request;
                    let failure = send_command daemon entry request |> rejected in
                    primary := Some failure;
                    [%test_eq: P.Error.code] Persistence_error failure.code;
                    [%test_eq: int] 1 fault.hits;
                    let candidate = Option.value_exn fault.candidate in
                    assert (Option.is_some (Fault.receipt candidate request));
                    [%test_eq: int64]
                      (Int64.succ (A.Goal_catalog.revision catalog))
                      (A.Goal_catalog.revision candidate);
                    let candidate_goal =
                      A.Goal_catalog.find candidate Goal.goal_id |> Option.value_exn
                    in
                    assert (
                      G.equal
                        (G.hold (Goal.goal before) (Budget_gate Admitted_attempts)
                         |> protocol_ok)
                        candidate_goal);
                    let assert_confirmed () =
                      Existing.assert_same before entry;
                      assert_metadata_same
                        metadata
                        (Agent_store.Session_store.Handle.metadata handle);
                      assert (
                        Option.is_none
                          (Fault.receipt
                             (Option.value_exn (state entry).goal_catalog)
                             request));
                      [%test_eq: int] 0 !providers;
                      assert (Option.is_none (state entry).active_operation)
                    in
                    assert_confirmed ();
                    (* The failed control save still retires this pending startup
                   cell before any provider can consume its admitted handle. *)
                    [%test_eq: P.Error.code]
                      Conflict
                      (execution.checkpoint () |> rejected).code;
                    Some (failure, assert_confirmed)
                in
                release_first ();
                match faulted with
                | Some (failure, assert_confirmed) ->
                  (* Join only the real parser/Factory cleanup here. Its secondary
                  result cannot replace the already selected writer failure;
                  assertions remain outside this narrow exception capture. *)
                  let _joined =
                    match Eio.Promise.await_exn work with
                    | result -> Ok result
                    | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
                  in
                  assert_confirmed ();
                  let saved = row (state entry) scope accounting_id in
                  assert (Ledger.Handle.equal handle (Ledger.Row.handle saved));
                  (match Observation.Attempt_record.state (Ledger.Row.record saved) with
                   | Prepared -> ()
                   | Running | Terminal _ | Interrupted _ ->
                     failwith "uncertain control writer changed confirmed inference facts");
                  [%test_eq: int64]
                    1L
                    (A.Goal_accounting.admitted_attempts (Goal.accounting (state entry)));
                  [%test_eq: int64]
                    0L
                    (A.Goal_accounting.admitted_turns (Goal.accounting (state entry)));
                  let episode = G.episode (Goal.goal (state entry)) |> Option.value_exn in
                  assert (
                    P.Run_receipt.equal
                      (Option.value_exn !admission)
                      (P.Goal_episode.admission episode));
                  raise (Observed_explicit_hold_failure failure)
                | None
                  when match cut with
                       | Source_revoked_settlement_failure -> true
                       | _ -> false ->
                  let joined =
                    match Eio.Promise.await_exn work with
                    | result -> Ok result
                    | exception exn -> Error exn
                  in
                  (match joined with
                   | Error _ -> ()
                   | Ok (Error error) ->
                     [%test_eq: P.Error.code] Persistence_error error.code
                   | Ok (Ok outcome) ->
                     let attempt = Deferred_source_tests.attempted outcome in
                     Factory.Deferred_run_attempt.result attempt |> rejected |> ignore;
                     assert (
                       Option.is_none (Factory.Deferred_run_attempt.no_effects attempt)));
                  [%test_eq: int] 1 fault.hits;
                  [%test_eq: int] 0 !providers;
                  assert (Option.is_some fault.settlement_candidate);
                  let current = state entry in
                  let saved = row current scope accounting_id in
                  assert (Ledger.Handle.equal handle (Ledger.Row.handle saved));
                  (match Observation.Attempt_record.state (Ledger.Row.record saved) with
                   | Running -> ()
                   | Prepared | Terminal _ | Interrupted _ ->
                     failwith "failed settlement changed confirmed facts");
                  [%test_eq: int64]
                    1L
                    (A.Goal_accounting.admitted_attempts (Goal.accounting current));
                  [%test_eq: int64]
                    0L
                    (A.Goal_accounting.admitted_turns (Goal.accounting current));
                  (* The real writer is unavailable after this physical failure.
                     This assertion requests no replay from its uncertain head. *)
                  assert (Result.is_error (Actor.reconcile_inference_recovery entry.actor));
                  (* Preserve this observed physical failure over cleanup's
                     expected refusal to claim that the uncertain writer joined. *)
                  raise Observed_settlement_failure
                | None ->
                  let attempt =
                    Eio.Promise.await_exn work
                    |> protocol_ok
                    |> Deferred_source_tests.attempted
                  in
                  (* Fresh parser finalization/root admission may correctly reject
                after a hold. Its acknowledged provider facts remain truthful. *)
                  Factory.Deferred_run_attempt.result attempt |> rejected |> ignore;
                  assert (Option.is_none (Factory.Deferred_run_attempt.no_effects attempt));
                  let final = state entry in
                  let saved = row final scope accounting_id in
                  assert (Ledger.Handle.equal handle (Ledger.Row.handle saved));
                  [%test_eq: int64]
                    1L
                    (A.Goal_accounting.admitted_attempts (Goal.accounting final));
                  [%test_eq: int64]
                    0L
                    (A.Goal_accounting.admitted_turns (Goal.accounting final));
                  let retained = Option.value_exn !admission in
                  let episode = G.episode (Goal.goal final) |> Option.value_exn in
                  assert (P.Run_receipt.equal retained (P.Goal_episode.admission episode));
                  (match
                     cut, Observation.Attempt_record.state (Ledger.Row.record saved)
                   with
                   | Automatic_hold, Terminal terminal ->
                     [%test_eq: int] 1 !providers;
                     assert (
                       Inference.Event.Terminal.equal_outcome
                         Completed
                         (Inference.Event.Terminal.outcome terminal))
                   | ( (Explicit_same_hold | Source_revoked)
                     , Interrupted { delivery = Possibly_submitted; _ } ) ->
                     (* The real Attempt enters its backend with conservative
                        delivery before the adapter's final submission guard.
                        Factual Running ACKs remain valid after revocation; do
                        not turn the rejected guard into a delivery downgrade. *)
                     [%test_eq: int] 0 !providers
                   | _, _ ->
                     raise_s
                       [%sexp
                         "unexpected guarded parser outcome"
                       , (cut : Cut.t)
                       , (!providers : int)
                       , (Observation.Attempt_record.to_json (Ledger.Row.record saved)
                          : Jsonaf.t)])))))
   with
   | () -> assert (Option.is_none !primary)
   | exception Observed_settlement_failure -> ()
   | exception Observed_explicit_hold_failure failure ->
     assert_error (Option.value_exn !primary) failure;
     [%test_eq: P.Error.code] Persistence_error failure.code);
  match cut with
  | Automatic_hold | Explicit_same_hold | Source_revoked -> [%test_eq: int] 0 fault.hits
  | Explicit_hold_before_sync | Source_revoked_settlement_failure ->
    [%test_eq: int] 1 fault.hits
;;

let%expect_test
    "actual input registration survives automatic hold but not fresh explicit hold"
  =
  List.iter
    [ Cut.Automatic_hold; Explicit_same_hold; Explicit_hold_before_sync ]
    ~f:(fun cut ->
      try run cut with
      | exn ->
        Exn.reraise exn (Sexp.to_string_hum [%sexp "Goal admitted guard", (cut : Cut.t)]));
  print_endline
    "same admitted parser handle submits once; fresh work and explicit hold stay fenced";
  [%expect
    {| same admitted parser handle submits once; fresh work and explicit hold stay fenced |}]
;;

let%expect_test "retained Goal source settles facts after original authority is revoked" =
  List.iter [ Cut.Source_revoked; Source_revoked_settlement_failure ] ~f:run;
  print_endline
    "revoked source cannot submit; factual ACKs settle or retain the failed physical \
     writer";
  [%expect
    {| revoked source cannot submit; factual ACKs settle or retain the failed physical writer |}]
;;
