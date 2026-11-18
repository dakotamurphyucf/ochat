open! Core
open Agent_server_test_support
open Retry_support
module Service = Agent_server.Goal_run_service
module Goal = Goal_service_tests
module G = P.Goal

module Winner = struct
  type t =
    | Complete
    | Cancel
  [@@deriving sexp_of]
end

let finish_action : P.Run_action.t = Finish { terminal = Completed None; relinquish = [] }

let evidence =
  G.Reason.of_string "Actual authored Turn_end completion evidence" |> protocol_ok
;;

let authored_prompt ~idle_turns =
  let intent =
    P.Goal_achievement.Intent.create
      ~expected_goal_revision:1L
      ~evidence
      ~action:finish_action
    |> protocol_ok
    |> P.Goal_achievement.Intent.to_json
    |> Jsonaf.to_string
  in
  sprintf
    {|<developer>Actual authored Goal finish owner.</developer>
<script id="goal_finish" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Turn_end ->
  if state < %d then Task.pure(state + 1)
  else
  let intent = Json.parse(%S) in
  Task.map(Goal.finish(intent), fun ignored -> state + 1)
| _ -> Task.pure(state)
</script>|}
    idle_turns
    intent
;;

let prompt_text = authored_prompt ~idle_turns:0

let create_goal daemon entry =
  let before = state entry in
  let catalog = Option.value before.goal_catalog ~default:A.Goal_catalog.empty in
  let _, source = A.Goal_catalog_source.capture before |> protocol_ok in
  let budget =
    G.Budget.create
      ~admitted_turns:(Some 1L)
      ~admitted_attempts:(Some 1L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  let request =
    A.Goal_command.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~source:(Some source)
      ~expected_catalog_revision:(A.Goal_catalog.revision catalog)
      ~expected_goal_revision:None
      ~key:(P.Idempotency_key.of_string "goal-finish-create" |> protocol_ok)
      ~mutation:
        (Create
           { id = Goal.goal_id
           ; objective =
               G.Objective.of_string "Finish through actual authored scope" |> protocol_ok
           ; budget
           })
    |> protocol_ok
  in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~session_id:before.identity.session_id
  in
  Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
  |> protocol_ok
  |> ignore;
  let current = state entry in
  assert (G.State.equal (Open (Held Source_review)) (G.state (Goal.goal current)));
  assert (Option.is_none (G.episode (Goal.goal current)));
  [%test_eq: int64] 0L (G.revision (Goal.goal current));
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_attempts (Goal.accounting current));
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_turns (Goal.accounting current));
  session, current
;;

let episode_request (before : A.Session_state.t) ~text ~key =
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:(User_submission { kind = Plain_text; text; attachments = [] })
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  A.Goal_activation_request.create
    ~goal_id:Goal.goal_id
    ~expected_goal_revision:(G.revision (Goal.goal before))
    ~expected_catalog_revision:
      (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
    ~request
  |> protocol_ok
;;

let start_episode daemon entry session request =
  let service =
    Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
  in
  Service.start_episode service ~entry ~actor:original ~session ~request |> protocol_ok
;;

let terminal_receipt index admission =
  match
    List.filter (A.Run_state.receipts index) ~f:(fun receipt ->
      P.Id.Run.equal admission.P.Run_receipt.run_id receipt.P.Run_receipt.run_id
      && P.Run_receipt.Kind.equal Terminal receipt.kind)
  with
  | [ receipt ] -> receipt
  | _ -> failwith "Goal finish did not retain exactly one actual terminal receipt"
;;

let run (winner : Winner.t) =
  let armed = ref None in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let released = ref false
  and hits = ref 0 in
  let release_worker () =
    if not !released
    then (
      released := true;
      Eio.Promise.resolve release_u ())
  in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr counters.providers;
      counters.transcripts
      := !(counters.transcripts)
         @ [ `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string ];
      completed_response "actual Goal finish provider result")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~qualify_chatml_extensions:true
    ~policy
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_rename
        ~matches:(fun path ->
          Option.exists !armed ~f:(fun directory ->
            String.equal directory (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun path ->
          armed := None;
          Int.incr hits;
          Eio.Promise.resolve reached_u path;
          Eio.Promise.await release))
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
           protect_cleanup ~finally:release_worker (fun () ->
             let writer, _ =
               Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             let session, before = create_goal daemon entry in
             let request =
               episode_request
                 before
                 ~text:"542 authored Goal finish input"
                 ~key:"goal-finish-episode"
             in
             let call () = start_episode daemon entry session request in
             let handle = Option.value_exn entry.store_handle in
             let response_directory =
               Agent_store.Session_store.Handle.responses_directory handle
             in
             armed := Some response_directory;
             let admission = call () |> Goal.activated in
             let rec await_barrier () =
               match Eio.Promise.peek reached with
               | Some path -> path
               | None ->
                 let current = state entry in
                 List.iter current.moderator_executions ~f:(fun receipt ->
                   match receipt.P.Moderator_execution.status with
                   | Completed _ | Running -> ()
                   | Failed _ | Interrupted _ ->
                     raise_s
                       [%sexp
                         "Goal finish callback failed before its cache barrier"
                       , (receipt : P.Moderator_execution.t)]);
                 let selected =
                   A.Run_state.find (Option.value_exn current.run_state) admission.run_id
                   |> Option.value_exn
                 in
                 (match selected.lifecycle with
                  | Admitted | Active | Waiting _ -> ()
                  | Terminal _ ->
                    raise_s
                      [%sexp
                        "Goal finish Run ended before its cache barrier"
                      , (selected : P.Run.t)]);
                 Eio.Fiber.yield ();
                 await_barrier ()
             in
             let path =
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await_barrier
             in
             [%test_eq: string] response_directory (Filename.dirname path);
             [%test_eq: int] 1 !hits;
             (* run_turn has ACKed Turn_end/Goal.finish. The actual cache rename
               precedes Operation_worker.Completed, so root join cannot have won. *)
             let pending = state entry in
             let pending_goal = Goal.goal pending in
             assert (G.State.equal (Open Enabled) (G.state pending_goal));
             let achievement_request =
               G.achievement_request pending_goal |> Option.value_exn
             in
             assert (Option.is_none (G.request_settlement pending_goal));
             let operation = Option.value_exn pending.active_operation in
             let index = Option.value_exn pending.run_state in
             let selected = A.Run_state.find index admission.run_id |> Option.value_exn in
             (match selected.lifecycle with
              | Active -> ()
              | _ ->
                raise_s
                  [%sexp "Finish prematurely completed its Run", (selected : P.Run.t)]);
             assert (
               P.Goal_episode.equal
                 (P.Goal_achievement.Request.episode achievement_request)
                 (G.episode pending_goal |> Option.value_exn));
             let action_receipt =
               P.Goal_achievement.Request.receipt achievement_request
             in
             assert (P.Run_receipt.Kind.equal Action action_receipt.kind);
             assert (
               P.Run_action.equal
                 finish_action
                 (P.Goal_achievement.Request.action achievement_request));
             assert (
               G.Reason.equal
                 evidence
                 (P.Goal_achievement.Request.evidence achievement_request));
             assert (
               List.exists
                 (A.Run_state.receipts index)
                 ~f:(P.Run_receipt.equal action_receipt));
             assert (
               List.exists (A.Run_state.intents index) ~f:(fun intent ->
                 P.Run_receipt.equal action_receipt intent.A.Run_intent.receipt
                 && P.Run_action.equal finish_action intent.action));
             assert (
               not
                 (List.exists (A.Run_state.receipts index) ~f:(fun receipt ->
                    P.Id.Run.equal selected.id receipt.P.Run_receipt.run_id
                    && P.Run_receipt.Kind.equal Terminal receipt.kind)));
             [%test_eq: int] 1 !(counters.providers);
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_attempts (Goal.accounting pending));
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_turns (Goal.accounting pending));
             (match winner with
              | Complete -> ()
              | Cancel ->
                Actor.cancel_operation
                  entry.actor
                  ~attachment_id:writer.id
                  ~operation_id:operation.id
                |> protocol_ok
                |> ignore);
             release_worker ();
             let rec await_terminal () =
               let current =
                 Actor.with_moderator_checkpoint entry.actor (fun () ->
                   Actor.state entry.actor)
                 |> protocol_ok
               in
               let index = Option.value_exn current.run_state in
               let run = A.Run_state.find index admission.run_id |> Option.value_exn in
               match run.lifecycle with
               | Admitted | Active | Waiting _ ->
                 Eio.Fiber.yield ();
                 await_terminal ()
               | Terminal _ ->
                 assert (Option.is_none current.active_operation);
                 assert (Option.is_some (G.request_settlement (Goal.goal current)));
                 current, index, run
             in
             let final, index, selected =
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await_terminal
             in
             let final_goal = Goal.goal final in
             let terminal = terminal_receipt index admission in
             assert (
               P.Goal_episode.equal
                 (G.episode pending_goal |> Option.value_exn)
                 (G.episode final_goal |> Option.value_exn));
             assert (
               P.Goal_achievement.Request.equal
                 achievement_request
                 (G.achievement_request final_goal |> Option.value_exn));
             let settlement = G.request_settlement final_goal |> Option.value_exn in
             assert (
               P.Run_receipt.equal terminal (P.Goal_episode.Settlement.receipt settlement));
             (match winner, selected.lifecycle, G.state final_goal with
              | Complete, Terminal (Completed None), Closed (Achieved achievement) ->
                assert (
                  P.Goal_achievement.Request.equal
                    achievement_request
                    (P.Goal_achievement.request achievement));
                assert (
                  P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement))
              | Cancel, Terminal Cancelled, Open (Held _) -> ()
              | _ ->
                raise_s
                  [%sexp
                    "actual Goal terminal winner changed"
                  , (winner : Winner.t)
                  , (selected : P.Run.t)
                  , (G.state final_goal : G.State.t)]);
             A.Goal_run_evidence.validate
               ~catalog:final.goal_catalog
               ~runs:final.run_state
             |> protocol_ok;
             List.iter
               P.Run_receipt.Kind.[ Admission; Action; Terminal ]
               ~f:(fun kind ->
                 [%test_eq: int]
                   1
                   (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                      P.Id.Run.equal selected.id receipt.P.Run_receipt.run_id
                      && P.Run_receipt.Kind.equal kind receipt.kind)));
             [%test_eq: int] 1 !(counters.providers);
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_attempts (Goal.accounting final));
             [%test_eq: int64]
               1L
               (A.Goal_accounting.admitted_turns (Goal.accounting final));
             let frames = Existing.frames env entry in
             let counts = !(counters.providers), !(counters.inference_ports) in
             (match call () with
              | Retained actual -> assert (P.Run_receipt.equal admission actual)
              | Attempted _ -> failwith "finished Goal replay acquired a new attempt");
             Existing.assert_same final entry;
             [%test_eq: string list] frames (Existing.frames env entry);
             [%test_eq: int * int]
               counts
               (!(counters.providers), !(counters.inference_ports));
             [%test_eq: int] 1 !hits;
             print_s [%sexp (winner : Winner.t)]))))
;;

let%expect_test "authored Goal.finish remains pending until the actual terminal winner" =
  List.iter Winner.[ Complete; Cancel ] ~f:run;
  [%expect
    {|
    Complete
    Cancel
  |}]
;;

module Checkpoint_fault = struct
  type candidate =
    { execution : P.Moderator_execution.t
    ; request : P.Goal_achievement.Request.t
    }

  type t =
    { mutable selected_session : P.Id.Session.t option
    ; mutable candidate : candidate option
    }

  let create () = { selected_session = None; candidate = None }

  let select changes =
    if
      not
        (List.exists changes ~f:(function
           | A.Session_delta.Moderator_changed (Some _) -> true
           | _ -> false))
    then None
    else
      let open Option.Let_syntax in
      let%bind execution =
        List.find_map changes ~f:(function
          | A.Session_delta.Moderator_execution_changed
              ({ context = { phase = Turn_end; _ }; status = Completed _; _ } as execution)
            -> Some execution
          | _ -> None)
      in
      let%bind request =
        List.find_map changes ~f:(function
          | A.Session_delta.Goal_catalog_changed catalog ->
            A.Goal_catalog.find catalog Goal.goal_id
            |> Option.bind ~f:G.achievement_request
          | _ -> None)
      in
      let receipt = P.Goal_achievement.Request.receipt request in
      let%map () =
        List.find_map changes ~f:(function
          | A.Session_delta.Run_state_changed index
            when List.exists
                   (A.Run_state.intents index)
                   ~f:(fun (intent : A.Run_intent.t) ->
                     P.Id.Moderator_execution.equal
                       execution.context.id
                       intent.execution_id
                     && P.Run_receipt.equal receipt intent.receipt
                     && P.Run_action.equal finish_action intent.action) -> Some ()
          | _ -> None)
      in
      { execution; request }
  ;;

  let reached t env path =
    Option.iter t.selected_session ~f:(fun session_id ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      match List.last scan.entries with
      | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
        let transaction =
          Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
          |> Existing.store_ok
        in
        if P.Id.Session.equal transaction.session_id session_id
        then (
          let delta =
            A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
            |> document_ok
            |> A.Session_delta_document.value
          in
          Option.iter
            (select (Goal_admission_fault_tests.atoms delta))
            ~f:(fun candidate ->
              t.selected_session <- None;
              t.candidate <- Some candidate;
              (* The actual callback bytes are already synced. The old Actor must
               still treat this checkpoint as unacknowledged. *)
              raise (Core_unix.Unix_error (EIO, "491 Goal.finish checkpoint", path))))
      | Some _ | None -> ())
  ;;
end

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec wait () =
      match f () with
      | Some value -> value
      | None ->
        Eio.Fiber.yield ();
        wait ()
    in
    wait ())
;;

let unrelated_completed_turn env entry counters =
  let writer, _ =
    Actor.attach entry.Registry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
  in
  let input =
    Owner.parse_user_content
      entry.runtime
      ~id:(A.History_id_source.allocate entry.history_ids |> protocol_ok)
      { kind = Plain_text
      ; text = "491 ordinary turn before Goal.finish"
      ; attachments = []
      }
    |> protocol_ok
    |> A.History_codec.to_protocol
  in
  let submission =
    Actor.submit_message_authenticated
      entry.actor
      ~actor:original
      ~attachment_id:writer.id
      input
    |> protocol_ok
  in
  let operation = Option.value_exn submission.operation_id in
  await env (fun () ->
    let current = state entry in
    Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
    let completed =
      List.exists
        current.moderator_executions
        ~f:(fun (execution : P.Moderator_execution.t) ->
          match execution.context.phase, execution.status with
          | Turn_end, Completed _ ->
            Option.exists
              execution.context.operation_id
              ~f:(P.Id.Operation.equal operation)
          | _ -> false)
    in
    if completed && Option.is_none current.active_operation && !(counters.providers) = 1
    then (
      match A.Inference_ledger.rows current.inference_ledger with
      | [ row ] -> Some row
      | _ -> failwith "unrelated genuine inference row was not retained")
    else None)
;;

let assert_unrelated_retained (current : A.Session_state.t) previous =
  let previous_handle = A.Inference_ledger.Row.handle previous in
  let matches =
    List.filter (A.Inference_ledger.rows current.inference_ledger) ~f:(fun row ->
      A.Inference_ledger.Handle.equal previous_handle (A.Inference_ledger.Row.handle row))
  in
  match matches with
  | [ row ] ->
    let json row =
      Inference.Observation.Attempt_record.to_json (A.Inference_ledger.Row.record row)
    in
    assert (Jsonaf.exactly_equal (json previous) (json row))
  | _ -> failwith "unrelated genuine inference row was lost or copied"
;;

let checkpoint_request before =
  episode_request
    before
    ~text:"491 Goal.finish checkpoint input"
    ~key:"goal-finish-checkpoint"
;;

let check_retained_checkpoint env daemon (entry : Registry.entry) old =
  let module Report = Agent_server.Session_disposal_report in
  let store = Daemon.store daemon in
  let handle = Option.value_exn entry.store_handle in
  let expected =
    P.Error.create
      Persistence_error
      ~message:
        (Sexp.to_string_hum
           ([%sexp_of: Agent_store.Store_error.t]
              (Corrupt
                 "canonical projection target cannot regress or alter equal-revision \
                  hints")))
      ~retryable:false
      ()
  in
  let bytes () = Uncertain_writer_test_support.physical_inventory entry ~env in
  let before = bytes () in
  let report = Registry.close_entry_reported entry in
  assert (not (Report.disposed report));
  assert (
    List.exists (Report.failures report) ~f:(function
      | Protocol failure ->
        Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json failure)
      | Raised _ -> false));
  [%test_eq: (string * string) list * (string * string) list] before (bytes ());
  Existing.assert_same old entry;
  assert (not (Agent_store.Session_store.is_closed store));
  assert (Agent_store.Session_store.owns_handle store handle);
  assert (Result.is_ok (Agent_store.Session_store.Handle.metadata_checked handle))
;;

let run_child root =
  let fault = Checkpoint_fault.create () in
  with_recoverable_fixture
    ~test_root:root
    ~prompt_text:(authored_prompt ~idle_turns:1)
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:After_sync
        ~matches:(fun path ->
          String.is_substring path ~substring:"/journal/"
          && String.is_suffix path ~suffix:".log")
        ~reached:(Checkpoint_fault.reached fault env))
    (fun env _sw _root daemon _policy entry _runtime _revision counters ->
       let unrelated = unrelated_completed_turn env entry counters in
       let session, before = create_goal daemon entry in
       let request = checkpoint_request before in
       fault.selected_session <- Some before.identity.session_id;
       let admission = start_episode daemon entry session request |> Goal.activated in
       let candidate =
         await env (fun () ->
           match fault.candidate with
           | Some _ as observed -> observed
           | None ->
             let current = state entry in
             Option.iter current.failure ~f:(fun error ->
               raise_s [%sexp (error : P.Error.t)]);
             let index = Option.value_exn current.run_state in
             let run = A.Run_state.find index admission.run_id |> Option.value_exn in
             (match run.lifecycle with
              | Admitted | Active | Waiting _ -> None
              | Terminal _ ->
                raise_s
                  [%sexp
                    "Run ended without reaching its own Finish checkpoint"
                  , (run : P.Run.t)]))
       in
       let old = state entry in
       let old_goal = Goal.goal old in
       assert (G.State.equal (Open Enabled) (G.state old_goal));
       assert (Option.is_none (G.achievement_request old_goal));
       assert (Option.is_none (G.request_settlement old_goal));
       assert (
         not
           (List.exists
              old.moderator_executions
              ~f:(P.Moderator_execution.equal candidate.execution)));
       let old_index = Option.value_exn old.run_state in
       let action = P.Goal_achievement.Request.receipt candidate.request in
       assert (P.Id.Run.equal admission.run_id action.run_id);
       assert (P.Run_receipt.Kind.equal Action action.kind);
       assert (
         G.Reason.equal evidence (P.Goal_achievement.Request.evidence candidate.request));
       assert (
         P.Run_action.equal
           finish_action
           (P.Goal_achievement.Request.action candidate.request));
       assert (
         not
           (List.exists (A.Run_state.receipts old_index) ~f:(P.Run_receipt.equal action)));
       [%test_eq: int] 2 !(counters.providers);
       assert_unrelated_retained old unrelated;
       check_retained_checkpoint env daemon entry old;
       let report =
         `Object
           [ "before", Legacy_idle_completion_fault_tests.state_json before
           ; "admission", P.Run_receipt.to_json admission
           ; "execution", P.Moderator_execution.to_json candidate.execution
           ; "achievement", P.Goal_achievement.Request.to_json candidate.request
           ]
       in
       (* The runtime still owns unacknowledged callback retirement. Only this
          child exits immediately, so the parent reopens after real process loss. *)
       Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout env);
       Core_unix.exit_immediately 73)
;;

let recover_finish env sw root report =
  let member name = Jsonaf.member_exn name report in
  let before = Callback_retirement_fault_tests.decode_state (member "before") in
  let admission = member "admission" |> P.Run_receipt.of_json |> protocol_ok in
  let execution = member "execution" |> P.Moderator_execution.of_json |> protocol_ok in
  let achievement =
    member "achievement" |> P.Goal_achievement.Request.of_json |> protocol_ok
  in
  let action = P.Goal_achievement.Request.receipt achievement in
  let unrelated = List.hd_exn (A.Inference_ledger.rows before.inference_ledger) in
  let request = checkpoint_request before in
  let providers = ref 0
  and inference_ports = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      completed_response "unexpected recovered Goal.finish provider")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  let recovered =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  let session_id = before.identity.session_id in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store recovered))
      ~session_id
  in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () ->
       let loaded = Registry.load (Daemon.registry recovered) session_id |> protocol_ok in
       let current = Retry_rotation_tests.await_recovered loaded before in
       let goal = Goal.goal current in
       let index = Option.value_exn current.run_state in
       let run = A.Run_state.find index admission.run_id |> Option.value_exn in
       (match run.lifecycle, G.state goal with
        | Terminal Interrupted, Open (Held _) -> ()
        | _ ->
          raise_s
            [%sexp
              "unacknowledged Goal.finish became a success"
            , (run : P.Run.t)
            , (G.state goal : G.State.t)]);
       assert (
         Option.exists
           (G.achievement_request goal)
           ~f:(P.Goal_achievement.Request.equal achievement));
       assert (List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal action));
       let settlement = G.request_settlement goal |> Option.value_exn in
       assert (
         P.Run_receipt.equal
           (terminal_receipt index admission)
           (P.Goal_episode.Settlement.receipt settlement));
       assert (
         List.exists
           current.moderator_executions
           ~f:(P.Moderator_execution.equal execution));
       assert_unrelated_retained current unrelated;
       A.Goal_run_evidence.validate ~catalog:current.goal_catalog ~runs:current.run_state
       |> protocol_ok;
       [%test_eq: int64]
         1L
         (A.Goal_accounting.admitted_attempts (Goal.accounting current));
       let frames = Existing.frames env loaded in
       let ports_before = !inference_ports in
       (match start_episode recovered loaded session request with
        | Retained actual -> assert (P.Run_receipt.equal admission actual)
        | Attempted _ -> failwith "Goal.finish recovery retried the original worker");
       [%test_eq: int] 0 !providers;
       [%test_eq: int] ports_before !inference_ports;
       Existing.assert_same current loaded;
       [%test_eq: string list] frames (Existing.frames env loaded))
;;

let%expect_test "authored Goal.finish checkpoint failure preserves its pending evidence" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
             let report, diagnostics =
               Callback_retirement_fault_tests.completed_child_output
                 ~mode:"goal-finish-checkpoint-unknown"
                 env
                 root
             in
             [%test_eq: string]
               "+queued moderator cleanup failed: (Corrupt\n\
                + \"canonical projection target cannot regress or alter equal-revision \
                hints\")"
               (String.strip diagnostics);
             recover_finish env sw root (Jsonaf.of_string report)))));
  print_endline "unacknowledged Finish reopened with adverse settlement and no replay";
  [%expect {| unacknowledged Finish reopened with adverse settlement and no replay |}]
;;
