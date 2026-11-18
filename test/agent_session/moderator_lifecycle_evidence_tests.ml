open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module E = P.Moderator_execution
module Actor = A.Session_actor
module M = Chat_response.Moderator_manager
module Lifecycle = A.Moderator_event.Lifecycle
module Store = Agent_store
module Persistence = A.Session_persistence

(* A failing observation must release its native gate before either owner is
   joined. Secondary teardown failures remain visible without replacing the
   original failure or its raw backtrace. *)
let with_cleanup ~finally f =
  let capture f =
    try Ok (f ()) with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let outcome = capture f in
  let cleanup = capture (fun () -> Eio.Cancel.protect finally) in
  match outcome, cleanup with
  | Ok value, Ok () -> value
  | Error (exn, backtrace), Ok () | Ok _, Error (exn, backtrace) ->
    Exn.raise_with_original_backtrace exn backtrace
  | Error (exn, backtrace), Error (secondary, secondary_backtrace) ->
    (try
       Eio.traceln
         "lifecycle fixture secondary cleanup failure: %s\n%s"
         (Exn.to_string secondary)
         (Stdlib.Printexc.raw_backtrace_to_string secondary_backtrace)
     with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let index state = Option.value_exn state.A.Session_state.run_state

let current_source state =
  A.Run_source_installation.captured
    (A.Run_state.installation (index state))
    ~generation:state.A.Session_state.identity.generation
  |> protocol_ok
;;

let selected state =
  let capture = A.Run_state.moderator_lifecycle (index state) |> Option.value_exn in
  assert (P.Run_source.equal (A.Moderator_lifecycle.source capture) (current_source state));
  List.find_exn state.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Id.Moderator_execution.equal
      receipt.E.context.id
      (A.Moderator_lifecycle.execution_id capture))
;;

let assert_selected state expected =
  assert (E.equal expected (selected state));
  A.Session_state.validate state |> protocol_ok
;;

let lifecycle_tools actor registry =
  A.Script_tool_calls.create
    ~registry:(fun () -> registry)
    ~moderator_names:String.Set.empty
    ~now:P.Timestamp.now
    ~is_halted:(fun () -> (Actor.state actor |> protocol_ok).halted)
    ~requires_active_moderator:(fun _ -> false)
    ~authorize:(fun _ _ -> Ok ())
    ~prepare_output:(fun _ -> Ok (`String "disclosed"))
    ~defer_observation:(fun _ -> Ok ())
;;

let run_lifecycle actor tools lifecycle =
  Lifecycle.run
    lifecycle
    ~claim:(fun ~event ->
      Actor.with_current_moderator_event actor ~operation_id:None ~event)
    ~script_tools:tools
    ~history:(fun () ->
      (Actor.state actor |> protocol_ok).conversation.canonical_history
      |> A.History_codec.all_of_protocol
      |> protocol_ok)
    ~available_tools:[]
    ~session_meta:`Null
    ~now:P.Timestamp.now
    ()
;;

let%expect_test
    "latest lifecycle claim survives terminal prewriter failure and physical reopen"
  =
  with_actor_workspace (fun env workspace ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      let calls = ref 0 in
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let release_native () =
        if not (Eio.Promise.is_resolved release) then Eio.Promise.resolve release_u ()
      in
      let registry =
        native_registry calls ~raises:false ~on_call:(fun () ->
          if Int.equal !calls 2
          then (
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release))
      in
      let body =
        "Task.bind(Tool.call(\"read_file\", `Object([])), fun result -> match result \
         with | `Error(code) -> Task.fail(code) | `Ok(value) -> let ignored = state[0] \
         <- state[0] + 1 in Task.pure(state))"
      in
      let manager, _, _ =
        handoff_definition
          env
          ~declare_tool:false
          ~capability_registry:registry
          ~events:
            ("| `Session_start -> "
             ^ body
             ^ " | `Session_resume -> "
             ^ body
             ^ " | _ -> Task.pure(state)")
      in
      let initial =
        actor_state
          ~workspace_instance:workspace
          ~liveness:Detached
          ~start_immediately:false
      in
      let initial =
        { initial with
          moderator =
            Some
              (A.Moderator_checkpoint.encode
                 (M.identity_snapshot manager |> Result.ok_or_failwith))
        }
      in
      let directory =
        Filename.concat workspace.canonical_root.native_path "lifecycle-journal"
      in
      let snapshots =
        Filename.concat workspace.canonical_root.native_path "lifecycle-snapshots"
      in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
      let final, final_hash, first, failed, source =
        Eio.Switch.run (fun sw ->
          let journal =
            Store.Journal.create
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let writer =
            Store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:8
            |> store_ok
          in
          let durable =
            Run_persistence_tests.create_persistence
              writer
              (Persistence.Restored.authored initial)
              ~previous_transaction_hash:None
          in
          let reject_id = ref None in
          let rejected = ref 0 in
          let persistence : Actor.persistence =
            { archive_reference
            ; commit =
                (fun ?observation ~command_audit ~previous next ->
                  let reject =
                    Option.value_map !reject_id ~default:false ~f:(fun id ->
                      List.exists
                        next.A.Session_transition.state.moderator_executions
                        ~f:(fun receipt ->
                          P.Id.Moderator_execution.equal id receipt.E.context.id
                          &&
                          match receipt.status with
                          | Completed _ -> true
                          | Running | Failed _ | Interrupted _ -> false))
                  in
                  if reject && Int.equal !rejected 0
                  then (
                    Int.incr rejected;
                    let open Result.Let_syntax in
                    let%bind () =
                      Option.value_map observation ~default:(Ok ()) ~f:(fun observation ->
                        A.Session_commit_observation.Host.observe_entry
                          observation
                          ~previous
                          ~candidate:next
                        |> Result.map ~f:ignore)
                    in
                    Error
                      (P.Error.create
                         Persistence_error
                         ~message:"fixture rejected exact resume terminal before storage"
                         ~retryable:false
                         ()))
                  else
                    Persistence.commit durable ?observation ~command_audit ~previous next)
            }
          in
          let actor = Run_persistence_tests.create_actor env sw initial persistence in
          with_cleanup
            ~finally:(fun () ->
              release_native ();
              with_cleanup
                ~finally:(fun () -> Store.Commit_writer.close writer)
                (fun () -> Actor.shutdown actor))
            (fun () ->
               let attachment, _ =
                 Actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
               in
               Actor.start actor ~attachment_id:attachment.id |> protocol_ok |> ignore;
               let tools = lifecycle_tools actor registry in
               (match
                  run_lifecycle actor tools (Lifecycle.create ~manager ~resume:false)
                  |> protocol_ok
                with
                | Activated _ -> ()
                | Unavailable | Already_active -> failwith "fresh start did not activate");
               let completed = Actor.state actor |> protocol_ok in
               let first = selected completed in
               (match first.status with
                | Completed _ -> ()
                | Running | Failed _ | Interrupted _ -> failwith "start was not completed");
               assert_selected completed first;
               [%test_eq: int] 1 !calls;
               let source = current_source completed in
               let result, running =
                 Eio.Fiber.pair
                   (fun () ->
                      run_lifecycle actor tools (Lifecycle.create ~manager ~resume:true))
                   (fun () ->
                      Exn.protect ~finally:release_native ~f:(fun () ->
                        Eio.Promise.await entered;
                        let running = Actor.state actor |> protocol_ok in
                        let latest = selected running in
                        assert (E.equal_status latest.status Running);
                        assert (
                          not
                            (P.Id.Moderator_execution.equal
                               first.context.id
                               latest.context.id));
                        assert (
                          List.exists running.moderator_executions ~f:(E.equal first));
                        assert (P.Run_source.equal source (current_source running));
                        assert_selected running latest;
                        reject_id := Some latest.context.id;
                        [%test_eq: int] 2 !calls;
                        latest))
               in
               assert (Result.is_error result);
               let final = Actor.state actor |> protocol_ok in
               let failed = selected final in
               assert (P.Id.Moderator_execution.equal failed.context.id running.context.id);
               (match failed.status with
                | Failed _ -> ()
                | Running | Completed _ | Interrupted _ ->
                  failwith "resume failure was not retained");
               assert_selected final failed;
               assert (List.exists final.moderator_executions ~f:(E.equal first));
               assert (P.Run_source.equal source (current_source final));
               assert_same_session_snapshot
                 final
                 (Persistence.Restored.state (Persistence.restored durable));
               [%test_eq: int] 2 !calls;
               [%test_eq: int] 1 !rejected;
               let snapshot =
                 A.Moderator_checkpoint.decode final.moderator
                 |> protocol_ok
                 |> Option.value_exn
               in
               (match snapshot.current_state with
                | Session.Snapshot.Array [ Int 1 ] -> ()
                | _ -> failwith "rejected terminal changed the checkpoint");
               let final_hash = Persistence.transaction_hash durable in
               Actor.checkpoint actor ~persist:(fun state ->
                 Persistence.install_snapshot_at
                   durable
                   ~env
                   ~directory:snapshots
                   ~max_payload_length:1048576
                   ~transaction_hash:final_hash
                   state
                 |> store_ok
                 |> ignore;
                 Ok ())
               |> protocol_ok;
               final, final_hash, first, failed, source))
      in
      (* No old actor or writer remains when the physical journal is reopened. *)
      let journal =
        Store.Journal.open_existing
          ~env
          ~directory
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:16
        |> store_ok
      in
      let recovered =
        Store.Recovery.load
          ~env
          ~journal
          ~snapshot_directory:snapshots
          ~max_snapshot_payload_length:1048576
          ~session_id
          ~initial:(Persistence.Restored.authored initial)
          ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
          ~apply:(Persistence.apply_transaction ~limits:document_limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let reopened = Persistence.Restored.state recovered.state in
      assert_same_session_snapshot final reopened;
      assert_selected reopened failed;
      assert (List.exists reopened.moderator_executions ~f:(E.equal first));
      assert (P.Run_source.equal source (current_source reopened));
      [%test_eq: string option] final_hash recovered.latest_transaction_hash;
      [%test_eq: int] 2 !calls;
      print_endline "latest Running/Failed survives physical reopen; native calls=2"));
  [%expect {| latest Running/Failed survives physical reopen; native calls=2 |}]
;;

(* The reference is an admission history, not a second mutable pointer. Terminal
   updates do not enter this log; the last admission is either captured or old
   uncaptured data. An installation boundary starts a new history. *)
module Admission = struct
  type t =
    | Captured of P.Id.Moderator_execution.t
    | Historical

  let latest = function
    | [] -> None
    | history ->
      (match List.last_exn history with
       | Captured id -> Some id
       | Historical -> None)
  ;;
end

let apply state delta =
  A.Session_delta.apply ~limits:document_limits state delta |> protocol_ok
;;

let assert_history state history =
  let actual =
    Option.bind state.A.Session_state.run_state ~f:(fun index ->
      Option.map
        (A.Run_state.moderator_lifecycle index)
        ~f:A.Moderator_lifecycle.execution_id)
  in
  assert (Option.equal P.Id.Moderator_execution.equal actual (Admission.latest history));
  A.Session_state.validate state |> protocol_ok
;;

let make_receipt state name phase =
  let source = current_source state in
  let snapshot =
    A.Moderator_checkpoint.decode state.A.Session_state.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  E.create
    { id = P.Id.Moderator_execution.of_string name |> protocol_ok
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; source = source.observer
    ; operation_id = None
    ; job = None
    ; phase
    ; event = `Null
    ; checkpoint_sha256 = A.Moderator_checkpoint.digest snapshot
    ; created_at = timestamp
    }
  |> protocol_ok
;;

let paired state receipt =
  let capture =
    A.Moderator_lifecycle.create
      ~source:(current_source state)
      ~execution_id:receipt.E.context.id
    |> protocol_ok
  in
  A.Session_delta.Moderator_lifecycle_claimed { receipt; capture }
;;

let complete receipt =
  E.complete
    receipt
    ~checkpoint_sha256:receipt.E.context.checkpoint_sha256
    ~requests:{ request_turn = false; request_compaction = false; end_session = None }
  |> protocol_ok
;;

let terminal receipt = function
  | `Completed -> complete receipt
  | `Failed ->
    E.fail
      receipt
      { code = "fixture.failed"
      ; message = "bounded model"
      ; details = `Null
      ; retryable = false
      }
    |> protocol_ok
  | `Interrupted -> E.interrupt receipt ~reason:"bounded model" |> protocol_ok
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected lifecycle fixture object"
;;

let remove json name =
  match json with
  | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal name)
  | _ -> failwith "expected lifecycle fixture object"
;;

let field json name =
  match Document_schema.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith "expected lifecycle fixture field"
;;

let decode_state ~required_semantics payload =
  Document_schema.Document.create
    ~limits:document_limits
    ~kind:"session.state"
    ~version:A.Session_state_document.current_version
    ~payload
  |> document_ok
  |> fun document ->
  set
    (Document_schema.Document.json document)
    "required_semantics"
    (`Array (List.map required_semantics ~f:(fun name -> `String name)))
  |> Document_schema.Document.inspect ~limits:document_limits
  |> document_ok
  |> A.Session_state_document.decode ~limits:document_limits
;;

let encode_state document =
  A.Session_state_document.encode document ~limits:document_limits
  |> document_ok
  |> Document_schema.Document.payload
;;

let decoded_basis state =
  state_document state
  |> A.Session_state_document.decode ~limits:document_limits
  |> document_ok
;;

let%expect_test
    "lifecycle evidence follows chronological claims and installation boundaries"
  =
  with_actor_workspace (fun env workspace ->
    let manager, _, _ = handoff_definition env ~declare_tool:false in
    let moderator =
      M.identity_snapshot manager
      |> Result.ok_or_failwith
      |> A.Moderator_checkpoint.encode
    in
    let observer =
      A.Moderator_checkpoint.observer (Some moderator) |> protocol_ok |> Option.value_exn
    in
    let installation =
      A.Run_source_installation.apply
        A.Run_source_installation.initial
        ~change:(Replace observer)
      |> protocol_ok
    in
    let installed =
      A.Run_state.replace_installation A.Run_state.empty ~installation ~retired_runs:[]
      |> protocol_ok
    in
    let initial =
      actor_state
        ~workspace_instance:workspace
        ~liveness:Detached
        ~start_immediately:false
    in
    let history_entry sequence text =
      let id =
        History_entry.Id.create ~namespace:(P.Id.Session.to_string session_id) ~sequence
        |> Result.ok_or_failwith
      in
      A.History_codec.user_text ~id text |> A.History_codec.to_protocol
    in
    let retained_entry = history_entry 0 "retained lifecycle prefix" in
    let retired_entry = history_entry 1 "retired lifecycle suffix" in
    let initial =
      { initial with
        moderator = Some moderator
      ; run_state = Some installed
      ; conversation =
          { initial.conversation with
            canonical_history = [ retained_entry; retired_entry ]
          ; next_history_sequence = 2L
          ; reserved_history_through = 2L
          }
      }
    in
    (* Successful typed admission advances its native transition basis without
       discarding the original raw template. Chaining must not require a
       decode/encode round trip to accept the same claim's terminal update. *)
    let chained = make_receipt initial "mex_evidence_chained" Session_start in
    let claimed =
      A.Session_transition.apply
        ~now:timestamp
        initial
        ~delta:(paired initial chained)
        ~payloads:[]
      |> protocol_ok
    in
    let admitted_claim =
      A.Session_document_transition.admit_encoded
        (decoded_basis initial)
        ~delta:claimed.delta
        ~next:claimed.state
        ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.Admitted.state_document
    in
    let chained_terminal = complete chained in
    let completed =
      A.Session_transition.apply
        ~now:timestamp
        claimed.state
        ~delta:(Moderator_execution_changed chained_terminal)
        ~payloads:[]
      |> protocol_ok
    in
    let admitted_terminal =
      A.Session_document_transition.admit_encoded
        admitted_claim
        ~delta:completed.delta
        ~next:completed.state
        ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.Admitted.state_document
    in
    assert_selected (A.Session_state_document.value admitted_terminal) chained_terminal;
    (* Exercise the public destructive carrier transfer. Its returned owner must
       retain the successful claim basis as well as the old raw preservation
       template, so the next encode does not misclassify Completed as a new claim. *)
    let admitted_terminal =
      A.Session_state_document.retire_canonical_suffix
        admitted_terminal
        ~retained_ids:[ retained_entry.id ]
        ~limits:document_limits
      |> document_ok
    in
    let retired_state = A.Session_state_document.value admitted_terminal in
    assert_selected retired_state chained_terminal;
    assert (
      List.equal
        P.History.equal_entry
        [ retained_entry ]
        retired_state.conversation.canonical_history);
    let retired_payload = encode_state admitted_terminal in
    assert (
      Jsonaf.exactly_equal
        (field (field retired_payload "run_state") "moderator_lifecycle")
        (A.Moderator_lifecycle.to_json
           (A.Run_state.moderator_lifecycle (index retired_state) |> Option.value_exn)));
    let terminal_basis = A.Session_state_document.value admitted_terminal in
    let uncaptured =
      make_receipt terminal_basis "mex_evidence_chained_legacy" Session_resume
    in
    let uncaptured_claim =
      A.Session_transition.apply
        ~now:timestamp
        terminal_basis
        ~delta:(Moderator_execution_changed uncaptured)
        ~payloads:[]
      |> protocol_ok
    in
    let admitted_uncaptured =
      A.Session_document_transition.admit_encoded
        admitted_terminal
        ~delta:uncaptured_claim.delta
        ~next:uncaptured_claim.state
        ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.Admitted.state_document
    in
    assert_history
      (A.Session_state_document.value admitted_uncaptured)
      [ Admission.Historical ];
    let uncaptured_payload = encode_state admitted_uncaptured in
    (match
       Document_schema.Json.field
         (field uncaptured_payload "run_state")
         ~name:"moderator_lifecycle"
     with
     | Null -> ()
     | Absent | Value _ -> failwith "uncaptured new claim did not retain explicit null");
    let traces = ref 0 in
    List.iter [ `Completed; `Failed; `Interrupted ] ~f:(fun ending ->
      let first = make_receipt initial "mex_evidence_first" Session_start in
      let first_running = apply initial (paired initial first) in
      let first_done = complete first in
      let first_completed =
        apply first_running (Moderator_execution_changed first_done)
      in
      let history = [ Admission.Captured first.context.id ] in
      assert_history first_completed history;
      let latest = make_receipt first_completed "mex_evidence_latest" Session_resume in
      let running = apply first_completed (paired first_completed latest) in
      let history = history @ [ Admission.Captured latest.context.id ] in
      assert_history running history;
      assert_selected running latest;
      let ended = terminal latest ending in
      let finished = apply running (Moderator_execution_changed ended) in
      assert_history finished history;
      assert_selected finished ended;
      assert (List.exists finished.moderator_executions ~f:(E.equal first_done));
      let without_private = A.Run_state.clear_moderator_lifecycle (index finished) in
      assert (A.Run_state.equal_observation (index finished) without_private);
      assert (not (A.Run_state.equal (index finished) without_private));
      (* Re-admitting an existing ID or admitting a terminal receipt cannot
         create a newer installation claim. Neither is an Actor ACK. *)
      assert (Result.is_error (A.Session_delta.apply running (paired running latest)));
      let new_terminal =
        make_receipt finished "mex_evidence_terminal" Session_resume |> complete
      in
      assert (
        Result.is_error (A.Session_delta.apply finished (paired finished new_terminal)));
      let terminal_capture =
        A.Moderator_lifecycle.create
          ~source:(current_source finished)
          ~execution_id:new_terminal.context.id
        |> protocol_ok
      in
      let forged_terminal =
        { finished with
          moderator_executions = new_terminal :: finished.moderator_executions
        ; run_state =
            Some
              (A.Run_state.capture_moderator_lifecycle (index finished) terminal_capture
               |> protocol_ok)
        }
      in
      assert (
        Result.is_error
          (A.Session_state_document.encode
             (A.Session_state_document.with_value
                (decoded_basis finished)
                forged_terminal)
             ~limits:document_limits));
      let future_source =
        P.Run_source.create
          ~observer:(current_source finished).observer
          ~generation:finished.identity.generation
          ~installation_epoch:(Int64.succ (current_source finished).installation_epoch)
        |> protocol_ok
      in
      let foreign_capture =
        A.Moderator_lifecycle.create
          ~source:future_source
          ~execution_id:new_terminal.context.id
        |> protocol_ok
      in
      let fresh = make_receipt finished "mex_evidence_wrong_source" Session_resume in
      let wrong_source =
        A.Session_delta.Moderator_lifecycle_claimed
          { receipt = fresh
          ; capture =
              A.Moderator_lifecycle.create
                ~source:future_source
                ~execution_id:fresh.context.id
              |> protocol_ok
          }
      in
      assert (Result.is_error (A.Session_delta.apply finished wrong_source));
      assert (
        Result.is_error
          (A.Moderator_lifecycle.validate
             foreign_capture
             ~session_id:finished.identity.session_id
             ~generation:finished.identity.generation
             ~current_source:(current_source finished)
             ~executions:(new_terminal :: finished.moderator_executions)));
      let legacy = make_receipt finished "mex_evidence_legacy" Session_resume in
      let legacy_running = apply finished (Moderator_execution_changed legacy) in
      let history = history @ [ Admission.Historical ] in
      assert_history legacy_running history;
      let legacy_done =
        apply legacy_running (Moderator_execution_changed (complete legacy))
      in
      assert_history legacy_done history;
      let selected_again = make_receipt legacy_done "mex_evidence_new" Session_resume in
      let captured_again = apply legacy_done (paired legacy_done selected_again) in
      let captured_done =
        apply captured_again (Moderator_execution_changed (complete selected_again))
      in
      let history = history @ [ Admission.Captured selected_again.context.id ] in
      assert_history captured_done history;
      (* Matching checkpoints preserve the installation. Explicit removal then
         same-code installation rotates it; identical bytes do not resurrect it. *)
      let continuity =
        A.Run_source_change.prepare
          captured_done
          ~delta:(Moderator_changed captured_done.moderator)
          ~now:timestamp
        |> protocol_ok
        |> apply captured_done
      in
      assert_history continuity history;
      let removed =
        A.Run_source_change.prepare
          continuity
          ~delta:(Moderator_changed None)
          ~now:timestamp
        |> protocol_ok
        |> apply continuity
      in
      assert_history removed [];
      let reinstalled =
        A.Run_source_change.prepare
          removed
          ~delta:(Moderator_changed (Some moderator))
          ~now:timestamp
        |> protocol_ok
        |> apply removed
      in
      assert_history reinstalled [];
      [%test_eq: int64]
        Int64.((current_source captured_done).installation_epoch + 2L)
        (current_source reinstalled).installation_epoch;
      let reset =
        A.Administration.reset
          captured_done
          { keep_history = false
          ; keep_tasks = false
          ; keep_grants = false
          ; keep_labels = true
          ; workspace_instance = None
          }
        |> protocol_ok
      in
      let reset =
        A.Run_source_change.prepare captured_done ~delta:(Created reset) ~now:timestamp
        |> protocol_ok
        |> apply captured_done
      in
      assert_history reset [];
      [%test_eq: int]
        (Int.succ captured_done.identity.generation)
        reset.identity.generation;
      Int.incr traces);
    (* A historical successful receipt is not synthesized into new evidence. *)
    let old = make_receipt initial "mex_evidence_old" Session_start in
    let historical = apply initial (Moderator_execution_changed old) in
    let historical = apply historical (Moderator_execution_changed (complete old)) in
    assert_history historical [];
    let invalid_id = P.Id.Moderator_execution.t_of_sexp (Sexp.Atom "") in
    assert (
      Result.is_error
        (A.Moderator_lifecycle.create
           ~source:(current_source initial)
           ~execution_id:invalid_id));
    [%test_eq: int] 3 !traces;
    print_endline
      "three terminal histories agree; historical/reinstalled evidence stays unknown");
  [%expect
    {| three terminal histories agree; historical/reinstalled evidence stays unknown |}]
;;

let%expect_test
    "lifecycle optional evidence preserves absence null and future document fields"
  =
  with_actor_workspace (fun env workspace ->
    let manager, _, _ = handoff_definition env ~declare_tool:false in
    let moderator =
      M.identity_snapshot manager
      |> Result.ok_or_failwith
      |> A.Moderator_checkpoint.encode
    in
    let observer =
      A.Moderator_checkpoint.observer (Some moderator) |> protocol_ok |> Option.value_exn
    in
    let installation =
      A.Run_source_installation.apply
        A.Run_source_installation.initial
        ~change:(Replace observer)
      |> protocol_ok
    in
    let installed =
      A.Run_state.replace_installation A.Run_state.empty ~installation ~retired_runs:[]
      |> protocol_ok
    in
    let initial =
      actor_state
        ~workspace_instance:workspace
        ~liveness:Detached
        ~start_immediately:false
    in
    let initial =
      { initial with moderator = Some moderator; run_state = Some installed }
    in
    let receipt = make_receipt initial "mex_evidence_presence" Session_start in
    let present = apply initial (paired initial receipt) in
    let present = apply present (Moderator_execution_changed (complete receipt)) in
    let present_document = state_document present in
    let required_semantics =
      Document_schema.Document.required_semantics present_document
    in
    let payload = Document_schema.Document.payload present_document in
    let raw_index = field payload "run_state" in
    let pointer =
      field raw_index "moderator_lifecycle"
      |> fun pointer -> set pointer "future_pointer" (`String "retained")
    in
    let raw_index = set raw_index "future_index" (`String "retained") in
    List.iter [ `Absent; `Null; `Present ] ~f:(fun presence ->
      let raw =
        match presence with
        | `Absent -> remove raw_index "moderator_lifecycle"
        | `Null -> set raw_index "moderator_lifecycle" `Null
        | `Present -> set raw_index "moderator_lifecycle" pointer
      in
      let original = set payload "run_state" raw in
      let document = decode_state ~required_semantics original |> document_ok in
      let before = A.Session_state_document.value document in
      let after = apply before (Stop_epoch_changed 1L) in
      let encoded = A.Session_state_document.with_value document after |> encode_state in
      assert (Jsonaf.exactly_equal raw (field encoded "run_state"));
      let expected =
        match presence with
        | `Present -> Some receipt.context.id
        | `Absent | `Null -> None
      in
      let actual =
        Option.map
          (A.Run_state.moderator_lifecycle (index after))
          ~f:A.Moderator_lifecycle.execution_id
      in
      assert (Option.equal P.Id.Moderator_execution.equal expected actual);
      (* Native mutation cannot silently change an unrelated Missing/Null wire
         distinction, nor attach evidence to an already terminal old receipt. *)
      let changed_index =
        match presence with
        | `Present -> A.Run_state.clear_moderator_lifecycle (index before)
        | `Absent | `Null ->
          A.Run_state.capture_moderator_lifecycle
            (index before)
            (A.Run_state.moderator_lifecycle (index present) |> Option.value_exn)
          |> protocol_ok
      in
      (match presence with
       | `Present -> ()
       | `Absent | `Null ->
         let swapped_raw =
           match presence with
           | `Absent -> set raw "moderator_lifecycle" `Null
           | `Null -> remove raw "moderator_lifecycle"
           | `Present -> assert false
         in
         let swapped_index = A.Run_state.of_jsonaf swapped_raw |> protocol_ok in
         assert (
           Result.is_error
             (A.Run_state.validate_transition ~previous:(index before) swapped_index)));
      let forged = { before with run_state = Some changed_index } in
      assert (
        Result.is_error
          (A.Session_state_document.encode
             (A.Session_state_document.with_value document forged)
             ~limits:document_limits)));
    let malformed =
      set payload "run_state" (set raw_index "moderator_lifecycle" (`Object []))
    in
    assert (Result.is_error (decode_state ~required_semantics malformed));
    let module D = Document_schema in
    let module SD = A.Session_state_document in
    let module DD = A.Session_delta_document in
    let marker = P.Session_storage_semantics.moderator_lifecycle in
    assert (List.mem required_semantics marker ~equal:String.equal);
    assert (
      not
        (List.mem
           (D.Document.required_semantics (state_document initial))
           marker
           ~equal:String.equal));
    let encoded_delta =
      DD.create
        (paired initial receipt)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
      |> DD.document
    in
    assert (
      List.mem (D.Document.required_semantics encoded_delta) marker ~equal:String.equal);
    let raw_capture =
      match field (D.Document.payload encoded_delta) "changes" with
      | `Array [ change ] ->
        let capture = field change "capture" in
        set
          capture
          "source"
          (set (field capture "source") "future_source" (`Number "1.00"))
        |> fun capture -> set capture "future_capture" (`String "retained from journal")
      | _ -> failwith "expected one paired claim atom"
    in
    let raw_delta =
      match field (D.Document.payload encoded_delta) "changes" with
      | `Array [ change ] ->
        set
          (D.Document.json encoded_delta)
          "payload"
          (set
             (D.Document.payload encoded_delta)
             "changes"
             (`Array [ set change "capture" raw_capture ]))
        |> D.Document.inspect ~limits:document_limits
        |> document_ok
      | _ -> failwith "expected one paired claim atom"
    in
    let replayed =
      DD.decode raw_delta ~limits:document_limits
      |> document_ok
      |> fun delta ->
      DD.apply delta ~limits:document_limits (SD.authored initial) |> document_ok
    in
    assert_selected (SD.value replayed) receipt;
    let replayed_pointer =
      field (field (encode_state replayed) "run_state") "moderator_lifecycle"
    in
    let unrelated =
      DD.create
        (Stop_epoch_changed 1L)
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
    in
    let updated = DD.apply unrelated ~limits:document_limits replayed |> document_ok in
    let pointer_after =
      field (field (encode_state updated) "run_state") "moderator_lifecycle"
    in
    (* Adoption keeps the destination's known-field order and appends new
       extensions. Compare complete JSON data across adoption, including exact
       numeric lexemes, then require exact order across the unrelated update. *)
    assert (D.Json.equal raw_capture pointer_after);
    assert (Jsonaf.exactly_equal replayed_pointer pointer_after);
    let change_markers document markers =
      set
        (D.Document.json document)
        "required_semantics"
        (`Array (List.map markers ~f:(fun name -> `String name)))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let missing document =
      change_markers
        document
        (List.filter (D.Document.required_semantics document) ~f:(fun name ->
           not (String.equal name marker)))
    in
    let unsupported document =
      change_markers
        document
        (D.Document.required_semantics document @ [ "session.moderator-lifecycle.future" ])
    in
    let require_rejected name = function
      | Error (D.Error.Required_semantics_unknown actual) ->
        [%test_eq: string] name actual
      | Error error -> raise_s [%sexp (error : D.Error.t)]
      | Ok _ -> failwith "lifecycle semantics fence was accepted"
    in
    SD.decode (missing present_document) ~limits:document_limits
    |> require_rejected ("missing " ^ marker);
    SD.decode (unsupported present_document) ~limits:document_limits
    |> require_rejected "session.moderator-lifecycle.future";
    DD.decode (missing raw_delta) ~limits:document_limits
    |> require_rejected ("missing " ^ marker);
    DD.decode (unsupported raw_delta) ~limits:document_limits
    |> require_rejected "session.moderator-lifecycle.future";
    print_endline
      "absence/null/value and capture extensions survive; semantic fences reject");
  [%expect
    {| absence/null/value and capture extensions survive; semantic fences reject |}]
;;
