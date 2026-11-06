open! Core
open Support

(* Same real dormant child and private admission as creation/recovery; source
   staging must not bypass ordinary execution fences or construct its runtime. *)
let assert_captured_source env daemon (child : Registry.entry) =
  let before = state child in
  let handle = Option.value_exn child.store_handle in
  let frames () =
    let journal =
      Agent_store.Journal.open_existing
        ~env
        ~directory:(S.Handle.journal_directory handle)
        ~max_payload_length:16777216
        ~max_segment_bytes:67108864L
        ~max_segment_frames:4096
      |> store_ok
    in
    let scan = Agent_store.Journal.scan journal |> store_ok in
    List.map scan.entries ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
  in
  let saved = frames () in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Single_turn
      ~input:Authored_start
      ~key:(P.Idempotency_key.of_string "captured-source-stage" |> protocol_ok)
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation child.actor ~actor:proof ~authorize ~request
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "captured source unexpectedly retained a Run"
  in
  protect_cleanup
    ~finally:(fun () -> Actor.end_run_preparation child.actor preparation |> protocol_ok)
    (fun () ->
       Factory.with_prepared_run_source
         (Daemon.factory daemon)
         child
         ~preparation
         (fun prepared ->
            Agent_server.Runtime_owner.Prepared.check_basis prepared |> protocol_ok;
            let source =
              Actor.Source_binding.source
                (Agent_server.Runtime_owner.Prepared.binding prepared)
            in
            match A.Runtime_builder.Source.selection source with
            | Captured retained ->
              assert (
                P.Id.Prompt_revision.equal
                  before.spec.prompt_revision_id
                  (A.Prompt_revision.id (A.Captured_recurrence_source.revision retained)));
              let locator = Option.value_exn before.spec.delegation in
              let parent = loaded daemon locator.key.parent_session_id in
              assert (
                Result.is_error
                  (A.Runtime_builder.Source.validate_target source (state parent)));
              Ok ()
            | Authored _ | Authored_child _ | Generated _ ->
              failwith "captured recurrence source lost its real admission")
       |> protocol_ok);
  assert (not (Agent_server.Runtime_owner.is_loaded child.runtime));
  assert (
    Jsonaf.exactly_equal
      (T.state_document before |> store_ok |> D.Document.json)
      (T.state_document (state child) |> store_ok |> D.Document.json));
  [%test_eq: string list] saved (frames ())
;;

let%expect_test "captured factory links one dormant same-script child after parent ACK" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / prompt)
           {|<developer>Complete captured same-script target.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
         let configuration = T.config root root prompt in
         let provider_calls = ref 0 in
         let constructions = ref 0 in
         let child = ref None in
         with_daemon
           ~on_runtime_construction:(fun () -> Int.incr constructions)
           env
           root
           configuration
           ~provider_calls
           (fun sw daemon ->
              let connection = T.connection daemon principal in
              protect_cleanup
                ~finally:(fun () -> Agent_client.Connection.close connection)
                (fun () ->
                   T.initialize connection;
                   let created, _ = T.create_session ~start_immediately:true connection in
                   let parent = loaded daemon created.id in
                   await_ready env parent |> ignore;
                   let handle =
                     Agent_client.Session_handle.attach
                       ~sw
                       ~clock:(Eio.Stdenv.clock env)
                       ~connection
                       ~session_id:created.id
                       ~mode:Read_write
                       ()
                     |> protocol_ok
                   in
                   protect_cleanup
                     ~finally:(fun () -> Agent_client.Session_handle.close handle)
                     (fun () ->
                        Agent_client.Session_handle.stop handle ~mode:Cancel
                        |> protocol_ok
                        |> ignore;
                        Agent_client.Session_handle.start handle ~queue_if_limited:false
                        |> protocol_ok
                        |> ignore);
                   let before = await_ready env parent in
                   assert (Int64.(before.stop_epoch > 0L));
                   with_planner env sw daemon parent (fun () ->
                     let captured = target env daemon parent ~root_file:prompt in
                     let definition_json =
                       Target.fresh_definition_json captured |> protocol_ok
                     in
                     Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / prompt);
                     let permit = reserve parent ~clock:(Eio.Stdenv.clock env) captured in
                     let row = Actor.Recurrence_creation.reservation permit in
                     let create () =
                       Factory.create_recurrence_child
                         (Daemon.factory daemon)
                         ~parent
                         ~permit
                     in
                     let result = create () |> protocol_ok in
                     let child_id =
                       P.Session_ref.session_id (Factory.Recurrence_child.session result)
                     in
                     child := Some child_id;
                     assert (not (P.Id.Session.equal created.id child_id));
                     let child, record = assert_dormant daemon child_id in
                     assert (Ledger.equal_stage Linked record.stage);
                     let current = state child in
                     let origin =
                       A.Recurrence_child_origin.of_record record |> protocol_ok
                     in
                     let floor = Option.value_exn record.admission.parent_stop_epoch in
                     assert (Int64.(floor > 0L));
                     List.iter
                       [ None; Some Int64.(floor - 1L) ]
                       ~f:(fun acknowledgement ->
                         assert (
                           Result.is_error
                             (A.Recurrence_child_origin.validate_initial_state
                                origin
                                { current with parent_stop_epoch = acknowledgement })));
                     (* Data validation only: accepting a later owner acknowledgement
                     does not fabricate a live parent or execution permission. *)
                     A.Recurrence_child_origin.validate_initial_state
                       origin
                       { current with parent_stop_epoch = Some Int64.(floor + 1L) }
                     |> protocol_ok;
                     assert (
                       P.Id.Prompt_revision.equal
                         before.spec.prompt_revision_id
                         record.admission.revision_id);
                     assert (
                       Ledger.Reference.equal
                         (Factory.Recurrence_child.reference result)
                         (Ledger.reference record));
                     let raw = Option.value_exn record.admission.captured_recurrence in
                     assert (
                       D.Json.equal
                         definition_json
                         (Agent_store.Delegation_captured_origin.definition raw));
                     assert (
                       P.Id.Session.equal
                         child_id
                         (A.Recurrence_child_reservation.child_session_id row));
                     let associated =
                       entry parent
                       |> Entry.frontier
                       |> protocol_ok
                       |> Frontier.child_associations
                     in
                     let association = List.hd_exn associated in
                     assert (
                       Ledger.Reference.equal
                         (A.Recurrence_child_association.reference association)
                         (Ledger.reference record));
                     let revision =
                       Catalog.revision
                         (Option.value_exn (state parent).recurrence_catalog)
                     in
                     let repeated = create () |> protocol_ok in
                     assert (
                       P.Session_ref.equal
                         (Factory.Recurrence_child.session result)
                         (Factory.Recurrence_child.session repeated));
                     [%test_eq: int64]
                       revision
                       (Catalog.revision
                          (Option.value_exn (state parent).recurrence_catalog));
                     let before_construction = !constructions in
                     assert_captured_source env daemon child;
                     [%test_eq: int] before_construction !constructions;
                     assert_ordinary_execution_blocked
                       env
                       sw
                       connection
                       daemon
                       child_id
                       ~start_error:
                         ( Invalid_state
                         , "captured recurrence child requires reciprocal occurrence Run \
                            admission" );
                     [%test_eq: int] 0 !provider_calls;
                     print_endline
                       "same exact artifact; distinct child; parent association then \
                        Linked; replay stable; dormant")));
         (* Fresh store/registry ownership and actual captured recovery, after
            the source file was deleted. No old actor or permit is reused. *)
         with_daemon
           ~on_runtime_construction:(fun () -> Int.incr constructions)
           env
           root
           configuration
           ~provider_calls
           (fun sw daemon ->
              let child_id = Option.value_exn !child in
              Registry.load (Daemon.registry daemon) child_id |> protocol_ok |> ignore;
              let child, _ = assert_dormant daemon child_id in
              assert (not (Registry.execution_selected (Daemon.registry daemon) child));
              let before_construction = !constructions in
              assert_captured_source env daemon child;
              [%test_eq: int] before_construction !constructions;
              let connection = T.connection daemon principal in
              protect_cleanup
                ~finally:(fun () -> Agent_client.Connection.close connection)
                (fun () ->
                   T.initialize connection;
                   assert_ordinary_execution_blocked
                     env
                     sw
                     connection
                     daemon
                     child_id
                     ~start_error:
                       (Conflict, "target has not been selected for ordinary execution"));
              [%test_eq: int] 0 !provider_calls;
              print_endline "ordinary Start/Send rejected; reopened dormant from capture")));
  [%expect
    {|
    same exact artifact; distinct child; parent association then Linked; replay stable; dormant
    ordinary Start/Send rejected; reopened dormant from capture
    |}]
;;
