open! Core
open Support

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
         let child = ref None in
         with_daemon env root configuration ~provider_calls (fun sw daemon ->
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
                  let permit = reserve parent captured in
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
                    Catalog.revision (Option.value_exn (state parent).recurrence_catalog)
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
                  assert_ordinary_execution_blocked env sw connection daemon child_id;
                  [%test_eq: int] 0 !provider_calls;
                  print_endline
                    "same exact artifact; distinct child; parent association then \
                     Linked; replay stable; dormant")));
         (* Fresh store/registry ownership and actual captured recovery, after
            the source file was deleted. No old actor or permit is reused. *)
         with_daemon env root configuration ~provider_calls (fun sw daemon ->
           let child_id = Option.value_exn !child in
           Registry.load (Daemon.registry daemon) child_id |> protocol_ok |> ignore;
           assert_dormant daemon child_id |> ignore;
           let connection = T.connection daemon principal in
           protect_cleanup
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                T.initialize connection;
                assert_ordinary_execution_blocked env sw connection daemon child_id);
           [%test_eq: int] 0 !provider_calls;
           print_endline "ordinary Start/Send rejected; reopened dormant from capture")));
  [%expect
    {|
    same exact artifact; distinct child; parent association then Linked; replay stable; dormant
    ordinary Start/Send rejected; reopened dormant from capture
    |}]
;;
