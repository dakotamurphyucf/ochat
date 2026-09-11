open Core
open Fixtures
module P = Agent_protocol
module Cursor = Agent_server.Managed_output_cursor

let%expect_test "history epochs detect unread deletion and survive until replay eviction" =
  Eio_main.run (fun _env ->
    let module Log = Agent_session.Durable_event_log in
    let event sequence payload =
      P.Event.Durable.of_payload
        ~session_id
        ~sequence
        ~revision:sequence
        ~timestamp
        payload
    in
    let idle =
      P.Event.Durable.Payload.Session_state_changed
        { desired_state = Running; observed_state = Idle }
    in
    let log =
      Log.create ~capacity:2 [ event 1L idle; event 2L (History_appended []) ]
      |> protocol_ok
    in
    let before = Log.history_epoch log ~through_sequence:2L |> protocol_ok in
    Log.append
      log
      [ event 3L (History_replaced (Agent_session.Session_state.history_window [])) ];
    let replaced = Log.history_epoch log ~through_sequence:3L |> protocol_ok in
    Log.append log [ event 4L idle ];
    let retained = Log.history_epoch log ~through_sequence:4L |> protocol_ok in
    assert (Log.equal_history_epoch replaced retained);
    Log.append log [ event 5L idle ];
    let evicted = Log.history_epoch log ~through_sequence:5L |> protocol_ok in
    assert (not (Log.equal_history_epoch retained evicted));
    (match Log.history_epoch log ~through_sequence:3L with
     | Error error ->
       [%test_eq: string] "snapshot_required" (P.Error.code_to_string error.code)
     | Ok _ -> failwith "old output snapshot accepted after replay eviction");
    let restored = Log.create ~capacity:2 [] |> protocol_ok in
    let fresh = Log.history_epoch restored ~through_sequence:10L |> protocol_ok in
    print_s
      [%sexp
        (before : Log.history_epoch)
      , (replaced : Log.history_epoch)
      , (evicted : Log.history_epoch)
      , (fresh : Log.history_epoch)]);
  [%expect {| ((Window_start 1) (Replacement 3) (Window_start 4) (Window_start 10)) |}]
;;

let%expect_test
    "managed output cursors survive appends, reject gaps and bind private relationship \
     and query"
  =
  Delegation_lifecycle_tests.with_fixture
    (fun
        _env
         _sw
         _ledger
         record
         foreign
         _actor
         _runtime
         _backend
         _closes
         _reject
         _original
       ->
       let signer = Cursor.create () in
       let context : Cursor.context =
         { relationship = Agent_store.Delegation_store.reference record
         ; generation = 0
         ; compaction_generation = 0
         ; history_epoch = Window_start 0L
         ; access_revision = "assistant-output.v1"
         ; query = All_outputs
         }
       in
       let entries = [ "first output"; "second output" ] in
       let issue entries position =
         Cursor.issue signer ~context ~entries position |> protocol_ok
       in
       let boundary = issue entries { entry = 1; byte = 0 } in
       let partial = issue entries { entry = 1; byte = 4 } in
       let tail = issue entries { entry = 2; byte = 0 } in
       let fresh = Cursor.resolve signer ~context ~entries None |> protocol_ok in
       assert (Cursor.equal_position fresh { entry = 0; byte = 0 });
       List.iter
         [ boundary, { Cursor.entry = 1; byte = 0 }
         ; partial, { entry = 1; byte = 4 }
         ; tail, { entry = 2; byte = 0 }
         ]
         ~f:(fun (cursor, expected) ->
           let actual =
             Cursor.resolve
               signer
               ~context
               ~entries:(entries @ [ "new output" ])
               (Some cursor)
             |> protocol_ok
           in
           assert (Cursor.equal_position expected actual));
       let reject label signer context entries cursor =
         match Cursor.resolve signer ~context ~entries (Some cursor) with
         | Ok _ -> failwith ("cursor accepted " ^ label)
         | Error error ->
           [%test_eq: string] "cursor_expired" (P.Error.code_to_string error.code);
           print_endline label
       in
       reject
         "consumed output replacement"
         signer
         context
         [ "edited"; "second output" ]
         boundary;
       reject "retention gap" signer context [ "second output" ] tail;
       reject
         "partially read output changed"
         signer
         context
         [ "first output"; "edited second output" ]
         partial;
       reject
         "target generation changed"
         signer
         { context with generation = 1 }
         entries
         boundary;
       reject
         "compaction requires snapshot"
         signer
         { context with compaction_generation = 1 }
         entries
         boundary;
       reject
         "access projection changed"
         signer
         { context with access_revision = "different-view" }
         entries
         boundary;
       reject
         "another management relationship"
         signer
         { context with relationship = Agent_store.Delegation_store.reference foreign }
         entries
         boundary;
       let receipt =
         History_entry.Id.create ~namespace:"receipt" ~sequence:0 |> Result.ok_or_failwith
       in
       reject
         "another output query"
         signer
         { context with query = Submission receipt }
         entries
         boundary;
       reject
         "restart expires process cursors"
         (Cursor.create ())
         context
         entries
         boundary;
       let decoded = Base64.decode_exn (P.Page.Cursor.to_string boundary) in
       let altered =
         String.substr_replace_first decoded ~pattern:":1:0:" ~with_:":0:0:"
         |> Base64.encode_exn
         |> P.Page.Cursor.of_string
         |> protocol_ok
       in
       reject "tampered offset" signer context entries altered;
       let again =
         Cursor.resolve signer ~context ~entries (Some boundary) |> protocol_ok
       in
       assert (Cursor.equal_position again { entry = 1; byte = 0 });
       print_endline
         "repeated reads keep independent cursor positions; appended output stays visible");
  [%expect
    {|
    consumed output replacement
    retention gap
    partially read output changed
    target generation changed
    compaction requires snapshot
    access projection changed
    another management relationship
    another output query
    restart expires process cursors
    tampered offset
    repeated reads keep independent cursor positions; appended output stays visible
    |}]
;;
