open Core
open Fixtures
module P = Agent_protocol
module Log = Agent_session.Durable_event_log

let notification sequence value =
  P.Event.Durable.of_payload
    ~session_id
    ~sequence
    ~revision:sequence
    ~timestamp
    (Moderator_notification value)
;;

let snapshot workspace_instance =
  actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  |> Agent_session.Session_state.snapshot ~now:timestamp
;;

let updated sequence snapshot =
  P.Event.Durable.of_payload
    ~session_id
    ~sequence
    ~revision:sequence
    ~timestamp
    (Session_updated snapshot.P.Snapshot.session)
  |> fun event -> P.Event.Durable.with_replacement_snapshot event snapshot
;;

let references log candidates =
  Log.retained_references log ~session_id ~candidates ~max_events:8 ~max_bytes:65536
  |> protocol_ok
;;

let%expect_test
    "replay-only results and replacement snapshots retain artifacts until eviction"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let first = P.Id.Blob.create ()
    and second = P.Id.Blob.create ()
    and absent = P.Id.Blob.create () in
    let candidates = [ first; second; absent ] in
    let old = notification 1L (`String (P.Id.Blob.to_string first)) in
    let replacement =
      { (snapshot workspace_instance) with
        active_tool_calls = [ `String (P.Id.Blob.to_string second) ]
      }
      |> updated 2L
    in
    let log = Log.create ~capacity:2 [ old; replacement ] |> protocol_ok in
    assert (
      List.equal
        P.Id.Blob.equal
        (List.sort [ first; second ] ~compare:P.Id.Blob.compare)
        (references log candidates));
    Log.append log [ notification 3L `Null ];
    assert (List.equal P.Id.Blob.equal [ second ] (references log candidates));
    Log.append log [ notification 4L `Null ];
    assert (List.is_empty (references log candidates));
    print_endline "notification and replacement snapshot retained both references";
    print_endline "each reference disappeared only after its event was evicted");
  [%expect
    {|
    notification and replacement snapshot retained both references
    each reference disappeared only after its event was evicted
    |}]
;;

let%expect_test "uncertain replay roots refuse retention proof without partial references"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let id = P.Id.Blob.create () in
    let good = notification 1L (`String (P.Id.Blob.to_string id)) in
    let replacement = updated 2L (snapshot workspace_instance) in
    let set_field event name value =
      match event.P.Event.Durable.payload with
      | `Object fields ->
        { event with
          payload =
            `Object
              ((name, value)
               :: List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
        }
      | _ -> failwith "expected object payload"
    in
    let invalid =
      [ "foreign session", { replacement with session_id = second_session_id }
      ; "redacted projection", { replacement with visibility = Redacted }
      ; "invalid payload", { replacement with payload = `Null }
      ; "invalid status", set_field replacement "extension_status" (`Array [ `Null ])
      ; "invalid snapshot", set_field replacement "replacement_snapshot" (`Object [])
      ; "wrong anchor", { replacement with revision = 3L }
      ; "replay gap", { replacement with sequence = 3L }
      ]
    in
    List.iter invalid ~f:(fun (name, event) ->
      let log = Log.create ~capacity:8 [ good ] |> protocol_ok in
      Log.append log [ event ];
      assert (
        Result.is_error
          (Log.retained_references
             log
             ~session_id
             ~candidates:[ id ]
             ~max_events:8
             ~max_bytes:65536));
      print_endline (name ^ ": refused"));
    let log = Log.create ~capacity:8 [ good; replacement ] |> protocol_ok in
    List.iter
      [ 1, 65536; 8, 1 ]
      ~f:(fun (max_events, max_bytes) ->
        assert (
          Result.is_error
            (Log.retained_references
               log
               ~session_id
               ~candidates:[ id ]
               ~max_events
               ~max_bytes)));
    assert (List.equal P.Id.Blob.equal [ id ] (references log [ id ]));
    print_endline "event/byte budget refusals left valid replay unchanged");
  [%expect
    {|
    foreign session: refused
    redacted projection: refused
    invalid payload: refused
    invalid status: refused
    invalid snapshot: refused
    wrong anchor: refused
    replay gap: refused
    event/byte budget refusals left valid replay unchanged
    |}]
;;
