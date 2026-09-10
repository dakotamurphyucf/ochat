open Core
open Fixtures
module P = Agent_protocol
module I = P.Invocation
module N = Agent_session.Notification_readiness
module E = P.Moderator_execution

let source : I.observer = { script_id = "publisher"; source_sha256 = String.make 64 'a' }

let resolved value outcome =
  I.dispatch value
  |> protocol_ok
  |> fun value -> I.resolve value ~session_id ~generation:0 outcome |> protocol_ok
;;

let%expect_test
    "notification readiness follows nested job and event ancestry without inventing tool \
     outputs"
  =
  Job_fixtures.with_actor (fun _ _ actor _ _ ->
    let root =
      I.create
        { (invocation_fixture ()).context with
          id = P.Id.Invocation.create ()
        ; origin = Model
        ; provider_call_id = Some "root-call"
        }
      |> protocol_ok
      |> fun value -> resolved value (Complete (`String "accepted"))
    in
    let published = I.publish root |> protocol_ok in
    let parent = Job_fixtures.add_claimed_job actor in
    let parent =
      { parent with
        launch =
          Some { owner = Invocation root.context.id; parent_job = None; nested_depth = 0 }
      }
    in
    let sub = P.Id.Subscription.create () in
    let leaf_context =
      { (invocation_fixture ()).context with
        id = P.Id.Invocation.create ()
      ; parent_job = Some parent.id
      }
    in
    let leaf = I.create leaf_context |> protocol_ok in
    let completed = resolved leaf (Pending (Subscription sub, `String "accepted")) in
    let intent creator work invocation_id =
      P.Delivery.create
        { id = P.Id.Delivery.create ()
        ; session_id
        ; generation = 0
        ; invocation_id
        ; work
        ; correlation = "ready"
        ; source = Moderator
        ; completion = Succeeded (`String "ready")
        ; wake = No_wake
        ; created_at = timestamp
        ; ownership = Some { source; creator }
        }
      |> protocol_ok
    in
    let delivery =
      intent
        (Invocation completed.context.id)
        (Some (Subscription sub))
        (Some completed.context.id)
    in
    let check label invocations jobs events delivery =
      let result = N.check ~invocations ~jobs ~events delivery in
      print_s [%sexp (label : string), (result : (unit, N.blockage) result)]
    in
    check "job-before-root-ack" [ root; completed ] [ parent ] [] delivery;
    check "job-after-root-ack" [ published; completed ] [ parent ] [] delivery;
    check "leaf-unresolved" [ published; leaf ] [ parent ] [] delivery;
    let discarded = I.discard_publication root ~reason:"cancelled batch" |> protocol_ok in
    check "discarded-root" [ discarded; completed ] [ parent ] [] delivery;
    check "missing-launch-owner" [ completed ] [ parent ] [] delivery;
    check
      "foreign-job-generation"
      [ published; completed ]
      [ { parent with generation = 1 } ]
      []
      delivery;
    let cyclic =
      { parent with
        launch =
          Some
            { owner = Invocation completed.context.id
            ; parent_job = None
            ; nested_depth = 0
            }
      }
    in
    check "cycle-through-job" [ completed ] [ cyclic ] [] delivery;
    let event =
      E.create
        { id = P.Id.Moderator_execution.create ()
        ; session_id
        ; generation = 0
        ; source
        ; operation_id = None
        ; job = Some { job_id = parent.id; attempt = parent.attempt; deadline = None }
        ; phase = Post_tool_response
        ; event = `Null
        ; checkpoint_sha256 = String.make 64 'b'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let event_leaf =
      I.create
        ~observer:source
        ~parent_event:event.context.id
        { leaf_context with origin = Moderator; parent_job = None }
      |> protocol_ok
      |> fun value -> resolved value (Pending (Subscription sub, `String "accepted"))
    in
    check "event-still-running" [ published; event_leaf ] [ parent ] [ event ] delivery;
    let finished =
      E.complete
        event
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    check "event-before-root-ack" [ root; event_leaf ] [ parent ] [ finished ] delivery;
    check
      "event-after-root-ack"
      [ published; event_leaf ]
      [ parent ]
      [ finished ]
      delivery;
    let interrupted = E.interrupt event ~reason:"restart" |> protocol_ok in
    check
      "interrupted-event"
      [ published; event_leaf ]
      [ parent ]
      [ interrupted ]
      delivery;
    let uncorrelated = intent (Invocation completed.context.id) None None in
    check "omitted-correlation-still-waits" [ root; completed ] [ parent ] [] uncorrelated;
    let publisher =
      E.create { event.context with id = P.Id.Moderator_execution.create (); job = None }
      |> protocol_ok
      |> fun value ->
      E.complete
        value
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let job_delivery =
      intent (Moderator_event publisher.context.id) (Some (Job parent.id)) None
    in
    check "job-reference-still-waits" [ root ] [ parent ] [ publisher ] job_delivery;
    check "job-reference-acknowledged" [ published ] [ parent ] [ publisher ] job_delivery;
    let direct =
      I.create
        { leaf_context with parent_job = None; parent_invocation = Some root.context.id }
      |> protocol_ok
      |> fun value -> resolved value (Pending (Subscription sub, `String "accepted"))
    in
    check "direct-nested-before-ack" [ root; direct ] [] [] delivery;
    check "direct-nested-after-ack" [ published; direct ] [] [] delivery;
    let wrong =
      intent
        (Invocation completed.context.id)
        (Some (Job parent.id))
        (Some completed.context.id)
    in
    check "mismatched-work" [ published; completed ] [ parent ] [] wrong;
    match completed.status, direct.status, event_leaf.status with
    | Resolved _, Resolved _, Resolved _ -> ()
    | _ -> failwith "ordering check changed nested invocation outcomes");
  [%expect
    {|
    (job-before-root-ack
     (Error (Waiting "initial acknowledgement is not published")))
    (job-after-root-ack (Ok ()))
    (leaf-unresolved
     (Error (Waiting "notification origin has no recorded outcome")))
    (discarded-root
     (Error (Rejected "notification ancestor publication was discarded")))
    (missing-launch-owner
     (Error (Rejected "notification invocation is not retained")))
    (foreign-job-generation
     (Error (Rejected "notification ancestry crosses session or generation")))
    (cycle-through-job
     (Error (Rejected "notification acknowledgement ancestry contains a cycle")))
    (event-still-running
     (Error (Waiting "notification creator has not committed")))
    (event-before-root-ack
     (Error (Waiting "initial acknowledgement is not published")))
    (event-after-root-ack (Ok ()))
    (interrupted-event
     (Error (Rejected "notification ancestor event did not complete")))
    (omitted-correlation-still-waits
     (Error (Waiting "initial acknowledgement is not published")))
    (job-reference-still-waits
     (Error (Waiting "initial acknowledgement is not published")))
    (job-reference-acknowledged (Ok ()))
    (direct-nested-before-ack
     (Error (Waiting "initial acknowledgement is not published")))
    (direct-nested-after-ack (Ok ()))
    (mismatched-work
     (Error
      (Rejected "delivery work differs from the originating acknowledgement")))
    |}]
;;
