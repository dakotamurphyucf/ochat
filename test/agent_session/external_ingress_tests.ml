open Core
open Fixtures
module P = Agent_protocol
module I = Agent_session.External_ingress

let at ms = P.Timestamp.add_ms timestamp ms |> protocol_ok

let schema =
  Jsonaf.of_string
    {|
{"type":"object","properties":{"value":{"type":"string"},"count":{"type":"integer"}},
 "required":["value","count"],"additionalProperties":false}
|}
;;

let fixture () =
  let subscription =
    Subscription_transaction_tests.make_subscription (invocation_fixture ())
  in
  let context : I.context =
    { id = P.Id.Capability.create ()
    ; session_id
    ; generation = 0
    ; subscription_id = subscription.context.id
    ; epoch = subscription.epoch
    ; source = Subscription_transaction_tests.source
    ; producer = P.Id.Principal.create ()
    ; namespace = "external.report"
    ; schema
    ; created_at = timestamp
    ; expires_at = at 5_000
    ; limits =
        { I.default_limits with
          max_receipts = 2
        ; rate_count = 1
        ; max_payload_bytes = 128
        ; max_payload_depth = 8
        }
    }
  in
  subscription, I.create context ~subscription |> protocol_ok
;;

let report value = `Object [ "value", `String value; "count", `Number "1" ]
let restored t = I.sexp_of_t t |> Sexp.to_string_mach |> Sexp.of_string |> I.t_of_sexp

let accepted = function
  | I.Accepted (state, receipt) -> state, receipt
  | Duplicate _ -> failwith "expected a new data event"
;;

let rejected label = function
  | Error (error : P.Error.t) ->
    print_s
      [%sexp (label : string), (error.code : P.Error.code), (error.retryable : bool)]
  | Ok _ -> failwith (label ^ " unexpectedly accepted")
;;

let%expect_test
    "external data admission preserves retry identity and rate/capacity through restore \
     and clock rollback"
  =
  Mirage_crypto_rng_unix.use_default ();
  let subscription, initial = fixture () in
  let allocated = ref 0 in
  let submit
        ?(producer = initial.context.producer)
        ?(namespace = initial.context.namespace)
        ?(generation = 0)
        ?(source = initial.context.source)
        t
        key
        payload
        now
    =
    I.prepare
      t
      ~session_id
      ~generation
      ~source
      ~subscription
      ~producer
      ~namespace
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~payload
      ~now
      ~create_event_id:(fun () ->
        Int.incr allocated;
        P.Id.Ingress_event.create ())
  in
  rejected
    "other producer"
    (submit ~producer:(P.Id.Principal.create ()) initial "one" (report "ready") (at 100));
  rejected
    "native event namespace"
    (submit ~namespace:"Tool_invoked" initial "one" (report "ready") (at 100));
  rejected
    "replacement generation"
    (submit ~generation:1 initial "one" (report "ready") (at 100));
  rejected
    "changed source"
    (submit
       ~source:{ initial.context.source with source_sha256 = String.make 64 'b' }
       initial
       "one"
       (report "ready")
       (at 100));
  rejected
    "schema"
    (submit initial "one" (`Object [ "type", `String "Permission_resolved" ]) (at 100));
  rejected "size" (submit initial "one" (report (String.make 200 'x')) (at 100));
  [%test_eq: int] 0 !allocated;
  let first, receipt =
    submit initial "one" (report "ready") (at 100) |> protocol_ok |> accepted
  in
  I.validate_transition ~subscription ~previous:(Some initial) first |> protocol_ok;
  let first = restored first in
  I.validate first |> protocol_ok;
  let reordered = `Object [ "count", `Number "1"; "value", `String "ready" ] in
  (match submit first "one" reordered (at 50) |> protocol_ok with
   | Duplicate duplicate -> assert (I.equal_receipt receipt duplicate)
   | _ -> failwith "retry allocated another event");
  rejected "retry payload changed" (submit first "one" (report "changed") (at 200));
  rejected "future timestamp after rollback" (submit first "two" (report "next") (at 50));
  rejected "inclusive rate boundary" (submit first "two" (report "next") (at 1_100));
  [%test_eq: int] 1 !allocated;
  let second, _ =
    submit first "two" (report "next") (at 1_101) |> protocol_ok |> accepted
  in
  I.validate_transition ~subscription ~previous:(Some first) second |> protocol_ok;
  let second = restored second in
  I.validate second |> protocol_ok;
  rejected "retained capacity" (submit second "three" (report "full") (at 3_000));
  (match submit second "one" reordered (at 3_000) |> protocol_ok with
   | Duplicate duplicate -> assert (I.equal_receipt receipt duplicate)
   | _ -> failwith "full registry lost its retry receipt");
  rejected "expired registration" (submit second "one" reordered (at 5_000));
  [%test_eq: int] 2 !allocated;
  print_endline
    "two events retained; retries allocate nothing; no payload or receipt evicted";
  [%expect
    {|
    ("other producer" Permission_denied false)
    ("native event namespace" Permission_denied false)
    ("replacement generation" Permission_denied false)
    ("changed source" Permission_denied false)
    (schema Invalid_request false)
    (size Invalid_request false)
    ("retry payload changed" Conflict false)
    ("future timestamp after rollback" Resource_limit true)
    ("inclusive rate boundary" Resource_limit true)
    ("retained capacity" Resource_limit false)
    ("expired registration" Invalid_state false)
    two events retained; retries allocate nothing; no payload or receipt evicted
    |}]
;;

let%expect_test
    "ingress epochs, terminal outcomes and revocation preserve audit data but prevent \
     new submissions"
  =
  Mirage_crypto_rng_unix.use_default ();
  let subscription, initial = fixture () in
  let submit t subscription key =
    I.prepare
      t
      ~session_id
      ~generation:0
      ~source:initial.context.source
      ~producer:initial.context.producer
      ~namespace:initial.context.namespace
      ~subscription
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~payload:(report "done")
      ~now:(at 100)
      ~create_event_id:P.Id.Ingress_event.create
  in
  let current, original = submit initial subscription "done" |> protocol_ok |> accepted in
  let retry subscription =
    match submit current subscription "done" |> protocol_ok with
    | Duplicate receipt -> assert (I.equal_receipt original receipt)
    | Accepted _ -> failwith "completed subscription retry created another event"
  in
  let armed =
    P.Subscription.arm subscription ~expected_epoch:0 ~timer_id:None ~job_id:None
    |> protocol_ok
  in
  I.validate_owner current armed |> protocol_ok;
  retry armed;
  rejected "old epoch" (submit current armed "new");
  let terminal =
    P.Subscription.finish
      subscription
      ~expected_epoch:0
      ~now:(at 100)
      (Succeeded (`String "complete"))
    |> protocol_ok
    |> fst
  in
  I.validate_owner current terminal |> protocol_ok;
  retry terminal;
  rejected "terminal subscription" (submit current terminal "new");
  let revoked = I.revoke current ~reason:"operator revoked helper" |> protocol_ok in
  I.validate_transition ~subscription ~previous:(Some current) revoked |> protocol_ok;
  let revoked = restored revoked in
  I.validate revoked |> protocol_ok;
  assert (I.equal revoked (I.revoke revoked ~reason:"later reason" |> protocol_ok));
  assert (List.equal I.equal_receipt current.receipts revoked.receipts);
  rejected "revoked retry" (submit revoked subscription "done");
  rejected
    "unrevocation"
    (I.validate_transition ~subscription ~previous:(Some revoked) current);
  rejected
    "receipt eviction"
    (I.validate_transition ~subscription ~previous:(Some current) initial);
  let rebound =
    I.create { initial.context with producer = P.Id.Principal.create () } ~subscription
    |> protocol_ok
  in
  rejected
    "rebound registration"
    (I.validate_transition ~subscription ~previous:(Some initial) rebound);
  print_endline
    "epoch and terminal changes retain audit ownership; revocation cannot erase receipts";
  [%expect
    {|
    ("old epoch" Permission_denied false)
    ("terminal subscription" Permission_denied false)
    ("revoked retry" Permission_denied false)
    (unrevocation Conflict false)
    ("receipt eviction" Conflict false)
    ("rebound registration" Conflict false)
    epoch and terminal changes retain audit ownership; revocation cannot erase receipts
    |}]
;;

let%expect_test
    "ingress registration and event admission replay through session transactions and \
     survive generation replacement as audit data"
  =
  let module State = Agent_session.Session_state in
  let module Delta = Agent_session.Session_delta in
  with_actor_workspace (fun _ workspace_instance ->
    let subscription, initial = fixture () in
    let state =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state =
      Agent_session.Session_transition.apply
        ~now:timestamp
        state
        ~payloads:[]
        ~delta:
          (Batch
             [ Invocation_changed (invocation_fixture ())
             ; Subscription_changed subscription
             ; Ingress_changed initial
             ])
      |> protocol_ok
      |> fun transition -> transition.Agent_session.Session_transition.state
    in
    let next, receipt =
      I.prepare
        initial
        ~session_id
        ~generation:0
        ~source:initial.context.source
        ~producer:initial.context.producer
        ~namespace:initial.context.namespace
        ~subscription
        ~key:(P.Idempotency_key.of_string "persisted" |> protocol_ok)
        ~payload:(report "retained")
        ~now:(at 100)
        ~create_event_id:P.Id.Ingress_event.create
      |> protocol_ok
      |> accepted
    in
    let transition =
      Agent_session.Session_transition.apply
        ~now:(at 100)
        state
        ~payloads:[]
        ~delta:(Ingress_changed next)
      |> protocol_ok
    in
    let transaction =
      Agent_store.Transaction.create
        ~session_id
        ~generation:0
        ~transaction_sequence:transition.state.counters.transaction_sequence
        ~previous_transaction_hash:None
        ~session_revision:transition.state.counters.revision
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (P.Timestamp.to_time_ns (at 100)
           |> Time_ns.to_int63_ns_since_epoch
           |> Int63.to_int64)
        ~command_audit:None
        ~delta:(Delta.sexp_of_t transition.delta |> Sexp.to_string_mach)
        ~durable_events:[]
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction state transaction |> store_ok
    in
    assert_same_session_snapshot transition.state replayed;
    let restored =
      State.sexp_of_t replayed
      |> Sexp.to_string_mach
      |> Agent_session.Session_persistence.restore_snapshot
      |> store_ok
    in
    let retained = List.hd_exn restored.ingress_registrations in
    assert (I.equal next retained);
    assert (I.equal_receipt receipt (List.hd_exn retained.receipts));
    assert (Result.is_error (Delta.apply restored (Ingress_changed initial)));
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (State.sexp_of_t { restored with subscriptions = [] } |> Sexp.to_string_mach)));
    let replacement = Delta.apply restored (Reset_generation 1) |> protocol_ok in
    let replacement =
      State.sexp_of_t replacement
      |> Sexp.to_string_mach
      |> Agent_session.Session_persistence.restore_snapshot
      |> store_ok
    in
    assert (
      List.equal I.equal restored.ingress_registrations replacement.ingress_registrations);
    assert (Result.is_error (Delta.apply replacement (Ingress_changed next)));
    print_endline
      "encoded journal and snapshot retain event identity; old generation is audit-only");
  [%expect
    {| encoded journal and snapshot retain event identity; old generation is audit-only |}]
;;

let%expect_test "administrative candidates cannot discard ingress retry receipts" =
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      let subscription, initial = fixture () in
      let next, _ =
        I.prepare
          initial
          ~session_id
          ~generation:0
          ~source:initial.context.source
          ~producer:initial.context.producer
          ~namespace:initial.context.namespace
          ~subscription
          ~key:(P.Idempotency_key.of_string "accepted" |> protocol_ok)
          ~payload:(report "retained")
          ~now:(at 100)
          ~create_event_id:P.Id.Ingress_event.create
        |> protocol_ok
        |> accepted
      in
      Agent_session.Session_transition.apply
        ~now:(at 100)
        state
        ~payloads:[]
        ~delta:
          (Batch
             [ Invocation_changed (invocation_fixture ())
             ; Subscription_changed subscription
             ; Ingress_changed initial
             ; Ingress_changed next
             ])
      |> protocol_ok
      |> fun transition -> transition.Agent_session.Session_transition.state)
    (fun _ _ actor writer backend ->
       let module A = Agent_session.Session_actor in
       A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
       let before = A.state actor |> protocol_ok in
       rejected
         "administrative receipt removal"
         (A.commit_administration
            actor
            ~command_audit:None
            ~attachment_id:writer.id
            ~expected_revision:before.counters.revision
            ~kind:Upgrade
            { before with ingress_registrations = [] });
       List.iter
         [ Agent_session.Session_state.Compaction_archive.Reset; Rebuild ]
         ~f:(fun kind ->
           match
             A.commit_administration
               actor
               ~command_audit:None
               ~attachment_id:writer.id
               ~expected_revision:before.counters.revision
               ~kind
               { before with ingress_registrations = [] }
           with
           | Error { code = Conflict; _ } -> ()
           | _ -> failwith "receipt removal without generation change was accepted");
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert_same_session_snapshot before (Agent_session.Memory_backend.state backend));
  [%expect {| ("administrative receipt removal" Conflict false) |}]
;;
