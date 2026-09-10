open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module I = Agent_session.External_ingress

type mode =
  | Complete
  | Revoke
[@@deriving sexp_of]

let sources mode =
  let finish =
    match mode with
    | Complete -> ""
    | Revoke -> {|let* revoked = Ingress.revoke(state.registration, "completed") in|}
  in
  [ ( "agent.chatmd"
    , {|<script id="receiver" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { subscription = ""; registration = "" }
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* subscription = Subscription.create("external-result", `Some(30000), `No_wake) in
  let* () = Task.catch(
    (let* discarded = Ingress.register(subscription, 0, "external.discarded", `Bool(true)) in
     Task.fail("discard registration")),
    fun error -> Task.pure(())) in
  let* registration = Ingress.register(subscription, 0, "external.report", `Bool(true)) in
  let* () = Task.catch(
    (let* discarded = Ingress.revoke(registration, "discard revocation") in
     Task.fail("discard revocation")),
    fun error -> Task.pure(())) in
  let* status = Ingress.get(registration) in
  let* () = Invocation.resolve(p.context.invocation_id,
    `Pending(`Subscription(subscription), `String("accepted"))) in
  Task.pure({ subscription = subscription; registration = registration })
| `Internal_event(payload) ->
  let* completed = Subscription.complete(state.subscription, 0, `String("received")) in
|}
      ^ finish
      ^ {|
  let* status = Ingress.get(state.registration) in
  Task.pure(state)
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="receiver" input_schema="any.json"
 output_schema="string.json" completion_schema="string.json"/>
|}
    )
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
;;

let%expect_test
    "compiled ingress scopes roll back, accept host data and complete a daemon \
     subscription"
  =
  List.iter [ Complete; Revoke ] ~f:(fun mode ->
    let receipt = ref None in
    let submit entry registration producer =
      Agent_server.Runtime_owner.submit_ingress
        entry.Agent_server.Session_registry.runtime
        ~producer
        ~registration_id:registration.I.context.id
        ~namespace:registration.context.namespace
        ~key:(P.Idempotency_key.of_string "helper-result" |> protocol_ok)
        ~payload:(`Object [ "type", `String "Tool_invoked"; "result", `String "ready" ])
    in
    with_daemon
      ~sources:(sources mode)
      ~expect_moderator:true
      ~calls:[ "watch-call", "watch", `Null ]
      ~after_turn:(fun _ handle entry ->
        let state = A.state entry.actor |> protocol_ok in
        [%test_eq: int] 1 (List.length state.ingress_registrations);
        let registration = List.hd_exn state.ingress_registrations in
        assert (Option.is_none registration.revoked);
        assert (List.is_empty registration.receipts);
        assert (
          P.Id.Principal.equal
            registration.context.producer
            (Option.value_exn state.identity.creating_principal));
        assert (Result.is_error (submit entry registration (P.Id.Principal.create ())));
        H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
        Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
        H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
        receipt
        := Some (submit entry registration registration.context.producer |> protocol_ok);
        match mode with
        | Revoke -> ()
        | Complete ->
          let duplicate =
            submit entry registration registration.context.producer |> protocol_ok
          in
          assert (I.equal_receipt duplicate (Option.value_exn !receipt)))
      ~settle:(fun env entry ->
        Subscription_tests.await_subscription env entry;
        let state = A.state entry.actor |> protocol_ok in
        let registration = List.hd_exn state.ingress_registrations in
        match mode with
        | Complete ->
          let duplicate =
            submit entry registration registration.context.producer |> protocol_ok
          in
          assert (I.equal_receipt duplicate (Option.value_exn !receipt))
        | Revoke ->
          assert (
            Result.is_error (submit entry registration registration.context.producer)))
      (fun state ->
         [%test_eq: int] 1 (List.length state.ingress_registrations);
         let registration = List.hd_exn state.ingress_registrations in
         assert (
           List.equal I.equal_receipt registration.receipts [ Option.value_exn !receipt ]);
         let subscription = List.hd_exn state.subscriptions in
         assert (
           Option.equal
             P.Completion.equal
             subscription.result
             (Some (Succeeded (`String "received"))));
         let handled =
           List.count state.moderator_executions ~f:(fun execution ->
             P.Moderator_execution.equal_phase execution.context.phase Internal_event)
         in
         [%test_eq: int] 1 handled;
         print_s
           [%sexp (mode : mode), (registration.revoked : string option), (handled : int)]));
  [%expect
    {|
    (Complete () 1)
    (Revoke (completed) 1)
    |}]
;;
