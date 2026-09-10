open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

let%expect_test
    "ingress protocol binds authenticated producer, scope and version before retry \
     acknowledgement"
  =
  with_daemon
    ~sources:(Ingress_tests.sources Revoke)
    ~expect_moderator:true
    ~calls:[ "watch-call", "watch", `Null ]
    ~after_turn_with_daemon:(fun env daemon entry ->
      let state = A.state entry.actor |> protocol_ok in
      let registration = List.hd_exn state.ingress_registrations in
      let owner = principal () in
      let request : P.Ingress.Submit_request.t =
        { session_id = state.identity.session_id
        ; registration_id = registration.context.id
        ; namespace = registration.context.namespace
        ; idempotency_key = P.Idempotency_key.of_string "scoped-protocol" |> protocol_ok
        ; payload = `Object [ "type", `String "Tool_invoked"; "value", `String "ready" ]
        }
      in
      let use principal f =
        let client = connection daemon principal in
        Exn.protect
          ~finally:(fun () -> Agent_client.Connection.close client)
          ~f:(fun () -> f client)
      in
      let denied label result =
        match result with
        | Error error ->
          print_s [%sexp (label : string), (error.P.Error.code : P.Error.code)]
        | Ok _ -> failwith (label ^ " was accepted")
      in
      use
        { owner with scopes = P.Scope.Set.of_list [ Send_messages ] }
        (fun client ->
           initialize client;
           denied "missing dedicated scope" (Agent_client.Ingress.submit client request));
      use
        { owner with
          id = P.Id.Principal.create ()
        ; scopes = P.Scope.Set.of_list [ Submit_ingress ]
        }
        (fun client ->
           initialize client;
           denied "foreign principal" (Agent_client.Ingress.submit client request));
      use owner (fun client ->
        let implementation =
          P.Initialize.Implementation.create ~name:"legacy" ~version:"test" |> protocol_ok
        in
        let initialize =
          P.Initialize.Request.create
            ~implementation
            ~protocol_min:P.Version.initial
            ~protocol_max:P.Version.initial
            ~features:[]
            ~event_encodings:[ Json ]
            ~max_inbound_event_bytes:1048576
            ()
          |> protocol_ok
        in
        (match
           Agent_client.Connection.request client (Protocol_initialize initialize)
           |> protocol_ok
         with
         | Protocol_initialize response ->
           assert (P.Version.equal response.selected_version P.Version.initial);
           assert (not (P.Principal.has_scope response.principal Submit_ingress))
         | _ -> failwith "unexpected legacy initialization");
        denied "legacy protocol" (Agent_client.Ingress.submit client request));
      use
        { owner with scopes = P.Scope.Set.of_list [ Submit_ingress ] }
        (fun client ->
           initialize client;
           (* No transcript or attachment permission is granted to this producer. *)
           let first = Agent_client.Ingress.submit client request |> protocol_ok in
           assert (P.Id.Capability.equal first.registration_id registration.context.id);
           Subscription_tests.await_subscription env entry;
           denied "revoked matching retry" (Agent_client.Ingress.submit client request);
           let state = A.state entry.actor |> protocol_ok in
           let saved = List.hd_exn state.ingress_registrations in
           [%test_eq: int] 1 (List.length saved.receipts);
           assert (P.Id.Ingress_event.equal (List.hd_exn saved.receipts).id first.event_id)))
    ~settle:Subscription_tests.await_subscription
    (fun state ->
       let registration = List.hd_exn state.ingress_registrations in
       [%test_eq: string option] (Some "completed") registration.revoked;
       print_endline "one scoped data event accepted; revoked retry was re-authorized");
  [%expect
    {|
    ("missing dedicated scope" Permission_denied)
    ("foreign principal" Permission_denied)
    ("legacy protocol" Incompatible_protocol)
    ("revoked matching retry" Permission_denied)
    one scoped data event accepted; revoked retry was re-authorized
    |}]
;;
