open! Core
module P = Agent_protocol
module C = P.Developer_common

let must = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length 'd')

let document_limit bytes =
  match
    Document_schema.Limits.create
      ~max_bytes:bytes
      ~max_depth:32
      ~max_fields:256
      ~max_nodes:512
  with
  | Ok value -> value
  | Error _ -> failwith "invalid test limit"
;;

let limits =
  C.Limits.create
    ~request:(document_limit 4096)
    ~receipt:(document_limit 512)
    ~response:(document_limit 4096)
    ~approval_page:(document_limit 512)
    ~max_output_bytes:128
    ~max_fixture_steps:4
    ~max_fixture_bytes:512
    ~max_approval_display_bytes:64
    ~max_approval_decision_reason_bytes:64
    ~max_approval_page:2
    ~max_event_bytes:128
  |> must
;;

let capabilities =
  C.Capabilities.create
    ~format_version:1
    ~methods:[ "developer.capabilities"; "developer.submit"; "developer.observe" ]
    ~limits
  |> must
;;

let initialization ?(events = false) () =
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-test" ~version:"1" |> must
  in
  let principal =
    P.Principal.create
      ~id:(P.Id.Principal.create_with generator)
      ~authentication_kind:"embedded"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> must
  in
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:P.Version.current
    ~implementation
    ~server_id:(P.Id.Server.create_with generator)
    ~enabled_features:
      (if events then [ C.Features.run; C.Features.events ] else [ C.Features.run ])
    ~extensions:None
    ~principal
    ~limits:
      { max_request_bytes = 16 * 1024 * 1024
      ; max_event_bytes = 16 * 1024 * 1024
      ; max_page_size = 1000
      ; max_attachments_per_connection = 8
      }
    ~event_retention:
      { minimum_age_ms = 1000; maximum_events = 1024; oldest_replayable_sequence = None }
    ~timing:
      { heartbeat_interval_ms = 1000
      ; owner_lease_duration_ms = 1000
      ; owner_renew_after_ms = 500
      ; disconnect_grace_default_ms = 100
      }
    ~server_time:(P.Timestamp.of_string "2026-10-18T00:00:00Z" |> must)
  |> must
;;

let initialize ?(events = false) connection (response : P.Initialize.Response.t) =
  let request =
    P.Initialize.Request.create
      ~implementation:
        (P.Initialize.Implementation.create ~name:"developer-test" ~version:"1" |> must)
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        (if events then [ C.Features.run; C.Features.events ] else [ C.Features.run ])
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
    |> must
  in
  match
    Agent_client.Connection.request_without_history
      connection
      (P.Command.Protocol_initialize request)
  with
  | Ok (Protocol_initialize installed) ->
    assert (P.Id.Server.equal installed.server_id response.server_id)
  | Ok _ -> failwith "unexpected Initialize response"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let submit () =
  let route =
    C.Route.local_source
      ~limits:(C.Limits.request limits)
      ~root_index:0
      ~member:(Source.Portable_path.create "source.chatmd" |> Or_error.ok_exn)
    |> must
  in
  C.Submit.create
    ~limits
    ~run_id:(P.Id.Developer_run.create_with generator)
    ~key:(P.Idempotency_key.of_string "same-key" |> must)
    ~kind:Check
    ~route
    ~target:One_off_v1
    ~prompt:None
    ~workspace:None
    ~entrypoint:"main"
    ~input:`Null
    ~fixtures:None
    ~requested_execution:None
    ~requested_max_output_bytes:None
  |> must
;;

let%expect_test "malformed and rejected Submit replies retain original retry custody" =
  Eio_main.run (fun _ ->
    let initialized = initialization () in
    let attempts = ref 0 in
    let reply = ref (Ok (`Object [])) in
    let other_reply =
      ref (Error (P.Error.invalid_request "unexpected Developer request"))
    in
    let public_initialized =
      P.Public.Result.Non_history.of_internal (Protocol_initialize initialized)
      |> must
      |> fun value -> P.Public.Result.Non_history value
    in
    let make_connection () =
      Agent_client.Transport.create_with_developer
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok public_initialized
          | _ -> Error (P.Error.invalid_request "unexpected command"))
        ~request_developer:(fun ~bound:_ request ->
          match request with
          | P.Developer_wire.Request.Capabilities ->
            Ok (C.Capabilities.to_json capabilities)
          | Submit _ ->
            Int.incr attempts;
            !reply
          | _ -> !other_reply)
        ~next_notification:(fun () -> None)
        ~close:Fn.id
      |> Agent_client.Connection.create
    in
    let connection = make_connection () in
    initialize connection initialized;
    let developer = Agent_client.Developer_connection.create connection |> must in
    let submission = submit () in
    let request = P.Developer_wire.Request.Submit submission in
    assert (Result.is_error (Agent_client.Developer_connection.send developer request));
    assert (
      Int.equal
        (List.length (Agent_client.Developer_connection.pending_submits developer))
        1);
    assert (Result.is_error (Agent_client.Developer_connection.send developer request));
    assert (Int.equal !attempts 1);
    let retained =
      List.hd_exn (Agent_client.Developer_connection.pending_submits developer)
    in
    Agent_client.Developer_connection.abandon_pending developer retained;
    reply := Error (P.Error.invalid_request "simulated post-delivery rejection");
    assert (Result.is_error (Agent_client.Developer_connection.send developer request));
    assert (Int.equal !attempts 2);
    assert (
      Int.equal
        (List.length (Agent_client.Developer_connection.pending_submits developer))
        1);
    let receipt =
      P.Developer_receipt.create
        ~limits:(C.Limits.receipt limits)
        (P.Developer_receipt.Check P.Developer_receipt.Check.Ready)
      |> must
    in
    let owner =
      C.Owner.create
        ~host_id:(P.Id.Server.of_string "srv_foreign" |> must)
        ~run_id:(C.Submit.run_id submission)
        ~epoch:1
        ~principal_id:initialized.principal.id
      |> must
    in
    let digest = Chatml_source.Digest.of_contents "captured" ~checkpoint:Fn.id in
    let foreign_view =
      C.View.create
        ~limits
        ~command_id:(P.Developer_command_id.of_string "cmd_foreign" |> must)
        ~owner
        ~key:(C.Submit.key submission)
        ~kind:Check
        ~phase:(C.View.Terminal { execution_started = false; receipt })
        ~request_fingerprint:digest
        ~capture_fingerprint:digest
      |> must
    in
    Agent_client.Developer_connection.abandon_pending
      developer
      (List.hd_exn (Agent_client.Developer_connection.pending_submits developer));
    reply
    := P.Developer_wire.Response.to_json
         (P.Developer_wire.Response.Submit foreign_view)
         ~limits
       |> must
       |> Result.return;
    assert (Result.is_error (Agent_client.Developer_connection.send developer request));
    assert (Int.equal !attempts 3);
    assert (
      Int.equal
        (List.length (Agent_client.Developer_connection.pending_submits developer))
        1);
    let replacement = make_connection () in
    initialize replacement initialized;
    let resumed = Agent_client.Developer_connection.create replacement |> must in
    let original = Agent_client.Developer_connection.pending_submits developer in
    Agent_client.Developer_connection.adopt_pending resumed original |> must;
    assert (Result.is_error (Agent_client.Developer_connection.send resumed request));
    assert (Int.equal !attempts 3);
    assert (
      Result.is_error
        (Agent_client.Developer_connection.retry_pending resumed (List.hd_exn original)));
    assert (Int.equal !attempts 4);
    let current_owner =
      C.Owner.create
        ~host_id:initialized.server_id
        ~run_id:(C.Submit.run_id submission)
        ~epoch:1
        ~principal_id:initialized.principal.id
      |> must
    in
    let current_view =
      C.View.create
        ~limits
        ~command_id:(P.Developer_command_id.of_string "cmd_current" |> must)
        ~owner:current_owner
        ~key:(C.Submit.key submission)
        ~kind:Check
        ~phase:(C.View.Terminal { execution_started = false; receipt })
        ~request_fingerprint:digest
        ~capture_fingerprint:digest
      |> must
    in
    reply
    := P.Developer_wire.Response.to_json
         (P.Developer_wire.Response.Submit current_view)
         ~limits
       |> must
       |> Result.return;
    assert (Int.equal !attempts 4);
    ignore
      (Agent_client.Developer_connection.retry_pending resumed (List.hd_exn original)
       |> must
       : C.View.t);
    assert (Int.equal !attempts 5);
    assert (List.is_empty (Agent_client.Developer_connection.pending_submits resumed));
    other_reply
    := Error
         (P.Error.create
            Invalid_request
            ~message:(String.make 70_000 'e')
            ~retryable:false
            ());
    let lookup =
      C.Lookup.create ~run_id:(C.Submit.run_id submission) ~key:(C.Submit.key submission)
    in
    (match
       Agent_client.Developer_connection.send
         resumed
         (P.Developer_wire.Request.Observe lookup)
     with
     | Error { code = Incompatible_protocol; _ } -> ()
     | Error error -> raise_s [%sexp "unbounded in-memory error", (error : P.Error.t)]
     | Ok _ -> failwith "unbounded in-memory error was accepted");
    print_endline
      "uncertain Submit retains original; only successful explicit retry clears it");
  [%expect
    {| uncertain Submit retains original; only successful explicit retry clears it |}]
;;

let%expect_test "resync hint requires an exact-key Watch and Observe baseline" =
  Eio_main.run (fun _ ->
    let initialized = initialization ~events:true () in
    let capabilities =
      C.Capabilities.create
        ~format_version:1
        ~methods:[ "developer.capabilities"; "developer.watch"; "developer.observe" ]
        ~limits
      |> must
    in
    let original = submit () in
    let run_id = C.Submit.run_id original in
    let key = C.Submit.key original in
    let lookup = C.Lookup.create ~run_id ~key in
    let same_lookup requested =
      P.Id.Developer_run.equal requested.C.Lookup.run_id run_id
      && P.Idempotency_key.equal requested.key key
    in
    let receipt =
      P.Developer_receipt.create
        ~limits:(C.Limits.receipt limits)
        (P.Developer_receipt.Check P.Developer_receipt.Check.Ready)
      |> must
    in
    let owner =
      C.Owner.create
        ~host_id:initialized.server_id
        ~run_id
        ~epoch:1
        ~principal_id:initialized.principal.id
      |> must
    in
    let digest = Chatml_source.Digest.of_contents "resync" ~checkpoint:Fn.id in
    let view =
      C.View.create
        ~limits
        ~command_id:(P.Developer_command_id.of_string "cmd_resync" |> must)
        ~owner
        ~key
        ~kind:Check
        ~phase:(C.View.Terminal { execution_started = false; receipt })
        ~request_fingerprint:digest
        ~capture_fingerprint:digest
      |> must
    in
    let watched = ref false in
    let observed = ref false in
    let public_initialized =
      P.Public.Result.Non_history.of_internal (Protocol_initialize initialized)
      |> must
      |> fun value -> P.Public.Result.Non_history value
    in
    let transport =
      Agent_client.Transport.create_with_developer
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok public_initialized
          | _ -> Error (P.Error.invalid_request "unexpected command"))
        ~request_developer:(fun ~bound:_ request ->
          match request with
          | P.Developer_wire.Request.Capabilities ->
            Ok (C.Capabilities.to_json capabilities)
          | Watch requested when same_lookup requested ->
            watched := true;
            P.Developer_wire.Response.to_json Watch ~limits
          | Observe requested when same_lookup requested && !watched ->
            observed := true;
            P.Developer_wire.Response.to_json (Observe (Some view)) ~limits
          | _ -> Error (P.Error.invalid_request "unexpected recovery request"))
        ~next_notification:(fun () -> None)
        ~close:Fn.id
    in
    let connection = Agent_client.Connection.create transport in
    initialize ~events:true connection initialized;
    let developer = Agent_client.Developer_connection.create connection |> must in
    let resync =
      P.Developer_wire.Change.create ~limits ~run_id ~key ~kind:Resync_required |> must
    in
    let envelope =
      P.Envelope.notification
        ~method_:P.Developer_wire.Change.method_name
        ~params:(P.Developer_wire.Change.to_json resync)
        ()
    in
    (match
       Agent_client.Developer_connection.change_of_notification developer envelope |> must
     with
     | Some change ->
       (match change.kind with
        | Resync_required -> ()
        | Command_changed | Approval_changed -> failwith "resync hint changed kind")
     | None -> failwith "resync hint was ignored");
    let recovered =
      Agent_client.Developer_connection.watch_and_observe developer lookup
      |> must
      |> Option.value_exn
    in
    assert !watched;
    assert !observed;
    assert (
      P.Developer_command_id.equal (C.View.command_id recovered) (C.View.command_id view));
    print_endline "resync hint rewatched and reobserved the exact key");
  [%expect {| resync hint rewatched and reobserved the exact key |}]
;;
