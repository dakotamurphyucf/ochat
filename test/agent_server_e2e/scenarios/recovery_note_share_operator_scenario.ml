open! Core
module P = Agent_protocol
module W = P.Recovery_note_share_control
module Operator = Agent_client.Recovery_windows.Note_shares
module Handle = Agent_client.Session_handle
module Connection = Agent_client.Connection
module Http = Support.Http_driver
module Integrity = Support.Data_integrity_http
module Config = Support.Config_fixture
module F = Crash_recovery_fixture
module Stock = Recovery_capacity_scenario
module Provider = Support.Tui_stream_provider
module Notes = Recovery_note_provider_scenario
module Sharing = Recovery_note_shared_provider_scenario
module Foreground = Recovery_checkpoint_scenario

let require = F.require
let key = F.key
let note_path = "handoff/source-owner-private"
let note_body = "Operator share β across durable sessions."

let capabilities connection features =
  Handle.initialize_with_features
    connection
    ~implementation_name:"note-share-control-e2e"
    ~implementation_version:"dev"
    ~features
  |> F.protocol_ok
;;

let fresh_client ~sw env fixture token =
  Http.create ~sw ~env ~port:(Config.http_port fixture) ~token:(Some token)
  |> Result.ok_or_failwith
;;

let expect_error (result : (_, P.Error.t) result) ~code =
  match result with
  | Error failure when P.Error.equal_code failure.code code -> failure
  | Error failure -> raise_s [%sexp "unexpected note share error", (failure : P.Error.t)]
  | Ok _ -> F.fail "note share request unexpectedly succeeded"
;;

let create_session client ~mode name =
  let session, attached = Integrity.create ~mode client name in
  Stock.start client session attached.id (name ^ "-start");
  session, attached
;;

let tool env provider client (session : P.Session.t) attachment_id ~name ~call_id args =
  let before = Provider.request_count provider in
  let command =
    P.Command.Session_send_message
      { session_id = session.id
      ; attachment_id
      ; timing = Safe_boundary
      ; content = { kind = Plain_text; text = "Run " ^ call_id; attachments = [] }
      ; idempotency_key = key call_id
      }
  in
  (* The stock daemon can hold its ordinary lifecycle lane briefly while
     releasing a prior request. Retry the same message key on busy only. *)
  Support.Background_fixture.await env call_id (fun () ->
    match Http.request_without_history client command with
    | Error { code = Conflict; retryable = true; _ } -> None
    | result ->
      (match F.protocol_ok result with
       | Session_send_message { operation_id = Some _; _ } -> Some ()
       | _ -> F.fail "note fixture did not admit the requested foreground operation"));
  Notes.function_call (Provider.await_request provider env before) ~id:call_id ~name args;
  let followup = Provider.await_request provider env (before + 1) in
  let result = Notes.output followup ~id:call_id in
  Provider.reply_summary followup "Tool operation finished.";
  Foreground.await_idle env client session;
  result
;;

let exact_receipt (a : W.Receipt.t) (b : W.Receipt.t) =
  W.Reference.equal a.reference b.reference
  && Int64.equal a.share_revision b.share_revision
;;

let verify_unix_owner
      ~sw
      env
      fixture
      ~server_id
      (http_source : P.Session.t)
      foreign_writer
  =
  let connection =
    Support.Unix_driver.connect ~sw ~env ~socket_path:(Config.unix_socket fixture)
  in
  Exn.protect
    ~finally:(fun () -> Connection.close connection)
    ~f:(fun () ->
      ignore (capabilities connection [ W.feature ] : P.Initialize.Response.t);
      ignore
        (expect_error
           (Operator.inspect connection foreign_writer ())
           ~code:Permission_denied
         : P.Error.t);
      match
        Connection.request
          connection
          (Session_create
             { spec = http_source.spec
             ; requested_mode = Some Owner_read_write
             ; subscribe = false
             ; idempotency_key = key "unix-note-owner"
             })
        |> F.protocol_ok
      with
      | Session_create { session; attachment = Some attached; _ } ->
        let source =
          W.Source.create
            (P.Session_ref.create ~server_id ~session_id:session.id)
            ~attachment_id:attached.attachment.id
            ~expected_generation:session.generation
          |> F.protocol_ok
        in
        let initial = Operator.inspect connection source () |> F.protocol_ok in
        require
          (Int64.equal initial.basis.catalog_revision 0L
           && Int64.equal initial.basis.share_revision 0L
           && Option.is_none initial.selection)
          "Unix connection did not get its own empty source basis"
      | _ -> F.fail "could not create the stock Unix source owner")
;;

let run env environment =
  let fixture = F.fixture env environment "note-share-operator" in
  Config.grant_public_all_scopes fixture;
  F.write env (Config.prompt_path fixture) Notes.default_prompt;
  let config_path =
    Stock.configuration fixture ""
    |> String.substr_replace_first
         ~pattern:"(idle_connection_timeout_ms 5000)"
         ~with_:"(idle_connection_timeout_ms 60000)"
    |> Config.write_configuration fixture ~name:"note-shares.sexp"
  in
  Eio.Switch.run (fun sw ->
    let port =
      let reservation = Support.Port_reservation.create ~sw ~env in
      let port = Support.Port_reservation.port reservation in
      Support.Port_reservation.release reservation;
      port
    in
    let provider = Provider.start ~sw ~env ~port in
    Stock.with_stock
      env
      fixture
      ~port
      ~config_path
      (fun first first_connection server_id ->
         ignore (capabilities first_connection [ W.feature ] : P.Initialize.Response.t);
         let source, attachment =
           create_session first ~mode:Owner_read_write "notes-source"
         in
         let recipient_client =
           fresh_client ~sw env fixture (Config.public_token fixture)
         in
         Eio.Switch.on_release sw (fun () -> Http.shutdown recipient_client);
         let recipient_connection = Integrity.connection recipient_client in
         ignore (capabilities recipient_connection [] : P.Initialize.Response.t);
         let recipient, recipient_attachment =
           create_session recipient_client ~mode:Read_write "notes-recipient"
         in
         let session_ref (session : P.Session.t) =
           P.Session_ref.create ~server_id ~session_id:session.id
         in
         let source_ref = session_ref source in
         let recipient_ref = session_ref recipient in
         let writer ~generation attachment_id =
           W.Source.create source_ref ~attachment_id ~expected_generation:generation
           |> F.protocol_ok
         in
         let original_writer = writer ~generation:source.generation attachment.id in
         verify_unix_owner ~sw env fixture ~server_id source original_writer;
         let path = W.Path.of_string note_path |> F.protocol_ok in
         let before =
           Operator.inspect first_connection original_writer ~path () |> F.protocol_ok
         in
         require (Option.is_none before.selection) "fresh source unexpectedly has a note";
         let changed =
           tool
             env
             provider
             first
             source
             attachment.id
             ~name:"recovery_note_write"
             ~call_id:"operator-create-note"
             (`Object
                 [ "kind", `String "create"
                 ; "expected", `Null
                 ; ( "expected_catalog_revision"
                   , `String (Int64.to_string before.basis.catalog_revision) )
                 ; "path", `String note_path
                 ; "content", `String note_body
                 ])
         in
         (match changed with
          | Complete `Null -> ()
          | _ ->
            F.fail "selected native write did not return its body-free acknowledgment");
         let inspected =
           Operator.inspect first_connection original_writer ~path () |> F.protocol_ok
         in
         let selection = Option.value_exn inspected.selection in
         require
           (Int64.equal selection.id 1L
            && Int64.equal selection.revision 1L
            && Int64.equal inspected.basis.catalog_revision 1L)
           "exact authorized source coordinates did not match the native write";
         let basis_only =
           Operator.inspect first_connection original_writer () |> F.protocol_ok
         in
         require (Option.is_none basis_only.selection) "basis-only read enumerated a note";
         let stale = writer ~generation:(source.generation + 1) attachment.id in
         ignore
           (expect_error
              (Operator.inspect first_connection stale ~path ())
              ~code:Permission_denied
            : P.Error.t);
         let expires_at =
           P.Timestamp.add_ms (P.Timestamp.now ()) 60_000 |> F.protocol_ok
         in
         let create connection writer ~recipient =
           Operator.create
             connection
             writer
             ~expected:inspected.basis
             ~selection
             ~recipient
             ~expires_at
             ~key:(key "source-share-original")
         in
         let first_receipt =
           create first_connection original_writer ~recipient:recipient_ref
           |> F.protocol_ok
         in
         ignore
           (expect_error
              (Http.request
                 recipient_client
                 (Recovery_note_shares
                    (Inspect { source = original_writer; path = Some path })))
              ~code:Method_not_found
            : P.Error.t);
         ignore
           (capabilities recipient_connection [ W.feature ] : P.Initialize.Response.t);
         let unauthorized =
           Operator.inspect recipient_connection original_writer ~path ()
         in
         let error = expect_error unauthorized ~code:Permission_denied in
         let denied ~recipient =
           expect_error
             (create recipient_connection original_writer ~recipient)
             ~code:Permission_denied
         in
         let fake_target =
           P.Session_ref.create ~server_id ~session_id:(P.Id.Session.create ())
         in
         let assert_same a b =
           require
             (Document_schema.Json.equal (P.Error.to_json a) (P.Error.to_json b))
             "denial depended on the supplied private coordinate"
         in
         assert_same error (denied ~recipient:recipient_ref);
         assert_same error (denied ~recipient:fake_target);
         let read ~id reference =
           tool
             env
             provider
             recipient_client
             recipient
             recipient_attachment.id
             ~name:"recovery_note_read"
             ~call_id:id
             (Sharing.shared_range reference ~start:15 ~stop:17)
         in
         (match read ~id:"operator-recipient-read" first_receipt.reference with
          | Complete value ->
            [%test_eq: string] (Notes.field "text" value |> Jsonaf.string_exn) "β"
          | _ ->
            F.fail "recipient did not read the reference returned to the source owner");
         let second = fresh_client ~sw env fixture (Config.admin_token fixture) in
         let second_connection = Integrity.connection second in
         Exn.protect
           ~finally:(fun () -> Http.shutdown second)
           ~f:(fun () ->
             ignore
               (capabilities second_connection [ W.feature ] : P.Initialize.Response.t);
             ignore
               (expect_error
                  (create second_connection original_writer ~recipient:recipient_ref)
                  ~code:Permission_denied
                : P.Error.t);
             ignore (Http.close_connection first |> Result.ok_or_failwith : Http.response);
             ignore
               (expect_error
                  (Operator.inspect first_connection original_writer ~path ())
                  ~code:Invalid_request
                : P.Error.t);
             let owner = Stock.reattach second source "reconnected-owner" in
             let active_writer = writer ~generation:source.generation owner in
             let recovered =
               create second_connection active_writer ~recipient:recipient_ref
               |> F.protocol_ok
             in
             require
               (exact_receipt recovered first_receipt)
               "reconnect changed the original grant receipt";
             ignore
               (expect_error
                  (Operator.create
                     second_connection
                     active_writer
                     ~expected:inspected.basis
                     ~selection
                     ~recipient:recipient_ref
                     ~expires_at:(P.Timestamp.add_ms expires_at 1 |> F.protocol_ok)
                     ~key:(key "source-share-original"))
                  ~code:Idempotency_conflict
                : P.Error.t);
             let revoke () =
               Operator.revoke
                 second_connection
                 active_writer
                 ~expected_share_revision:recovered.share_revision
                 recovered.reference
                 ~key:(key "source-revoke-original")
               |> F.protocol_ok
             in
             let revoked = revoke () in
             require
               (Int64.equal revoked.share_revision (Int64.succ recovered.share_revision)
                && W.Reference.equal revoked.reference recovered.reference
                && exact_receipt revoked (revoke ()))
               "revocation retry changed its factual receipt";
             let historical =
               create second_connection active_writer ~recipient:recipient_ref
               |> F.protocol_ok
             in
             require
               (exact_receipt historical first_receipt)
               "original create replay changed after its grant was withdrawn";
             let revoked_outcome =
               read ~id:"operator-recipient-revoked" historical.reference
             in
             (match revoked_outcome with
              | Fail { code = "recovery.unavailable"; details = `Null; _ } -> ()
              | _ -> F.fail "revocation did not stop subsequent protected delivery");
             let page = P.Page.Request.create ~limit:100 () |> F.protocol_ok in
             let audit =
               match
                 F.request
                   second
                   (Audit_read
                      { page
                      ; session_id = Some source.id
                      ; principal_id = None
                      ; minimum_level = None
                      ; name_prefix = None
                      })
               with
               | Audit_read page -> List.map page.items ~f:P.Audit.to_json
               | _ -> F.fail "unexpected source audit reply"
             in
             let logged = Jsonaf.to_string (`Array audit) in
             require
               (String.is_substring logged ~substring:"session.recovery.notes.share")
               "source grant operation was missing from redacted audit";
             let response = F.get second source.id in
             let projected =
               P.Public.Snapshot.Fields.sexp_of_t response |> Sexp.to_string
             in
             List.iter
               [ note_path
               ; note_body
               ; Notes.field "nonce" (W.Reference.to_json recovered.reference)
                 |> Jsonaf.string_exn
               ; P.Id.Session.to_string recipient.id
               ; "source-share-original"
               ; "source-revoke-original"
               ]
               ~f:(fun marker ->
                 require
                   ((not (String.is_substring logged ~substring:marker))
                    && not (String.is_substring projected ~substring:marker))
                   "private share data escaped the stock response or audit boundary"))))
;;
