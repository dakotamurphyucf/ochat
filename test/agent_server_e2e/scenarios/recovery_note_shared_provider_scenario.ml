open Core
open Agent_server_test_support
module C = Recovery_notes.Catalog
module H = Agent_client.Session_handle
module P = Agent_protocol
module A = Agent_session.Session_actor
module S = Agent_session.Recovery_note_share
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module Shares = Agent_server.Session_factory.Recovery_note_shares
module F = Recovery_note_provider_scenario
module Provider = Support.Tui_stream_provider

let state (entry : Agent_server.Session_registry.entry) =
  A.state entry.actor |> protocol_ok
;;

let key name = P.Idempotency_key.of_string name |> protocol_ok
let private_body = "share-β-private-fixture"

let shared_range reference ~start ~stop =
  `Object
    [ ( "selection"
      , `Object [ "kind", `String "shared"; "reference", S.Reference.to_json reference ] )
    ; ( "range"
      , `Object
          [ "start", `Number (Int.to_string start); "stop", `Number (Int.to_string stop) ]
      )
    ]
;;

let test env environment =
  let before_auth = ref ignore in
  let auth ~sw:_ _ =
    !before_auth ();
    Openai.Responses_driver.Auth.bearer "recovery-note-shared-loopback-fixture"
  in
  F.with_fixture
    env
    environment
    ~auth
    ~max_sessions:3
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let open_session ~mode =
         let handle = F.open_session sw env client ~mode in
         ignore (H.start handle ~queue_if_limited:false |> protocol_ok : P.Session.t);
         handle
       in
       let source = open_session ~mode:Owner_read_write in
       let recipient = open_session ~mode:Read_write in
       let other = open_session ~mode:Read_write in
       let registry = D.registry daemon in
       let entry handle = R.find registry (H.session_id handle) |> Option.value_exn in
       let reference handle =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
           ~session_id:(H.session_id handle)
       in
       let source_entry = entry source in
       let factory = D.factory daemon in
       let owner = reference source in
       let access =
         Agent_session.Recovery_note_access.operator
           proof
           ~owner
           ~generation:(state source_entry).identity.generation
           ~attachment_id:(Some (H.attachment source).id)
           ~authorize:(fun _ ->
             Agent_server.Session_factory.check_registered_owner
               factory
               source_entry
               ~session:owner)
       in
       let mutate key_name operation =
         A.mutate_recovery_note_operator
           source_entry.actor
           ~access
           ~key:(key key_name)
           operation
         |> protocol_ok
       in
       let note =
         mutate
           "shared-provider-note"
           (Create
              { expected_catalog_revision = C.Catalog_revision.of_int64 0L |> protocol_ok
              ; path = C.Note_path.of_string "handoff/shared" |> protocol_ok
              ; content = C.Content.of_string private_body |> protocol_ok
              })
       in
       let revision () =
         Shares.management_revision factory ~source:source_entry ~access |> protocol_ok
       in
       let grant (note : C.Receipt.t) label expires_at =
         Shares.create
           factory
           ~source:source_entry
           ~access
           ~recipient:(reference recipient)
           ~key:(key label)
           { expected_revision = revision ()
           ; expected_catalog_revision = note.catalog_revision
           ; selection = note.selection
           ; expires_at
           }
         |> protocol_ok
         |> S.Receipt.reference
       in
       let later delay = P.Timestamp.add_ms (P.Timestamp.now ()) delay |> protocol_ok in
       let allowed = grant note "shared-provider-grant" (later 60_000) in
       let expires_at = later 2000 in
       let expired = grant note "shared-provider-expired" expires_at in
       let count () = Provider.request_count provider in
       let await_request = Provider.await_request provider env in
       let await_idle handle =
         Support.Tui_fixture.await env (fun () ->
           let current = state (entry handle) in
           if Option.is_none current.active_operation then Some current else None)
       in
       let send_read handle ~label ~provider_index =
         let session_id = H.session_id handle in
         let original = entry handle in
         let command =
           P.Command.Session_send_message
             { session_id
             ; attachment_id = (H.attachment handle).id
             ; idempotency_key = key ("shared-provider-read-" ^ label)
             ; timing = Safe_boundary
             ; content = { kind = Plain_text; text = label; attachments = [] }
             }
         in
         let send = Agent_client.Connection.request_without_history client in
         let rec submit () =
           match send command with
           | Ok (Session_send_message submission) -> submission
           | Error
               { code = Conflict
               ; message = "session lifecycle is already reserved"
               ; retryable = true
               ; _
               } ->
             (* Snapshot retention can outlive the idle Actor observation. Join the
                actual reservation before replaying this proven unadmitted send. *)
             let receipt =
               P.Command.Command_receipt
                 { method_name = P.Command.method_name command
                 ; original_params = P.Command.params command
                 }
             in
             (match send receipt |> protocol_ok with
              | Command_receipt Missing -> ()
              | _ -> failwith "lifecycle-conflicted read send has an admitted receipt");
             [%test_eq: int] (count ()) provider_index;
             R.with_loaded_read registry session_id (fun current ->
               assert (phys_equal current original);
               Ok ())
             |> protocol_ok;
             submit ()
           | Ok _ -> failwith "expected a shared-read send receipt"
           | Error error -> protocol_ok (Error error)
         in
         Eio.Time.Timeout.run_exn
           (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.)
           submit
       in
       let start_read handle request ~label ~before_dispatch =
         let index = count () in
         let submission = send_read handle ~label ~provider_index:index in
         assert (Option.is_some submission.operation_id);
         let initial = await_request index in
         before_dispatch ();
         F.function_call initial ~id:label ~name:"recovery_note_read" request;
         index + 1
       in
       let check_read handle request ~label f =
         let reply =
           await_request (start_read handle request ~label ~before_dispatch:ignore)
         in
         f (F.output reply ~id:label);
         Provider.reply_summary reply "Finished";
         ignore (await_idle handle : Agent_session.Session_state.t)
       in
       let unavailable = function
         | P.Invocation.Fail { code = "recovery.unavailable"; details = `Null; _ } -> ()
         | outcome ->
           raise_s
             [%sexp
               "expected one shared-unavailable result", (outcome : P.Invocation.outcome)]
       in
       let read reference = shared_range reference ~start:6 ~stop:8 in
       check_read recipient (read allowed) ~label:"allowed-share" (function
         | Complete value ->
           [%test_eq: string] (F.field "text" value |> Jsonaf.string_exn) "β";
           [%test_eq: int] (F.field "start" value |> Jsonaf.int_exn) 6;
           [%test_eq: int] (F.field "stop" value |> Jsonaf.int_exn) 8
         | outcome ->
           raise_s [%sexp "expected exact shared range", (outcome : P.Invocation.outcome)]);
       check_read other (read allowed) ~label:"wrong-recipient" unavailable;
       check_read
         recipient
         (shared_range allowed ~start:7 ~stop:8)
         ~label:"invalid-scalar"
         unavailable;
       let expiry =
         P.Timestamp.to_time_ns expires_at
         |> Time_ns.to_span_since_epoch
         |> Time_ns.Span.to_sec
       in
       Eio.Time.sleep_until (Eio.Stdenv.clock env) expiry;
       check_read recipient (read expired) ~label:"expired-share" unavailable;
       let cutpoint reference ~label mutate_source =
         let entered, materialized = Eio.Promise.create () in
         let resumed, resume = Eio.Promise.create () in
         let before_dispatch () =
           before_auth
           := fun () ->
                before_auth := ignore;
                Eio.Promise.resolve materialized ();
                Eio.Promise.await resumed
         in
         let expected_requests =
           start_read recipient (read reference) ~label ~before_dispatch
         in
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           Eio.Promise.await entered);
         mutate_source ();
         Eio.Promise.resolve resume ();
         ignore (await_idle recipient : Agent_session.Session_state.t);
         [%test_eq: int] (count ()) expected_requests
       in
       let revoked = grant note "shared-provider-revoked" (later 60_000) in
       cutpoint revoked ~label:"revoked-before-write" (fun () ->
         Shares.revoke
           factory
           ~source:source_entry
           ~access
           ~key:(key "shared-provider-revoke")
           { expected_revision = revision (); reference = revoked }
         |> protocol_ok
         |> ignore);
       check_read recipient (read revoked) ~label:"revoked-share" unavailable;
       let assert_private_state (current : Agent_session.Session_state.t) =
         let grant_json = S.Reference.to_json allowed in
         let private_values =
           [ private_body
           ; "β"
           ; "γ"
           ; Jsonaf.to_string grant_json
           ; F.field "nonce" grant_json |> Jsonaf.string_exn
           ; P.Id.Session.to_string (H.session_id source)
           ]
         in
         let assert_private encoded =
           List.iter private_values ~f:(fun substring ->
             assert (not (String.is_substring encoded ~substring)))
         in
         List.iter current.invocations ~f:(fun invocation ->
           assert (Document_schema.Json.equal invocation.context.input `Null);
           assert_private (P.Invocation.to_json invocation |> Jsonaf.to_string));
         List.iter current.conversation.canonical_history ~f:(fun entry ->
           assert_private (P.History.sexp_of_entry entry |> Sexp.to_string))
       in
       assert_private_state (state (entry recipient));
       let reset handle =
         let generation = (state (entry handle)).identity.generation in
         ignore (H.stop handle ~mode:Cancel |> protocol_ok : P.Session.t);
         ignore
           (H.reset
              handle
              ~expected_revision:(state (entry handle)).counters.revision
              ~keep_history:false
              ~keep_tasks:false
              ~keep_cache:false
              ~keep_workspace:true
              ~keep_grants:true
              ~keep_labels:true
            |> protocol_ok
            : P.Session.t);
         assert ((state (entry handle)).identity.generation > generation)
       in
       reset recipient;
       ignore (H.start recipient ~queue_if_limited:false |> protocol_ok : P.Session.t);
       check_read recipient (read allowed) ~label:"replaced-recipient" unavailable;
       let edited = grant note "shared-provider-to-edit" (later 60_000) in
       let content = C.Content.of_string "share-γ-new-head" |> protocol_ok in
       let updated =
         mutate "replace-source-head" (Replace { expected = note.selection; content })
       in
       check_read recipient (read edited) ~label:"edited-source" unavailable;
       let replaced = grant updated "shared-provider-to-replace" (later 60_000) in
       cutpoint replaced ~label:"replaced-source-before-write" (fun () -> reset source);
       assert_private_state (state (entry recipient));
       List.iter [ source; recipient; other ] ~f:H.close)
;;
