open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server
module A = Agent_session

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let text value : P.Session.Message_content.t =
  { kind = Plain_text; text = value; attachments = [] }
;;

let with_daemon sw env ~root config f =
  let daemon = Catalog_service_tests.start_catalog_daemon sw env ~root config in
  let clients = ref [] in
  let own client =
    clients := client :: !clients;
    client
  in
  let connect principal = connection daemon principal |> own in
  Exn.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Exn.protect
          ~finally:(fun () -> S.Daemon.shutdown daemon |> protocol_ok)
          ~f:(fun () -> List.iter !clients ~f:C.Connection.close)))
    ~f:(fun () -> f daemon ~connect ~own)
;;

let%expect_test
    "lost pending replacement reply reconciles original command and current authority"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Pending recovery fixture.</developer>";
        Eio.Switch.run (fun sw ->
          with_daemon
            sw
            env
            ~root
            (config root workspace prompt_file)
            (fun daemon ~connect ~own ->
               let creator = connect (principal_with_id "pri_foreign_pending_creator") in
               initialize creator;
               let session, _ = create_session creator in
               let original = connect (principal ()) in
               initialize original;
               let handle =
                 C.Session_handle.attach
                   ~sw
                   ~clock:(Eio.Stdenv.clock env)
                   ~connection:original
                   ~session_id:session.id
                   ~mode:Read_write
                   ~subscribe:false
                   ()
                 |> protocol_ok
               in
               C.Session_handle.start handle ~queue_if_limited:false
               |> protocol_ok
               |> ignore;
               let entry =
                 S.Session_registry.find (S.Daemon.registry daemon) session.id
                 |> Option.value_exn
               in
               let started, started_u = Eio.Promise.create () in
               let finish, finish_u = Eio.Promise.create () in
               let runs = ref 0 in
               let worker =
                 A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                   Int.incr runs;
                   if Int.equal !runs 1
                   then (
                     Eio.Promise.resolve started_u ();
                     Eio.Promise.await finish);
                   Completed
                     { final_history = input.history
                     ; runtime_requests = []
                     ; moderator_snapshot = None
                     })
               in
               A.Session_actor.set_operation_worker entry.actor (Some worker)
               |> protocol_ok;
               Exn.protect
                 ~finally:(fun () -> ignore (Eio.Promise.try_resolve finish_u ()))
                 ~f:(fun () ->
                   Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                     C.Session_handle.send_message handle (text "root held")
                     |> protocol_ok
                     |> ignore;
                     Eio.Promise.await started;
                     let pending_submission =
                       C.Session_handle.send_message
                         handle
                         ~timing:After_current_operation
                         (text "pending original")
                       |> protocol_ok
                     in
                     let second_submission =
                       C.Session_handle.send_message handle (text "second FIFO input")
                       |> protocol_ok
                     in
                     let first_page =
                       C.Session_handle.pending_inputs
                         handle
                         (P.Page.Request.create ~limit:1 () |> protocol_ok)
                       |> protocol_ok
                     in
                     let cursor = Option.value_exn first_page.page.next_cursor in
                     let next_page_request =
                       P.Page.Request.create ~limit:1 ~cursor () |> protocol_ok
                     in
                     let second_page =
                       C.Session_handle.pending_inputs handle next_page_request
                       |> protocol_ok
                     in
                     let oversized =
                       C.Session_handle.pending_inputs
                         handle
                         (P.Page.Request.create ~limit:1001 () |> protocol_ok)
                     in
                     let cursor_foreign =
                       connect (principal_with_id "pri_pending_cursor_foreign")
                     in
                     initialize cursor_foreign;
                     let foreign_cursor =
                       C.Connection.request_without_history
                         cursor_foreign
                         (Session_pending_inputs
                            (P.Pending_query.Request.create
                               ~session_id:session.id
                               ~page:next_page_request
                             |> protocol_ok))
                     in
                     let state = A.Session_actor.state entry.actor |> protocol_ok in
                     let queued =
                       List.hd_exn state.conversation.deferred_user_entries
                       |> A.Pending_input_document.entry
                     in
                     let target =
                       P.Pending_control.Cancel_request.create
                         ~session_id:session.id
                         ~attachment_id:(C.Session_handle.attachment handle).id
                         ~expected_generation:state.identity.generation
                         ~expected_pending_revision:state.conversation.pending_revision
                         ~history_id:pending_submission.history_id
                         ~expected_content_revision:queued.content_revision
                         ~idempotency_key:
                           (P.Idempotency_key.of_string "pending-lost-replacement"
                            |> protocol_ok)
                       |> protocol_ok
                     in
                     let request =
                       P.Pending_control.Replace_request.create
                         ~target
                         ~text:"private saved replacement"
                       |> protocol_ok
                     in
                     let command = P.Command.Session_replace_pending_input request in
                     let replacements = ref 0 in
                     let lossy =
                       C.Transport.create
                         ~request:(fun command ->
                           let result = C.Connection.request original command in
                           if
                             String.equal
                               (P.Command.method_name command)
                               "session.replace_pending_input"
                           then (
                             Int.incr replacements;
                             Error
                               (P.Error.create
                                  Interrupted
                                  ~message:"reply lost after actual pending commit"
                                  ~retryable:true
                                  ()))
                           else result)
                         ~next_notification:(fun () -> None)
                         ~close:Fn.id
                       |> C.Connection.create
                       |> own
                     in
                     initialize lossy;
                     let lost = C.Connection.request_without_history lossy command in
                     let unresolved = C.Connection.pending_commands lossy in
                     let saved = A.Session_actor.state entry.actor |> protocol_ok in
                     let stale_cursor =
                       C.Session_handle.pending_inputs handle next_page_request
                     in
                     let reduced =
                       principal_with_scopes
                         (P.Id.Principal.to_string (principal ()).id)
                         (P.Scope.Set.of_list [ Send_messages; View_session_transcript ])
                     in
                     let reduced_client = connect reduced in
                     initialize reduced_client;
                     let replay =
                       C.Connection.request_without_history reduced_client command
                     in
                     let no_transcript =
                       connect
                         (principal_with_scopes
                            (P.Id.Principal.to_string (principal ()).id)
                            (P.Scope.Set.of_list
                               [ Send_messages; Administer_configuration ]))
                     in
                     initialize no_transcript;
                     let receipt_request =
                       P.Command_receipt.Request.
                         { method_name = P.Command.method_name command
                         ; original_params = P.Command.params command
                         }
                     in
                     let hidden_receipt =
                       C.Connection.request_without_history
                         no_transcript
                         (Command_receipt receipt_request)
                     in
                     let foreign =
                       connect (principal_with_id "pri_foreign_pending_recovery")
                     in
                     initialize foreign;
                     let foreign_transfer =
                       C.Connection.adopt_pending foreign unresolved
                     in
                     let wrong_host_requests = ref 0 in
                     let wrong_host =
                       C.Transport.create
                         ~request:(fun command ->
                           match C.Connection.request original command with
                           | Ok (Non_history result) ->
                             (match P.Public.Result.Non_history.value result with
                              | Protocol_initialize response ->
                                Ok
                                  (P.Public.Result.Non_history
                                     (P.Public.Result.Non_history.of_internal
                                        (Protocol_initialize
                                           { response with
                                             server_id = P.Id.Server.create ()
                                           })
                                      |> protocol_ok))
                              | _ ->
                                Int.incr wrong_host_requests;
                                Ok (P.Public.Result.Non_history result))
                           | result ->
                             Int.incr wrong_host_requests;
                             result)
                         ~next_notification:(fun () -> None)
                         ~close:Fn.id
                       |> C.Connection.create
                       |> own
                     in
                     initialize wrong_host;
                     let wrong_host_transfer =
                       C.Connection.adopt_pending wrong_host unresolved
                     in
                     let wrong_host_reconcile =
                       C.Connection.reconcile wrong_host (List.hd_exn unresolved)
                     in
                     let never_submitted =
                       P.Pending_control.Cancel_request.create
                         ~session_id:session.id
                         ~attachment_id:(C.Session_handle.attachment handle).id
                         ~expected_generation:saved.identity.generation
                         ~expected_pending_revision:saved.conversation.pending_revision
                         ~history_id:target.history_id
                         ~expected_content_revision:
                           (List.hd_exn saved.conversation.deferred_user_entries
                            |> A.Pending_input_document.entry)
                             .content_revision
                         ~idempotency_key:
                           (P.Idempotency_key.of_string "pending-not-submitted"
                            |> protocol_ok)
                       |> protocol_ok
                       |> fun request -> P.Command.Session_cancel_pending_input request
                     in
                     let dropped =
                       C.Transport.create
                         ~request:(fun command ->
                           if
                             String.equal
                               (P.Command.method_name command)
                               "session.cancel_pending_input"
                           then
                             Error
                               (P.Error.create
                                  Interrupted
                                  ~message:"request lost before admission"
                                  ~retryable:true
                                  ())
                           else C.Connection.request original command)
                         ~next_notification:(fun () -> None)
                         ~close:Fn.id
                       |> C.Connection.create
                       |> own
                     in
                     initialize dropped;
                     C.Connection.request_without_history dropped never_submitted
                     |> ignore;
                     let missing_pending =
                       List.hd_exn (C.Connection.pending_commands dropped)
                     in
                     let missing_receipt =
                       C.Connection.reconcile dropped missing_pending |> protocol_ok
                     in
                     let missing_still_uncertain =
                       not (List.is_empty (C.Connection.pending_commands dropped))
                     in
                     let recovery = connect (principal ()) in
                     initialize recovery;
                     C.Connection.adopt_pending recovery unresolved |> protocol_ok;
                     let receipt =
                       C.Connection.reconcile recovery (List.hd_exn unresolved)
                       |> protocol_ok
                     in
                     let after = A.Session_actor.state entry.actor |> protocol_ok in
                     let lookup =
                       C.Connection.request_without_history
                         recovery
                         (Session_pending_input
                            { session_id = session.id; history_id = target.history_id })
                       |> protocol_ok
                     in
                     printf
                       "FIFO-page-one=%b FIFO-page-two=%b complete=%b page-bound=%s \
                        foreign-cursor=%s stale-cursor=%s\n"
                       (P.History.Id.equal
                          (List.hd_exn first_page.page.items).history.id
                          pending_submission.history_id)
                       (P.History.Id.equal
                          (List.hd_exn second_page.page.items).history.id
                          second_submission.history_id)
                       (Option.is_none second_page.page.next_cursor)
                       (status oversized)
                       (status foreign_cursor)
                       (status stale_cursor);
                     printf
                       "lost=%s retained-original=%d actual-replacements=%d \
                        current-visibility-replay=%s receipt-without-transcript=%s \
                        foreign-transfer=%s\n"
                       (status lost)
                       (List.length unresolved)
                       !replacements
                       (status replay)
                       (status hidden_receipt)
                       (status foreign_transfer);
                     printf
                       "wrong-host-transfer=%s wrong-host-reconcile=%s \
                        no-foreign-host-request=%b missing-receipt=%b still-uncertain=%b\n"
                       (status wrong_host_transfer)
                       (status wrong_host_reconcile)
                       (Int.equal !wrong_host_requests 0)
                       (match missing_receipt with
                        | Missing -> true
                        | Committed _ | Failed _ | Pending _ | Unavailable -> false)
                       missing_still_uncertain;
                     printf
                       "committed-original=%b resolved=%b queue-still-pending=%b \
                        no-second-mutation=%b edited-once=%b\n"
                       (match receipt with
                        | Committed (Session_mutation { session_id; _ }) ->
                          P.Id.Session.equal session_id session.id
                        | _ -> false)
                       (List.is_empty (C.Connection.pending_commands recovery))
                       (match lookup with
                        | Session_pending_input (Pending input) ->
                          P.History.Id.equal input.history.id target.history_id
                        | _ -> false)
                       (Int64.equal saved.counters.revision after.counters.revision)
                       (Int64.equal
                          1L
                          (P.History.Content_revision.to_int64
                             (List.hd_exn after.conversation.deferred_user_entries
                              |> A.Pending_input_document.entry)
                               .content_revision));
                     [%expect
                       {|FIFO-page-one=true FIFO-page-two=true complete=true page-bound=invalid_request foreign-cursor=invalid_request stale-cursor=conflict
lost=interrupted retained-original=1 actual-replacements=1 current-visibility-replay=permission_denied receipt-without-transcript=permission_denied foreign-transfer=permission_denied
wrong-host-transfer=permission_denied wrong-host-reconcile=permission_denied no-foreign-host-request=true missing-receipt=true still-uncertain=true
committed-original=true resolved=true queue-still-pending=true no-second-mutation=true edited-once=true|}]))))))
;;
