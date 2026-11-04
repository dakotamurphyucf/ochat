open! Core
module F = Legacy_source_recovery_fixture
open F

let%expect_test
    "captured historical Single stays unloaded until authenticated Pinned rebuild"
  =
  let cases : (P.Session.desired_state * F.Preservation.t * bool) list =
    [ Stopped, Known, false
    ; Running, Known, false
    ; Stopped, Protected_unknown, false
    ; Stopped, Known, true
    ]
  in
  List.iter cases ~f:(fun (desired, preservation, declared_child) ->
    let saved : F.t option ref = ref None in
    T.with_daemon
      ~after_close:(fun ~env ~root ~(config : Agent_server.Config.t) ->
        let prior = Option.value_exn !saved in
        if declared_child
        then
          assert (
            List.exists prior.artifact.sources ~f:(fun source ->
              String.equal source.relative_path "child.chatmd"));
        let data_dir = Eio.Path.native_exn Eio.Path.(root / "prior-data") in
        let prior_checkpoint =
          Eio.Switch.run (fun sw -> install_prior ~sw ~env ~data_dir prior)
        in
        assert_preservation preservation prior_checkpoint;
        Eio.Path.unlink Eio.Path.(root / "prompt.chatmd");
        if declared_child then Eio.Path.unlink Eio.Path.(root / "child.chatmd");
        let config = { config with server = { config.server with data_dir } } in
        let provider_calls = ref 0 in
        let constructor_calls = ref 0 in
        let inference_policy =
          Agent_server_test_support.inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              incr provider_calls;
              failwith "historical recovery must not submit a provider request")
        in
        let options =
          { (T.inference_options ()) with
            inference_policy =
              { inference_policy with
                runtime_inference_ports =
                  (fun actor ->
                    incr constructor_calls;
                    inference_policy.runtime_inference_ports actor)
              }
          }
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Agent_server.Daemon.start
              ~options
              ~sw
              ~env
              ~config
              ~tool_dir:(Eio.Path.native_exn root)
              ~home:(Eio.Path.native_exn root)
              ~process_start_identity:None
              ()
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> ok)
            ~f:(fun () ->
              let actor = Operator_authorization.trusted_local prior.principal in
              let connection = T.connection ~actor daemon prior.principal in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close connection)
                ~f:(fun () ->
                  let attached =
                    match
                      T.request
                        connection
                        (Session_attach
                           { session_id = prior.state.identity.session_id
                           ; requested_mode = Read_write
                           ; subscribe = false
                           ; after_sequence = None
                           ; reclaim_token = None
                           ; idempotency_key = T.key "prior-attach"
                           })
                    with
                    | Session_attach value -> value.attachment
                    | _ -> failwith "expected prior attachment"
                  in
                  let entry, before = T.state daemon prior.state.identity.session_id in
                  assert (not (Runtime.is_loaded entry.runtime));
                  assert (!provider_calls = 0);
                  assert (!constructor_calls = 0);
                  assert (State.execution_migration_required before);
                  let retained_checkpoint = stored_checkpoint ~env entry in
                  assert_preservation preservation retained_checkpoint;
                  assert (D.Json.equal prior_checkpoint retained_checkpoint);
                  assert_queued_unchanged before;
                  assert_blocked_claims entry;
                  (match
                     Agent_client.Connection.request
                       connection
                       (Session_start
                          { session_id = before.identity.session_id
                          ; attachment_id = attached.id
                          ; queue_if_limited = false
                          ; idempotency_key = T.key "prior-start"
                          })
                   with
                   | Error error ->
                     assert (P.Error.equal_code error.code Migration_required)
                   | Ok _ -> failwith "old checkpoint started");
                  assert (!constructor_calls = 0);
                  assert (!provider_calls = 0);
                  ignore
                    (T.request
                       connection
                       (Session_stop
                          { session_id = before.identity.session_id
                          ; attachment_id = attached.id
                          ; mode = Cancel
                          ; idempotency_key = T.key "prior-stop"
                          })
                     : P.Public.Result.t);
                  let stopped = A.state entry.actor |> ok in
                  assert (State.execution_migration_required stopped);
                  assert (not (Runtime.is_loaded entry.runtime));
                  assert (!constructor_calls = 0);
                  (* Stop may cancel Jobs by its existing explicit policy; the
                     unowned legacy timer remains blocked until rebuild. *)
                  let timer = List.hd_exn stopped.schedules in
                  (match
                     A.claim_schedule
                       entry.actor
                       ~schedule_id:timer.id
                       ~generation:timer.generation
                   with
                   | Error error ->
                     assert (P.Error.equal_code error.code Migration_required)
                   | Ok _ -> failwith "Stop erased the migration cause");
                  let rebuild =
                    Agent_client.Connection.request
                      connection
                      (Session_rebuild
                         { session_id = stopped.identity.session_id
                         ; attachment_id = attached.id
                         ; expected_revision = stopped.counters.revision
                         ; prompt_choice = Pinned
                         ; idempotency_key = T.key "prior-pinned-rebuild"
                         })
                  in
                  match preservation, declared_child, rebuild with
                  | Protected_unknown, _, Error failure ->
                    assert (P.Error.equal_code failure.code Persistence_error);
                    let expected =
                      Agent_store.Store_error.Document
                        (D.Error.Extension_conflict [ "payload"; "moderator" ])
                      |> Agent_store.Store_error.sexp_of_t
                    in
                    assert (Sexp.equal (Sexp.of_string failure.message) expected);
                    let after = A.state entry.actor |> ok in
                    assert (
                      State_document.equal_values stopped ~limits:D.Limits.default after
                      |> document_ok);
                    let retained = stored_checkpoint ~env entry in
                    assert_preservation preservation retained;
                    assert (D.Json.equal retained_checkpoint retained);
                    assert (not (Runtime.is_loaded entry.runtime));
                    assert (!constructor_calls = 0 && !provider_calls = 0);
                    print_endline
                      "protected checkpoint unknown refuses rebuild without state loss \
                       or initializer"
                  | Known, false, Ok (Non_history result) ->
                    (match P.Public.Result.Non_history.value result with
                     | Session_rebuild _ -> ()
                     | _ -> failwith "expected rebuild response");
                    let fresh = A.state entry.actor |> ok in
                    assert (!constructor_calls > 0);
                    assert (fresh.identity.generation = stopped.identity.generation + 1);
                    assert (not (State.execution_migration_required fresh));
                    assert (List.is_empty fresh.jobs && List.is_empty fresh.schedules);
                    let snapshot =
                      Agent_session.Moderator_checkpoint.decode fresh.moderator
                      |> ok
                      |> Option.value_exn
                    in
                    assert (String.length snapshot.script_source_hash = 64);
                    assert (
                      String.equal snapshot.script_source_hash prior.canonical_sha256);
                    assert (
                      not
                        (String.equal
                           snapshot.script_source_hash
                           (Agent_session.Moderator_checkpoint.decode stopped.moderator
                            |> ok
                            |> Option.value_exn)
                             .script_source_hash));
                    let archive = List.last_exn fresh.conversation.compaction_archives in
                    let archived = archived_checkpoint ~env entry archive in
                    assert_preservation preservation archived;
                    assert (D.Json.equal retained_checkpoint archived);
                    assert (!provider_calls = 0);
                    print_endline
                      "physical captured MD5 blocked with queued work intact; \
                       Stop/Pinned rebuild archives and rotates SHA256"
                  | Known, true, Error failure ->
                    assert (P.Error.equal_code failure.code Invalid_state);
                    [%test_eq: string]
                      "captured agent runner requires owned parser and program custody"
                      failure.message;
                    (* Administration committed its new generation before loading.
                       Source admission still denies the declared child; this is
                       not rollback of the successful archival transaction. *)
                    let after = A.state entry.actor |> ok in
                    assert (after.identity.generation = stopped.identity.generation + 1);
                    assert (!constructor_calls > 0 && !provider_calls = 0);
                    assert (not (Runtime.is_loaded entry.runtime));
                    let archive = List.last_exn after.conversation.compaction_archives in
                    let archived = archived_checkpoint ~env entry archive in
                    assert (D.Json.equal retained_checkpoint archived);
                    print_endline
                      "declared historical child refuses missing custody after archival \
                       generation change"
                  | Known, false, Error failure -> raise_s [%sexp (failure : P.Error.t)]
                  | Protected_unknown, _, Ok _ ->
                    failwith "unknown container was retired without ownership"
                  | Known, _, Ok _ -> failwith "unexpected rebuild result"))))
      (fun _sw _env root daemon ->
         if declared_child
         then (
           T.save Eio.Path.(root / "child.chatmd") "<user>retained child</user>";
           ignore
             (T.reload
                root
                daemon
                (T.initial_prompt ^ {|<tool name="child" agent="child.chatmd" local/>|})
              : P.Id.Prompt_revision.t));
         let principal = T.principal () in
         let connection = T.connection daemon principal in
         Exn.protect
           ~finally:(fun () -> Agent_client.Connection.close connection)
           ~f:(fun () ->
             let session, _ = T.create_session connection in
             saved
             := Some (prior_edition daemon principal session.id desired ~preservation))));
  [%expect
    {|
    physical captured MD5 blocked with queued work intact; Stop/Pinned rebuild archives and rotates SHA256
    physical captured MD5 blocked with queued work intact; Stop/Pinned rebuild archives and rotates SHA256
    protected checkpoint unknown refuses rebuild without state loss or initializer
    declared historical child refuses missing custody after archival generation change
    |}]
;;
