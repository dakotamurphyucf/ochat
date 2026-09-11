open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module Artifacts = Agent_store.Prompt_artifact_store

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let digest = Chatmd_shell_spec.Source_ref.digest

let child_source =
  {|<developer>Retained child definition.</developer>
<script id="child" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("inspection must not initialize a moderator")
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let install_child ~env ~sw ~daemon ~(parent : State.t) ~mode =
  let store = Daemon.store daemon in
  let ledger = S.delegations store in
  let artifacts =
    Artifacts.create
      ~env
      ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
    |> store_ok
  in
  let child_id = P.Id.Session.create () in
  let artifact =
    Artifacts.Artifact.create
      ~revision_id:(P.Id.Prompt_revision.create ())
      ~root_relative_path:"child/main.chatmd"
      ~root_chatmd:child_source
      ~sources:[]
      ~parser_schema_version:4
      ~runtime_schema_version:
        (match mode with
         | `Authored_contract -> 1
         | _ -> 2)
      ~created_at:parent.identity.created_at
      ()
    |> store_ok
  in
  let admission : D.Admission.t =
    { child_session_id = child_id
    ; revision_id = artifact.revision_id
    ; transaction_id = P.Id.Transaction.create ()
    ; manifest_sha256 =
        (match mode with
         | `Wrong_manifest -> digest "substituted admission"
         | _ -> artifact.manifest_sha256)
    ; parent_revision_id = parent.spec.prompt_revision_id
    ; authority_sha256 =
        Agent_session.Delegation_authority.fingerprint parent |> protocol_ok
    ; capability_pins = []
    ; lifetime = Owned
    ; created_at = parent.identity.created_at
    }
  in
  let record =
    D.reserve
      ledger
      ~key:
        { parent_session_id = parent.identity.session_id
        ; parent_generation = parent.identity.generation
        ; principal_id = (principal ()).id
        ; idempotency_key = P.Idempotency_key.of_string "stored-child" |> protocol_ok
        }
      ~request_sha256:(digest "generated request")
      ~admission
      ~max_records:8
      ~max_bytes:1048576
    |> store_ok
    |> function
    | D.New record -> record
    | _ -> assert false
  in
  Artifacts.install artifacts ~transaction_id:admission.transaction_id artifact
  |> store_ok;
  ignore (D.advance ledger record Artifact_installed |> store_ok : D.record);
  let id =
    History_entry.Id.create ~namespace:(P.Id.Session.to_string child_id) ~sequence:0
    |> Result.ok_or_failwith
  in
  let retained =
    Agent_session.History_codec.user_text ~id "retained child transcript"
    |> Agent_session.History_codec.to_protocol
  in
  let spec : State.Spec.t =
    { parent.spec with
      protocol =
        { parent.spec.protocol with
          prompt = Generated artifact.revision_id
        ; start_immediately = false
        }
    ; prompt_definition_id = None
    ; prompt_revision_id = artifact.revision_id
    ; delegation = Some (D.reference record)
    ; quota_key = None
    }
  in
  let state =
    State.create
      ~identity:
        { parent.identity with session_id = child_id; display_name = Some "stored child" }
      ~spec
      ~initial_history:[ retained ]
  in
  let state =
    { state with
      conversation =
        { state.conversation with
          next_history_sequence = 1L
        ; reserved_history_through = 1L
        }
    }
  in
  State.validate state |> protocol_ok;
  let handle =
    S.create_session
      store
      ~sw
      ~transaction_id:admission.transaction_id
      ~actor_lock_nonce:"stored-generated-child"
      { schema_version = S.current_schema_version
      ; session = State.summary state
      ; prompt_artifact = P.Id.Prompt_revision.to_string artifact.revision_id
      ; workspace_identity = state.spec.workspace_instance.conflict_domain
      ; data_schema_version = State.current_schema_version
      }
    |> store_ok
  in
  Exn.protect
    ~finally:(fun () -> S.close_session store handle |> store_ok)
    ~f:(fun () ->
      ignore
        (Agent_store.Journal.create
           ~env
           ~directory:(S.Handle.journal_directory handle)
           ~max_payload_length:1048576
           ~max_segment_bytes:1048576L
           ~max_segment_frames:1024
         |> store_ok
         : Agent_store.Journal.t);
      ignore
        (Agent_session.Session_persistence.install_snapshot
           ~env
           ~handle
           ~max_payload_length:1048576
           ~transaction_hash:None
           state
         |> store_ok
         : Agent_store.Snapshot.installed);
      ignore (D.advance ledger record Child_installed |> store_ok : D.record);
      ignore (D.advance ledger record Linked |> store_ok : D.record);
      ignore (D.revoke ledger record Parent_deleted |> store_ok : D.record));
  child_id, retained
;;

let%expect_test
    "factory restores retained generated children without their deleted parent or \
     execution"
  =
  List.iter [ `Valid; `Wrong_manifest; `Authored_contract ] ~f:(fun mode ->
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      let root = temporary_root env in
      Exn.protect
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        ~f:(fun () ->
          let prompt_file = Filename.concat root "parent.chatmd" in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / prompt_file)
            "<developer>Parent fixture.</developer>";
          let config = config root root prompt_file in
          let requests = ref 0 in
          let options =
            { Daemon.default_options with
              qualify_chatml_extensions = true
            ; model_post_stream =
                Some
                  (fun ~sw:_ ~inputs:_ ->
                    Int.incr requests;
                    failwith "unexpected provider request")
            }
          in
          let start sw =
            Daemon.start
              ~sw
              ~env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options
              ()
            |> protocol_ok
          in
          let parent_id, child_id, retained =
            Eio.Switch.run (fun sw ->
              let daemon = start sw in
              Exn.protect
                ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
                ~f:(fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      let parent, _ = create_session client in
                      let entry =
                        Agent_server.Session_registry.find
                          (Daemon.registry daemon)
                          parent.id
                        |> Option.value_exn
                      in
                      let parent_state = A.state entry.actor |> protocol_ok in
                      let child_id, retained =
                        install_child ~env ~sw ~daemon ~parent:parent_state ~mode
                      in
                      parent.id, child_id, retained)))
          in
          Eio.Switch.run (fun sw ->
            let store =
              S.open_existing
                ~env
                ~sw
                ~root:config.server.data_dir
                ~process_start_identity:None
                ~lock_nonce:"remove-deleted-parent"
              |> store_ok
            in
            Exn.protect
              ~finally:(fun () -> S.close store |> store_ok)
              ~f:(fun () -> S.remove_session store parent_id |> store_ok));
          List.iter [ 1; 2 ] ~f:(fun restart ->
            Eio.Switch.run (fun sw ->
              let daemon = start sw in
              Exn.protect
                ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
                ~f:(fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      assert (
                        Option.is_none
                          (Agent_server.Session_registry.find
                             (Daemon.registry daemon)
                             parent_id));
                      let result =
                        Agent_client.Connection.request
                          client
                          (Session_get { session_id = child_id; history = None })
                      in
                      match mode, result with
                      | `Valid, Ok (Session_get snapshot) ->
                        assert (
                          List.exists snapshot.canonical_history.entries ~f:(fun entry ->
                            P.History.Id.equal entry.id retained.id));
                        let entry =
                          Agent_server.Session_registry.find
                            (Daemon.registry daemon)
                            child_id
                          |> Option.value_exn
                        in
                        assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
                        let handle =
                          Agent_client.Session_handle.attach
                            ~sw
                            ~clock:(Eio.Stdenv.clock env)
                            ~connection:client
                            ~session_id:child_id
                            ~mode:Read_write
                            ~subscribe:false
                            ()
                          |> protocol_ok
                        in
                        let before = A.state entry.actor |> protocol_ok in
                        (match
                           Agent_client.Session_handle.start
                             handle
                             ~queue_if_limited:false
                         with
                         | Error { code = Invalid_state; message; _ } ->
                           assert (
                             String.is_substring
                               message
                               ~substring:"delegation.runtime_unavailable")
                         | _ ->
                           failwith "generated execution did not require the parent host");
                        [%test_eq: Sexp.t]
                          (State.sexp_of_t before)
                          (State.sexp_of_t (A.state entry.actor |> protocol_ok));
                        Agent_client.Session_handle.detach handle |> protocol_ok;
                        [%test_eq: int] 0 !requests;
                        print_s
                          [%sexp
                            (restart : int)
                          , "retained transcript recovered; parent absent; runtime never \
                             initialized"]
                      | (`Wrong_manifest | `Authored_contract), Error error ->
                        assert (P.Error.equal_code error.code Prompt_unavailable);
                        assert (
                          String.is_substring
                            error.message
                            ~substring:
                              (match mode with
                               | `Wrong_manifest -> "delegation.artifact_identity"
                               | _ -> "delegation.artifact_contract"));
                        assert (
                          Option.is_none
                            (Agent_server.Session_registry.find
                               (Daemon.registry daemon)
                               child_id));
                        [%test_eq: int] 0 !requests;
                        print_s
                          [%sexp
                            (restart : int)
                          , "invalid generated source rejected before transcript \
                             disclosure"]
                      | _, Error error -> raise_s [%sexp (error : P.Error.t)]
                      | _ -> failwith "unexpected generated session response")))))));
  [%expect
    {|
    (1 "retained transcript recovered; parent absent; runtime never initialized")
    (2 "retained transcript recovered; parent absent; runtime never initialized")
    (1 "invalid generated source rejected before transcript disclosure")
    (2 "invalid generated source rejected before transcript disclosure")
    (1 "invalid generated source rejected before transcript disclosure")
    (2 "invalid generated source rejected before transcript disclosure")
    |}]
;;
