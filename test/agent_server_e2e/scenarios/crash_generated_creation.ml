open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module A = Agent_session.Session_actor
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module G = Agent_session.Generated_definition
module C = Chat_response.Tool_capability
module Owner = Agent_server.Runtime_owner

let create_child_result env root daemon parent_id =
  let parent =
    Agent_server.Session_registry.find (Daemon.registry daemon) parent_id
    |> Option.value_exn
  in
  let definition =
    Owner.with_background_runtime parent.runtime (fun runtime ->
      let native =
        Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
      in
      let capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let bundle =
        Chatmd_source_bundle.create
          ~root_file:"child.chatmd"
          ~sources:
            [ ( "child.chatmd"
              , {|<developer>Generated crash child.</developer><tool type="inherited" name="read_file"/>|}
              )
            ]
          ()
        |> Result.ok_or_failwith
      in
      G.prepare
        ~env
        ~dir:(F.path env root)
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:(P.Timestamp.now ())
        ~current_capabilities:(fun () -> capabilities)
        ~references:(C.references capabilities)
        bundle
      |> Result.map_error ~f:(fun errors ->
        P.Error.invalid_request
          (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
           |> String.concat ~sep:"\n")))
    |> F.protocol_ok
  in
  Agent_server.Session_factory.create_generated_session
    (Daemon.factory daemon)
    ~parent_session_id:parent_id
    ~idempotency_key:(F.key "generated-crash-create")
    ~display_name:(Some "crash child")
    definition
;;

let create_child env root daemon parent_id =
  create_child_result env root daemon parent_id |> F.protocol_ok
;;

let run_child env ~root ~boundary ~recover =
  let armed = ref false in
  let ledger_prefix = ref ""
  and artifact_prefix = ref ""
  and session_prefix = ref "" in
  let writes = ref 0 in
  let wrapped =
    if recover
    then env
    else
      Support.Crash_fault_io.wrap
        env
        ~boundary:
          (match boundary with
           | "artifact-partial" | "snapshot-partial" -> After_bytes 3
           | _ -> After_rename)
        ~matches:(fun path ->
          !armed
          &&
          match boundary with
          | "artifact-partial" ->
            String.is_prefix path ~prefix:!artifact_prefix
            && String.is_suffix path ~suffix:".chatmd"
          | "snapshot-partial" ->
            String.is_prefix path ~prefix:!session_prefix
            && String.is_suffix path ~suffix:".bin"
          | "artifact" ->
            String.is_prefix path ~prefix:!artifact_prefix
            && String.equal (Filename.dirname path) (Filename.dirname !artifact_prefix)
          | "child" ->
            String.is_prefix path ~prefix:!session_prefix
            && String.equal (Filename.dirname path) (Filename.dirname !session_prefix)
          | _ -> String.is_prefix path ~prefix:!ledger_prefix)
        ~reached:(fun _ ->
          Int.incr writes;
          let target =
            match boundary with
            | "artifact-record" -> 2
            | "child-record" | "preflight" | "parent-stopped" | "parent-missing" -> 3
            | "linked" -> 4
            | _ -> 1
          in
          if Int.equal !writes target
          then (
            match boundary with
            | "preflight" | "parent-stopped" | "parent-missing" ->
              armed := false;
              raise
                (Core_unix.Unix_error
                   (EIO, "injected creation acknowledgement failure", root))
            | _ ->
              Eio.Flow.copy_string "generated-creation-boundary\n" (Eio.Stdenv.stdout env);
              Eio.Fiber.await_cancel ()))
  in
  let prompt_file = Filename.concat root "parent.chatmd" in
  let requests = ref 0 in
  let start sw =
    Daemon.start
      ~sw
      ~env:wrapped
      ~config:(config root root prompt_file)
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          qualify_chatml_extensions = true
        ; model_post_stream =
            Some
              (fun ~sw:_ ~inputs:_ ->
                Int.incr requests;
                Stdlib.Seq.empty)
        }
      ()
    |> F.protocol_ok
  in
  let stored_id = ref None in
  List.iter
    (if recover then [ 1; 2 ] else [ 0 ])
    ~f:(fun restart ->
      Eio.Switch.run (fun sw ->
        let daemon = start sw in
        Exn.protect
          ~finally:(fun () -> Daemon.shutdown daemon |> F.protocol_ok)
          ~f:(fun () ->
            let store = Daemon.store daemon in
            let data_root = S.data_root store in
            ledger_prefix
            := Filename.concat (Agent_store.Data_root.path data_root) "delegations/";
            artifact_prefix
            := Agent_store.Data_root.prompt_artifacts_path data_root ^ "/.install-";
            session_prefix
            := Agent_store.Data_root.sessions_path data_root ^ "/.creating-";
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () -> Agent_client.Connection.close client)
              ~f:(fun () ->
                initialize client;
                match recover with
                | false ->
                  let parent, _ = create_session ~start_immediately:true client in
                  armed := true;
                  (match boundary with
                   | "preflight" | "parent-stopped" | "parent-missing" ->
                     (match create_child_result wrapped root daemon parent.id with
                      | Error { code = Persistence_error; _ } when !writes = 3 -> ()
                      | _ -> F.fail "injected child-stage acknowledgement did not fail");
                     let records =
                       D.with_records
                         (S.delegations store)
                         ~max_records:8
                         ~max_bytes:1048576
                         ~f:(fun records -> Ok records)
                       |> F.store_ok
                     in
                     let record =
                       match records with
                       | [ record ] -> record
                       | _ -> F.fail "lost creation intent"
                     in
                     F.require
                       (D.equal_stage record.stage Child_installed)
                       "lost committed child stage";
                     Agent_server.Session_registry.index
                       (Daemon.registry daemon)
                       (Agent_store.Session_index.find
                          (S.session_index store)
                          record.admission.child_session_id
                        |> Option.value_exn);
                     let handle =
                       Agent_client.Session_handle.attach
                         ~sw
                         ~clock:(Eio.Stdenv.clock env)
                         ~connection:client
                         ~session_id:record.admission.child_session_id
                         ~mode:Read_write
                         ~subscribe:false
                         ()
                       |> F.protocol_ok
                     in
                     let entry =
                       Agent_server.Session_registry.find
                         (Daemon.registry daemon)
                         record.admission.child_session_id
                       |> Option.value_exn
                     in
                     let before = A.state entry.actor |> F.protocol_ok in
                     (match
                        Agent_client.Session_handle.start handle ~queue_if_limited:false
                      with
                      | Error { code = Permission_denied; message; _ }
                        when String.is_substring
                               message
                               ~substring:"delegation.not_linked" -> ()
                      | _ -> F.fail "unlinked child passed execution preflight");
                     F.require_equal
                       "unlinked start changed state"
                       Agent_session.Session_state.sexp_of_t
                       before
                       (A.state entry.actor |> F.protocol_ok);
                     F.require (!requests = 0) "unlinked child called a provider";
                     Agent_client.Session_handle.detach handle |> F.protocol_ok;
                     (match boundary with
                      | "parent-stopped" | "parent-missing" ->
                        let parent_handle =
                          Agent_client.Session_handle.attach
                            ~sw
                            ~clock:(Eio.Stdenv.clock env)
                            ~connection:client
                            ~session_id:parent.id
                            ~mode:Read_write
                            ~subscribe:false
                            ()
                          |> F.protocol_ok
                        in
                        Agent_client.Session_handle.stop parent_handle ~mode:Cancel
                        |> F.protocol_ok
                        |> ignore;
                        Agent_client.Session_handle.detach parent_handle |> F.protocol_ok;
                        (match boundary with
                         | "parent-missing" ->
                           let parent_entry =
                             Agent_server.Session_registry.find
                               (Daemon.registry daemon)
                               parent.id
                             |> Option.value_exn
                           in
                           parent_entry.close ();
                           ignore
                             (Agent_server.Session_registry.remove
                                (Daemon.registry daemon)
                                parent.id
                              : Agent_server.Session_registry.entry option);
                           S.remove_session store parent.id |> F.store_ok
                         | _ -> ())
                      | _ -> ());
                     Eio.Flow.copy_string
                       "generated-preflight-blocked\n"
                       (Eio.Stdenv.stdout env)
                   | _ ->
                     ignore
                       (create_child wrapped root daemon parent.id
                        : Agent_server.Session_registry.entry);
                     F.fail "creation did not hit its crash boundary")
                | true
                  when String.equal boundary "parent-stopped"
                       || String.equal boundary "parent-missing" ->
                  let records =
                    D.with_records
                      (S.delegations store)
                      ~max_records:8
                      ~max_bytes:1048576
                      ~f:(fun records -> Ok records)
                    |> F.store_ok
                  in
                  let record =
                    match records with
                    | [ record ] -> record
                    | _ -> F.fail "lost revoked creation"
                  in
                  let reason =
                    if String.equal boundary "parent-missing"
                    then D.Parent_deleted
                    else Parent_stopped
                  in
                  F.require
                    (Option.equal D.equal_revocation record.revocation (Some reason))
                    "startup did not revoke invalid parent authority";
                  let child_id = record.admission.child_session_id in
                  (match !stored_id with
                   | None -> stored_id := Some child_id
                   | Some expected ->
                     F.require
                       (P.Id.Session.equal expected child_id)
                       "revoked child identity changed");
                  let handle =
                    Agent_client.Session_handle.attach
                      ~sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~connection:client
                      ~session_id:child_id
                      ~mode:Read_write
                      ~subscribe:false
                      ()
                    |> F.protocol_ok
                  in
                  let child =
                    Agent_server.Session_registry.find (Daemon.registry daemon) child_id
                    |> Option.value_exn
                  in
                  let state = A.state child.actor |> F.protocol_ok in
                  F.require
                    (P.Session.equal_desired_state state.lifecycle.desired Stopped)
                    "unfinished child started during revocation";
                  F.require
                    (not (Owner.is_loaded child.runtime))
                    "revoked child acquired native resources";
                  F.require
                    (List.exists state.conversation.canonical_history ~f:(fun entry ->
                       String.is_substring
                         (Jsonaf.to_string entry.P.History.payload)
                         ~substring:"Generated crash child."))
                    "revocation discarded child instructions";
                  (match
                     Agent_client.Session_handle.start handle ~queue_if_limited:false
                   with
                   | Error { code = Permission_denied; _ } -> ()
                   | _ -> F.fail "revoked creation started");
                  F.require_equal
                    "revoked start changed state"
                    Agent_session.Session_state.sexp_of_t
                    state
                    (A.state child.actor |> F.protocol_ok);
                  F.require (!requests = 0) "revoked creation called provider";
                  Agent_client.Session_handle.detach handle |> F.protocol_ok
                | true ->
                  let records =
                    D.with_records
                      (S.delegations store)
                      ~max_records:8
                      ~max_bytes:1048576
                      ~f:(fun records -> Ok records)
                    |> F.store_ok
                  in
                  let record =
                    match records with
                    | [ record ] -> record
                    | _ -> F.fail "creation mapping was lost or duplicated"
                  in
                  (match !stored_id with
                   | None -> stored_id := Some record.admission.child_session_id
                   | Some id ->
                     F.require
                       (P.Id.Session.equal id record.admission.child_session_id)
                       "child ID changed after restart");
                  if restart = 1
                  then (
                    let expected =
                      match boundary with
                      | "reserved" | "artifact-partial" -> D.Reserved
                      | "artifact" | "artifact-record" | "snapshot-partial" ->
                        Artifact_installed
                      | _ -> Linked
                    in
                    F.require_equal
                      "startup creation stage"
                      D.sexp_of_stage
                      expected
                      record.stage);
                  F.require
                    (Int.equal !requests (restart - 1))
                    "startup or creation called a provider";
                  let child =
                    create_child wrapped root daemon record.key.parent_session_id
                  in
                  let state = A.state child.actor |> F.protocol_ok in
                  F.require
                    (P.Id.Session.equal
                       state.identity.session_id
                       record.admission.child_session_id)
                    "retry created a different child";
                  F.require
                    (List.length (S.list_sessions store) = 2)
                    "retry duplicated child storage";
                  F.require
                    (List.exists state.conversation.canonical_history ~f:(fun entry ->
                       String.is_substring
                         (Jsonaf.to_string entry.P.History.payload)
                         ~substring:"Generated crash child."))
                    "creation lost child instructions";
                  let handle =
                    Agent_client.Session_handle.attach
                      ~sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~connection:client
                      ~session_id:state.identity.session_id
                      ~mode:Read_write
                      ~subscribe:false
                      ()
                    |> F.protocol_ok
                  in
                  Agent_client.Session_handle.start handle ~queue_if_limited:false
                  |> F.protocol_ok
                  |> ignore;
                  Agent_client.Session_handle.send_message
                    handle
                    { kind = Plain_text; text = "Continue."; attachments = [] }
                  |> F.protocol_ok
                  |> ignore;
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    let rec wait () =
                      let state = A.state child.actor |> F.protocol_ok in
                      match state.active_operation with
                      | None ->
                        F.require (Option.is_none state.failure) "recovered child failed"
                      | Some _ ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        wait ()
                    in
                    wait ());
                  F.require
                    (Int.equal !requests restart)
                    "recovered child did not execute exactly one turn";
                  Agent_client.Session_handle.detach handle |> F.protocol_ok))));
  Eio.Flow.copy_string "generated-creation-recovered\n" (Eio.Stdenv.stdout env)
;;

let test env environment =
  List.iter
    [ "reserved"
    ; "artifact-partial"
    ; "artifact"
    ; "artifact-record"
    ; "snapshot-partial"
    ; "child"
    ; "child-record"
    ; "linked"
    ; "preflight"
    ; "parent-stopped"
    ; "parent-missing"
    ]
    ~f:(fun boundary ->
      let root =
        Filename.concat
          (Support.Temporary_environment.roots environment).temporary
          ("generated-" ^ boundary)
      in
      Eio.Path.mkdir ~perm:0o700 (F.path env root);
      F.write
        env
        (Filename.concat root "parent.chatmd")
        {|<developer>Root.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
      Eio.Switch.run (fun sw ->
        let child =
          F.child
            ~sw
            env
            environment
            ~case:"generated-create"
            ~arguments:[ "generated-create"; root; boundary ]
        in
        Exn.protect
          ~finally:(fun () -> F.terminate env child)
          ~f:(fun () ->
            match boundary with
            | "preflight" | "parent-stopped" | "parent-missing" ->
              let result =
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  Support.Process_manager.await child)
              in
              (match result.exit with
               | Exited 0
                 when String.is_substring
                        result.stdout.contents
                        ~substring:"generated-preflight-blocked" -> ()
               | _ ->
                 raise_s
                   [%sexp
                     "unlinked preflight failed"
                   , (result : Support.Process_manager.result)])
            | _ ->
              F.await_marker env child "generated-creation-boundary";
              F.kill env child);
        let recovery =
          F.child
            ~sw
            env
            environment
            ~case:"generated-recover"
            ~arguments:[ "generated-recover"; root; boundary ]
        in
        Exn.protect
          ~finally:(fun () -> F.terminate env recovery)
          ~f:(fun () ->
            let result =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                Support.Process_manager.await recovery)
            in
            match result.exit with
            | Exited 0
              when String.is_substring
                     result.stdout.contents
                     ~substring:"generated-creation-recovered" -> ()
            | _ ->
              raise_s
                [%sexp
                  "generated recovery failed"
                , (boundary : string)
                , (result : Support.Process_manager.result)])))
;;
