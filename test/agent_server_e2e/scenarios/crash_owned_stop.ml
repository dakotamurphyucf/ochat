open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module A = Agent_session.Session_actor
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module H = Agent_client.Session_handle
module Registry = Agent_server.Session_registry

let run_child ?interrupt_recovery env ~root ~recover =
  let read name = Eio.Path.load (F.path env (Filename.concat root name)) in
  let journal =
    ref
      (match interrupt_recovery with
       | Some "journal" -> Some (read "middle-journal")
       | _ -> None)
  in
  let index_path = Option.map interrupt_recovery ~f:(fun _ -> read "index-path") in
  let wrapped =
    if recover && Option.is_none interrupt_recovery
    then env
    else
      Support.Crash_fault_io.wrap
        env
        ~boundary:
          (match interrupt_recovery with
           | Some "index" -> After_rename
           | _ -> After_sync)
        ~matches:(fun path ->
          match interrupt_recovery with
          | Some "index" ->
            Option.exists index_path ~f:(fun index ->
              String.is_prefix path ~prefix:(index ^ ".tmp-"))
          | _ -> Option.exists !journal ~f:(fun prefix -> String.is_prefix path ~prefix))
        ~reached:(fun path ->
          let hit =
            match interrupt_recovery with
            | None -> true
            | Some "journal" ->
              String.is_substring
                (Eio.Path.load (F.path env path))
                ~substring:"Stop_epoch_changed"
            | Some "index" ->
              let index =
                Agent_store.Session_index.open_or_create ~env ~path |> F.store_ok
              in
              let middle = P.Id.Session.of_string (read "middle-id") |> F.protocol_ok in
              Option.exists (Agent_store.Session_index.find index middle) ~f:(fun entry ->
                P.Session.equal_desired_state entry.session.desired_state Stopped)
            | Some _ -> F.fail "invalid recovery interruption"
          in
          if hit
          then (
            Eio.Flow.copy_string
              (if recover
               then "owned-child-stop-durable\n"
               else "owned-parent-stop-durable\n")
              (Eio.Stdenv.stdout env);
            Eio.Fiber.await_cancel ()))
  in
  let requests = ref 0 in
  let start sw =
    Daemon.start
      ~sw
      ~env:wrapped
      ~config:(config root root (Filename.concat root "parent.chatmd"))
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
  List.iter
    (if recover then [ 1; 2 ] else [ 0 ])
    ~f:(fun restart ->
      Eio.Switch.run (fun sw ->
        let daemon = start sw in
        Exn.protect
          ~finally:(fun () -> Daemon.shutdown daemon |> F.protocol_ok)
          ~f:(fun () ->
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () -> Agent_client.Connection.close client)
              ~f:(fun () ->
                initialize client;
                let attach id =
                  H.attach
                    ~sw
                    ~clock:(Eio.Stdenv.clock env)
                    ~connection:client
                    ~session_id:id
                    ~mode:Read_write
                    ~subscribe:false
                    ()
                  |> F.protocol_ok
                in
                if not recover
                then (
                  let parent, _ = create_session ~start_immediately:true client in
                  let middle =
                    Crash_generated_creation.create_child
                      ~start_immediately:true
                      wrapped
                      root
                      daemon
                      parent.id
                  in
                  let middle_state = A.state middle.actor |> F.protocol_ok in
                  let leaf =
                    Crash_generated_creation.create_child
                      ~start_immediately:true
                      wrapped
                      root
                      daemon
                      middle_state.identity.session_id
                  in
                  F.require (!requests = 0) "creation invoked a provider";
                  F.require
                    (P.Session.equal_desired_state
                       (A.state leaf.actor |> F.protocol_ok).lifecycle.desired
                       Running)
                    "grandchild was not active before crash";
                  let handle = attach parent.id in
                  F.write
                    env
                    (Filename.concat root "middle-journal")
                    (S.Handle.journal_directory (Option.value_exn middle.store_handle)
                     ^ "/");
                  F.write
                    env
                    (Filename.concat root "middle-id")
                    (P.Id.Session.to_string middle_state.identity.session_id);
                  F.write
                    env
                    (Filename.concat root "index-path")
                    (Filename.concat
                       (Agent_store.Data_root.indexes_path
                          (S.data_root (Daemon.store daemon)))
                       "sessions.snapshot");
                  let entry =
                    Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                  in
                  journal
                  := Some
                       (S.Handle.journal_directory (Option.value_exn entry.store_handle)
                        ^ "/");
                  H.stop handle ~mode:Cancel |> F.protocol_ok |> ignore;
                  F.fail "stop did not reach its durable crash boundary")
                else (
                  let records =
                    D.with_records
                      (S.delegations (Daemon.store daemon))
                      ~max_records:8
                      ~max_bytes:1048576
                      ~f:(fun records -> Ok records)
                    |> F.store_ok
                  in
                  F.require (List.length records = 2) "lost generated tree";
                  let child_ids =
                    List.map records ~f:(fun record ->
                      record.D.admission.child_session_id)
                  in
                  let middle_record =
                    List.find_exn records ~f:(fun record ->
                      not
                        (List.mem
                           child_ids
                           record.D.key.parent_session_id
                           ~equal:P.Id.Session.equal))
                  in
                  let leaf_record =
                    List.find_exn records ~f:(fun record ->
                      P.Id.Session.equal
                        record.D.key.parent_session_id
                        middle_record.admission.child_session_id)
                  in
                  let root_id = middle_record.key.parent_session_id in
                  let ids =
                    [ root_id
                    ; middle_record.admission.child_session_id
                    ; leaf_record.admission.child_session_id
                    ]
                  in
                  let handles = List.map ids ~f:attach in
                  let entries =
                    List.map ids ~f:(fun id ->
                      Registry.find (Daemon.registry daemon) id |> Option.value_exn)
                  in
                  List.iter entries ~f:(fun entry ->
                    let state = A.state entry.actor |> F.protocol_ok in
                    F.require
                      (P.Session.equal_desired_state state.lifecycle.desired Stopped)
                      "child resumed before owned cleanup";
                    F.require
                      (not (Agent_server.Runtime_owner.is_loaded entry.runtime))
                      "recovery initialized a stopped descendant");
                  List.iter records ~f:(fun record ->
                    F.require
                      (D.equal_stage record.stage Linked
                       && Option.is_none record.revocation)
                      "stop revoked reusable child";
                    let parent =
                      Registry.find (Daemon.registry daemon) record.key.parent_session_id
                      |> Option.value_exn
                    in
                    let child =
                      Registry.find
                        (Daemon.registry daemon)
                        record.admission.child_session_id
                      |> Option.value_exn
                    in
                    let child_state = A.state child.actor |> F.protocol_ok in
                    let parent_state = A.state parent.actor |> F.protocol_ok in
                    F.require
                      (Option.equal
                         Int64.equal
                         child_state.parent_stop_epoch
                         (Some parent_state.stop_epoch))
                      "cleanup acknowledgement was not persisted";
                    F.require
                      (List.exists
                         child_state.conversation.canonical_history
                         ~f:(fun entry ->
                           String.is_substring
                             (Jsonaf.to_string entry.P.History.payload)
                             ~substring:"Generated crash child."))
                      "cleanup lost child history");
                  F.require (!requests = restart - 1) "recovery called provider";
                  List.iter handles ~f:(fun handle ->
                    H.start handle ~queue_if_limited:false |> F.protocol_ok |> ignore);
                  let leaf = List.last_exn handles in
                  H.send_message
                    leaf
                    { kind = Plain_text
                    ; text = "Resume after owned cleanup."
                    ; attachments = []
                    }
                  |> F.protocol_ok
                  |> ignore;
                  let leaf_entry = List.last_exn entries in
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    let rec settled () =
                      match
                        (A.state leaf_entry.actor |> F.protocol_ok).active_operation
                      with
                      | None -> ()
                      | Some _ ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        settled ()
                    in
                    settled ());
                  F.require
                    (!requests = restart)
                    "resumed child did not execute exactly one turn";
                  (* Explicitly retire from leaves upward here. The tested crash boundary
               is durable parent stop before any descendant received cleanup. *)
                  List.iter (List.rev handles) ~f:(fun handle ->
                    H.stop handle ~mode:Cancel |> F.protocol_ok |> ignore);
                  List.iter handles ~f:(fun handle -> H.detach handle |> F.protocol_ok))))));
  Eio.Flow.copy_string "owned-stop-recovery-passed\n" (Eio.Stdenv.stdout env)
;;

let test env environment =
  let root =
    Filename.concat
      (Support.Temporary_environment.roots environment).temporary
      "owned-stop-recovery"
  in
  Eio.Path.mkdir ~perm:0o700 (F.path env root);
  F.write
    env
    (Filename.concat root "parent.chatmd")
    {|<developer>Root.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
  Eio.Switch.run (fun sw ->
    let child =
      F.child ~sw env environment ~case:"owned-stop" ~arguments:[ "owned-stop"; root ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "owned-parent-stop-durable";
        F.kill env child);
    List.iter [ "journal"; "index" ] ~f:(fun boundary ->
      let interrupted =
        F.child
          ~sw
          env
          environment
          ~case:("owned-stop-recover-" ^ boundary)
          ~arguments:[ "owned-stop-recover"; root; boundary ]
      in
      Exn.protect
        ~finally:(fun () -> F.terminate env interrupted)
        ~f:(fun () ->
          F.await_marker env interrupted "owned-child-stop-durable";
          F.kill env interrupted));
    let recovery =
      F.child
        ~sw
        env
        environment
        ~case:"owned-stop-recover"
        ~arguments:[ "owned-stop-recover"; root ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env recovery)
      ~f:(fun () ->
        let result =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
            Support.Process_manager.await recovery)
        in
        match result.exit with
        | Exited 0
          when String.is_substring
                 result.stdout.contents
                 ~substring:"owned-stop-recovery-passed" -> ()
        | _ ->
          raise_s
            [%sexp
              "owned stop recovery failed", (result : Support.Process_manager.result)]))
;;
