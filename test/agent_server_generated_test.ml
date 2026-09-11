open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module Artifacts = Agent_store.Prompt_artifact_store
module H = Agent_client.Session_handle
module Owner = Agent_server.Runtime_owner

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

let install_child
      ~env
      ~sw
      ~daemon
      ~(parent : State.t)
      ~mode
      ~source
      ~capability_pins
      ~revoke
  =
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
      ~root_chatmd:source
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
    ; capability_pins
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
        ; idempotency_key =
            P.Idempotency_key.of_string (P.Id.Session.to_string child_id) |> protocol_ok
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
      if revoke then ignore (D.revoke ledger record Parent_deleted |> store_ok : D.record));
  Agent_server.Session_registry.index
    (Daemon.registry daemon)
    (Agent_store.Session_index.find (S.session_index store) child_id |> Option.value_exn);
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
                        install_child
                          ~env
                          ~sw
                          ~daemon
                          ~parent:parent_state
                          ~mode
                          ~source:child_source
                          ~capability_pins:[]
                          ~revoke:true
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

let%expect_test
    "factory executes an inherited file tool and parent stop joins the child's model \
     cleanup"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root / "data");
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / root / "data/value.txt")
          "inherited-parent-file";
        let prompt_file = Filename.concat root "parent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<developer>Parent.</developer><tool name="read_file"><read id="data" path="${workspace}/data"/></tool><tool name="append_to_file"/>|};
        let config = config root root prompt_file in
        let requests = ref 0 in
        let entered, entered_u = Eio.Promise.create () in
        let cleaning, cleaning_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let released = ref false in
        let release_cleanup () =
          if not !released
          then (
            released := true;
            Eio.Promise.resolve release_u ())
        in
        let never, _ = Eio.Promise.create () in
        let cleaned = ref false in
        let post_stream ~sw:_ ~inputs:_ =
          Int.incr requests;
          match !requests with
          | 1 ->
            let open Openai.Responses.Response_stream in
            [ Output_item_added
                { item =
                    Function_call
                      { name = "read_file"
                      ; arguments = ""
                      ; call_id = "read"
                      ; _type = "function_call"
                      ; id = Some "read-item"
                      ; status = None
                      }
                ; output_index = 0
                ; type_ = "response.output_item.added"
                }
            ; Function_call_arguments_done
                { arguments = {|{"root":"data","file":"value.txt"}|}
                ; item_id = "read-item"
                ; output_index = 0
                ; type_ = "response.function_call_arguments.done"
                }
            ]
            |> Stdlib.List.to_seq
          | 2 -> Stdlib.Seq.empty
          | 3 ->
            Eio.Promise.resolve entered_u ();
            Exn.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await release;
                  cleaned := true))
              ~f:(fun () ->
                Eio.Promise.await never;
                Stdlib.Seq.empty)
          | _ -> failwith "unexpected extra provider request"
        in
        let options =
          { Daemon.default_options with
            qualify_chatml_extensions = true
          ; model_post_stream = Some post_stream
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
        let child_id =
          Eio.Switch.run (fun sw ->
            let daemon = start sw in
            Exn.protect
              ~finally:(fun () ->
                release_cleanup ();
                Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      let parent, _ = create_session ~start_immediately:true client in
                      let parent_entry =
                        Agent_server.Session_registry.find
                          (Daemon.registry daemon)
                          parent.id
                        |> Option.value_exn
                      in
                      let pins =
                        Owner.with_background_runtime parent_entry.runtime (fun runtime ->
                          let native =
                            Option.value_exn
                              runtime.Agent_session.Runtime_builder.native_runtime
                          in
                          let capabilities =
                            Lazy.force native.capabilities
                            |> Result.map_error ~f:(fun error ->
                              error.Chat_response.Tool_capability.message)
                            |> Result.ok_or_failwith
                          in
                          let selected =
                            Chat_response.Tool_capability.select
                              capabilities
                              ~names:[ "read_file" ]
                            |> Result.map_error ~f:(fun error ->
                              error.Chat_response.Tool_capability.message)
                            |> Result.ok_or_failwith
                          in
                          Chat_response.Background_request.capability_pins selected)
                        |> protocol_ok
                      in
                      let parent_state = A.state parent_entry.actor |> protocol_ok in
                      let bad_id, _ =
                        install_child
                          ~env
                          ~sw
                          ~daemon
                          ~parent:parent_state
                          ~mode:`Valid
                          ~source:
                            {|<developer>Bad initializer.</developer><script id="bad" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = if true then fail("fixture initialization failure") else 0
let on_event ctx state event = Task.pure(state)
</script>|}
                          ~capability_pins:[]
                          ~revoke:false
                      in
                      let bad =
                        H.attach
                          ~sw
                          ~clock:(Eio.Stdenv.clock env)
                          ~connection:client
                          ~session_id:bad_id
                          ~mode:Read_write
                          ~subscribe:false
                          ()
                        |> protocol_ok
                      in
                      assert (Result.is_error (H.start bad ~queue_if_limited:false));
                      H.detach bad |> protocol_ok;
                      [%test_eq: int] 0 !requests;
                      assert (Owner.is_loaded parent_entry.runtime);
                      let child_id, _ =
                        install_child
                          ~env
                          ~sw
                          ~daemon
                          ~parent:parent_state
                          ~mode:`Valid
                          ~source:
                            {|<config model="child-test" reasoning_effort="high"/><developer>Child.</developer><tool type="inherited" name="read_file"/>|}
                          ~capability_pins:pins
                          ~revoke:false
                      in
                      let child =
                        H.attach
                          ~sw
                          ~clock:(Eio.Stdenv.clock env)
                          ~connection:client
                          ~session_id:child_id
                          ~mode:Read_write
                          ~subscribe:false
                          ()
                        |> protocol_ok
                      in
                      let parent_handle =
                        H.attach
                          ~sw
                          ~clock:(Eio.Stdenv.clock env)
                          ~connection:client
                          ~session_id:parent.id
                          ~mode:Read_write
                          ~subscribe:false
                          ()
                        |> protocol_ok
                      in
                      H.start child ~queue_if_limited:false |> protocol_ok |> ignore;
                      let child_entry =
                        Agent_server.Session_registry.find
                          (Daemon.registry daemon)
                          child_id
                        |> Option.value_exn
                      in
                      let send text =
                        H.send_message child { kind = Plain_text; text; attachments = [] }
                        |> protocol_ok
                        |> ignore
                      in
                      send "Read the inherited file.";
                      let rec idle () =
                        let state = A.state child_entry.actor |> protocol_ok in
                        match state.active_operation with
                        | None -> state
                        | Some _ ->
                          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                          idle ()
                      in
                      let state = idle () in
                      assert (
                        List.exists state.conversation.canonical_history ~f:(fun entry ->
                          String.is_substring
                            (Jsonaf.to_string entry.P.History.payload)
                            ~substring:"inherited-parent-file"));
                      [%test_eq: int] 2 !requests;
                      assert (
                        List.for_all state.invocations ~f:(fun invocation ->
                          String.equal
                            invocation.P.Invocation.context.tool_name
                            "read_file"));
                      H.stop child ~mode:Cancel |> protocol_ok |> ignore;
                      assert (Owner.is_loaded parent_entry.runtime);
                      assert (
                        P.Session.equal_desired_state
                          (A.state parent_entry.actor |> protocol_ok).lifecycle.desired
                          Running);
                      H.start child ~queue_if_limited:false |> protocol_ok |> ignore;
                      [%test_eq: int] 2 !requests;
                      send "Wait for parent cancellation.";
                      Eio.Promise.await entered;
                      let stop =
                        Eio.Fiber.fork_promise ~sw (fun () ->
                          H.stop parent_handle ~mode:Cancel)
                      in
                      Eio.Promise.await cleaning;
                      assert (not !cleaned);
                      assert (Owner.is_loaded parent_entry.runtime);
                      release_cleanup ();
                      Eio.Promise.await_exn stop |> protocol_ok |> ignore;
                      let state = idle () in
                      assert !cleaned;
                      assert (
                        P.Session.equal_desired_state state.lifecycle.desired Stopped);
                      assert (not (Owner.is_loaded parent_entry.runtime));
                      [%test_eq: int] 3 !requests;
                      H.detach child |> protocol_ok;
                      H.detach parent_handle |> protocol_ok;
                      child_id))))
        in
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
                  match
                    Agent_client.Connection.request
                      client
                      (Session_get { session_id = child_id; history = None })
                    |> protocol_ok
                  with
                  | Session_get snapshot ->
                    assert (
                      P.Session.equal_desired_state snapshot.session.desired_state Stopped);
                    assert (
                      List.exists snapshot.canonical_history.entries ~f:(fun entry ->
                        String.is_substring
                          (Jsonaf.to_string entry.P.History.payload)
                          ~substring:"inherited-parent-file"));
                    [%test_eq: int] 3 !requests;
                    print_endline
                      "inherited read executed; parent stop joined child provider \
                       cleanup; stopped history recovered"
                  | _ -> failwith "unexpected child snapshot")))));
  [%expect
    {| inherited read executed; parent stop joined child provider cleanup; stopped history recovered |}]
;;

let%expect_test
    "active generated descendants recover in dependency order with fresh bindings and \
     configured depth"
  =
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
          {|<developer>Root.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool><tool name="run_chatml"/><tool name="append_to_file"/>|};
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "delegated.txt")
          "grandchild-owned-read";
        let request source input tools =
          `Object
            [ "source", `String source
            ; "input", input
            ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
            ]
        in
        let nested_request =
          request
            {|let main input =
  let* result = Tool.call("run_chatml", input) in
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(code) -> Task.fail(code)|}
            (request
               {|let main input =
  let* result = Tool.call("read_file", input) in
  let* denied = Tool.call("append_to_file", input) in
  match result with
  | `Error(code) -> Task.fail(code)
  | `Ok(value) ->
    match denied with
    | `Ok(_) -> Task.fail("unselected tool ran")
    | `Error(code) -> Task.pure(`Object([
        {key = "read"; value = value},
        {key = "blocked"; value = `String(code)}]))|}
               (`Object [ "root", `String "data"; "file", `String "delegated.txt" ])
               [ "read_file" ])
            [ "run_chatml"; "read_file" ]
          |> Jsonaf.to_string
        in
        let config = config root root prompt_file in
        let requests = ref 0 in
        let start sw depth =
          let options =
            { Daemon.default_options with
              qualify_chatml_extensions = true
            ; factory_limits =
                { Daemon.default_options.factory_limits with
                  delegation_max_depth = depth
                }
            ; model_post_stream =
                Some
                  (fun ~sw:_ ~inputs:_ ->
                    Int.incr requests;
                    match !requests % 2 with
                    | 0 -> Stdlib.Seq.empty
                    | _ ->
                      let open Openai.Responses.Response_stream in
                      [ Output_item_added
                          { item =
                              Function_call
                                { name = "run_chatml"
                                ; arguments = ""
                                ; call_id = sprintf "nested-%d" !requests
                                ; _type = "function_call"
                                ; id = Some "nested-item"
                                ; status = None
                                }
                          ; output_index = 0
                          ; type_ = "response.output_item.added"
                          }
                      ; Function_call_arguments_done
                          { arguments = nested_request
                          ; item_id = "nested-item"
                          ; output_index = 0
                          ; type_ = "response.function_call_arguments.done"
                          }
                      ]
                      |> Stdlib.List.to_seq)
            }
          in
          Daemon.start
            ~sw
            ~env
            ~config
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options
            ()
        in
        let child_creation daemon parent_id =
          let module G = Agent_session.Generated_definition in
          let module C = Chat_response.Tool_capability in
          let entry =
            Agent_server.Session_registry.find (Daemon.registry daemon) parent_id
            |> Option.value_exn
          in
          let prepare () =
            Owner.with_background_runtime entry.runtime (fun runtime ->
              let native =
                Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
              in
              let capabilities =
                Lazy.force native.capabilities
                |> Result.map_error ~f:(fun error -> error.C.message)
                |> Result.ok_or_failwith
              in
              let selected =
                C.select capabilities ~names:[ "read_file"; "run_chatml" ]
                |> Result.map_error ~f:(fun error -> error.C.message)
                |> Result.ok_or_failwith
              in
              let source =
                {|<authoring_context policy="manual"/><developer>Generated descendant.</developer><tool type="inherited" name="read_file"/><tool type="inherited" name="run_chatml"/>|}
              in
              let bundle =
                Chatmd_source_bundle.create
                  ~root_file:"child.chatmd"
                  ~sources:[ "child.chatmd", source ]
                  ()
                |> Result.ok_or_failwith
              in
              G.prepare
                ~env
                ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
                ~revision_id:(P.Id.Prompt_revision.create ())
                ~created_at:(P.Timestamp.now ())
                ~current_capabilities:(fun () -> capabilities)
                ~references:(C.references selected)
                bundle
              |> Result.map_error ~f:(fun errors ->
                P.Error.invalid_request
                  (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                   |> String.concat ~sep:"\n")))
            |> protocol_ok
          in
          let create ~display_name definition =
            Agent_server.Session_factory.create_generated_session
              (Daemon.factory daemon)
              ~parent_session_id:parent_id
              ~idempotency_key:
                (P.Idempotency_key.of_string (P.Id.Session.to_string parent_id)
                 |> protocol_ok)
              ~display_name
              definition
          in
          create, prepare
        in
        let create_child daemon parent_id =
          let create, prepare = child_creation daemon parent_id in
          let created = create ~display_name:None (prepare ()) |> protocol_ok in
          let state = A.state created.actor |> protocol_ok in
          let repeated = create ~display_name:None (prepare ()) |> protocol_ok in
          assert (
            P.Id.Session.equal
              state.identity.session_id
              (A.state repeated.actor |> protocol_ok).identity.session_id);
          (match create ~display_name:(Some "different request") (prepare ()) with
           | Error { code = Conflict; _ } -> ()
           | _ -> failwith "changed creation payload did not conflict");
          assert (
            List.exists state.conversation.canonical_history ~f:(fun entry ->
              String.is_substring
                (Jsonaf.to_string entry.P.History.payload)
                ~substring:"Generated descendant."));
          assert (
            not
              (List.exists state.conversation.canonical_history ~f:(fun entry ->
                 String.is_substring
                   (Jsonaf.to_string entry.P.History.payload)
                   ~substring:"Root.")));
          state.identity.session_id
        in
        let root_id, child_id, leaf_id =
          Eio.Switch.run (fun sw ->
            let daemon = start sw 2 |> protocol_ok in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    let parent, _ = create_session ~start_immediately:true client in
                    let make_child parent_id =
                      let id = create_child daemon parent_id in
                      let handle =
                        H.attach
                          ~sw
                          ~clock:(Eio.Stdenv.clock env)
                          ~connection:client
                          ~session_id:id
                          ~mode:Read_write
                          ~subscribe:false
                          ()
                        |> protocol_ok
                      in
                      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
                      H.detach handle |> protocol_ok;
                      id
                    in
                    let child = make_child parent.id in
                    let leaf = make_child child in
                    [%test_eq: int] 0 !requests;
                    parent.id, child, leaf)))
        in
        Eio.Switch.run (fun sw ->
          match start sw 1 with
          | Error { code = Persistence_error; message; _ } ->
            assert (String.is_substring message ~substring:"depth")
          | Ok daemon ->
            Daemon.shutdown daemon |> protocol_ok;
            failwith "recovery ignored configured ancestry limit"
          | Error error -> raise_s [%sexp (error : P.Error.t)]);
        List.iter [ 1; 2 ] ~f:(fun restart ->
          Eio.Switch.run (fun sw ->
            let daemon = start sw 2 |> protocol_ok in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                assert (P.Id.Session.equal child_id (create_child daemon root_id));
                assert (P.Id.Session.equal leaf_id (create_child daemon child_id));
                List.iter [ root_id; child_id; leaf_id ] ~f:(fun id ->
                  let entry =
                    Agent_server.Session_registry.find (Daemon.registry daemon) id
                    |> Option.value_exn
                  in
                  assert (
                    P.Session.equal_desired_state
                      (A.state entry.actor |> protocol_ok).lifecycle.desired
                      Running);
                  assert (Owner.is_loaded entry.runtime));
                let registry = Daemon.registry daemon in
                let previous =
                  Agent_server.Session_registry.find registry leaf_id |> Option.value_exn
                in
                previous.close ();
                ignore
                  (Agent_server.Session_registry.remove registry leaf_id
                   : Agent_server.Session_registry.entry option);
                Agent_server.Session_registry.index
                  registry
                  (Agent_store.Session_index.find
                     (S.session_index (Daemon.store daemon))
                     leaf_id
                   |> Option.value_exn);
                let reloaded =
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    Agent_server.Session_registry.load registry leaf_id |> protocol_ok)
                in
                assert (Owner.is_loaded reloaded.runtime);
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    let handle =
                      H.attach
                        ~sw
                        ~clock:(Eio.Stdenv.clock env)
                        ~connection:client
                        ~session_id:leaf_id
                        ~mode:Read_write
                        ~subscribe:false
                        ()
                      |> protocol_ok
                    in
                    H.send_message
                      handle
                      { kind = Plain_text
                      ; text = "Continue after restart."
                      ; attachments = []
                      }
                    |> protocol_ok
                    |> ignore;
                    let entry =
                      Agent_server.Session_registry.find (Daemon.registry daemon) leaf_id
                      |> Option.value_exn
                    in
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                      let rec done_ () =
                        match (A.state entry.actor |> protocol_ok).active_operation with
                        | None -> ()
                        | Some _ ->
                          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                          done_ ()
                      in
                      done_ ());
                    [%test_eq: int] (restart * 2) !requests;
                    let state = A.state entry.actor |> protocol_ok in
                    [%test_eq: int] (restart * 5) (List.length state.invocations);
                    assert (Option.is_none state.failure);
                    List.iter state.invocations ~f:(fun invocation ->
                      assert (P.Id.Session.equal invocation.context.session_id leaf_id);
                      match invocation.status with
                      | Resolved (Complete _) | Published (Complete _) -> ()
                      | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
                    let outputs =
                      List.filter_map state.invocations ~f:(fun invocation ->
                        match invocation.status with
                        | Published (Complete value) -> Some (Jsonaf.to_string value)
                        | _ -> None)
                    in
                    [%test_pred: string list]
                      (List.exists ~f:(fun text ->
                         String.is_substring text ~substring:"grandchild-owned-read"
                         && String.is_substring
                              text
                              ~substring:"invocation.unselected_tool"))
                      outputs;
                    List.iter [ root_id; child_id ] ~f:(fun id ->
                      let ancestor =
                        Agent_server.Session_registry.find registry id |> Option.value_exn
                      in
                      let state = A.state ancestor.actor |> protocol_ok in
                      assert (List.is_empty state.invocations);
                      assert (List.is_empty state.jobs));
                    (match restart with
                     | 2 ->
                       H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
                       H.detach handle |> protocol_ok;
                       entry.close ();
                       ignore
                         (Agent_server.Session_registry.remove registry leaf_id
                          : Agent_server.Session_registry.entry option);
                       let create, prepare = child_creation daemon child_id in
                       let unavailable () =
                         match create ~display_name:None (prepare ()) with
                         | Error { code = Session_not_found; _ } -> ()
                         | _ -> failwith "creation retry resurrected a retained child"
                       in
                       S.archive_session (Daemon.store daemon) leaf_id |> store_ok;
                       unavailable ();
                       S.remove_session (Daemon.store daemon) leaf_id |> store_ok;
                       unavailable ();
                       assert (List.length (S.list_sessions (Daemon.store daemon)) = 2)
                     | _ -> H.detach handle |> protocol_ok);
                    print_s
                      [%sexp
                        (restart : int)
                      , "active ancestry recovered; fresh inherited bindings execute"]))))));
  [%expect
    {|
    (1 "active ancestry recovered; fresh inherited bindings execute")
    (2 "active ancestry recovered; fresh inherited bindings execute")
    |}]
;;
