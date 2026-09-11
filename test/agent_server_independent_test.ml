open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Factory = Agent_server.Session_factory
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let observe_rename (Eio.Resource.T (directory, handler) as native_directory) reached =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let rename directory source _destination target =
      Original.rename directory source native_directory target;
      reached target
    ;;
  end
  in
  let bindings =
    Eio.Resource.bindings handler
    |> List.filter ~f:(function
      | H (Eio.Fs.Pi.Dir, _) -> false
      | _ -> true)
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler (H (Eio.Fs.Pi.Dir, (module Directory)) :: bindings))
;;

let state (entry : Registry.entry) = A.state entry.actor |> protocol_ok
let id entry = (state entry).identity.session_id

let prepare env root (parent : Registry.entry) =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  Owner.with_background_runtime parent.runtime (fun runtime ->
    let native = Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime in
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
            , {|<developer>Read the inherited workspace.</developer><tool type="inherited" name="read_file"/>|}
            )
          ]
        ()
      |> Result.ok_or_failwith
    in
    G.prepare
      ~env
      ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
      ~revision_id:(P.Id.Prompt_revision.create ())
      ~created_at:(P.Timestamp.now ())
      ~current_capabilities:(fun () -> capabilities)
      ~references:(C.references capabilities)
      bundle
    |> Result.map_error ~f:(fun errors ->
      P.Error.invalid_request
        (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
         |> String.concat ~sep:"\n")))
  |> protocol_ok
;;

let create daemon parent definition ~key ~lifetime =
  Factory.create_generated_session
    ~start_immediately:true
    ~lifetime
    (Daemon.factory daemon)
    ~parent_session_id:(id parent)
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~display_name:None
    definition
;;

let%expect_test "a linked initial start retains its workspace before resource admission" =
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
          {|<developer>Parent.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
        let base = config root root prompt_file in
        let config =
          { base with
            workspaces =
              List.map base.workspaces ~f:(fun workspace ->
                { workspace with
                  source =
                    Temporary
                      { location = Session_dir
                      ; cleanup = On_session_stop
                      ; managed_root = None
                      }
                })
          }
        in
        let release, release_u = Eio.Promise.create () in
        let clock_started, clock_started_u = Eio.Promise.create () in
        let module Clock = struct
          type t = unit
          type time = float

          let now () = Eio.Time.now (Eio.Stdenv.clock env)

          let sleep_until () time =
            if Option.is_none (Eio.Promise.peek clock_started)
            then Eio.Promise.resolve clock_started_u ();
            Eio.Promise.await release;
            Eio.Time.sleep_until (Eio.Stdenv.clock env) time
          ;;
        end
        in
        let clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
        let armed = ref false in
        let reached target =
          if
            !armed
            && String.is_substring target ~substring:"/delegations/"
            && String.is_suffix target ~suffix:".frame"
          then (
            let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / target) in
            let payload =
              match
                Agent_store.Frame.decode ~max_payload_length:262144 ~contents ~offset:0
              with
              | Ok (Complete { frame; _ }) -> Agent_store.Frame.payload frame
              | _ -> failwith "invalid delegation frame"
            in
            if String.is_substring payload ~substring:"(stage Linked)"
            then (
              armed := false;
              failwith "injected lost link acknowledgement"))
        in
        let directory, fs_path = Eio.Stdenv.fs env in
        let observed_env =
          object
            method fs = observe_rename directory reached, fs_path
            method cwd = env#cwd
            method stdin = env#stdin
            method stdout = env#stdout
            method stderr = env#stderr
            method net = env#net
            method domain_mgr = env#domain_mgr
            method process_mgr = env#process_mgr
            method clock = clock
            method mono_clock = env#mono_clock
            method secure_random = env#secure_random
            method debug = env#debug
            method backend_id = env#backend_id
          end
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Daemon.start
              ~sw
              ~env:observed_env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Daemon.default_options with
                  qualify_chatml_extensions = true
                ; independent_lifetime_policy = Some "pending-v1"
                ; model_post_stream =
                    Some (fun ~sw:_ ~inputs:_ -> failwith "unexpected provider call")
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () ->
              Eio.Promise.resolve release_u ();
              Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    Eio.Promise.await clock_started;
                    let parent, _ = create_session ~start_immediately:true client in
                    let entry =
                      Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    let workspace =
                      (state entry).spec.workspace_instance.canonical_root.native_path
                    in
                    let marker =
                      Eio.Path.(Eio.Stdenv.fs env / workspace / "retained.txt")
                    in
                    Eio.Path.save ~create:(`Exclusive 0o600) marker "pending-child";
                    let definition = prepare observed_env root entry in
                    armed := true;
                    (match
                       create
                         daemon
                         entry
                         definition
                         ~key:"lost-linked"
                         ~lifetime:Independent
                     with
                     | Error { code = Persistence_error; _ } -> ()
                     | Error error -> raise_s [%sexp (error : P.Error.t)]
                     | Ok _ -> failwith "creation did not lose its link acknowledgement");
                    let module D = Agent_store.Delegation_store in
                    let record =
                      D.with_records
                        (Agent_store.Session_store.delegations (Daemon.store daemon))
                        ~max_records:8
                        ~max_bytes:1048576
                        ~f:(fun records -> Ok (List.hd_exn records))
                      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                      |> protocol_ok
                    in
                    assert (D.equal_stage record.stage Linked);
                    let indexed =
                      Agent_store.Session_index.find
                        (Agent_store.Session_store.session_index (Daemon.store daemon))
                        record.admission.child_session_id
                      |> Option.value_exn
                    in
                    assert indexed.pending_initial_start;
                    assert (
                      Option.is_none
                        (Registry.find
                           (Daemon.registry daemon)
                           record.admission.child_session_id));
                    let handle =
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
                    H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
                    [%test_eq: string] "pending-child" (Eio.Path.load marker);
                    (match
                       H.reset
                         handle
                         ~expected_revision:(state entry).counters.revision
                         ~keep_history:true
                         ~keep_tasks:true
                         ~keep_cache:false
                         ~keep_workspace:false
                         ~keep_grants:true
                         ~keep_labels:true
                     with
                     | Error { code = Conflict; _ } -> ()
                     | _ -> failwith "reset removed resources of a linked pending child");
                    H.close handle;
                    Agent_client.Connection.close client;
                    ignore
                      (Registry.unload_inactive
                         (Daemon.registry daemon)
                         ~index_entries:
                           (Agent_store.Session_store.list_sessions (Daemon.store daemon))
                       : int);
                    Factory.resume_generated_initial_starts (Daemon.factory daemon);
                    let child =
                      Registry.find
                        (Daemon.registry daemon)
                        record.admission.child_session_id
                      |> Option.value_exn
                    in
                    let current = state child in
                    assert (not current.pending_initial_start);
                    assert (
                      P.Session.equal_desired_state current.lifecycle.desired Running);
                    assert (Option.is_none current.failure);
                    assert (Owner.is_loaded child.runtime);
                    let parent =
                      Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    assert (not (Owner.is_loaded parent.runtime));
                    print_endline
                      "lost linked acknowledgement preserves pending child roots through \
                       parent stop, eviction and initial activation"))))));
  [%expect
    {| lost linked acknowledgement preserves pending child roots through parent stop, eviction and initial activation |}]
;;

let%expect_test "independent ancestry retains temporary roots across stop and restart" =
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
          {|<developer>Parent.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
        let base = config root root prompt_file in
        let configuration =
          { base with
            workspaces =
              List.map base.workspaces ~f:(fun workspace ->
                { workspace with
                  source =
                    Temporary
                      { location = Session_dir
                      ; cleanup = On_session_stop
                      ; managed_root = None
                      }
                })
          }
        in
        let requests = ref 0 in
        let post_stream ~sw:_ ~inputs =
          Int.incr requests;
          match List.last inputs with
          | Some (Openai.Responses.Item.Function_call_output _) -> Stdlib.Seq.empty
          | _ ->
            let open Openai.Responses.Response_stream in
            [ Output_item_added
                { item =
                    Function_call
                      { name = "read_file"
                      ; arguments = ""
                      ; call_id = sprintf "read-%d" !requests
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
        in
        let with_daemon policy f =
          Eio.Switch.run (fun sw ->
            let daemon =
              Daemon.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; independent_lifetime_policy = policy
                  ; model_post_stream = Some post_stream
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let attach sw client entry =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:(id entry)
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let find daemon session_id =
          Registry.find (Daemon.registry daemon) session_id |> Option.value_exn
        in
        let rec idle entry =
          let current = state entry in
          match current.active_operation with
          | None -> current
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let read handle entry expected =
          let previous =
            List.map (state entry).invocations ~f:(fun invocation ->
              invocation.P.Invocation.context.id)
          in
          H.send_message
            handle
            { kind = Plain_text; text = "Read value.txt."; attachments = [] }
          |> protocol_ok
          |> ignore;
          let current = idle entry in
          let fresh =
            List.filter current.invocations ~f:(fun invocation ->
              not
                (List.mem
                   previous
                   invocation.P.Invocation.context.id
                   ~equal:P.Id.Invocation.equal))
          in
          match fresh with
          | [ { status = Published (Complete (`String text)); _ } ] ->
            assert (String.is_substring text ~substring:expected)
          | _ ->
            raise_s
              [%sexp "expected one new successful read", (fresh : P.Invocation.t list)]
        in
        let running entry =
          assert (P.Session.equal_desired_state (state entry).lifecycle.desired Running)
        in
        let stopped entry =
          assert (P.Session.equal_desired_state (state entry).lifecycle.desired Stopped)
        in
        with_daemon None (fun sw daemon client ->
          let parent, _ =
            create_session ~key:"default-denial" ~start_immediately:true client
          in
          let entry = find daemon parent.id in
          let definition = prepare env root entry in
          (match create daemon entry definition ~key:"denied" ~lifetime:Independent with
           | Error { code = Permission_denied; _ } -> ()
           | _ -> failwith "ungranted independent lifetime was admitted");
          [%test_eq: int]
            1
            (List.length (Agent_store.Session_store.list_sessions (Daemon.store daemon)));
          let handle = attach sw client entry in
          H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
          H.close handle);
        let parent_id, middle_id, child_id, leaf_id, workspace =
          with_daemon (Some "fixture-v1") (fun sw daemon client ->
            let parent, _ =
              create_session ~key:"independent-root" ~start_immediately:true client
            in
            let parent = find daemon parent.id in
            let parent_handle = attach sw client parent in
            let workspace =
              (state parent).spec.workspace_instance.canonical_root.native_path
            in
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
              "before-restart";
            let middle =
              create daemon parent (prepare env root parent) ~key:"middle" ~lifetime:Owned
              |> protocol_ok
            in
            let child_definition = prepare env root middle in
            let child =
              create daemon middle child_definition ~key:"child" ~lifetime:Independent
              |> protocol_ok
            in
            let replay =
              create daemon middle child_definition ~key:"child" ~lifetime:Independent
              |> protocol_ok
            in
            assert (P.Id.Session.equal (id child) (id replay));
            (match create daemon middle child_definition ~key:"child" ~lifetime:Owned with
             | Error { code = Conflict; _ } -> ()
             | _ -> failwith "lifetime change did not conflict with the retained key");
            let leaf =
              create daemon child (prepare env root child) ~key:"leaf" ~lifetime:Owned
              |> protocol_ok
            in
            let child_handle = attach sw client child in
            let leaf_handle = attach sw client leaf in
            H.stop parent_handle ~mode:Cancel |> protocol_ok |> ignore;
            stopped parent;
            stopped middle;
            running child;
            running leaf;
            assert (not (Owner.is_loaded parent.runtime));
            assert (not (Owner.is_loaded middle.runtime));
            read child_handle child "before-restart";
            read leaf_handle leaf "before-restart";
            let before = state parent in
            let reset =
              Agent_client.Connection.request
                client
                (Session_reset
                   { session_id = id parent
                   ; attachment_id = (H.attachment parent_handle).id
                   ; expected_revision = before.counters.revision
                   ; keep_history = true
                   ; keep_tasks = true
                   ; keep_cache = false
                   ; keep_workspace = false
                   ; keep_grants = true
                   ; keep_labels = true
                   ; idempotency_key =
                       P.Idempotency_key.of_string "retained-reset" |> protocol_ok
                   })
            in
            (match reset with
             | Error { code = Conflict; _ } -> ()
             | _ -> failwith "reset changed retained roots");
            H.close parent_handle;
            H.close child_handle;
            H.close leaf_handle;
            Agent_client.Connection.close client;
            ignore
              (Registry.unload_inactive
                 (Daemon.registry daemon)
                 ~index_entries:
                   (Agent_store.Session_store.list_sessions (Daemon.store daemon))
               : int);
            assert (Option.is_some (Registry.find (Daemon.registry daemon) (id parent)));
            assert (Option.is_some (Registry.find (Daemon.registry daemon) (id middle)));
            id parent, id middle, id child, id leaf, workspace)
        in
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
          "after-restart";
        with_daemon (Some "fixture-v1") (fun sw daemon client ->
          let parent = find daemon parent_id
          and middle = find daemon middle_id in
          let child = find daemon child_id
          and leaf = find daemon leaf_id in
          stopped parent;
          stopped middle;
          running child;
          running leaf;
          assert (not (Owner.is_loaded parent.runtime));
          assert (not (Owner.is_loaded middle.runtime));
          let child_handle = attach sw client child
          and leaf_handle = attach sw client leaf in
          read child_handle child "after-restart";
          read leaf_handle leaf "after-restart";
          assert (List.is_empty (state parent).invocations);
          assert (List.is_empty (state middle).invocations);
          H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
          stopped child;
          stopped leaf;
          H.close child_handle;
          H.close leaf_handle;
          Agent_client.Connection.close client;
          ignore
            (Registry.unload_inactive
               (Daemon.registry daemon)
               ~index_entries:
                 (Agent_store.Session_store.list_sessions (Daemon.store daemon))
             : int);
          assert (Option.is_none (Registry.find (Daemon.registry daemon) parent_id));
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close client)
            ~f:(fun () ->
              initialize client;
              let child =
                Registry.load (Daemon.registry daemon) child_id |> protocol_ok
              in
              let child_handle = attach sw client child in
              H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
              read child_handle child "after-restart";
              let parent = find daemon parent_id in
              assert (not (Owner.is_loaded parent.runtime));
              H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
              H.close child_handle));
        with_daemon (Some "fixture-v2") (fun sw daemon client ->
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let handle = attach sw client child in
          let before = !requests in
          (match H.start handle ~queue_if_limited:false with
           | Error { code = Permission_denied; message; _ } ->
             assert (String.is_substring message ~substring:"delegation.lifetime_denied")
           | _ -> failwith "changed host grant admitted old independent session");
          [%test_eq: int] before !requests;
          assert (not (Owner.is_loaded child.runtime));
          H.close handle);
        with_daemon (Some "fixture-v1") (fun sw daemon client ->
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let child_handle = attach sw client child in
          H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          let leaf = Registry.load (Daemon.registry daemon) leaf_id |> protocol_ok in
          let leaf_handle = attach sw client leaf in
          H.start leaf_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          read leaf_handle leaf "after-restart";
          let parent = find daemon parent_id in
          let parent_handle = attach sw client parent in
          H.delete
            parent_handle
            ~expected_revision:(state parent).counters.revision
            ~policy:Remove
            ~confirmation:(P.Id.Session.to_string parent_id)
          |> protocol_ok
          |> ignore;
          stopped child;
          stopped leaf;
          assert (
            match
              Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / workspace)
            with
            | `Not_found -> true
            | _ -> false);
          H.close child_handle;
          H.close leaf_handle;
          H.close parent_handle);
        print_endline
          "explicit grant and stable replay; lifetime conflict; stopped ancestors retain \
           roots and avoid execution";
        print_endline
          "independent child and owned leaf read across restart; parent reset/eviction \
           excluded; owned stop preserved";
        print_endline
          "lazy ancestor reload avoids execution; deleting parent stops child before \
           removing roots";
        print_endline
          "changed host policy denies restart; restored grant permits use; deletion \
           joins owned descendants"));
  [%expect
    {|
    explicit grant and stable replay; lifetime conflict; stopped ancestors retain roots and avoid execution
    independent child and owned leaf read across restart; parent reset/eviction excluded; owned stop preserved
    lazy ancestor reload avoids execution; deleting parent stops child before removing roots
    changed host policy denies restart; restored grant permits use; deletion joins owned descendants
  |}]
;;
