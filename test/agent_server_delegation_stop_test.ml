open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let create_child ?(key = "child") env root daemon parent =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  let state = A.state parent.Registry.actor |> protocol_ok in
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
              , {|<developer>Owned descendant.</developer><tool type="inherited" name="read_file"/>|}
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
  in
  Agent_server.Session_factory.create_generated_session
    ~start_immediately:true
    (Daemon.factory daemon)
    ~parent_session_id:state.identity.session_id
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let run ~active_parent =
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
          {|<developer>Root.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
        let entered, entered_u = Eio.Promise.create () in
        let sibling_entered, sibling_entered_u = Eio.Promise.create () in
        let cleaning, cleaning_u = Eio.Promise.create () in
        let sibling_cleaning, sibling_cleaning_u = Eio.Promise.create () in
        let parent_entered, parent_entered_u = Eio.Promise.create () in
        let parent_cleaning, parent_cleaning_u = Eio.Promise.create () in
        let parent_release, parent_release_u = Eio.Promise.create () in
        let parent_released = ref false in
        let release, release_u = Eio.Promise.create () in
        let never, _ = Eio.Promise.create () in
        let released = ref false
        and cleaned = ref 0
        and requests = ref 0 in
        let release_parent_cleanup () =
          if not !parent_released
          then (
            parent_released := true;
            Eio.Promise.resolve parent_release_u ())
        in
        let release_cleanup () =
          release_parent_cleanup ();
          if not !released
          then (
            released := true;
            Eio.Promise.resolve release_u ())
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Daemon.start
              ~sw
              ~env
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
                        let cleanup, completion =
                          match !requests with
                          | 1 ->
                            Eio.Promise.resolve entered_u ();
                            cleaning_u, release
                          | 2 ->
                            Eio.Promise.resolve sibling_entered_u ();
                            sibling_cleaning_u, release
                          | 3 when active_parent ->
                            Eio.Promise.resolve parent_entered_u ();
                            parent_cleaning_u, parent_release
                          | _ -> failwith "unexpected provider request"
                        in
                        Exn.protect
                          ~finally:(fun () ->
                            Eio.Cancel.protect (fun () ->
                              Eio.Promise.resolve cleanup ();
                              Eio.Promise.await completion;
                              Int.incr cleaned))
                          ~f:(fun () ->
                            Eio.Promise.await never;
                            Stdlib.Seq.empty))
                }
              ()
            |> protocol_ok
          in
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
                      Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    let middle = create_child env root daemon parent_entry in
                    let leaf = create_child env root daemon middle in
                    let sibling = create_child ~key:"sibling" env root daemon middle in
                    let leaf_id =
                      (A.state leaf.actor |> protocol_ok).identity.session_id
                    in
                    let attach id =
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
                    let parent_handle = attach parent.id
                    and leaf_handle = attach leaf_id
                    and sibling_handle =
                      attach (A.state sibling.actor |> protocol_ok).identity.session_id
                    in
                    H.send_message
                      leaf_handle
                      { kind = Plain_text
                      ; text = "Wait for cancellation."
                      ; attachments = []
                      }
                    |> protocol_ok
                    |> ignore;
                    Eio.Promise.await entered;
                    H.send_message
                      sibling_handle
                      { kind = Plain_text; text = "Sibling work."; attachments = [] }
                    |> protocol_ok
                    |> ignore;
                    Eio.Promise.await sibling_entered;
                    if active_parent
                    then (
                      H.send_message
                        parent_handle
                        { kind = Plain_text; text = "Parent work."; attachments = [] }
                      |> protocol_ok
                      |> ignore;
                      Eio.Promise.await parent_entered);
                    let stopped =
                      Eio.Fiber.fork_promise ~sw (fun () ->
                        H.stop parent_handle ~mode:Cancel)
                    in
                    if active_parent then Eio.Promise.await parent_cleaning;
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      Eio.Promise.await cleaning;
                      Eio.Promise.await sibling_cleaning);
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
                    [%test_eq: int] 0 !cleaned;
                    if active_parent
                    then (
                      let progress = Eio.Promise.await_exn stopped |> protocol_ok in
                      assert (P.Session.equal_desired_state progress.desired_state Stopped);
                      match progress.observed_state with
                      | Running_turn _ -> ()
                      | _ -> failwith "parent stop hid pending cleanup")
                    else assert (Option.is_none (Eio.Promise.peek stopped));
                    assert (Owner.is_loaded parent_entry.runtime);
                    if active_parent
                    then (
                      release_parent_cleanup ();
                      let rec idle () =
                        match
                          (A.state parent_entry.actor |> protocol_ok).active_operation
                        with
                        | None -> ()
                        | Some _ ->
                          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                          idle ()
                      in
                      idle ();
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
                      [%test_eq: int] 1 !cleaned;
                      assert (Owner.is_loaded parent_entry.runtime));
                    release_cleanup ();
                    let result = Eio.Promise.await_exn stopped |> protocol_ok in
                    assert (P.Session.equal_desired_state result.desired_state Stopped);
                    let entries = [ parent_entry; middle; leaf; sibling ] in
                    let rec retired () =
                      match
                        List.exists entries ~f:(fun entry ->
                          Owner.is_loaded entry.runtime)
                      with
                      | false -> ()
                      | true ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        retired ()
                    in
                    retired ();
                    [%test_eq: int] (if active_parent then 3 else 2) !cleaned;
                    List.iter entries ~f:(fun entry ->
                      let state = A.state entry.actor |> protocol_ok in
                      assert (
                        P.Session.equal_desired_state state.lifecycle.desired Stopped);
                      assert (Option.is_none state.active_operation);
                      assert (not (Owner.is_loaded entry.runtime)));
                    [%test_eq: int] (if active_parent then 3 else 2) !requests;
                    H.detach sibling_handle |> protocol_ok;
                    H.detach leaf_handle |> protocol_ok;
                    H.detach parent_handle |> protocol_ok;
                    print_endline
                      "both grandchildren cancelled before either cleanup finished; root \
                       stop joined all runtimes"))))))
;;

let%expect_test
    "root stop cancels sibling grandchildren before joining their blocked cleanup"
  =
  run ~active_parent:false;
  [%expect
    {| both grandchildren cancelled before either cleanup finished; root stop joined all runtimes |}]
;;

let%expect_test
    "active parent cancellation reaches descendants before parent cleanup finishes"
  =
  run ~active_parent:true;
  [%expect
    {| both grandchildren cancelled before either cleanup finished; root stop joined all runtimes |}]
;;
