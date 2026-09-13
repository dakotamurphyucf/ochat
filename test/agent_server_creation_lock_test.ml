open Core
open Agent_server_test_support
module P = Agent_protocol
module A = Agent_session.Session_actor
module D = Agent_store.Delegation_store
module G = Agent_session.Generated_definition
module Owner = Agent_server.Runtime_owner
module Daemon = Agent_server.Daemon

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

let%expect_test "child publication never reacquires its parent runtime from the actor" =
  List.iter [ false; true ] ~f:(fun moderated ->
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      let root = temporary_root env in
      let path = Eio.Path.(Eio.Stdenv.fs env / root) in
      Exn.protect
        ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
        ~f:(fun () ->
          let source =
            "<developer>Parent.</developer>"
            ^
            if moderated
            then
              {|<script id="policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
            else ""
          in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(path / "parent.chatmd")
            source;
          let installed, installed_u = Eio.Promise.create () in
          let proceed, proceed_u = Eio.Promise.create () in
          let linked, linked_u = Eio.Promise.create () in
          let paused = ref false in
          let reached target =
            if
              String.is_substring target ~substring:"/delegations/"
              && String.is_suffix target ~suffix:".frame"
            then (
              let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / target) in
              let payload =
                match
                  Agent_store.Frame.decode ~max_payload_length:262144 ~contents ~offset:0
                with
                | Ok (Complete { frame; _ }) -> Agent_store.Frame.payload frame
                | _ -> failwith "invalid test delegation frame"
              in
              (* Observe real persisted stages without recursively entering the
               ledger mutex from its filesystem callback. *)
              if
                String.is_substring payload ~substring:"(stage Child_installed)"
                && not !paused
              then (
                paused := true;
                Eio.Promise.resolve installed_u ();
                Eio.Promise.await proceed);
              if
                String.is_substring payload ~substring:"(stage Linked)"
                && Option.is_none (Eio.Promise.peek linked)
              then Eio.Promise.resolve linked_u ())
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
              method clock = env#clock
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
                ~config:(config root root (Filename.concat root "parent.chatmd"))
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; model_post_stream =
                      Some (fun ~sw:_ ~inputs:_ -> failwith "unexpected model")
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    let parent, _ = create_session ~start_immediately:true client in
                    let entry =
                      Agent_server.Session_registry.find
                        (Daemon.registry daemon)
                        parent.id
                      |> Option.value_exn
                    in
                    let definition =
                      Owner.with_background_runtime entry.runtime (fun runtime ->
                        let native =
                          Option.value_exn
                            runtime.Agent_session.Runtime_builder.native_runtime
                        in
                        let capabilities =
                          Lazy.force native.capabilities
                          |> Result.map_error ~f:(fun e ->
                            e.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                        in
                        let bundle =
                          Chatmd_source_bundle.create
                            ~root_file:"child.chatmd"
                            ~sources:[ "child.chatmd", "<developer>Child.</developer>" ]
                            ()
                          |> Result.ok_or_failwith
                        in
                        G.prepare
                          ~env
                          ~dir:path
                          ~revision_id:(P.Id.Prompt_revision.create ())
                          ~created_at:(P.Timestamp.now ())
                          ~current_capabilities:(fun () -> capabilities)
                          ~references:[]
                          bundle
                        |> Result.map_error ~f:(fun errors ->
                          P.Error.invalid_request
                            (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                             |> String.concat ~sep:"\n")))
                      |> protocol_ok
                    in
                    let creation =
                      Eio.Fiber.fork_promise ~sw (fun () ->
                        Agent_server.Session_factory.create_generated_session
                          (Daemon.factory daemon)
                          ~parent_session_id:parent.id
                          ~idempotency_key:
                            (P.Idempotency_key.of_string "publication" |> protocol_ok)
                          ~display_name:None
                          definition)
                    in
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                      Eio.Promise.await installed);
                    let linked_while_locked =
                      Owner.For_testing.with_loaded_runtime entry.runtime (fun () ->
                        Eio.Promise.resolve proceed_u ();
                        Ok
                          (Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
                             Eio.Promise.await linked;
                             Ok ())))
                      |> protocol_ok
                      |> Result.is_ok
                    in
                    (* Release the owner before awaiting creation, even on regression,
                 so the failing test can finish cleanup rather than hang. *)
                    let child = Eio.Promise.await_exn creation |> protocol_ok in
                    let state = A.state child.actor |> protocol_ok in
                    let record =
                      D.resolve
                        (Agent_store.Session_store.delegations (Daemon.store daemon))
                        (Option.value_exn state.spec.delegation)
                      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                      |> protocol_ok
                    in
                    assert (D.equal_stage record.stage Linked);
                    print_s [%sexp (moderated : bool), (linked_while_locked : bool)]))))));
  [%expect
    {|
    (false true)
    (true true)
    |}]
;;
