open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module Embedded = Agent_server.Embedded

let stopped = function
  | P.Session.Stopped -> true
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let%expect_test "local host reopens explicit root without HOME or replacement creation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Local selection fixture.</developer>";
        let durable =
          Agent_server.Local_storage.Root.create
            ~name:"selected"
            ~path:(Filename.concat root "store")
            ()
          |> protocol_ok
        in
        let options : Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Durable durable
          ; start_immediately = false
          ; permission_profile = Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        let calls = ref 0 in
        let daemon_options =
          { Agent_server.Daemon.default_options with
            inference_policy =
              inference_policy
                ~default_model:"fixture-model"
                ~post_stream:(fun ~sw:_ ~inputs:_ ->
                  Int.incr calls;
                  failwith "unexpected provider")
          }
        in
        let id =
          Eio.Switch.run (fun sw ->
            let created =
              Embedded.start ~sw ~env ~daemon_options options |> protocol_ok
            in
            let id = Embedded.session_id created in
            Embedded.close created;
            id)
        in
        let options = { options with home = None } in
        let result =
          Eio.Switch.run (fun sw ->
            Embedded.with_local_host ~sw ~env ~daemon_options options ~f:(fun host ->
              let open Result.Let_syntax in
              let connection = Embedded.host_connection host in
              let%bind before = C.Admin.list_sessions connection in
              let%bind snapshot = C.Admin.get_session connection id in
              let fields = P.Public.Snapshot.fields snapshot in
              let%bind lifecycle =
                Result.of_option
                  fields.lifecycle
                  ~error:(P.Error.invalid_request "missing lifecycle")
              in
              let%bind selected =
                Embedded.attach_retained
                  host
                  ~mode:options.attachment_mode
                  ~expected:(P.Session_lifecycle.Observation.expected lifecycle)
              in
              let%bind after = C.Admin.list_sessions connection in
              let%map snapshot = C.Admin.get_session connection id in
              let selected_state = P.Public.Snapshot.fields snapshot in
              ( List.length before
              , List.length after
              , P.Id.Session.equal (Embedded.session_id selected) id
              , stopped fields.session.observed_state
              , stopped selected_state.session.observed_state
              , Option.is_some fields.session.prompt_revision
                && Option.equal
                     P.Id.Prompt_revision.equal
                     fields.session.prompt_revision
                     selected_state.session.prompt_revision
              , !calls )))
          |> protocol_ok
        in
        print_s [%sexp (result : int * int * bool * bool * bool * bool * int)]));
  [%expect {| (1 1 true true true true 0) |}]
;;

let%expect_test "default embedded storage keeps durable records after close" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          "<developer>Default durable fixture.</developer>";
        let options : Embedded.options =
          { prompt_file = prompt
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Default
          ; start_immediately = false
          ; permission_profile = Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 32
          }
        in
        let calls = ref 0 in
        let daemon_options =
          { Agent_server.Daemon.default_options with
            inference_policy =
              inference_policy
                ~default_model:"fixture-model"
                ~post_stream:(fun ~sw:_ ~inputs:_ ->
                  Int.incr calls;
                  failwith "unexpected provider")
          }
        in
        let id =
          Eio.Switch.run (fun sw ->
            let created =
              Embedded.start ~sw ~env ~daemon_options options |> protocol_ok
            in
            let id = Embedded.session_id created in
            Embedded.close created;
            id)
        in
        let default_root = Filename.concat root ".ochat/agent-store" in
        let retained =
          Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / default_root)
        in
        let same, durable, count =
          Eio.Switch.run (fun sw ->
            Embedded.with_local_host ~sw ~env ~daemon_options options ~f:(fun host ->
              let open Result.Let_syntax in
              let connection = Embedded.host_connection host in
              let%bind sessions = C.Admin.list_sessions connection in
              let%map snapshot = C.Admin.get_session connection id in
              let fields = P.Public.Snapshot.fields snapshot in
              ( P.Id.Session.equal fields.session.id id
              , P.Session.equal_persistence fields.session.spec.persistence Durable
              , List.length sessions ))
            |> protocol_ok)
        in
        print_s
          [%sexp
            ((retained, same, durable, count, !calls) : bool * bool * bool * int * int)]));
  [%expect {| (true true true 1 0) |}]
;;
