open Core
open Agent_server_test_support
module P = Agent_protocol
module C = Chat_response.Tool_capability
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : R.entry) = A.state entry.actor |> protocol_ok

let create_child env root daemon parent =
  let definition =
    Agent_server.Runtime_owner.with_background_runtime parent.R.runtime (fun runtime ->
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
              , {|<developer>Use only the inherited echo.</developer><tool type="inherited" name="echo"/>|}
              )
            ]
          ()
        |> Result.ok_or_failwith
      in
      Agent_session.Generated_definition.prepare
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
    ~parent_session_id:(state parent).identity.session_id
    ~idempotency_key:(P.Idempotency_key.of_string "mcp-child" |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let%expect_test
    "persisted MCP descendants retain one connection, caller approvals and schema pins"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "mcp");
        let catalog mode =
          Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(path / "mcp/catalog") mode
        in
        catalog "original";
        let peer = Filename.concat (Core_unix.getcwd ()) "mcp_delegation_peer.exe" in
        let prompt =
          sprintf
            {|<developer>Parent MCP fixture.</developer><tool mcp_server="stdio:%s %s/mcp"/>|}
            peer
            root
        in
        Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "parent.chatmd") prompt;
        let configuration =
          config
            ~profile:{ permission_profile with tool_default = Ask }
            root
            root
            (Filename.concat root "parent.chatmd")
        in
        let next_call = ref None in
        let provider_calls = ref 0 in
        let with_daemon f =
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
                  ; model_post_stream =
                      Some
                        (fun ~sw:_ ~inputs:_ ->
                          Int.incr provider_calls;
                          match !next_call with
                          | None -> Stdlib.Seq.empty
                          | Some (name, arguments) ->
                            next_call := None;
                            let open Openai.Responses.Response_stream in
                            [ Output_item_added
                                { item =
                                    Function_call
                                      { name
                                      ; arguments = ""
                                      ; call_id = sprintf "mcp-%d" !provider_calls
                                      ; _type = "function_call"
                                      ; id = Some "mcp-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments
                                ; item_id = "mcp-item"
                                ; output_index = 0
                                ; type_ = "response.function_call_arguments.done"
                                }
                            ]
                            |> Stdlib.List.to_seq)
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let attach sw client id =
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
        let call sw client entry name arguments =
          let before = state entry in
          let handle = attach sw client before.identity.session_id in
          next_call := Some (name, arguments);
          H.send_message
            handle
            { kind = Plain_text; text = "Run the requested tool"; attachments = [] }
          |> protocol_ok
          |> ignore;
          let rec idle () =
            let current = state entry in
            List.iter current.permissions ~f:(fun permission ->
              match permission.P.Permission.state with
              | Pending ->
                assert (
                  P.Id.Session.equal permission.session_id before.identity.session_id);
                H.respond_permission
                  handle
                  ~permission_id:permission.id
                  ~permission_generation:permission.generation
                  ~choice:Approve_once
                  ~reason:None
                |> protocol_ok
                |> ignore
              | _ -> ());
            match current.active_operation with
            | None -> current
            | Some _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              idle ()
          in
          let current = idle () in
          H.detach handle |> protocol_ok;
          List.filter current.invocations ~f:(fun invocation ->
            not
              (List.exists before.invocations ~f:(fun previous ->
                 P.Id.Invocation.equal previous.context.id invocation.context.id)))
        in
        let success entry = function
          | [ invocation ] ->
            assert (
              P.Id.Session.equal
                invocation.P.Invocation.context.session_id
                (state entry).identity.session_id);
            (match invocation.status with
             | Published (Complete (`String "peer-result")) -> ()
             | status ->
               raise_s [%sexp "MCP result failed", (status : P.Invocation.status)])
          | _ -> failwith "expected one caller-owned MCP invocation"
        in
        let lines file = In_channel.read_lines (Filename.concat root ("mcp/" ^ file)) in
        let connections () = List.length (lines "connections") in
        let parent_id, child_id, leaf_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              R.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let connected = connections () in
            assert (connected > 0);
            let child = create_child env root daemon parent_entry in
            let leaf = create_child env root daemon child in
            [%test_eq: int] connected (connections ());
            call sw client child "echo" "{}" |> success child;
            call sw client leaf "echo" "{}" |> success leaf;
            List.iter [ child; leaf ] ~f:(fun entry ->
              let current = state entry in
              let invocation = List.hd_exn current.invocations in
              match current.permissions with
              | [ { state = Approved
                  ; owner = Invocation id
                  ; resolution = Some { choice = Approve_once; _ }
                  ; _
                  }
                ] -> assert (P.Id.Invocation.equal id invocation.context.id)
              | permissions ->
                raise_s
                  [%sexp
                    "MCP caller approval missing", (permissions : P.Permission.t list)]);
            assert (List.is_empty (state parent_entry).permissions);
            assert (
              List.is_empty (call sw client leaf "change_catalog" {|{"mode":"schema"}|}));
            [%test_eq: string list] [ "echo"; "echo" ] (lines "calls");
            parent.id, (state child).identity.session_id, (state leaf).identity.session_id)
        in
        let initial_connections = connections () in
        with_daemon (fun sw daemon client ->
          let parent = R.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = R.load (Daemon.registry daemon) child_id |> protocol_ok in
          let leaf = R.load (Daemon.registry daemon) leaf_id |> protocol_ok in
          assert (connections () > initial_connections);
          let connected = connections () in
          call sw client leaf "echo" "{}" |> success leaf;
          [%test_eq: int] connected (connections ());
          call sw client parent "change_catalog" {|{"mode":"schema"}|} |> success parent;
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          (match call sw client leaf "echo" "{}" with
           | [ { status = Published (Fail error); _ } ] ->
             [%test_eq: string] "invocation.handler_failed" error.code;
             [%test_eq: string] "Tool execution failed." error.message
           | invocations ->
             raise_s
               [%sexp "changed remote schema ran", (invocations : P.Invocation.t list)]);
          [%test_eq: string list]
            [ "echo"; "echo"; "echo"; "change_catalog" ]
            (lines "calls");
          let handle = attach sw client child_id in
          H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
          H.detach handle |> protocol_ok;
          assert (P.Session.equal_desired_state (state child).lifecycle.desired Stopped);
          assert (P.Session.equal_desired_state (state leaf).lifecycle.desired Stopped));
        with_daemon (fun sw daemon client ->
          let before = !provider_calls in
          let handle = attach sw client child_id in
          (match H.start handle ~queue_if_limited:false with
           | Error error ->
             assert (String.is_substring error.message ~substring:"capabilit")
           | Ok _ -> failwith "changed remote schema restored old delegation");
          [%test_eq: int] before !provider_calls;
          [%test_eq: string list]
            [ "echo"; "echo"; "echo"; "change_catalog" ]
            (lines "calls");
          H.detach handle |> protocol_ok);
        catalog "original";
        with_daemon (fun sw daemon client ->
          let handle = attach sw client child_id in
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          H.detach handle |> protocol_ok;
          let child = R.load (Daemon.registry daemon) child_id |> protocol_ok in
          call sw client child "echo" "{}" |> success child);
        [%test_eq: string list]
          [ "echo"; "echo"; "echo"; "change_catalog"; "echo" ]
          (lines "calls");
        print_endline
          "MCP connection shared; child approvals isolated; descendants restore; live \
           and restored schema substitution blocked; original catalog recovers"));
  [%expect
    {| MCP connection shared; child approvals isolated; descendants restore; live and restored schema substitution blocked; original catalog recovers |}]
;;
