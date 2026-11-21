open Core
open Agent_server_test_support
module P = Agent_protocol
module C = Chat_response.Tool_capability
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : R.entry) = A.state entry.actor |> protocol_ok

let create_child
      ?(lifetime = Agent_server.Session_factory.Owned)
      ?(control = false)
      env
      root
      daemon
      parent
  =
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
              , {|<developer>Use inherited tools.</developer><tool type="inherited" name="echo"/>|}
                ^
                if control then {|<tool type="inherited" name="change_catalog"/>|} else ""
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
    ~lifetime
    (Daemon.factory daemon)
    ~parent_session_id:(state parent).identity.session_id
    ~idempotency_key:(P.Idempotency_key.of_string "mcp-child" |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let run (lifetime : Agent_server.Session_factory.generated_lifetime) =
  let independent =
    Agent_server.Session_factory.equal_generated_lifetime lifetime Independent
  in
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
        let await stage f =
          with_stage_timeout ~clock:(Eio.Stdenv.clock env) ~seconds:15. ~stage f
        in
        let next_call = ref None in
        let provider_calls = ref 0 in
        let with_daemon
              ?(shutdown = fun daemon -> Daemon.shutdown daemon |> protocol_ok)
              f
          =
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
                  ; independent_lifetime_policy = Some "mcp-fixture-v1"
                  ; inference_policy =
                      Agent_server_test_support.inference_policy
                        ~default_model:"fixture-model"
                        ~post_stream:(fun ~sw:_ ~inputs:_ ->
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
                            ; Output_item_done
                                { item =
                                    Function_call
                                      { name
                                      ; arguments
                                      ; call_id = sprintf "mcp-%d" !provider_calls
                                      ; _type = "function_call"
                                      ; id = Some "mcp-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.done"
                                }
                            ]
                            |> Stdlib.List.to_seq)
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> shutdown daemon)
              ~f:(fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    await "client initialization" (fun () -> initialize client);
                    f sw daemon client)))
        in
        let attach sw client id =
          await "MCP caller attachment" (fun () ->
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok)
        in
        let call sw client entry name arguments =
          let before = state entry in
          let handle = attach sw client before.identity.session_id in
          next_call := Some (name, arguments);
          await "MCP tool submission" (fun () ->
            H.send_message
              handle
              { kind = Plain_text; text = "Run the requested tool"; attachments = [] }
            |> protocol_ok
            |> ignore);
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
          let current = await ("MCP tool settlement for " ^ name) idle in
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
            let parent, parent_attachment =
              await "MCP parent creation" (fun () ->
                create_session ~start_immediately:true client)
            in
            let parent_entry =
              R.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let connected = connections () in
            assert (connected > 0);
            let child =
              await "MCP child creation" (fun () ->
                create_child ~lifetime ~control:independent env root daemon parent_entry)
            in
            (match lifetime with
             | Owned -> ()
             | Independent ->
               await "MCP parent stop" (fun () ->
                 A.stop
                   parent_entry.actor
                   ~attachment_id:parent_attachment.id
                   ~mode:Cancel
                 |> protocol_ok
                 |> ignore;
                 Agent_server.Runtime_owner.unload_and_wait parent_entry.runtime
                 |> protocol_ok);
               assert (not (Agent_server.Runtime_owner.is_loaded parent_entry.runtime)));
            let retained_connections = connections () in
            let leaf =
              await "MCP leaf creation" (fun () -> create_child env root daemon child)
            in
            [%test_eq: int] retained_connections (connections ());
            (* Both lifetimes retain the canonical parent's connection; an
               Independent child no longer reconstructs its own MCP pool. *)
            [%test_eq: int] connected retained_connections;
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
        let load stage daemon id =
          await stage (fun () -> R.load (Daemon.registry daemon) id |> protocol_ok)
        in
        let initial_connections = connections () in
        with_daemon (fun sw daemon client ->
          let parent = load "MCP parent recovery" daemon parent_id in
          let child = load "MCP child recovery" daemon child_id in
          let leaf = load "MCP leaf recovery" daemon leaf_id in
          (match lifetime with
           | Owned -> ()
           | Independent ->
             assert (
               P.Session.equal_desired_state (state parent).lifecycle.desired Stopped);
             assert (not (Agent_server.Runtime_owner.is_loaded parent.runtime)));
          assert (connections () > initial_connections);
          let connected = connections () in
          call sw client leaf "echo" "{}" |> success leaf;
          [%test_eq: int] connected (connections ());
          let controller =
            match lifetime with
            | Owned -> parent
            | Independent -> child
          in
          call sw client controller "change_catalog" {|{"mode":"schema"}|}
          |> success controller;
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
          await "MCP descendant stop" (fun () ->
            H.stop handle ~mode:Cancel |> protocol_ok |> ignore);
          H.detach handle |> protocol_ok;
          assert (P.Session.equal_desired_state (state child).lifecycle.desired Stopped);
          assert (P.Session.equal_desired_state (state leaf).lifecycle.desired Stopped));
        let denied_start = ref None in
        let expected_start_shutdown daemon =
          match !denied_start with
          | None -> Daemon.shutdown daemon |> protocol_ok
          | Some (expected : P.Error.t) ->
            let module Report = Agent_server.Session_disposal_report in
            let report = Daemon.shutdown_reported daemon in
            assert (Report.disposed report);
            (match Report.failures report with
             | [ Protocol actual ] ->
               assert (P.Error.equal_code expected.code actual.code);
               [%test_eq: string] expected.message actual.message;
               assert (Bool.equal expected.retryable actual.retryable);
               assert (Jsonaf.exactly_equal expected.data actual.data)
             | failures ->
               let failures =
                 List.map failures ~f:(function
                   | Protocol error -> [%sexp "Protocol", (error : P.Error.t)]
                   | Raised (exn, _) -> [%sexp "Raised", (Exn.to_string exn : string)])
               in
               raise_s
                 [%sexp
                   "denied start shutdown changed its actual diagnostic"
                 , (failures : Sexp.t list)])
        in
        let shutdown =
          match lifetime with
          | Owned -> None
          | Independent -> Some expected_start_shutdown
        in
        with_daemon ?shutdown (fun sw daemon client ->
          let before = !provider_calls in
          let handle = attach sw client child_id in
          (match
             await "MCP changed-schema start rejection" (fun () ->
               H.start handle ~queue_if_limited:false)
           with
           | Error error ->
             assert (String.is_substring error.message ~substring:"capabilit");
             (match lifetime with
              | Owned -> ()
              | Independent ->
                assert (P.Error.equal_code error.code Prompt_unavailable);
                assert (not error.retryable);
                assert (
                  String.is_substring
                    error.message
                    ~substring:"delegation.capability_changed");
                denied_start := Some error)
           | Ok _ -> failwith "changed remote schema restored old delegation");
          [%test_eq: int] before !provider_calls;
          [%test_eq: string list]
            [ "echo"; "echo"; "echo"; "change_catalog" ]
            (lines "calls");
          H.detach handle |> protocol_ok);
        catalog "original";
        with_daemon (fun sw daemon client ->
          let handle = attach sw client child_id in
          await "MCP restored-schema start" (fun () ->
            H.start handle ~queue_if_limited:false |> protocol_ok |> ignore);
          H.detach handle |> protocol_ok;
          let child = load "MCP child recovery" daemon child_id in
          call sw client child "echo" "{}" |> success child);
        [%test_eq: string list]
          [ "echo"; "echo"; "echo"; "change_catalog"; "echo" ]
          (lines "calls");
        with_daemon (fun sw daemon client ->
          let parent = load "MCP parent recovery" daemon parent_id in
          let child = load "MCP child recovery" daemon child_id in
          let leaf = load "MCP leaf recovery" daemon leaf_id in
          let handle = attach sw client leaf_id in
          await "MCP authority-gate start" (fun () ->
            H.start handle ~queue_if_limited:false |> protocol_ok |> ignore);
          H.detach handle |> protocol_ok;
          let controller =
            match lifetime with
            | Owned -> parent
            | Independent -> child
          in
          Eio.Switch.run (fun gate_switch ->
            let listener =
              Eio.Net.listen
                ~sw:gate_switch
                ~backlog:1
                (Eio.Stdenv.net env)
                (`Unix (Filename.concat root "mcp/catalog-gate.sock"))
            in
            call sw client controller "change_catalog" {|{"mode":"gate"}|}
            |> success controller;
            let calls_before = lines "calls" in
            let before = state leaf in
            let result =
              Eio.Fiber.fork_promise ~sw:gate_switch (fun () ->
                call sw client leaf "echo" "{}")
            in
            (* A real list request must enter before we change authority. A
               rejected invocation cannot strand an accept or a peer waiter. *)
            let flow =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                Eio.Fiber.first
                  (fun () ->
                     let flow, _ = Eio.Net.accept ~sw:gate_switch listener in
                     let input = Eio.Buf_read.of_flow flow ~max_size:32 in
                     match Eio.Buf_read.line input with
                     | "entered" -> flow
                     | line ->
                       Eio.Flow.close flow;
                       raise_s [%sexp "invalid catalog gate entry", (line : string)]
                     | exception exn ->
                       let trace = Stdlib.Printexc.get_raw_backtrace () in
                       Eio.Flow.close flow;
                       Exn.raise_with_original_backtrace exn trace)
                  (fun () ->
                     Eio.Promise.await_exn result |> ignore;
                     failwith "MCP invocation finished before its catalog gate"))
            in
            Exn.protect
            (* EOF releases the actual peer even if an assertion below fails,
                 before the enclosing daemon begins shutdown. *)
              ~finally:(fun () -> Eio.Flow.close flow)
              ~f:(fun () ->
                let waiting = state leaf in
                let invocation : P.Invocation.t =
                  match
                    List.filter waiting.invocations ~f:(fun invocation ->
                      not
                        (List.exists before.invocations ~f:(fun previous ->
                           P.Id.Invocation.equal previous.context.id invocation.context.id)))
                  with
                  | [ invocation ] -> invocation
                  | invocations ->
                    raise_s
                      [%sexp
                        "catalog gate lacks one admitted invocation"
                      , (invocations : P.Invocation.t list)]
                in
                assert (P.Invocation.equal_status Dispatching invocation.status);
                assert (P.Id.Session.equal leaf_id invocation.context.session_id);
                assert (
                  List.exists waiting.permissions ~f:(fun permission ->
                    P.Permission.equal_state permission.state Approved
                    && P.Permission.equal_owner
                         permission.owner
                         (Invocation invocation.context.id)));
                let permissions = List.map waiting.permissions ~f:P.Permission.to_json in
                let module D = Agent_store.Delegation_store in
                let store = Agent_store.Session_store.delegations (Daemon.store daemon) in
                let reference = Option.value_exn waiting.spec.delegation in
                assert (P.Id.Session.equal leaf_id reference.child_session_id);
                let record =
                  D.resolve store reference
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                  |> protocol_ok
                in
                assert (D.Reference.equal reference (D.reference record));
                D.revoke store record Authority_changed
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
                |> ignore;
                Eio.Flow.copy_string "release\n" flow;
                let invocations = Eio.Promise.await_exn result in
                [%test_eq: string list] calls_before (lines "calls");
                assert (
                  List.equal
                    Jsonaf.exactly_equal
                    permissions
                    (List.map (state leaf).permissions ~f:P.Permission.to_json));
                match invocations with
                | [ { status = Published (Fail error); _ } ] ->
                  [%test_eq: string] "invocation.handler_failed" error.code
                | invocations ->
                  raise_s
                    [%sexp
                      "revoked MCP consumer reached its effect"
                    , (invocations : P.Invocation.t list)])));
        print_endline
          "MCP connection shared; child approvals isolated; descendants restore; live \
           and restored schema substitution blocked; original catalog recovers"))
;;

let%expect_test
    "MCP descendants retain connection, approval and schema authority in both lifetimes"
  =
  List.iter [ Agent_server.Session_factory.Owned; Independent ] ~f:(fun lifetime ->
    print_s [%sexp (lifetime : Agent_server.Session_factory.generated_lifetime)];
    run lifetime);
  [%expect
    {|
    Owned
    MCP connection shared; child approvals isolated; descendants restore; live and restored schema substitution blocked; original catalog recovers
    Independent
    MCP connection shared; child approvals isolated; descendants restore; live and restored schema substitution blocked; original catalog recovers
    |}]
;;
