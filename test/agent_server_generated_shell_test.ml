open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let create_child
      ?(lifetime = Agent_server.Session_factory.Owned)
      env
      root
      daemon
      (parent : Registry.entry)
  =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  let state = A.state parent.actor |> protocol_ok in
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
              , {|<developer>Child.</developer><tool type="inherited" name="fixed_echo"/>|}
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
    ~lifetime
    (Daemon.factory daemon)
    ~parent_session_id:state.identity.session_id
    ~idempotency_key:(P.Idempotency_key.of_string "shell-child" |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let run (lifetime : Agent_server.Session_factory.generated_lifetime) =
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
          {|<developer>Parent.</developer>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
  <audit format="none"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct" command="/bin/echo delegated-shell" result="stdout"/>|};
        let profile = { permission_profile with tool_default = Ask } in
        let configuration = config ~profile root root prompt_file in
        let requests = ref 0 in
        let start sw =
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
              ; independent_lifetime_policy = Some "shell-fixture-v1"
              ; model_post_stream =
                  Some
                    (fun ~sw:_ ~inputs ->
                      Int.incr requests;
                      match List.last inputs with
                      | Some (Openai.Responses.Item.Function_call_output _) ->
                        Stdlib.Seq.empty
                      | _ ->
                        let open Openai.Responses.Response_stream in
                        [ Output_item_added
                            { item =
                                Function_call
                                  { name = "fixed_echo"
                                  ; arguments = ""
                                  ; call_id = sprintf "echo-%d" !requests
                                  ; _type = "function_call"
                                  ; id = Some "echo-item"
                                  ; status = None
                                  }
                            ; output_index = 0
                            ; type_ = "response.output_item.added"
                            }
                        ; Function_call_arguments_done
                            { arguments = "{}"
                            ; item_id = "echo-item"
                            ; output_index = 0
                            ; type_ = "response.function_call_arguments.done"
                            }
                        ]
                        |> Stdlib.List.to_seq)
              }
            ()
          |> protocol_ok
        in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let daemon = start sw in
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
        let state (entry : Registry.entry) = A.state entry.actor |> protocol_ok in
        let rec idle entry =
          let current = state entry in
          match current.active_operation with
          | None -> current
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let send handle =
          H.send_message
            handle
            { kind = Plain_text; text = "Run echo."; attachments = [] }
          |> protocol_ok
          |> ignore
        in
        let check_output current =
          assert (not (List.is_empty current.Agent_session.Session_state.invocations));
          List.iter current.invocations ~f:(fun invocation ->
            match invocation.P.Invocation.status with
            | Published (Complete (`String text)) ->
              [%test_eq: string] "delegated-shell\n" text
            | status ->
              raise_s [%sexp "unexpected shell outcome", (status : P.Invocation.status)])
        in
        let parent_id, child_id, grandchild_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let parent_handle = attach sw client parent.id in
            let child = create_child ~lifetime env root daemon parent_entry in
            (match lifetime with
             | Owned -> ()
             | Independent ->
               H.stop parent_handle ~mode:Cancel |> protocol_ok |> ignore;
               assert (not (Owner.is_loaded parent_entry.runtime)));
            let child_id = (state child).identity.session_id in
            let child_handle = attach sw client child_id in
            send child_handle;
            let rec pending child =
              let current = state child in
              match
                List.find current.permissions ~f:(fun permission ->
                  P.Permission.equal_state permission.state Pending)
              with
              | Some permission -> permission
              | None ->
                (match current.active_operation with
                 | Some _ ->
                   Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                   pending child
                 | None -> failwith "child shell did not request its own approval")
            in
            let permission = pending child in
            assert (P.Id.Session.equal permission.session_id child_id);
            let current = state child in
            (match permission.owner with
             | Invocation id ->
               assert (
                 List.exists current.invocations ~f:(fun invocation ->
                   P.Id.Invocation.equal invocation.context.id id))
             | _ -> failwith "shell approval lost native invocation provenance");
            assert (List.is_empty (state parent_entry).permissions);
            assert (List.is_empty (state parent_entry).invocations);
            H.respond_permission
              child_handle
              ~permission_id:permission.id
              ~permission_generation:permission.generation
              ~choice:Approve_session
              ~reason:None
            |> protocol_ok
            |> ignore;
            let current = idle child in
            check_output current;
            [%test_eq: int] 1 (List.length current.shell.approval_grants);
            assert (List.is_empty (state parent_entry).shell.approval_grants);
            send child_handle;
            let current = idle child in
            [%test_eq: int] 1 (List.length current.permissions);
            [%test_eq: int] 4 !requests;
            let grandchild = create_child env root daemon child in
            let grandchild_id = (state grandchild).identity.session_id in
            let grandchild_handle = attach sw client grandchild_id in
            send grandchild_handle;
            let waiting = pending grandchild in
            assert (P.Id.Session.equal waiting.session_id grandchild_id);
            [%test_eq: int] 1 (List.length (state child).permissions);
            let module D = Agent_store.Delegation_store in
            let ledger = Agent_store.Session_store.delegations (Daemon.store daemon) in
            let reference = Option.value_exn (state grandchild).spec.delegation in
            let record =
              D.resolve ledger reference
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            D.revoke ledger record Authority_changed
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
            |> ignore;
            H.respond_permission
              grandchild_handle
              ~permission_id:waiting.id
              ~permission_generation:waiting.generation
              ~choice:Approve_session
              ~reason:None
            |> protocol_ok
            |> ignore;
            let rejected = idle grandchild in
            assert (List.is_empty rejected.shell.approval_grants);
            assert (not (List.is_empty rejected.invocations));
            List.iter rejected.invocations ~f:(fun invocation ->
              match invocation.P.Invocation.status with
              | Resolved (Fail error) | Published (Fail error) ->
                [%test_eq: string] "invocation.disclosure_rejected" error.code
              | status ->
                raise_s [%sexp "revoked shell invocation", (status : P.Invocation.status)]);
            [%test_eq: int] 5 !requests;
            H.detach grandchild_handle |> protocol_ok;
            H.detach child_handle |> protocol_ok;
            H.detach parent_handle |> protocol_ok;
            parent.id, child_id, grandchild_id)
        in
        with_daemon (fun sw daemon client ->
          let parent = Registry.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          (match lifetime with
           | Owned -> ()
           | Independent ->
             assert (
               P.Session.equal_desired_state (state parent).lifecycle.desired Stopped);
             assert (not (Owner.is_loaded parent.runtime)));
          let handle = attach sw client child_id in
          send handle;
          let current = idle child in
          check_output current;
          [%test_eq: int] 1 (List.length current.permissions);
          [%test_eq: int] 1 (List.length current.shell.approval_grants);
          assert (List.is_empty (state parent).permissions);
          assert (List.is_empty (state parent).shell.approval_grants);
          [%test_eq: int] 7 !requests;
          let grandchild =
            Registry.load (Daemon.registry daemon) grandchild_id |> protocol_ok
          in
          assert (
            P.Session.equal_desired_state (state grandchild).lifecycle.desired Stopped);
          H.detach handle |> protocol_ok);
        print_endline
          "child approval and grant; no parent permission state; inherited shell grant \
           reused after restart"))
;;

let%expect_test
    "inherited shell approvals and grants belong to the child in both lifetimes"
  =
  List.iter [ Agent_server.Session_factory.Owned; Independent ] ~f:(fun lifetime ->
    print_s [%sexp (lifetime : Agent_server.Session_factory.generated_lifetime)];
    run lifetime);
  [%expect
    {|
    Owned
    child approval and grant; no parent permission state; inherited shell grant reused after restart
    Independent
    child approval and grant; no parent permission state; inherited shell grant reused after restart
    |}]
;;
