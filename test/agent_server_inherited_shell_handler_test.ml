open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module C = Chat_response.Tool_capability

let run (lifetime : Agent_server.Session_factory.generated_lifetime) =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save file contents =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / file)
            contents
        in
        save "schema.json" "true";
        save
          "parent.chatmd"
          {|<developer>Parent.</developer>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="false" arbitrary_code="false" privilege_change="false"><read path="${workspace}"/></capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/><approvals provider="ui" unavailable="deny" scopes="once,exact_session"/><audit format="none"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct" command="/bin/echo delegated-handler" result="stdout"/>
<script id="handler" language="chatml" kind="tool">
let run ctx input = let* result = Tool.call("fixed_echo", `Object([])) in match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.fail(code)
</script>
<tool name="echo_report" type="chatml" script="handler" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="fixed_echo"/></tool>|};
        Eio.Switch.run (fun sw ->
          let configuration =
            config
              ~profile:{ permission_profile with tool_default = Ask }
              root
              root
              (Filename.concat root "parent.chatmd")
          in
          let emitted = ref false in
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
                ; independent_lifetime_policy = Some "handler-fixture-v1"
                ; model_post_stream =
                    Some
                      (fun ~sw:_ ~inputs:_ ->
                        match !emitted with
                        | true -> Stdlib.Seq.empty
                        | false ->
                          emitted := true;
                          let open Openai.Responses.Response_stream in
                          [ Output_item_added
                              { item =
                                  Function_call
                                    { name = "echo_report"
                                    ; arguments = ""
                                    ; call_id = "echo-report"
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
          Exn.protect
            ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    let parent, parent_attachment =
                      create_session ~start_immediately:true client
                    in
                    let parent_entry =
                      R.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    let definition =
                      Agent_server.Runtime_owner.with_background_runtime
                        parent_entry.runtime
                        (fun runtime ->
                           let native =
                             Option.value_exn
                               runtime.Agent_session.Runtime_builder.native_runtime
                           in
                           let caps =
                             Lazy.force native.capabilities
                             |> Result.map_error ~f:(fun e -> e.C.message)
                             |> Result.ok_or_failwith
                           in
                           let bundle =
                             Chatmd_source_bundle.create
                               ~root_file:"child.chatmd"
                               ~sources:
                                 [ ( "child.chatmd"
                                   , {|<developer>Child.</developer><tool type="inherited" name="echo_report"/>|}
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
                             ~current_capabilities:(fun () -> caps)
                             ~references:(C.references caps)
                             bundle
                           |> Result.map_error ~f:(fun errors ->
                             P.Error.invalid_request
                               (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                                |> String.concat ~sep:"\n")))
                      |> protocol_ok
                    in
                    let child =
                      Agent_server.Session_factory.create_generated_session
                        ~start_immediately:true
                        ~lifetime
                        (Daemon.factory daemon)
                        ~parent_session_id:parent.id
                        ~idempotency_key:
                          (P.Idempotency_key.of_string "shell-handler-child"
                           |> protocol_ok)
                        ~display_name:None
                        definition
                      |> protocol_ok
                    in
                    (match lifetime with
                     | Owned -> ()
                     | Independent ->
                       A.stop
                         parent_entry.actor
                         ~attachment_id:parent_attachment.id
                         ~mode:Cancel
                       |> protocol_ok
                       |> ignore;
                       Agent_server.Runtime_owner.unload_and_wait parent_entry.runtime
                       |> protocol_ok;
                       assert (
                         not (Agent_server.Runtime_owner.is_loaded parent_entry.runtime)));
                    let state () = A.state child.actor |> protocol_ok in
                    let child_id = (state ()).identity.session_id in
                    let handle =
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
                    H.send_message
                      handle
                      { kind = Plain_text; text = "Run the handler"; attachments = [] }
                    |> protocol_ok
                    |> ignore;
                    let rec idle () =
                      let current = state () in
                      List.iter current.permissions ~f:(fun permission ->
                        match P.Permission.equal_state permission.state Pending with
                        | false -> ()
                        | true ->
                          assert (P.Id.Session.equal permission.session_id child_id);
                          H.respond_permission
                            handle
                            ~permission_id:permission.id
                            ~permission_generation:permission.generation
                            ~choice:Approve_once
                            ~reason:None
                          |> protocol_ok
                          |> ignore);
                      match current.active_operation with
                      | None -> current
                      | Some _ ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        idle ()
                    in
                    let completed = idle () in
                    let root_call =
                      List.find_exn completed.invocations ~f:(fun i ->
                        String.equal i.context.tool_name "echo_report")
                    in
                    (match root_call.status with
                     | Published (Complete (`String text)) ->
                       [%test_eq: string] "delegated-handler\n" text
                     | status ->
                       raise_s
                         [%sexp "private shell failed", (status : P.Invocation.status)]);
                    let shell_call =
                      List.find_exn completed.invocations ~f:(fun i ->
                        String.equal i.context.tool_name "fixed_echo")
                    in
                    let owned =
                      List.filter completed.permissions ~f:(fun permission ->
                        match permission.owner with
                        | Invocation id -> P.Id.Invocation.equal id shell_call.context.id
                        | _ -> false)
                    in
                    (match List.length owned with
                     | 1 -> ()
                     | _ ->
                       raise_s
                         [%sexp
                           "unexpected shell approvals"
                         , (List.map completed.permissions ~f:(fun p ->
                              p.tool_name, p.owner)
                            : (string * P.Permission.owner) list)]);
                    assert (
                      List.is_empty
                        (A.state parent_entry.actor |> protocol_ok).permissions);
                    H.detach handle |> protocol_ok))))))
;;

let%expect_test
    "private shell dependency keeps one child shell approval in both lifetimes"
  =
  List.iter [ Agent_server.Session_factory.Owned; Independent ] ~f:(fun lifetime ->
    print_s [%sexp (lifetime : Agent_server.Session_factory.generated_lifetime)];
    run lifetime);
  [%expect
    {|
    Owned
    Independent
  |}]
;;
