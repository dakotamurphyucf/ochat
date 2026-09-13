open Core
open Agent_server_test_support
module P = Agent_protocol
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : R.entry) = A.state entry.actor |> protocol_ok

let inline_model =
  `Object
    [ "version", `Number "1"
    ; "target", `String "moderator"
    ; "tools", `Array []
    ; ( "source"
      , `String
          "let initial_state = 0\n\
           let on_event ctx state event = let* result = Model.call(\"worker\", `Null) in \
           Task.pure(state)" )
    ]
;;

let bundle tools source =
  `Object
    [ "version", `Number "1"
    ; "target", `String "generated_chatmd"
    ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
    ; "root_file", `String "candidate.chatmd"
    ; ( "sources"
      , `Array [ `Object [ "path", `String "candidate.chatmd"; "text", `String source ] ]
      )
    ]
;;

let%expect_test "inherited validation uses the child's surface and tools after restart" =
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
          {|<developer>Parent.</developer><tool name="ochat_validate"/><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
        let configuration = config root root prompt_file in
        let host =
          V.create_host
            ~runtime_identity:"generated-validation-fixture"
            ~targets:[ Moderator; Generated_chatmd ]
            ~moderator_surface:Ordinary
            ~compilation:Chatml_compilation.default_limits
          |> Result.ok_or_failwith
        in
        let candidate = ref inline_model in
        let requests = ref 0 in
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
                  ; authoring_validation_host = Some host
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
                                      { name = "ochat_validate"
                                      ; arguments = ""
                                      ; call_id = sprintf "validation-%d" !requests
                                      ; _type = "function_call"
                                      ; id = Some "validation-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments = Jsonaf.to_string !candidate
                                ; item_id = "validation-item"
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
        let validate sw client entry request expected =
          candidate := request;
          let call_id = sprintf "validation-%d" (!requests + 1) in
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:(state entry).identity.session_id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          H.send_message
            handle
            { kind = Plain_text; text = "Validate candidate"; attachments = [] }
          |> protocol_ok
          |> ignore;
          let rec idle () =
            match (state entry).active_operation with
            | None -> state entry
            | Some _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              idle ()
          in
          let current = idle () in
          let invocation =
            List.find_exn current.invocations ~f:(fun invocation ->
              Option.equal String.equal invocation.context.provider_call_id (Some call_id))
          in
          let report =
            match invocation.status with
            | Published (Complete (`String text)) -> Jsonaf.of_string text
            | status ->
              raise_s
                [%sexp "validator failed to publish", (status : P.Invocation.status)]
          in
          [%test_eq: bool] expected (Jsonaf.member_exn "valid" report |> Jsonaf.bool_exn);
          assert (
            List.for_all current.invocations ~f:(fun invocation ->
              String.equal invocation.context.tool_name "ochat_validate"));
          H.detach handle |> protocol_ok;
          report
        in
        let parent_id, child_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              R.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            ignore (validate sw client parent_entry inline_model true);
            let definition =
              Agent_server.Runtime_owner.with_background_runtime
                parent_entry.runtime
                (fun runtime ->
                   let native =
                     Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
                   in
                   let capabilities =
                     Lazy.force native.capabilities
                     |> Result.map_error ~f:(fun e -> e.C.message)
                     |> Result.ok_or_failwith
                   in
                   let source =
                     Chatmd_source_bundle.create
                       ~root_file:"child.chatmd"
                       ~sources:
                         [ ( "child.chatmd"
                           , {|<developer>Child.</developer><tool type="inherited" name="ochat_validate"/>|}
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
                     source
                   |> Result.map_error ~f:(fun ds ->
                     P.Error.invalid_request
                       (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string
                        |> String.concat ~sep:"\n")))
              |> protocol_ok
            in
            let child =
              Agent_server.Session_factory.create_generated_session
                ~start_immediately:true
                (Daemon.factory daemon)
                ~parent_session_id:parent.id
                ~idempotency_key:
                  (P.Idempotency_key.of_string "validator-child" |> protocol_ok)
                ~display_name:None
                definition
              |> protocol_ok
            in
            let child_id = (state child).identity.session_id in
            let denied = validate sw client child inline_model false in
            let codes =
              Jsonaf.member_exn "diagnostics" denied
              |> Jsonaf.list_exn
              |> List.map ~f:(fun d ->
                Jsonaf.member_exn "diagnostic" d
                |> Jsonaf.member_exn "code"
                |> Jsonaf.string_exn)
            in
            assert (List.mem codes "chatml.type_error" ~equal:String.equal);
            let reader =
              bundle [ "read_file" ] {|<tool type="inherited" name="read_file"/>|}
            in
            ignore (validate sw client parent_entry reader true);
            ignore (validate sw client child reader false);
            parent.id, child_id)
        in
        with_daemon (fun sw daemon client ->
          let parent = R.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = R.load (Daemon.registry daemon) child_id |> protocol_ok in
          ignore (validate sw client child inline_model false);
          let good =
            bundle
              []
              {|<developer>Generated candidate.</developer>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("VALIDATION-MUST-NOT-EXECUTE")
let on_event ctx state event = Task.pure(state)
</script>|}
          in
          let report = validate sw client child good true in
          [%test_eq: string]
            "generated_bundle"
            (Jsonaf.member_exn "scope" report |> Jsonaf.string_exn);
          assert (Option.is_none (state child).moderator);
          ignore (validate sw client parent inline_model true));
        print_endline
          "parent surface stays ordinary; child validation narrows surface and tools; \
           restart preserves context; generated initializers never run"));
  [%expect
    {| parent surface stays ordinary; child validation narrows surface and tools; restart preserves context; generated initializers never run |}]
;;
