open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : R.entry) = A.state entry.actor |> protocol_ok

let create_child env root daemon parent ~model ~reasoning ~tokens ~marker =
  let module C = Chat_response.Tool_capability in
  let definition =
    Agent_server.Runtime_owner.with_background_runtime parent.R.runtime (fun runtime ->
      let native =
        Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
      in
      let capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun e -> e.C.message)
        |> Result.ok_or_failwith
      in
      let bundle =
        Chatmd_source_bundle.create
          ~root_file:"child.chatmd"
          ~sources:
            [ ( "child.chatmd"
              , sprintf
                  {|<config model="%s" reasoning_effort="%s" max_tokens="%d"/><developer>%s</developer><tool type="inherited" name="read_file"/>|}
                  model
                  reasoning
                  tokens
                  marker )
            ]
          ()
        |> Result.ok_or_failwith
      in
      Agent_session.Generated_definition.prepare
        ~env
        ~dir:(F.path env root)
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
    ~idempotency_key:(F.key "provider-child")
    ~display_name:None
    definition
  |> protocol_ok
;;

let run_child env root =
  (* API_URL is read when Openai.Responses initializes. This executable is launched
     with isolated loopback configuration, never an ambient provider endpoint/key. *)
  assert (
    Option.exists (Sys.getenv "API_URL") ~f:(String.is_prefix ~prefix:"http://127.0.0.1:"));
  [%test_eq: string option] (Some "generated-local-fixture") (Sys.getenv "OPENAI_API_KEY");
  let prompt_file = Filename.concat root "parent.chatmd" in
  F.write
    env
    prompt_file
    {|<config model="gpt-4.1" reasoning_effort="low" max_tokens="257"/>
<developer>PARENT-PRIVATE-INSTRUCTIONS</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool><tool name="append_to_file"/>|};
  let configuration = config root root prompt_file in
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
          ~options:{ Daemon.default_options with qualify_chatml_extensions = true }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () -> Agent_client.Connection.close client)
              ~f:(fun () ->
                initialize client;
                f sw daemon client))))
  in
  let send sw client entry =
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
      { kind = Plain_text; text = "Return a response"; attachments = [] }
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
    assert (Option.is_none current.failure);
    assert (not current.halted);
    assert (List.is_empty current.invocations);
    H.detach handle |> protocol_ok
  in
  let parent_id, child_id, grandchild_id =
    with_daemon (fun sw daemon client ->
      let parent, _ = create_session ~start_immediately:true client in
      let root_entry = R.find (Daemon.registry daemon) parent.id |> Option.value_exn in
      let child =
        create_child
          env
          root
          daemon
          root_entry
          ~model:"o3"
          ~reasoning:"high"
          ~tokens:513
          ~marker:"MIDDLE-PRIVATE-INSTRUCTIONS"
      in
      let grandchild =
        create_child
          env
          root
          daemon
          child
          ~model:"o4-mini"
          ~reasoning:"medium"
          ~tokens:769
          ~marker:"GRANDCHILD-PRIVATE-INSTRUCTIONS"
      in
      List.iter [ root_entry; child; grandchild ] ~f:(send sw client);
      parent.id, (state child).identity.session_id, (state grandchild).identity.session_id)
  in
  (* Editing the live parent file must not change any session's captured definition. *)
  F.write
    env
    prompt_file
    {|<config model="unrelated-live-edit" reasoning_effort="none"/><developer>UNPINNED-EDIT</developer>|};
  with_daemon (fun sw daemon client ->
    List.iter [ child_id; parent_id; grandchild_id ] ~f:(fun id ->
      R.load (Daemon.registry daemon) id |> protocol_ok |> send sw client));
  Eio.Flow.copy_string "generated-provider-complete\n" (Eio.Stdenv.stdout env)
;;

let test env environment =
  Eio.Switch.run (fun sw ->
    let port =
      Eio.Switch.run (fun reserve_sw ->
        let reservation = Support.Port_reservation.create ~sw:reserve_sw ~env in
        let port = Support.Port_reservation.port reservation in
        Support.Port_reservation.release reservation;
        port)
    in
    let requests = ref [] in
    let handler ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
      match Piaf.Request.meth request, Piaf.Request.target request with
      | `POST, "/v1/responses" ->
        let json =
          Piaf.Body.to_string (Piaf.Request.body request)
          |> Result.map_error ~f:Piaf.Error.to_string
          |> Result.ok_or_failwith
          |> Jsonaf.of_string
        in
        requests := !requests @ [ json ];
        let headers = Piaf.Headers.of_list [ "content-type", "text/event-stream" ] in
        let body =
          "data: "
          ^ Jsonaf.to_string Support.Fake_openai.completed_event
          ^ "\n\ndata: [DONE]\n\n"
        in
        Piaf.Response.of_string ~headers ~body `OK
      | _ -> Piaf.Server.Handler.not_found ()
    in
    let ready, notify_ready = Eio.Promise.create () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run (fun server_sw ->
        let config =
          Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
        in
        let server = Piaf.Server.create ~config handler in
        ignore
          (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t);
        Eio.Promise.resolve notify_ready ());
      `Stop_daemon);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Promise.await ready);
    let root =
      Filename.concat
        (Support.Temporary_environment.roots environment).temporary
        "generated-provider"
    in
    Eio.Path.mkdir ~perm:0o700 (F.path env root);
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"generated-provider"
        ~arguments:[ "generated-provider"; root ]
        ~environment_overrides:
          [ "API_URL", sprintf "http://127.0.0.1:%d" port
          ; "OPENAI_API_KEY", "generated-local-fixture"
          ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        let result =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
            Support.Process_manager.await child)
        in
        (match result.exit with
         | Exited 0 ->
           assert (
             String.is_substring
               result.stdout.contents
               ~substring:"generated-provider-complete")
         | _ ->
           raise_s
             [%sexp
               "generated provider child failed"
             , (result : Support.Process_manager.result)]);
        let parent =
          ( "gpt-4.1"
          , "low"
          , 257
          , "PARENT-PRIVATE-INSTRUCTIONS"
          , [ "append_to_file"; "read_file" ] )
        in
        let child = "o3", "high", 513, "MIDDLE-PRIVATE-INSTRUCTIONS", [ "read_file" ] in
        let grandchild =
          "o4-mini", "medium", 769, "GRANDCHILD-PRIVATE-INSTRUCTIONS", [ "read_file" ]
        in
        List.iter2_exn
          [ parent; child; grandchild; child; parent; grandchild ]
          !requests
          ~f:(fun (model, effort, tokens, marker, tools) json ->
            [%test_eq: string] model (Jsonaf.member_exn "model" json |> Jsonaf.string_exn);
            [%test_eq: string]
              effort
              (Jsonaf.member_exn "reasoning" json
               |> Jsonaf.member_exn "effort"
               |> Jsonaf.string_exn);
            [%test_eq: int]
              tokens
              (Jsonaf.member_exn "max_output_tokens" json |> Jsonaf.int_exn);
            [%test_eq: bool] true (Jsonaf.member_exn "stream" json |> Jsonaf.bool_exn);
            let names =
              Jsonaf.member_exn "tools" json
              |> Jsonaf.list_exn
              |> List.map ~f:(fun tool ->
                Jsonaf.member_exn "name" tool |> Jsonaf.string_exn)
              |> List.sort ~compare:String.compare
            in
            [%test_eq: string list] tools names;
            let inputs = Jsonaf.member_exn "input" json |> Jsonaf.to_string in
            assert (String.is_substring inputs ~substring:marker);
            assert (not (String.is_substring inputs ~substring:"UNPINNED-EDIT"));
            List.iter
              [ "PARENT-PRIVATE-INSTRUCTIONS"
              ; "MIDDLE-PRIVATE-INSTRUCTIONS"
              ; "GRANDCHILD-PRIVATE-INSTRUCTIONS"
              ]
              ~f:(fun forbidden ->
                match String.equal marker forbidden with
                | true -> ()
                | false -> assert (not (String.is_substring inputs ~substring:forbidden))))))
;;
