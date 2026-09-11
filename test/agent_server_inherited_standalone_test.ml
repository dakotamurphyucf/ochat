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
              , {|<developer>Child.</developer><tool type="inherited" name="read_report"/><tool type="inherited" name="start_report"/>|}
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
    ~idempotency_key:(P.Idempotency_key.of_string "standalone-child" |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let%expect_test
    "inherited standalone handlers keep private dependencies and caller ownership"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root / "data");
        let save path text =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / path)
            text
        in
        save "data/value.txt" "delegated-report-sentinel";
        save "secret.txt" "private-parent-sentinel";
        save "schema.json" "true";
        save
          "parent.chatmd"
          {|<developer>Parent.</developer>
<tool name="read_file"><read id="data" path="${workspace}/data"/></tool>
<script id="reader" language="chatml" kind="tool">
let run ctx input = let* result = Tool.call("read_file", input) in match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.fail(code)
</script>
<script id="report" language="chatml" kind="tool">
let run ctx input = let* result = Tool.call("private_reader", input) in match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.fail(code)
</script>
<script id="start-report" language="chatml" kind="tool">
let run ctx input = let* id = Job.start_tool("private_reader", input) in
  Task.pure(`Pending(`Job(id), `String("accepted")))
</script>
<script id="policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
  | `Pre_tool_call(call) -> Task.pure(state + 1)
  | _ -> Task.pure(state)
</script>
<tool name="private_reader" type="chatml" script="reader" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>
<tool name="read_report" type="chatml" script="report" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="private_reader"/></tool>
<tool name="start_report" type="chatml" script="start-report" entrypoint="run" input_schema="schema.json" output_schema="schema.json" completion_schema="schema.json"><uses tool="private_reader"/></tool>|};
        let profile = { permission_profile with tool_default = Ask } in
        let configuration =
          config ~profile root root (Filename.concat root "parent.chatmd")
        in
        let requested_tool = ref "read_report" in
        let pending_call = ref false in
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
                  ; model_post_stream =
                      Some
                        (fun ~sw:_ ~inputs:_ ->
                          Int.incr requests;
                          match !pending_call with
                          | false -> Stdlib.Seq.empty
                          | true ->
                            pending_call := false;
                            let open Openai.Responses.Response_stream in
                            [ Output_item_added
                                { item =
                                    Function_call
                                      { name = !requested_tool
                                      ; arguments = ""
                                      ; call_id = sprintf "report-%d" !requests
                                      ; _type = "function_call"
                                      ; id = Some "report-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments = {|{"root":"data","file":"value.txt"}|}
                                ; item_id = "report-item"
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
                List.iter
                  (Agent_session.Prompt_catalog.entries (Daemon.prompts daemon))
                  ~f:(fun entry ->
                    match entry.availability with
                    | Ready _ -> ()
                    | Unavailable diagnostics ->
                      raise_s
                        [%sexp
                          (diagnostics
                           : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
                    | Disabled -> failwith "fixture prompt disabled");
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let call
              ?(before_permission = fun (_ : P.Permission.t) -> ())
              sw
              client
              entry
              name
          =
          requested_tool := name;
          pending_call := true;
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
            { kind = Plain_text; text = "Read the report"; attachments = [] }
          |> protocol_ok
          |> ignore;
          let rec idle () =
            let current = state entry in
            List.iter current.permissions ~f:(fun permission ->
              match P.Permission.equal_state permission.state Pending with
              | false -> ()
              | true ->
                assert (
                  P.Id.Session.equal permission.session_id current.identity.session_id);
                before_permission permission;
                H.respond_permission
                  handle
                  ~permission_id:permission.id
                  ~permission_generation:permission.generation
                  ~choice:Approve_once
                  ~reason:None
                |> protocol_ok
                |> ignore);
            let running_jobs =
              List.exists current.jobs ~f:(fun job ->
                match job.status with
                | Queued | Running | Waiting_permission _ | Waiting_completion _ -> true
                | _ -> false)
            in
            match current.active_operation, running_jobs with
            | None, false -> current
            | _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              idle ()
          in
          let current = idle () in
          H.detach handle |> protocol_ok;
          current
        in
        let check_public entry =
          Agent_server.Runtime_owner.with_background_runtime
            entry.R.runtime
            (fun runtime ->
               let native =
                 Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
               in
               let registry =
                 Lazy.force native.capabilities
                 |> Result.map_error ~f:(fun e -> P.Error.invalid_request e.C.message)
               in
               Result.map registry ~f:(fun registry ->
                 [%test_eq: string list]
                   [ "read_report"; "start_report" ]
                   (List.map (C.references registry) ~f:(fun r -> r.name));
                 assert (List.is_empty native.functions)))
          |> protocol_ok
        in
        let check_success current =
          assert (Option.is_none current.Agent_session.Session_state.moderator);
          let root_call =
            List.find_exn current.invocations ~f:(fun invocation ->
              String.equal invocation.context.tool_name "read_report")
          in
          (match root_call.status with
           | Published (Complete (`String text)) ->
             assert (String.is_substring text ~substring:"delegated-report-sentinel")
           | status ->
             raise_s [%sexp "inherited standalone failed", (status : P.Invocation.status)]);
          [%test_eq: int] 3 (List.length current.invocations);
          let middle =
            List.find_exn current.invocations ~f:(fun invocation ->
              String.equal invocation.context.tool_name "private_reader")
          in
          let reader =
            List.find_exn current.invocations ~f:(fun invocation ->
              String.equal invocation.context.tool_name "read_file")
          in
          assert (
            Option.equal
              P.Id.Invocation.equal
              middle.context.parent_invocation
              (Some root_call.context.id));
          assert (
            Option.equal
              P.Id.Invocation.equal
              reader.context.parent_invocation
              (Some middle.context.id));
          List.iter current.invocations ~f:(fun invocation ->
            assert (
              P.Id.Session.equal invocation.context.session_id current.identity.session_id))
        in
        let check_parent entry count =
          let current = state entry in
          assert (List.is_empty current.invocations);
          assert (List.is_empty current.permissions);
          let snapshot =
            Agent_session.Moderator_checkpoint.decode current.moderator
            |> protocol_ok
            |> Option.value_exn
          in
          match snapshot.current_state with
          | Session.Snapshot.Int actual -> [%test_eq: int] count actual
          | _ -> failwith "parent moderation state lost"
        in
        let parent_id, child_id, grandchild_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              R.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let child = create_child env root daemon parent_entry in
            check_public child;
            call sw client child "read_report" |> check_success;
            check_parent parent_entry 3;
            let rejected = call sw client child "read_file" in
            [%test_eq: int] 3 (List.length rejected.invocations);
            check_parent parent_entry 3;
            let grandchild = create_child env root daemon child in
            ( parent.id
            , (state child).identity.session_id
            , (state grandchild).identity.session_id ))
        in
        with_daemon (fun sw daemon client ->
          let parent = R.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = R.load (Daemon.registry daemon) child_id |> protocol_ok in
          let grandchild = R.load (Daemon.registry daemon) grandchild_id |> protocol_ok in
          check_public child;
          check_public grandchild;
          call sw client grandchild "read_report" |> check_success;
          check_parent parent 6;
          let started = call sw client grandchild "start_report" in
          let invocation =
            List.find_exn started.invocations ~f:(fun invocation ->
              String.equal invocation.context.tool_name "start_report")
          in
          let id =
            match invocation.status with
            | Published (Pending (Job id, `String "accepted")) -> id
            | status ->
              raise_s
                [%sexp
                  "inherited background launch failed", (status : P.Invocation.status)]
          in
          let rec completed () =
            let job =
              List.find_exn (state grandchild).jobs ~f:(fun job ->
                P.Id.Job.equal job.id id)
            in
            match job.status with
            | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              completed ()
            | Succeeded -> job
            | status ->
              raise_s
                [%sexp
                  "inherited background job failed"
                , (status : P.Job.status)
                , (job.result : Jsonaf.t option)]
          in
          let job = completed () in
          assert (P.Id.Session.equal job.session_id grandchild_id);
          (match P.Job.terminal_completion job |> protocol_ok with
           | Some (Succeeded (`String text)) ->
             assert (String.is_substring text ~substring:"delegated-report-sentinel")
           | _ -> failwith "inherited background completion missing");
          assert (List.is_empty (state parent).jobs);
          assert (List.is_empty (state child).jobs);
          let before = (state grandchild).invocations in
          let revoked = ref false in
          let revoke permission =
            match
              String.equal permission.P.Permission.tool_name "read_file", !revoked
            with
            | true, false ->
              revoked := true;
              let module D = Agent_store.Delegation_store in
              let store = Agent_store.Session_store.delegations (Daemon.store daemon) in
              let reference = Option.value_exn (state grandchild).spec.delegation in
              let record =
                D.resolve store reference
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
              in
              D.revoke store record Authority_changed
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
              |> ignore
            | _ -> ()
          in
          let denied =
            call ~before_permission:revoke sw client grandchild "read_report"
          in
          assert !revoked;
          let fresh =
            List.filter denied.invocations ~f:(fun invocation ->
              not
                (List.exists before ~f:(fun old ->
                   P.Id.Invocation.equal old.context.id invocation.context.id)))
          in
          [%test_eq: int] 3 (List.length fresh);
          List.iter fresh ~f:(fun invocation ->
            match invocation.status with
            | Resolved (Fail _) | Published (Fail _) -> ()
            | status ->
              raise_s
                [%sexp
                  "revoked private effect returned success"
                , (status : P.Invocation.status)]));
        print_endline
          "private dependency chain executes in child; parent policy remains \
           parent-owned; direct private calls blocked; grandchild restart and private \
           background work succeed"));
  [%expect
    {| private dependency chain executes in child; parent policy remains parent-owned; direct private calls blocked; grandchild restart and private background work succeed |}]
;;
