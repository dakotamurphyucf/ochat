open Core
open Agent_server_test_support
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module I = Agent_protocol.Invocation

let report_a = [%blob "../chatml_extensibility_fixtures/x01-report/report-a.json"]
let report_b = [%blob "../chatml_extensibility_fixtures/x01-report/report-b.json"]

let call_events calls =
  List.concat_mapi calls ~f:(fun index (id, name, arguments) ->
    let open Openai.Responses.Response_stream in
    [ Output_item_added
        { item =
            Function_call
              { name
              ; arguments = ""
              ; call_id = id
              ; _type = "function_call"
              ; id = Some id
              ; status = None
              }
        ; output_index = index
        ; type_ = "response.output_item.added"
        }
    ; Function_call_arguments_done
        { arguments = Jsonaf.to_string arguments
        ; item_id = id
        ; output_index = index
        ; type_ = "response.function_call_arguments.done"
        }
    ])
  |> Stdlib.List.to_seq
;;

let with_daemon
      ?validation_host
      ?(runtime_policy = Chat_response.Runtime_semantics.default_policy)
      ?settle
      ?after_turn
      ?after_turn_with_daemon
      ?(connect = fun ~sw:_ ~env:_ ~root:_ daemon -> connection daemon (principal ()))
      ?(inspect_request = fun _ _ -> ())
      ?(expected_requests = 2)
      ?(initial_requests = 2)
      ?(expected_schedules = 0)
      ?(expect_moderator = false)
      ~sources
      ~calls
      f
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdirs
          ~exists_ok:true
          ~perm:0o700
          Eio.Path.(Eio.Stdenv.fs env / workspace / "reports");
        let save path source =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / path)
            source
        in
        List.iter sources ~f:(fun (name, source) ->
          save (Filename.concat root name) source);
        save (Filename.concat workspace "reports/report-a.json") report_a;
        save (Filename.concat workspace "reports/report-b.json") report_b;
        save (Filename.concat workspace "secret.json") "PRIVATE-REPORT-SENTINEL";
        let configuration = config root workspace (Filename.concat root "agent.chatmd") in
        let requests = ref 0 in
        let post_stream ~sw:_ ~inputs =
          incr requests;
          inspect_request !requests inputs;
          match !requests with
          | 1 -> call_events calls
          | request when request <= expected_requests -> Stdlib.Seq.empty
          | _ -> failwith "tool execution requested an unexpected model turn"
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:configuration
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Agent_server.Daemon.default_options with
                  qualify_chatml_extensions = true
                ; chatml_runtime_policy = runtime_policy
                ; authoring_validation_host = validation_host
                ; model_post_stream = Some post_stream
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              List.iter
                (Agent_session.Prompt_catalog.entries
                   (Agent_server.Daemon.prompts daemon))
                ~f:(fun entry ->
                  match entry.availability with
                  | Ready _ -> ()
                  | Disabled -> failwith "composition prompt is disabled"
                  | Unavailable diagnostics ->
                    raise_s
                      [%sexp
                        (diagnostics
                         : Agent_session.Prompt_revision_builder.Diagnostic.t list)]);
              let client = connect ~sw ~env ~root daemon in
              initialize client;
              let session, _ = create_session ~start_immediately:true client in
              let handle =
                H.attach
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~connection:client
                  ~session_id:session.id
                  ~mode:Read_write
                  ()
                |> protocol_ok
              in
              let entry =
                Agent_server.Session_registry.find
                  (Agent_server.Daemon.registry daemon)
                  session.id
                |> Option.value_exn
              in
              let submission =
                H.send_message
                  handle
                  { kind = Plain_text
                  ; text = "Inspect the fixture reports."
                  ; attachments = []
                  }
                |> protocol_ok
              in
              let operation_id = Option.value_exn submission.operation_id in
              let final =
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  let rec wait () =
                    let state = A.state entry.actor |> protocol_ok in
                    match state.active_operation with
                    | None when !requests >= initial_requests -> state
                    | _ ->
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                      wait ()
                  in
                  wait ())
              in
              let events =
                match
                  Agent_session.Durable_event_log.replay
                    entry.durable_events
                    ~after_sequence:0L
                    ~through_sequence:Int64.max_value
                with
                | Available events -> events
                | Snapshot_required -> failwith "lost operation completion evidence"
              in
              assert (
                List.exists events ~f:(fun event ->
                  match
                    Agent_protocol.Event.Durable.Payload.of_json
                      ~kind:event.kind
                      event.payload
                    |> protocol_ok
                  with
                  | Operation_completed operation ->
                    Agent_protocol.Id.Operation.equal operation.id operation_id
                  | _ -> false));
              [%test_eq: int] initial_requests !requests;
              [%test_eq: int]
                1
                (List.length
                   (Agent_server.Session_registry.entries
                      (Agent_server.Daemon.registry daemon)));
              [%test_eq: int]
                1
                (List.length
                   (Agent_store.Session_store.list_sessions
                      (Agent_server.Daemon.store daemon)));
              [%test_eq: bool] expect_moderator (Option.is_some final.moderator);
              (match settle with
               | None -> assert (List.is_empty final.jobs)
               | Some _ -> ());
              [%test_eq: int] expected_schedules (List.length final.schedules);
              List.iter final.invocations ~f:(fun invocation ->
                assert (
                  Agent_protocol.Id.Session.equal invocation.context.session_id session.id);
                match invocation.context.origin with
                | Model ->
                  let output_id = Option.value_exn invocation.output_entry_id in
                  assert (
                    List.exists final.conversation.canonical_history ~f:(fun entry ->
                      Agent_protocol.History.Id.equal entry.id output_id))
                | Script ->
                  assert (
                    Option.is_some invocation.context.parent_invocation
                    || (Option.is_some settle
                        && Option.is_some invocation.context.parent_job))
                | Moderator when expect_moderator ->
                  let observation = Option.value_exn invocation.observation in
                  let source =
                    Agent_session.Runtime_builder.moderator_snapshot_observer
                      final.moderator
                    |> protocol_ok
                    |> Option.value_exn
                  in
                  assert (I.equal_observer source observation.observer);
                  assert (
                    Option.is_some invocation.context.parent_invocation
                    || Option.is_some invocation.parent_event)
                | Moderator | Delegated_agent | External_adapter ->
                  failwith "unexpected invocation origin");
              let final =
                Option.iter after_turn ~f:(fun after_turn -> after_turn env handle entry);
                Option.iter after_turn_with_daemon ~f:(fun after_turn ->
                  after_turn env daemon entry);
                match settle with
                | None -> final
                | Some settle ->
                  settle env entry;
                  A.state entry.actor |> protocol_ok
              in
              [%test_eq: int] expected_requests !requests;
              f final;
              H.close handle;
              Agent_client.Connection.close client))))
;;

let model_invocation state id =
  List.find_exn state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    Option.exists invocation.context.provider_call_id ~f:(String.equal id))
;;

let outcome invocation =
  match invocation.I.status with
  | Published outcome | Resolved outcome -> outcome
  | _ -> raise_s [%sexp "invocation did not publish", (invocation : I.t)]
;;

let result state id = outcome (model_invocation state id)

let children state invocation =
  List.filter state.Agent_session.Session_state.invocations ~f:(fun child ->
    Option.exists
      child.context.parent_invocation
      ~f:(Agent_protocol.Id.Invocation.equal invocation.I.context.id))
;;

let native_reads state =
  List.filter state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    String.equal invocation.context.tool_name "read_file")
;;
