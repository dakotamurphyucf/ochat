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

let with_daemon ~sources ~calls f =
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
        let post_stream ~sw:_ ~inputs:_ =
          incr requests;
          match !requests with
          | 1 -> call_events calls
          | 2 -> Stdlib.Seq.empty
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
                ; model_post_stream = Some post_stream
                }
              ()
            |> protocol_ok
          in
          let client = connection daemon (principal ()) in
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
                | None when !requests >= 2 -> state
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
          [%test_eq: int] 2 !requests;
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
          assert (Option.is_none final.moderator);
          assert (List.is_empty final.jobs);
          assert (List.is_empty final.schedules);
          List.iter final.invocations ~f:(fun invocation ->
            assert (
              Agent_protocol.Id.Session.equal invocation.context.session_id session.id);
            match invocation.context.origin with
            | Model ->
              let output_id = Option.value_exn invocation.output_entry_id in
              assert (
                List.exists final.conversation.canonical_history ~f:(fun entry ->
                  Agent_protocol.History.Id.equal entry.id output_id))
            | Script -> assert (Option.is_some invocation.context.parent_invocation)
            | Moderator | Delegated_agent | External_adapter ->
              failwith "unexpected invocation origin");
          f final;
          H.close handle;
          Agent_client.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok)))
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
