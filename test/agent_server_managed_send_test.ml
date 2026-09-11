open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module M = Agent_session.Managed_submission
module Res = Openai.Responses

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn

let state daemon id =
  R.load (D.registry daemon) id
  |> protocol_ok
  |> fun (e : R.entry) -> A.state e.actor |> protocol_ok
;;

let function_call name arguments =
  let open Res.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name
            ; arguments = ""
            ; call_id = "managed-call"
            ; _type = "function_call"
            ; id = Some "managed-item"
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = "managed-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test
    "managed send uses actual caller, retains admission across restart, and never \
     resumes stopped children"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt = Filename.concat root "parent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          {|<developer>MANAGED_SEND_PARENT</developer><tool name="agent_create"/><tool name="agent_send"/><tool name="agent_read"/><tool name="agent_wait"/><tool name="agent_stop"/><tool name="run_chatml"/>|};
        let configuration = config root root prompt in
        let queued = ref None in
        let child_calls = ref 0 in
        let child_inputs = Queue.create () in
        let answer = String.concat (List.init 2500 ~f:(fun _ -> "📚\"\\\n")) in
        let child_gate = ref None in
        let terminal_gate = ref None in
        let phase = ref "startup" in
        let provider ~sw:_ ~inputs =
          (* Match developer input only: the parent's tool result/request contains
             the child's definition too. *)
          let child =
            List.exists inputs ~f:(function
              | Res.Item.Input_message message ->
                let json = Res.Item.jsonaf_of_t (Input_message message) in
                String.equal (text json "role") "developer"
                && String.is_substring
                     (Jsonaf.to_string json)
                     ~substring:"MANAGED_SEND_CHILD"
              | _ -> false)
          in
          match child with
          | true ->
            Int.incr child_calls;
            Queue.enqueue
              child_inputs
              (`Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string);
            Option.iter !child_gate ~f:Eio.Promise.await;
            (match !child_calls with
             | 1 -> Stdlib.Seq.empty
             | _ ->
               let message : Res.Output_message.t =
                 { role = Assistant
                 ; id = "managed-answer"
                 ; status = "completed"
                 ; content =
                     [ { annotations = []; text = answer; _type = "output_text" } ]
                 ; phase = None
                 ; _type = "message"
                 }
               in
               let item = Res.Response_stream.Item.Output_message message in
               [ Res.Response_stream.Output_item_added
                   { item; output_index = 0; type_ = "response.output_item.added" }
               ; Output_text_delta
                   { item_id = message.id
                   ; output_index = 0
                   ; content_index = 0
                   ; delta = answer
                   ; type_ = "response.output_text.delta"
                   }
               ; Output_item_done
                   { item; output_index = 0; type_ = "response.output_item.done" }
               ]
               |> Stdlib.List.to_seq
               |> fun events ->
               Stdlib.Seq.append events (fun () ->
                 Option.iter !terminal_gate ~f:Eio.Promise.await;
                 Stdlib.Seq.Nil))
          | false ->
            (match !queued with
             | None -> Stdlib.Seq.empty
             | Some (name, args) ->
               queued := None;
               function_call name args)
        in
        let await predicate =
          let rec loop () =
            match predicate () with
            | true -> ()
            | false ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              loop ()
          in
          loop ()
        in
        let with_daemon ?(page_bytes = 4096) f =
          Eio.Switch.run (fun sw ->
            let calls_before = !child_calls in
            let daemon =
              D.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { D.default_options with
                    qualify_chatml_extensions = true
                  ; model_post_stream = Some provider
                  ; factory_limits =
                      { D.default_options.factory_limits with
                        managed_submission_max_count = Some 2
                      ; managed_message_max_bytes = Some 128
                      ; managed_output_page_max_bytes = page_bytes
                      }
                  }
                ()
              |> protocol_ok
            in
            [%test_eq: int] calls_before !child_calls;
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                try
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                    let client = connection daemon (principal ()) in
                    Exn.protect
                      ~finally:(fun () -> Agent_client.Connection.close client)
                      ~f:(fun () ->
                        initialize client;
                        f sw daemon client))
                with
                | Eio.Time.Timeout -> failwith ("managed send timeout: " ^ !phase)))
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
        let invoke sw daemon client caller name args =
          phase := name;
          let before = state daemon caller in
          let handle = attach sw client caller in
          Exn.protect
            ~finally:(fun () -> H.close handle)
            ~f:(fun () ->
              queued := Some (name, args);
              H.send_message
                handle
                { kind = Plain_text; text = "Run tool."; attachments = [] }
              |> protocol_ok
              |> ignore;
              await (fun () -> Option.is_none (state daemon caller).active_operation);
              let current = state daemon caller in
              let invocation =
                List.find_exn current.invocations ~f:(fun invocation ->
                  P.Invocation.equal_origin invocation.context.origin Model
                  && not
                       (List.exists before.invocations ~f:(fun previous ->
                          P.Id.Invocation.equal previous.context.id invocation.context.id)))
              in
              invocation.status)
        in
        let complete = function
          | P.Invocation.Published (Complete value) -> value
          | status -> raise_s [%sexp "tool failed", (status : P.Invocation.status)]
        in
        let wait_dispatched daemon caller =
          await (fun () ->
            List.exists (state daemon caller).invocations ~f:(fun call ->
              match call.P.Invocation.status with
              | Dispatching -> String.equal call.context.tool_name "agent_wait"
              | _ -> false))
        in
        let denied code = function
          | P.Invocation.Published (Fail error) ->
            [%test_eq: string] code error.code;
            assert (Jsonaf.exactly_equal error.details `Null)
          | status ->
            raise_s [%sexp "unexpected tool result", (status : P.Invocation.status)]
        in
        let message child key value =
          `Object
            [ "session_id", `String (P.Id.Session.to_string child)
            ; "idempotency_key", `String key
            ; "message", `String value
            ]
        in
        let script ?(tool = "agent_send") args =
          `Object
            [ ( "source"
              , `String
                  (sprintf
                     {|let main input =
  let* result = Tool.call(%S, input) in
  match result with
  | `Ok(receipt) -> Task.pure(receipt)
  | `Error(code) -> Task.fail(code)|}
                     tool) )
            ; "input", args
            ; "tools", `Array [ `String tool ]
            ]
        in
        let read_request child receipt cursor =
          `Object
            ([ "session_id", P.Id.Session.to_json child; "receipt_id", `String receipt ]
             @ Option.to_list (Option.map cursor ~f:(fun value -> "cursor", value)))
        in
        let wait_request child receipt cursor timeout_ms =
          match read_request child receipt cursor with
          | `Object fields ->
            `Object (fields @ [ "timeout_ms", `Number (Int.to_string timeout_ms) ])
          | _ -> assert false
        in
        let stop_request child key mode =
          `Object
            [ "session_id", P.Id.Session.to_json child
            ; "idempotency_key", `String key
            ; "mode", `String mode
            ]
        in
        let parent_id, child_id, receipt_id, output_cursor, stop_receipt =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let child =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_create"
                (`Object
                    [ "version", `Number "1"
                    ; "root_file", `String "child.chatmd"
                    ; ( "sources"
                      , `Array
                          [ `Object
                              [ "path", `String "child.chatmd"
                              ; ( "text"
                                , `String {|<developer>MANAGED_SEND_CHILD</developer>|} )
                              ]
                          ] )
                    ; "tools", `Array []
                    ; "start_immediately", `True
                    ; "idempotency_key", `String "managed-child"
                    ])
              |> complete
            in
            let child_id =
              text child "session_id" |> P.Id.Session.of_string |> protocol_ok
            in
            let initial = state daemon child_id in
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message child_id "oversized" (String.make 129 'x'))
            |> denied "agent.send.invalid_request";
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (`Object
                  [ "session_id", P.Id.Session.to_json child_id
                  ; "message", `String "hello"
                  ; "idempotency_key", `String "unknown-option"
                  ; "resume", `True
                  ])
            |> denied "invocation.invalid_input";
            let foreign = P.Id.Session.create () in
            invoke
              sw
              daemon
              client
              parent.id
              "agent_stop"
              (stop_request foreign "foreign-stop" "cancel")
            |> denied "agent.management.denied";
            invoke
              sw
              daemon
              client
              parent.id
              "agent_wait"
              (wait_request
                 foreign
                 (P.History.Id.to_string
                    (List.hd_exn initial.conversation.canonical_history).id)
                 None
                 0)
            |> denied "agent.management.denied";
            invoke
              sw
              daemon
              client
              parent.id
              "agent_read"
              (`Object [ "session_id", P.Id.Session.to_json foreign ])
            |> denied "agent.management.denied";
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message foreign "foreign" "hello")
            |> denied "agent.management.denied";
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message parent.id "self" "hello")
            |> denied "agent.management.denied";
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t initial)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            let gate, release = Eio.Promise.create () in
            child_gate := Some gate;
            let receipt =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_send"
                (message child_id "first" "Retain this exact message.")
              |> complete
            in
            phase := "first child executing";
            await (fun () -> Int.equal !child_calls 1);
            [%test_eq: string] "assigned" (text receipt "status");
            assert (Jsonaf.exactly_equal (field receipt "terminal") `False);
            let pending =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_read"
                (read_request child_id (text receipt "receipt_id") None)
              |> complete
            in
            assert (Jsonaf.exactly_equal (field pending "caught_up") `True);
            assert (
              Jsonaf.exactly_equal (field (field pending "receipt") "terminal") `False);
            assert (List.is_empty (field pending "items" |> Jsonaf.list_exn));
            let pending_output_cursor = field pending "next_cursor" in
            let before_wait = state daemon child_id in
            let timed_out =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_wait"
                (wait_request child_id (text receipt "receipt_id") None 20)
              |> complete
            in
            [%test_eq: string] "timeout" (text timed_out "reason");
            assert (
              Jsonaf.exactly_equal (field (field timed_out "receipt") "terminal") `False);
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t before_wait)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            Eio.Fiber.fork ~sw (fun () ->
              wait_dispatched daemon parent.id;
              let handle = attach sw client parent.id in
              let operation =
                Option.value_exn (state daemon parent.id).active_operation
              in
              H.cancel_operation handle operation.id |> protocol_ok |> ignore;
              H.close handle);
            (match
               invoke
                 sw
                 daemon
                 client
                 parent.id
                 "agent_wait"
                 (wait_request child_id (text receipt "receipt_id") None 30000)
             with
             | Published (Cancelled _) | Resolved (Cancelled _) -> ()
             | status ->
               raise_s [%sexp "wait cancellation failed", (status : P.Invocation.status)]);
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t before_wait)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            let replay =
              invoke
                sw
                daemon
                client
                parent.id
                "run_chatml"
                (script (message child_id "first" "Retain this exact message."))
              |> complete
            in
            assert (Jsonaf.exactly_equal receipt replay);
            let before_conflict = state daemon child_id in
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message child_id "first" "Changed.")
            |> denied "agent.send.conflict";
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t before_conflict)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            let deferred =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_send"
                (message child_id "second" "Continue when ready.")
              |> complete
            in
            [%test_eq: string] "deferred" (text deferred "status");
            [%test_eq: int] 1 !child_calls;
            let at_capacity = state daemon child_id in
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message child_id "capacity" "Do not admit a third message.")
            |> denied "agent.send.invalid_state";
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t at_capacity)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            let terminal, release_terminal = Eio.Promise.create () in
            terminal_gate := Some terminal;
            Eio.Fiber.fork ~sw (fun () ->
              wait_dispatched daemon parent.id;
              child_gate := None;
              Eio.Promise.resolve release ());
            let available =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_wait"
                (wait_request
                   child_id
                   (text receipt "receipt_id")
                   (Some pending_output_cursor)
                   5000)
              |> complete
            in
            [%test_eq: string] "output_available" (text available "reason");
            assert (Jsonaf.exactly_equal (field available "cursor") pending_output_cursor);
            assert (
              Jsonaf.exactly_equal (field (field available "receipt") "terminal") `False);
            let still_pending =
              invoke
                sw
                daemon
                client
                parent.id
                "run_chatml"
                (script
                   ~tool:"agent_wait"
                   (wait_request child_id (text deferred "receipt_id") None 0))
              |> complete
            in
            [%test_eq: string] "timeout" (text still_pending "reason");
            let stopping =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_stop"
                (stop_request child_id "native-stop" "graceful")
              |> complete
            in
            [%test_eq: string] "stopping" (text stopping "progress");
            [%test_eq: string] "stopped" (text (field stopping "status") "desired_state");
            assert (Option.is_some (state daemon child_id).active_operation);
            let stop_receipt = field stopping "receipt" in
            Eio.Fiber.fork ~sw (fun () ->
              wait_dispatched daemon parent.id;
              terminal_gate := None;
              Eio.Promise.resolve release_terminal ());
            let finished =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_wait"
                (wait_request child_id (text deferred "receipt_id") None 5000)
              |> complete
            in
            [%test_eq: string] "receipt_terminal" (text finished "reason");
            (match text (field finished "receipt") "status" with
             | "completed" -> ()
             | _ ->
               let current = state daemon child_id in
               let child_entry = R.load (D.registry daemon) child_id |> protocol_ok in
               let events =
                 Agent_session.Durable_event_log.replay
                   child_entry.durable_events
                   ~after_sequence:0L
                   ~through_sequence:current.counters.event_sequence
               in
               let failures =
                 match events with
                 | Snapshot_required -> []
                 | Available events ->
                   List.filter_map events ~f:(fun event ->
                     match event.P.Event.Durable.kind with
                     | Operation_failed -> Some event.payload
                     | _ -> None)
               in
               raise_s [%sexp "graceful child failed", (failures : Jsonaf.t list)]);
            [%test_eq: string] "completed" (text (field finished "receipt") "status");
            phase := "child receipts complete";
            await (fun () ->
              let current = state daemon child_id in
              let done_ =
                List.for_all current.managed_submissions ~f:(fun receipt ->
                  match receipt.M.status with
                  | Terminal (_, Completed) -> true
                  | _ -> false)
              in
              match done_, current.active_operation with
              | false, None ->
                let entry = R.load (D.registry daemon) child_id |> protocol_ok in
                let events =
                  Agent_session.Durable_event_log.replay
                    entry.durable_events
                    ~after_sequence:0L
                    ~through_sequence:current.counters.event_sequence
                in
                let failures =
                  match events with
                  | Snapshot_required -> []
                  | Available events ->
                    List.filter_map events ~f:(fun event ->
                      match event.P.Event.Durable.kind with
                      | Operation_failed -> Some event.payload
                      | _ -> None)
                in
                raise_s
                  [%sexp
                    "child finished without complete receipts"
                  , (current.managed_submissions : M.t list)
                  , (failures : Jsonaf.t list)]
              | _ -> done_);
            [%test_eq: int] 2 !child_calls;
            assert (
              String.is_substring
                (Queue.last_exn child_inputs)
                ~substring:"Continue when ready.");
            (match (state daemon child_id).managed_submissions with
             | [ { status = Terminal (Some first, Completed); _ }
               ; { status = Terminal (Some second, Completed); _ }
               ] -> assert (P.Id.Operation.equal first second)
             | _ -> failwith "deferred inputs did not share the completed operation");
            await (fun () ->
              match (state daemon child_id).lifecycle.observed with
              | Stopped -> true
              | _ -> false);
            let calls_before = !child_calls in
            let stopped = state daemon child_id in
            let output = Buffer.create 32768 in
            let rec read_pages cursor count =
              assert (count < 32);
              let args = read_request child_id (text receipt "receipt_id") cursor in
              let page =
                (match cursor with
                 | None -> invoke sw daemon client parent.id "agent_read" args
                 | Some _ ->
                   invoke
                     sw
                     daemon
                     client
                     parent.id
                     "run_chatml"
                     (script ~tool:"agent_read" args))
                |> complete
              in
              assert (String.length (Jsonaf.to_string page) <= 4096);
              List.iter
                (field page "items" |> Jsonaf.list_exn)
                ~f:(fun item ->
                  [%test_eq: string] "output_fragment" (text item "kind");
                  let fragment = text item "text" in
                  assert (Stdlib.String.is_valid_utf_8 fragment);
                  Buffer.add_string output fragment);
              let next = field page "next_cursor" in
              match Jsonaf.exactly_equal (field page "caught_up") `True with
              | true -> next
              | false ->
                Option.iter cursor ~f:(fun previous ->
                  assert (not (Jsonaf.exactly_equal previous next)));
                read_pages (Some next) (count + 1)
            in
            let output_cursor = read_pages (Some pending_output_cursor) 0 in
            let record = Jsonaf.of_string (Buffer.contents output) in
            let payload = field (field record "history") "payload" in
            let content = field payload "content" |> Jsonaf.list_exn |> List.hd_exn in
            [%test_eq: string] answer (text content "text");
            [%test_eq: int]
              2
              (field record "submission_ids" |> Jsonaf.list_exn |> List.length);
            [%test_eq: int]
              1
              (field record "operation_ids" |> Jsonaf.list_exn |> List.length);
            let caught_up =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_read"
                (read_request child_id (text receipt "receipt_id") (Some output_cursor))
              |> complete
            in
            assert (List.is_empty (field caught_up "items" |> Jsonaf.list_exn));
            let waiting_at_tail =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_wait"
                (wait_request child_id (text receipt "receipt_id") (Some output_cursor) 0)
              |> complete
            in
            [%test_eq: string] "timeout" (text waiting_at_tail "reason");
            [%test_eq: string] "stopped" (text (field waiting_at_tail "status") "state");
            let replay =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_send"
                (message child_id "first" "Retain this exact message.")
              |> complete
            in
            [%test_eq: string] (text receipt "receipt_id") (text replay "receipt_id");
            [%test_eq: string] "completed" (text replay "status");
            invoke
              sw
              daemon
              client
              parent.id
              "agent_send"
              (message child_id "third" "Do not restart.")
            |> denied "agent.send.invalid_state";
            [%test_eq: int] calls_before !child_calls;
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t stopped)
              (Agent_session.Session_state.sexp_of_t (state daemon child_id));
            parent.id, child_id, text receipt "receipt_id", output_cursor, stop_receipt)
        in
        with_daemon ~page_bytes:65536 (fun sw daemon client ->
          let before = state daemon child_id in
          let stopped_replay =
            invoke
              sw
              daemon
              client
              parent_id
              "agent_stop"
              (stop_request child_id "native-stop" "graceful")
            |> complete
          in
          assert (Jsonaf.exactly_equal stop_receipt (field stopped_replay "receipt"));
          [%test_eq: string] "stopped" (text stopped_replay "progress");
          let retained =
            invoke
              sw
              daemon
              client
              parent_id
              "agent_wait"
              (wait_request child_id receipt_id None 0)
            |> complete
          in
          [%test_eq: string] "receipt_terminal" (text retained "reason");
          (match
             invoke
               sw
               daemon
               client
               parent_id
               "agent_wait"
               (wait_request child_id receipt_id (Some output_cursor) 0)
           with
           | Published (Fail error) ->
             [%test_eq: string] "agent.wait.cursor_expired" error.code;
             assert (Jsonaf.exactly_equal (field error.details "snapshot_required") `True)
           | _ -> failwith "wait accepted a previous-process cursor");
          (match
             invoke
               sw
               daemon
               client
               parent_id
               "agent_read"
               (read_request child_id receipt_id (Some output_cursor))
           with
           | Published (Fail error) ->
             [%test_eq: string] "agent.read.cursor_expired" error.code;
             assert (Jsonaf.exactly_equal (field error.details "snapshot_required") `True)
           | _ -> failwith "old process output cursor did not expire");
          let fresh =
            invoke
              sw
              daemon
              client
              parent_id
              "run_chatml"
              (script ~tool:"agent_read" (read_request child_id receipt_id None))
            |> complete
          in
          assert (Jsonaf.exactly_equal (field fresh "snapshot") `True);
          [%test_eq: int] 1 (field fresh "items" |> Jsonaf.list_exn |> List.length);
          let replay =
            invoke
              sw
              daemon
              client
              parent_id
              "run_chatml"
              (script (message child_id "first" "Retain this exact message."))
            |> complete
          in
          [%test_eq: string] receipt_id (text replay "receipt_id");
          [%test_eq: string] "completed" (text replay "status");
          [%test_eq: Sexp.t]
            (Agent_session.Session_state.sexp_of_t before)
            (Agent_session.Session_state.sexp_of_t (state daemon child_id));
          [%test_eq: int] 2 (List.length before.managed_submissions);
          let read_all cursor =
            invoke
              sw
              daemon
              client
              parent_id
              "agent_read"
              (`Object
                  ([ "session_id", P.Id.Session.to_json child_id ]
                   @ Option.to_list (Option.map cursor ~f:(fun value -> "cursor", value))
                  ))
          in
          let all = read_all None |> complete in
          assert (Jsonaf.exactly_equal (field all "caught_up") `True);
          [%test_eq: int] 1 (field all "items" |> Jsonaf.list_exn |> List.length);
          let tail = field all "next_cursor" in
          let handle = attach sw client child_id in
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          await (fun () -> observed_idle (state daemon child_id).lifecycle.observed);
          let resumed = state daemon child_id in
          let old_stop =
            invoke
              sw
              daemon
              client
              parent_id
              "run_chatml"
              (script ~tool:"agent_stop" (stop_request child_id "native-stop" "graceful"))
            |> complete
          in
          assert (Jsonaf.exactly_equal stop_receipt (field old_stop "receipt"));
          [%test_eq: string] "superseded" (text old_stop "progress");
          [%test_eq: string] "running" (text (field old_stop "status") "desired_state");
          invoke
            sw
            daemon
            client
            parent_id
            "agent_stop"
            (stop_request child_id "native-stop" "cancel")
          |> denied "agent.stop.conflict";
          [%test_eq: Sexp.t]
            (Agent_session.Session_state.sexp_of_t resumed)
            (Agent_session.Session_state.sexp_of_t (state daemon child_id));
          H.send_message
            handle
            { kind = Plain_text; text = "Generate an unread response."; attachments = [] }
          |> protocol_ok
          |> ignore;
          await (fun () -> Option.is_none (state daemon child_id).active_operation);
          let current = state daemon child_id in
          let outputs =
            List.filter current.conversation.canonical_history ~f:(fun entry ->
              match entry.P.History.role, entry.kind with
              | Assistant, Message -> true
              | _ -> false)
          in
          [%test_eq: int] 2 (List.length outputs);
          H.delete_history
            handle
            ~expected_revision:current.counters.revision
            (List.last_exn outputs).id
          |> protocol_ok
          |> ignore;
          let edited = state daemon child_id in
          [%test_eq: int] current.identity.generation edited.identity.generation;
          [%test_eq: int]
            current.conversation.compaction_generation
            edited.conversation.compaction_generation;
          (match read_all (Some tail) with
           | Published (Fail error) ->
             [%test_eq: string] "agent.read.cursor_expired" error.code
           | _ -> failwith "deleting unread output silently returned an empty page");
          let refreshed = read_all None |> complete in
          [%test_eq: int] 1 (field refreshed "items" |> Jsonaf.list_exn |> List.length);
          let stopped_again =
            invoke
              sw
              daemon
              client
              parent_id
              "agent_stop"
              (stop_request child_id "stop-new-lifetime" "cancel")
            |> complete
          in
          [%test_eq: string] "stopped" (text stopped_again "progress");
          H.close handle;
          let quiet = state daemon child_id in
          let ledger = Agent_store.Session_store.delegations (D.store daemon) in
          let record =
            Agent_store.Delegation_store.resolve
              ledger
              (Option.value_exn quiet.spec.delegation)
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
          in
          Eio.Fiber.fork ~sw (fun () ->
            wait_dispatched daemon parent_id;
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
            Agent_store.Delegation_store.revoke ledger record Authority_changed
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
            |> ignore);
          invoke
            sw
            daemon
            client
            parent_id
            "agent_wait"
            (`Object
                [ "session_id", P.Id.Session.to_json child_id
                ; "cursor", field refreshed "next_cursor"
                ; "timeout_ms", `Number "30000"
                ])
          |> denied "agent.management.denied";
          [%test_eq: Sexp.t]
            (Agent_session.Session_state.sexp_of_t quiet)
            (Agent_session.Session_state.sexp_of_t (state daemon child_id)));
        print_endline
          "waits distinguish output from terminal receipts; timeout/cancellation \
           preserve children; quiet revocation denies disclosure";
        print_endline
          "native graceful stop completes existing work; restart/script retries retain \
           receipt without stopping a new lifetime";
        print_endline
          "native/script retry shares one receipt; busy sends defer; conflicts and \
           foreign IDs do not mutate children; stopped/restarted receipt replay never \
           runs a child"));
  [%expect
    {|
    waits distinguish output from terminal receipts; timeout/cancellation preserve children; quiet revocation denies disclosure
    native graceful stop completes existing work; restart/script retries retain receipt without stopping a new lifetime
    native/script retry shares one receipt; busy sends defer; conflicts and foreign IDs do not mutate children; stopped/restarted receipt replay never runs a child
    |}]
;;
