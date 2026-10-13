open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module M = Agent_session.Managed_submission
module Res = Openai.Responses

type fixture_client =
  { connection : Agent_client.Connection.t
  ; invocation_handles : (P.Id.Session.t, H.t) Hashtbl.t
  }

let close_fixture_client client =
  Eio.Cancel.protect (fun () ->
    Exn.protect
      ~finally:(fun () -> Agent_client.Connection.close client.connection)
      ~f:(fun () ->
        let handles = Hashtbl.data client.invocation_handles in
        Hashtbl.clear client.invocation_handles;
        let rec close = function
          | [] -> ()
          | handle :: rest ->
            Exn.protect ~finally:(fun () -> close rest) ~f:(fun () -> H.close handle)
        in
        close handles))
;;

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn

let state daemon id =
  (* Diagnostic observation borrows the checked owner; it must not compete with
     maintenance's lifecycle eligibility reservation or implicitly load actors.
     Activation remains explicit through the fixture's attachment operations. *)
  R.read_state (D.registry daemon) ~authorize:(fun _ -> Ok ()) id |> protocol_ok
;;

let assert_same_state before after ~context =
  let module State = Agent_session.Session_state in
  let before_sexp = State.sexp_of_t before in
  let after_sexp = State.sexp_of_t after in
  if not (Sexp.equal before_sexp after_sexp)
  then (
    let fields = function
      | Sexp.List fields ->
        List.map fields ~f:(function
          | Sexp.List [ Atom name; value ] -> name, value
          | _ -> failwith "unexpected state field sexp")
      | _ -> failwith "unexpected state sexp"
    in
    let after_fields = fields after_sexp in
    let changed =
      List.filter_map (fields before_sexp) ~f:(fun (name, value) ->
        match List.Assoc.find after_fields ~equal:String.equal name with
        | Some next when Sexp.equal value next -> None
        | Some _ | None -> Some name)
    in
    raise_s
      [%sexp
        "child state changed"
      , (context : string)
      , (changed : string list)
      , (before.counters : State.Counters.t)
      , (after.counters : State.Counters.t)
      , (before.identity.updated_at : P.Timestamp.t)
      , (after.identity.updated_at : P.Timestamp.t)
      , (before.lifecycle : State.Lifecycle.t)
      , (after.lifecycle : State.Lifecycle.t)])
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
  ; Output_item_done
      { item =
          Function_call
            { name
            ; arguments = Jsonaf.to_string arguments
            ; call_id = "managed-call"
            ; _type = "function_call"
            ; id = Some "managed-item"
            ; status = Some "completed"
            }
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test "stdio managed send crosses native and ChatML receipt/page boundaries" =
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
        (* Keep the UTF-8/escaping payload larger than a page without turning
           every parent tool call into a growing-history stress workload. *)
        let answer = String.concat (List.init 600 ~f:(fun _ -> "📚\"\\\n")) in
        assert (String.length answer > 4096);
        let child_gate = ref None in
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
            Option.iter !child_gate ~f:Eio.Promise.await;
            let message : Res.Output_message.t =
              { role = Assistant
              ; id = "managed-answer"
              ; status = "completed"
              ; content = [ { annotations = []; text = answer; _type = "output_text" } ]
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
                  ; inference_policy =
                      Agent_server_test_support.inference_policy
                        ~default_model:"fixture-model"
                        ~post_stream:provider
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
                  (* Bound the complete stdio receipt, cancellation and two-page
                     boundary witness, including daemon and transport teardown.
                     The operation's wait deadline remains independent. *)
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 300. (fun () ->
                    let client =
                      { connection =
                          Agent_server_wire_fixture.connect_stdio
                            ~sw
                            ~env
                            ~daemon
                            ~socket_path:(Filename.concat root "managed.sock")
                      ; invocation_handles = Hashtbl.create (module P.Id.Session)
                      }
                    in
                    Exn.protect
                      ~finally:(fun () -> close_fixture_client client)
                      ~f:(fun () ->
                        initialize client.connection;
                        f sw daemon client))
                with
                | Eio.Time.Timeout -> failwith ("managed send timeout: " ^ !phase)))
        in
        let attach sw client id =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client.connection
            ~session_id:id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let invoke sw daemon client caller name args =
          phase := name;
          let before = state daemon caller in
          (* One writer belongs to this actual connection/session. Receipt and
             page checks share it; explicit cancellation and child-reader handles
             retain their independent ownership. *)
          let handle =
            Hashtbl.find_or_add client.invocation_handles caller ~default:(fun () ->
              attach sw client caller)
          in
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
          invocation.status
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
        with_daemon (fun sw daemon client ->
          let parent, _ = create_session ~start_immediately:true client.connection in
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
                            ; "text", `String "<developer>MANAGED_SEND_CHILD</developer>"
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
          let reader =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client.connection
              ~session_id:child_id
              ~mode:Read_only
              ~subscribe:false
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> H.close reader)
            ~f:(fun () ->
              await (fun () -> Option.is_none (state daemon child_id).active_operation);
              let calls_before = !child_calls in
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
              await (fun () -> !child_calls > calls_before);
              [%test_eq: string] "assigned" (text receipt "status");
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
              let before_wait = state daemon child_id in
              Eio.Fiber.fork ~sw (fun () ->
                wait_dispatched daemon parent.id;
                let cancellation = attach sw client parent.id in
                Exn.protect
                  ~finally:(fun () -> H.close cancellation)
                  ~f:(fun () ->
                    let operation =
                      Option.value_exn (state daemon parent.id).active_operation
                    in
                    H.cancel_operation cancellation operation.id |> protocol_ok |> ignore));
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
                 raise_s
                   [%sexp "wait cancellation failed", (status : P.Invocation.status)]);
              assert_same_state
                before_wait
                (state daemon child_id)
                ~context:"wait cancellation";
              child_gate := None;
              Eio.Promise.resolve release ();
              await (fun () ->
                let child = state daemon child_id in
                Option.is_none child.active_operation
                && List.for_all child.managed_submissions ~f:(fun receipt ->
                  match receipt.M.status with
                  | Terminal (_, Completed) -> true
                  | _ -> false));
              let before_read = state daemon child_id in
              let first =
                invoke
                  sw
                  daemon
                  client
                  parent.id
                  "agent_read"
                  (read_request child_id (text receipt "receipt_id") None)
                |> complete
              in
              assert (Jsonaf.exactly_equal (field first "caught_up") `False);
              let second =
                invoke
                  sw
                  daemon
                  client
                  parent.id
                  "run_chatml"
                  (script
                     ~tool:"agent_read"
                     (read_request
                        child_id
                        (text receipt "receipt_id")
                        (Some (field first "next_cursor"))))
                |> complete
              in
              List.iter [ first; second ] ~f:(fun page ->
                assert (String.length (Jsonaf.to_string page) <= 4096);
                let items = field page "items" |> Jsonaf.list_exn in
                assert (not (List.is_empty items));
                List.iter items ~f:(fun item ->
                  [%test_eq: string] "output_fragment" (text item "kind");
                  assert (Stdlib.String.is_valid_utf_8 (text item "text"))));
              assert (
                not
                  (Jsonaf.exactly_equal
                     (field first "next_cursor")
                     (field second "next_cursor")));
              assert_same_state
                before_read
                (state daemon child_id)
                ~context:"native and ChatML page reads";
              let foreign = P.Id.Session.create () in
              invoke
                sw
                daemon
                client
                parent.id
                "agent_read"
                (`Object [ "session_id", P.Id.Session.to_json foreign ])
              |> denied "agent.management.denied";
              assert_same_state
                before_read
                (state daemon child_id)
                ~context:"foreign disclosure denial";
              let stopped =
                invoke
                  sw
                  daemon
                  client
                  parent.id
                  "agent_stop"
                  (stop_request child_id "native-stop" "graceful")
                |> complete
              in
              [%test_eq: string] "stopped" (text stopped "progress")))));
  print_endline
    "stdio native/ChatML retries and pages preserve scoped receipts; cancelled waits \
     preserve child work";
  [%expect
    {| stdio native/ChatML retries and pages preserve scoped receipts; cancelled waits preserve child work |}]
;;
