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

let check_history expected actual =
  [%test_eq: Sexp.t]
    ([%sexp_of: P.History.entry list] expected)
    ([%sexp_of: P.History.entry list] actual)
;;

let state daemon id =
  let entry = R.load (D.registry daemon) id |> protocol_ok in
  A.state entry.actor |> protocol_ok
;;

let call_events index id name args =
  let open Res.Response_stream in
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
      { arguments = Jsonaf.to_string args
      ; item_id = id
      ; output_index = index
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let answer id value =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id
    ; status = "completed"
    ; content = [ { annotations = []; text = value; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = id
      ; output_index = 0
      ; content_index = 0
      ; delta = value
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test
    "concurrent authored instances and pending receipts use shared lifecycle tools over \
     HTTP"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save name value =
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / name)
            value
        in
        save
          "parent.chatmd"
          {|<developer>PENDING_PARENT</developer>
<tool name="researcher" agent="child.chatmd" local persistence="optional"/>
<tool name="agent_send"/><tool name="agent_read"/><tool name="agent_status"/><tool name="agent_wait"/><tool name="agent_stop"/>
<tool name="run_chatml"/>|};
        save "child.chatmd" {|<developer>PENDING_CHILD</developer>|};
        let queued = ref [] in
        let serial = ref 0 in
        let child_calls = ref 0 in
        let release, release_u = Eio.Promise.create () in
        let phase = ref "startup" in
        let provider ~sw:_ ~inputs =
          let serialized =
            List.map inputs ~f:(fun item -> Res.Item.jsonaf_of_t item |> Jsonaf.to_string)
            |> String.concat ~sep:"\n"
          in
          if String.is_substring serialized ~substring:"PENDING_CHILD"
          then (
            Int.incr child_calls;
            let index = !child_calls in
            Eio.Promise.await release;
            let label =
              if String.is_substring serialized ~substring:"LEFT_WORK"
              then "left-result"
              else "right-result"
            in
            answer (sprintf "child-answer-%d" index) label)
          else (
            let batch = !queued in
            queued := [];
            List.concat_mapi batch ~f:(fun index (id, name, args) ->
              let events = call_events index id name args in
              (* Repeated delivery of the same provider completion must retain
                 one native invocation, creation identity and send receipt. *)
              events @ [ List.last_exn events ])
            |> Stdlib.List.to_seq)
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
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let before = !child_calls in
            let daemon =
              D.start
                ~sw
                ~env
                ~config:(config root root (Filename.concat root "parent.chatmd"))
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { D.default_options with
                    qualify_chatml_extensions = true
                  ; model_post_stream = Some provider
                  }
                ()
              |> protocol_ok
            in
            [%test_eq: int] before !child_calls;
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                (* Two independent batches deliberately wait out the production 10s
                 named-tool response deadline. This is an outer fixture guard. *)
                try
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
                    let connect =
                      Agent_server_wire_fixture.http_connector
                        ~sw
                        ~env
                        ~daemon
                        ~root
                        ~principal:(principal ())
                    in
                    let client = ref (connect ()) in
                    let disconnect () = Agent_client.Connection.close !client in
                    let reconnect () =
                      client := connect ();
                      initialize !client
                    in
                    Exn.protect ~finally:disconnect ~f:(fun () ->
                      initialize !client;
                      let attach id =
                        H.attach
                          ~sw
                          ~clock:(Eio.Stdenv.clock env)
                          ~connection:!client
                          ~session_id:id
                          ~mode:Read_write
                          ~subscribe:false
                          ()
                        |> protocol_ok
                      in
                      f ~disconnect ~reconnect sw daemon !client attach))
                with
                | Eio.Time.Timeout -> failwith ("pending fixture timeout: " ^ !phase)))
        in
        let invoke_batch_status daemon attach parent calls =
          let before = state daemon parent in
          queued
          := List.map calls ~f:(fun (name, args) ->
               Int.incr serial;
               sprintf "pending-call-%d" !serial, name, args);
          let handle = attach parent in
          Exn.protect
            ~finally:(fun () -> H.close handle)
            ~f:(fun () ->
              H.send_message
                handle
                { kind = Plain_text
                ; text = "Run the requested operations."
                ; attachments = []
                }
              |> protocol_ok
              |> ignore;
              await (fun () -> Option.is_none (state daemon parent).active_operation);
              let fresh =
                List.filter (state daemon parent).invocations ~f:(fun invocation ->
                  P.Invocation.equal_origin invocation.context.origin Model
                  && not
                       (List.exists before.invocations ~f:(fun old ->
                          P.Id.Invocation.equal old.context.id invocation.context.id)))
              in
              [%test_eq: int] (List.length calls) (List.length fresh);
              List.map fresh ~f:(fun invocation ->
                invocation.context.input, invocation.status))
        in
        let invoke_batch daemon attach parent calls =
          invoke_batch_status daemon attach parent calls
          |> List.map ~f:(fun (input, status) ->
            match status with
            | Published (Complete value) -> input, value
            | status ->
              raise_s [%sexp "pending native call failed", (status : P.Invocation.status)])
        in
        let invoke daemon attach parent name args =
          match invoke_batch daemon attach parent [ name, args ] with
          | [ (_, result) ] -> result
          | _ -> assert false
        in
        let named ?session_id input =
          `Object
            ([ "input", `String input; "mode", `String "persistent" ]
             @ Option.to_list
                 (Option.map session_id ~f:(fun id ->
                    "session_id", P.Id.Session.to_json id)))
        in
        let session json =
          field json "session_id" |> P.Id.Session.of_json |> protocol_ok
        in
        let receipt json =
          field json "receipt" |> fun receipt -> text receipt "receipt_id"
        in
        let read_args id receipt =
          `Object [ "session_id", P.Id.Session.to_json id; "receipt_id", `String receipt ]
        in
        let wait_args id receipt =
          `Object
            [ "session_id", P.Id.Session.to_json id
            ; "receipt_id", `String receipt
            ; "timeout_ms", `Number "0"
            ]
        in
        let send_args id =
          `Object
            [ "session_id", P.Id.Session.to_json id
            ; "idempotency_key", `String "generic-deferred"
            ; "message", `String "Also include the generic follow-up."
            ]
        in
        let script tool args =
          `Object
            [ ( "source"
              , `String
                  (sprintf
                     {|let main input =
  let* result = Tool.call(%S, input) in
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(code) -> Task.fail(code)|}
                     tool) )
            ; "input", args
            ; "tools", `Array [ `String tool ]
            ]
        in
        let stop_args id =
          `Object
            [ "session_id", P.Id.Session.to_json id
            ; "idempotency_key", `String "authored-stop"
            ; "mode", `String "cancel"
            ]
        in
        let parent, left, right, left_receipts, right_receipt, stopped_history =
          with_daemon (fun ~disconnect ~reconnect sw daemon client attach ->
            let parent, _ = create_session ~start_immediately:true client in
            phase := "concurrent creation";
            let first =
              Eio.Fiber.fork_promise ~sw (fun () ->
                invoke_batch
                  daemon
                  attach
                  parent.id
                  [ "researcher", named "LEFT_WORK"; "researcher", named "RIGHT_WORK" ])
            in
            await (fun () -> Int.equal !child_calls 2);
            [%test_eq: int]
              2
              (List.count (state daemon parent.id).invocations ~f:(fun invocation ->
                 match invocation.status with
                 | Dispatching -> true
                 | _ -> false));
            let results = Eio.Promise.await_exn first in
            let find label =
              List.find_map_exn results ~f:(fun (input, value) ->
                if String.equal (text input "input") label then Some value else None)
            in
            let left_result = find "LEFT_WORK"
            and right_result = find "RIGHT_WORK" in
            List.iter [ left_result; right_result ] ~f:(fun value ->
              [%test_eq: string] "pending" (text value "status");
              [%test_eq: string] "assigned" (text (field value "receipt") "status");
              assert (
                List.is_empty (field (field value "output") "items" |> Jsonaf.list_exn)));
            let left = session left_result
            and right = session right_result in
            assert (not (P.Id.Session.equal left right));
            let records =
              Agent_store.Delegation_store.with_records
                (Agent_store.Session_store.delegations (D.store daemon))
                ~max_records:8
                ~max_bytes:1048576
                ~f:(fun records -> Ok records)
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            [%test_eq: int] 2 (List.length records);
            List.iter records ~f:(fun record ->
              assert (P.Id.Session.equal record.key.parent_session_id parent.id);
              match record.admission.authored_tool with
              | Some origin -> [%test_eq: string] "researcher" origin.name
              | None -> failwith "lost authored origin");
            phase := "concurrent deferred continuation";
            let continued =
              invoke_batch
                daemon
                attach
                parent.id
                [ "researcher", named ~session_id:left "Deferred first."
                ; "researcher", named ~session_id:left "Deferred second."
                ]
            in
            let deferred =
              List.map continued ~f:(fun (_, value) ->
                assert (P.Id.Session.equal left (session value));
                [%test_eq: string] "pending" (text value "status");
                [%test_eq: string] "deferred" (text (field value "receipt") "status");
                receipt value)
            in
            [%test_eq: int] 2 !child_calls;
            let generic = invoke daemon attach parent.id "agent_send" (send_args left) in
            [%test_eq: string] "deferred" (text generic "status");
            let replay =
              invoke
                daemon
                attach
                parent.id
                "run_chatml"
                (script "agent_send" (send_args left))
            in
            assert (Jsonaf.exactly_equal generic replay);
            phase := "cancel caller after accepted continuation";
            let before_cancel = (state daemon left).managed_submissions in
            let cancelled_call =
              Eio.Fiber.fork_promise ~sw (fun () ->
                invoke_batch_status
                  daemon
                  attach
                  parent.id
                  [ ( "researcher"
                    , named
                        ~session_id:left
                        "Keep this accepted request after caller cancellation." )
                  ])
            in
            await (fun () -> List.length (state daemon left).managed_submissions = 5);
            let accepted = state daemon left in
            let cancelled_receipt =
              List.filter accepted.managed_submissions ~f:(fun submission ->
                not
                  (List.exists before_cancel ~f:(fun old ->
                     P.History.Id.equal old.M.history_id submission.history_id)))
              |> function
              | [ submission ] -> P.History.Id.to_string submission.M.history_id
              | _ ->
                failwith "cancelled caller duplicated or lost its accepted submission"
            in
            let parent_handle = attach parent.id in
            Exn.protect
              ~finally:(fun () -> H.close parent_handle)
              ~f:(fun () ->
                let operation =
                  Option.value_exn (state daemon parent.id).active_operation
                in
                H.cancel_operation parent_handle operation.id |> protocol_ok |> ignore);
            (match Eio.Promise.await_exn cancelled_call with
             | [ (_, (Published (Cancelled _) | Resolved (Cancelled _))) ] -> ()
             | results ->
               raise_s
                 [%sexp
                   "persistent caller cancellation failed"
                 , (results : (Jsonaf.t * P.Invocation.status) list)]);
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t accepted)
              (Agent_session.Session_state.sexp_of_t (state daemon left));
            let left_receipts =
              receipt left_result
              :: text generic "receipt_id"
              :: cancelled_receipt
              :: deferred
            in
            [%test_eq: int]
              5
              (List.dedup_and_sort left_receipts ~compare:String.compare |> List.length);
            [%test_eq: int] 5 (List.length (state daemon left).managed_submissions);
            [%test_eq: int] 1 (List.length (state daemon right).managed_submissions);
            let waiting =
              invoke
                daemon
                attach
                parent.id
                "agent_wait"
                (wait_args left (List.hd_exn deferred))
            in
            [%test_eq: string] "timeout" (text waiting "reason");
            let pending =
              invoke
                daemon
                attach
                parent.id
                "agent_read"
                (read_args left (List.hd_exn deferred))
            in
            assert (List.is_empty (field pending "items" |> Jsonaf.list_exn));
            let running =
              invoke
                daemon
                attach
                parent.id
                "agent_status"
                (`Object [ "session_id", P.Id.Session.to_json left ])
            in
            [%test_eq: string] "running" (text running "state");
            (* Accepted child work belongs to the daemon, including while every
               requesting HTTP client is disconnected. Release both providers
               only after the original logical connection has closed. *)
            disconnect ();
            phase := "release and correlate";
            Eio.Promise.resolve release_u ();
            await (fun () ->
              List.for_all [ left; right ] ~f:(fun child ->
                let current = state daemon child in
                Option.is_none current.active_operation
                && List.for_all current.managed_submissions ~f:(fun submission ->
                  match submission.M.status with
                  | Terminal (_, Completed) -> true
                  | Terminal (_, _) ->
                    let entry = R.load (D.registry daemon) child |> protocol_ok in
                    let failures =
                      match
                        Agent_session.Durable_event_log.replay
                          entry.durable_events
                          ~after_sequence:0L
                          ~through_sequence:current.counters.event_sequence
                      with
                      | Snapshot_required -> []
                      | Available events ->
                        List.filter_map events ~f:(fun event ->
                          match
                            P.Event.Durable.Payload.of_json
                              ~kind:event.P.Event.Durable.kind
                              event.payload
                            |> protocol_ok
                          with
                          | Operation_failed operation -> Some operation
                          | _ -> None)
                    in
                    raise_s
                      [%sexp
                        "released child failed"
                      , (submission.M.status : M.status)
                      , (failures : P.Operation.t list)]
                  | _ -> false)));
            [%test_eq: int] 3 !child_calls;
            reconnect ();
            let assigned id =
              List.find_exn (state daemon left).managed_submissions ~f:(fun submission ->
                String.equal (P.History.Id.to_string submission.history_id) id)
            in
            (match List.map deferred ~f:(fun id -> (assigned id).status) with
             | [ Terminal (Some first, Completed); Terminal (Some second, Completed) ] ->
               assert (P.Id.Operation.equal first second)
             | _ -> failwith "concurrent deferred receipts did not coalesce");
            List.iter left_receipts ~f:(fun id ->
              let page =
                invoke daemon attach parent.id "agent_read" (read_args left id)
              in
              [%test_eq: string] "completed" (text (field page "receipt") "status");
              let contents = Jsonaf.to_string (field page "items") in
              assert (String.is_substring contents ~substring:"left-result");
              assert (not (String.is_substring contents ~substring:"right-result")));
            let right_receipt = receipt right_result in
            let page =
              invoke
                daemon
                attach
                parent.id
                "run_chatml"
                (script "agent_read" (read_args right right_receipt))
            in
            assert (
              String.is_substring
                (Jsonaf.to_string (field page "items"))
                ~substring:"right-result");
            let history = (state daemon left).conversation.canonical_history in
            let stopped = invoke daemon attach parent.id "agent_stop" (stop_args left) in
            [%test_eq: string] "stopped" (text stopped "progress");
            check_history history (state daemon left).conversation.canonical_history;
            parent.id, left, right, left_receipts, right_receipt, history)
        in
        save "child.chatmd" "<developer>Edited source after pending results.</developer>";
        with_daemon (fun ~disconnect:_ ~reconnect:_ _sw daemon _client attach ->
          phase := "retained pending receipts after restart";
          check_history stopped_history (state daemon left).conversation.canonical_history;
          List.iter
            ((right, right_receipt) :: List.map left_receipts ~f:(fun id -> left, id))
            ~f:(fun (child, id) ->
              let page = invoke daemon attach parent "agent_read" (read_args child id) in
              [%test_eq: string] "completed" (text (field page "receipt") "status");
              assert (not (List.is_empty (field page "items" |> Jsonaf.list_exn))));
          let stopped = invoke daemon attach parent "agent_stop" (stop_args left) in
          [%test_eq: string] "stopped" (text stopped "progress");
          check_history stopped_history (state daemon left).conversation.canonical_history;
          [%test_eq: int] 3 !child_calls)));
  print_endline
    "concurrent first calls create separate durable instances before either finishes";
  print_endline
    "duplicate provider completions do not create extra invocations or children";
  print_endline
    "pending continuation receipts remain distinct, coalesce, and interoperate with \
     generic native/ChatML tools";
  print_endline
    "results stay scoped and readable after stop/restart; send/stop replays add no \
     effects";
  print_endline "cancelling the persistent caller preserves its accepted child request";
  print_endline
    "children finish without an HTTP client; reconnect reads retained receipts";
  [%expect
    {| 
    concurrent first calls create separate durable instances before either finishes
    duplicate provider completions do not create extra invocations or children
    pending continuation receipts remain distinct, coalesce, and interoperate with generic native/ChatML tools
    results stay scoped and readable after stop/restart; send/stop replays add no effects
    cancelling the persistent caller preserves its accepted child request
    children finish without an HTTP client; reconnect reads retained receipts
  |}]
;;
