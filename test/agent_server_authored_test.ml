open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module Res = Openai.Responses

type fixture_client =
  { connection : Agent_client.Connection.t
  ; invocation_handles : (P.Id.Session.t, H.t) Hashtbl.t
  }

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn

let state daemon id =
  let entry = R.load (D.registry daemon) id |> protocol_ok in
  A.state entry.actor |> protocol_ok
;;

let function_call ~id name arguments =
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
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item =
          Function_call
            { name
            ; arguments = Jsonaf.to_string arguments
            ; call_id = id
            ; _type = "function_call"
            ; id = Some id
            ; status = Some "completed"
            }
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let answer ~id =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id
    ; status = "completed"
    ; content =
        [ { annotations = []; text = "Specialist finished."; _type = "output_text" } ]
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
      ; delta = "Specialist finished."
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test
    "authored native sessions retain private moderator state over Unix sockets and \
     daemon restart"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save name contents =
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / name)
            contents
        in
        save
          "parent.chatmd"
          [%blob "chatml_extensibility_fixtures/x05-child-session/authored-agent.chatmd"];
        save
          "input.json"
          [%blob "chatml_extensibility_fixtures/x05-child-session/input.json"];
        save
          "output.json"
          [%blob "chatml_extensibility_fixtures/x05-child-session/output.json"];
        save "value.txt" "private-approved-content";
        save
          "researcher.chatmd"
          [%blob "chatml_extensibility_fixtures/x05-child-session/researcher.chatmd"];
        let configuration = config root root (Filename.concat root "parent.chatmd") in
        let queued = ref None in
        let child_calls = ref 0 in
        let calls = ref 0 in
        let child_preflight = ref (fun () -> ()) in
        let provider ~sw:_ ~inputs =
          Int.incr calls;
          let child =
            List.exists inputs ~f:(function
              | Res.Item.Input_message message ->
                let json = Res.Item.jsonaf_of_t (Input_message message) in
                String.equal (text json "role") "developer"
                && String.is_substring
                     (Jsonaf.to_string json)
                     ~substring:"AUTHORED_SPECIALIST"
              | _ -> false)
          in
          match child with
          | true ->
            let preflight = !child_preflight in
            (child_preflight := fun () -> ());
            preflight ();
            Int.incr child_calls;
            (match !child_calls mod 2 with
             | 1 -> function_call ~id:(sprintf "counter-%d" !calls) "counter" (`Object [])
             | _ -> answer ~id:(sprintf "answer-%d" !calls))
          | false ->
            (match !queued with
             | None -> Stdlib.Seq.empty
             | Some (name, args) ->
               queued := None;
               function_call ~id:(sprintf "parent-%d" !calls) name args)
        in
        let phase = ref "startup" in
        let diagnostic =
          Failure_diagnostic.create ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
        in
        let invocation_ordinal = ref 0 in
        let state daemon id =
          let current = state daemon id in
          Failure_diagnostic.observe diagnostic current;
          current
        in
        (* Each required operation has its own progress deadline. The workflow
           has many independent invocations; their accumulated CPU time is not
           a bound on any single invocation's liveness. *)
        let run_phase name f =
          phase := name;
          Failure_diagnostic.mark diagnostic ("phase: " ^ name);
          try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. f with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            Eio.Cancel.protect (fun () ->
              Failure_diagnostic.report
                diagnostic
                ~context:
                  [%sexp
                    "authored fixture failure"
                  , (name : string)
                  , (Exn.to_string exn : string)
                  , (!phase : string)
                  , (!invocation_ordinal : int)
                  , (!calls : int)
                  , (!child_calls : int)]);
            Exn.raise_with_original_backtrace exn backtrace
        in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            Failure_diagnostic.reset diagnostic;
            let before = !calls in
            let daemon =
              run_phase "start daemon" (fun () ->
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
                    }
                  ()
                |> protocol_ok)
            in
            [%test_eq: int] before !calls;
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                let client =
                  { connection =
                      run_phase "connect client" (fun () ->
                        Agent_server_wire_fixture.connect_unix
                          ~sw
                          ~env
                          ~daemon
                          ~socket_path:(Filename.concat root "authored.sock"))
                  ; invocation_handles = Hashtbl.create (module P.Id.Session)
                  }
                in
                Exn.protect
                  ~finally:(fun () ->
                    Eio.Cancel.protect (fun () ->
                      Exn.protect
                        ~finally:(fun () ->
                          Agent_client.Connection.close client.connection)
                        ~f:(fun () ->
                          let handles = Hashtbl.data client.invocation_handles in
                          Hashtbl.clear client.invocation_handles;
                          let rec close = function
                            | [] -> ()
                            | handle :: rest ->
                              Exn.protect
                                ~finally:(fun () -> close rest)
                                ~f:(fun () ->
                                  (* Shield cleanup from the failed phase while
                                     giving this detach its own cancellable scope. *)
                                  run_phase "detach invocation writer" (fun () ->
                                    H.close handle))
                          in
                          close handles)))
                  ~f:(fun () ->
                    run_phase "initialize client" (fun () -> initialize client.connection);
                    f sw daemon client)))
        in
        (* Writers belong to the connection's daemon generation. Reusing them
           avoids copying the growing transcript for each tool invocation. *)
        let invocation_handle sw client parent =
          Hashtbl.find_or_add client.invocation_handles parent ~default:(fun () ->
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client.connection
              ~session_id:parent
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok)
        in
        let invoke_status sw daemon client parent name args =
          run_phase ("invoke " ^ name) (fun () ->
            phase := name;
            Int.incr invocation_ordinal;
            let mark stage =
              Failure_diagnostic.mark
                diagnostic
                (sprintf "invocation %d %s: %s" !invocation_ordinal name stage)
            in
            mark "read before";
            let before = state daemon parent in
            mark "attach";
            let handle = invocation_handle sw client parent in
            mark "send";
            queued := Some (name, args);
            H.send_message
              handle
              { kind = Plain_text
              ; text = "Run the requested specialist operation."
              ; attachments = []
              }
            |> protocol_ok
            |> ignore;
            mark "wait for inactive parent";
            let rec wait () =
              let current = state daemon parent in
              match current.active_operation with
              | Some _ ->
                Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                wait ()
              | None -> current
            in
            let current = wait () in
            mark "read invocation outcome";
            let fresh =
              List.filter current.invocations ~f:(fun invocation ->
                not
                  (List.exists before.invocations ~f:(fun old ->
                     P.Id.Invocation.equal
                       invocation.P.Invocation.context.id
                       old.context.id)))
            in
            let invocation =
              List.find_exn fresh ~f:(fun invocation ->
                P.Invocation.equal_origin invocation.context.origin Model)
            in
            invocation.status)
        in
        let invoke sw daemon client parent name args =
          match invoke_status sw daemon client parent name args with
          | Published (Complete value) -> value
          | status ->
            raise_s [%sexp "authored call failed", (status : P.Invocation.status)]
        in
        let denied sw daemon client parent name args =
          match invoke_status sw daemon client parent name args with
          | Published (Fail error) ->
            [%test_eq: string] "agent.authored.permission_denied" error.code
          | status ->
            raise_s [%sexp "foreign authored ID accepted", (status : P.Invocation.status)]
        in
        let call sw daemon client parent ?session_id input =
          let fields = [ "input", `String input; "mode", `String "persistent" ] in
          let fields =
            match session_id with
            | None -> fields
            | Some id -> fields @ [ "session_id", P.Id.Session.to_json id ]
          in
          let result = invoke sw daemon client parent "researcher" (`Object fields) in
          [%test_eq: string] "completed" (text result "status");
          field result "session_id" |> P.Id.Session.of_json |> protocol_ok
        in
        let check_counter daemon child expected =
          run_phase ("check private counter " ^ expected) (fun () ->
            let current = state daemon child in
            let values =
              List.filter_map current.invocations ~f:(fun invocation ->
                match invocation.status with
                | Published (Complete (`String value))
                  when String.is_prefix value ~prefix:"count-" -> Some value
                | _ -> None)
            in
            assert (List.mem values expected ~equal:String.equal);
            let reads =
              List.filter_map current.invocations ~f:(fun invocation ->
                match invocation.status with
                | (Published (Complete value) | Resolved (Complete value))
                  when String.is_substring
                         (Jsonaf.to_string value)
                         ~substring:"private-approved-content" -> Some value
                | _ -> None)
            in
            assert (not (List.is_empty reads)))
        in
        let one_off_children = ref [] in
        let one_off sw daemon client caller =
          run_phase "one-off lifecycle" (fun () ->
            let records () =
              Agent_store.Delegation_store.with_records
                (Agent_store.Session_store.delegations (D.store daemon))
                ~max_records:128
                ~max_bytes:1048576
                ~f:(fun records -> Ok records)
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            let before = records () in
            let value =
              invoke
                sw
                daemon
                client
                caller
                "researcher"
                (`Object [ "input", `String "A default one-off request." ])
            in
            assert (Jsonaf.exactly_equal value (`String "Specialist finished."));
            let record =
              records ()
              |> List.filter ~f:(fun record ->
                not
                  (List.exists before ~f:(fun old ->
                     P.Id.Session.equal
                       old.Agent_store.Delegation_store.admission.child_session_id
                       record.admission.child_session_id)))
              |> function
              | [ record ] -> record
              | _ -> failwith "one-off did not create exactly one admitted child"
            in
            (match record.admission.lifetime with
             | Invocation_owned { invocation_id } ->
               let owner =
                 List.find_exn (state daemon caller).invocations ~f:(fun invocation ->
                   P.Id.Invocation.equal invocation.context.id invocation_id)
               in
               (match owner.status with
                | Published (Complete _) -> ()
                | _ -> failwith "one-off owner was not published")
             | _ -> failwith "one-off received reusable lifetime");
            let child = record.admission.child_session_id in
            let stopped = state daemon child in
            (match stopped.lifecycle.desired, stopped.lifecycle.observed with
             | Stopped, Stopped -> ()
             | _ -> failwith "one-off child cleanup did not finish");
            assert (Option.is_none stopped.active_operation);
            check_counter daemon child "count-1";
            denied
              sw
              daemon
              client
              caller
              "researcher"
              (`Object
                  [ "input", `String "Cannot continue a one-off."
                  ; "mode", `String "persistent"
                  ; "session_id", P.Id.Session.to_json child
                  ]);
            one_off_children := child :: !one_off_children)
        in
        let parent, child, caller, grandchild =
          with_daemon (fun sw daemon client ->
            let parent, _ =
              run_phase "create parent session" (fun () ->
                create_session ~start_immediately:true client.connection)
            in
            let child = call sw daemon client parent.id "First request." in
            check_counter daemon child "count-1";
            let continued =
              call sw daemon client parent.id ~session_id:child "Second request."
            in
            assert (P.Id.Session.equal child continued);
            check_counter daemon child "count-2";
            let separate = call sw daemon client parent.id "A separate instance." in
            assert (not (P.Id.Session.equal child separate));
            check_counter daemon separate "count-1";
            check_counter daemon child "count-2";
            let fixed =
              invoke
                sw
                daemon
                client
                parent.id
                "reviewer"
                (`Object [ "input", `String "Fixed persistent mode." ])
            in
            [%test_eq: string] "completed" (text fixed "status");
            let fixed_id =
              field fixed "session_id" |> P.Id.Session.of_json |> protocol_ok
            in
            check_counter daemon fixed_id "count-1";
            denied
              sw
              daemon
              client
              parent.id
              "reviewer"
              (`Object
                  [ "input", `String "Wrong declaration."
                  ; "session_id", P.Id.Session.to_json child
                  ]);
            let caller =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_create"
                (`Object
                    [ "version", `Number "1"
                    ; "root_file", `String "caller.chatmd"
                    ; ( "sources"
                      , `Array
                          [ `Object
                              [ "path", `String "caller.chatmd"
                              ; ( "text"
                                , `String
                                    {|<authoring_context policy="manual"/><developer>INHERITED_CALLER</developer><tool type="inherited" name="researcher"/>|}
                                )
                              ]
                          ] )
                    ; "tools", `Array [ `String "researcher" ]
                    ; "start_immediately", `True
                    ; "idempotency_key", `String "authored-caller"
                    ])
            in
            let caller =
              field caller "session_id" |> P.Id.Session.of_json |> protocol_ok
            in
            denied
              sw
              daemon
              client
              caller
              "researcher"
              (`Object
                  [ "input", `String "Another caller's instance."
                  ; "mode", `String "persistent"
                  ; "session_id", P.Id.Session.to_json child
                  ]);
            let grandchild =
              call sw daemon client caller "Through an inherited wrapper."
            in
            check_counter daemon grandchild "count-1";
            denied
              sw
              daemon
              client
              parent.id
              "researcher"
              (`Object
                  [ "input", `String "Only the actual caller owns this instance."
                  ; "mode", `String "persistent"
                  ; "session_id", P.Id.Session.to_json grandchild
                  ]);
            let status =
              invoke
                sw
                daemon
                client
                parent.id
                "agent_status"
                (`Object [ "session_id", P.Id.Session.to_json child ])
            in
            [%test_eq: string] "idle" (text status "state");
            one_off sw daemon client parent.id;
            one_off sw daemon client caller;
            run_phase "cancel one-off and verify joined cleanup" (fun () ->
              let entered, entered_u = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let provider_cleaned = ref false in
              (child_preflight
               := fun () ->
                    Exn.protect
                      ~finally:(fun () -> provider_cleaned := true)
                      ~f:(fun () ->
                        Eio.Promise.resolve entered_u ();
                        Eio.Promise.await never));
              let handle = invocation_handle sw client parent.id in
              Eio.Switch.run (fun cancellation_sw ->
                let cancelled, cancelled_u = Eio.Promise.create () in
                Eio.Fiber.fork ~sw:cancellation_sw (fun () ->
                  Eio.Promise.await entered;
                  Failure_diagnostic.mark diagnostic "cancel entered child provider";
                  let operation =
                    Option.value_exn (state daemon parent.id).active_operation
                  in
                  H.cancel_operation handle operation.id |> protocol_ok |> ignore;
                  Failure_diagnostic.mark diagnostic "parent cancellation accepted";
                  Eio.Promise.resolve cancelled_u ());
                (match
                   invoke_status
                     sw
                     daemon
                     client
                     parent.id
                     "researcher"
                     (`Object
                         [ "input", `String "Cancel while the specialist is working." ])
                 with
                 | Published (Cancelled _) | Resolved (Cancelled _) -> ()
                 | status ->
                   raise_s
                     [%sexp "one-off cancellation failed", (status : P.Invocation.status)]);
                Eio.Promise.await cancelled);
              assert !provider_cleaned;
              let owned_records =
                Agent_store.Delegation_store.with_records
                  (Agent_store.Session_store.delegations (D.store daemon))
                  ~max_records:128
                  ~max_bytes:1048576
                  ~f:(fun records -> Ok records)
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
              in
              List.iter owned_records ~f:(fun record ->
                match record.admission.lifetime with
                | Invocation_owned _ ->
                  let current = state daemon record.admission.child_session_id in
                  (match current.lifecycle.desired, current.lifecycle.observed with
                   | Stopped, Stopped -> ()
                   | _ -> failwith "cancellation left a one-off child running")
                | _ -> ()));
            parent.id, child, caller, grandchild)
        in
        (* A new catalog revision must not replace either saved specialist's
           captured definition or its inherited wrapper after restart. *)
        save
          "researcher.chatmd"
          "<developer>Replacement specialist without a counter.</developer>";
        with_daemon (fun sw daemon client ->
          let continued =
            call sw daemon client parent ~session_id:child "After restart."
          in
          assert (P.Id.Session.equal child continued);
          check_counter daemon child "count-3";
          let continued =
            call sw daemon client caller ~session_id:grandchild "Inherited after restart."
          in
          assert (P.Id.Session.equal grandchild continued);
          check_counter daemon grandchild "count-2";
          List.iter !one_off_children ~f:(fun id ->
            run_phase "verify one-off remains stopped after restart" (fun () ->
              let stopped = state daemon id in
              [%test_eq: P.Session.desired_state] Stopped stopped.lifecycle.desired;
              check_counter daemon id "count-1")));
        print_endline
          "authored wrapper creates and continues a persisted child; generic management \
           sees it";
        print_endline "private tool use and moderator counter survive daemon restart";
        print_endline
          "fixed and optional wrappers preserve declaration identity and actual caller \
           ownership";
        print_endline
          "default one-off calls return text, join child cleanup, and remain stopped \
           after restart";
        print_endline "cancelling a one-off caller joins its blocked child provider"));
  [%expect
    {|
    authored wrapper creates and continues a persisted child; generic management sees it
    private tool use and moderator counter survive daemon restart
    fixed and optional wrappers preserve declaration identity and actual caller ownership
    default one-off calls return text, join child cleanup, and remain stopped after restart
    cancelling a one-off caller joins its blocked child provider
    |}]
;;
