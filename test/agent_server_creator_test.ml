open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

type failure_state =
  { session_id : P.Id.Session.t
  ; revision : int64
  ; desired : P.Session.desired_state
  ; lifecycle : string
  ; operation : (P.Id.Operation.t * string) option
  ; pending_permissions : int
  ; invocations : (string * string) list
  ; failure : P.Error.code option
  }
[@@deriving sexp_of]

let request ~key ~tools source =
  `Object
    [ "version", `Number "1"
    ; "root_file", `String "child.chatmd"
    ; ( "sources"
      , `Array [ `Object [ "path", `String "child.chatmd"; "text", `String source ] ] )
    ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
    ; "start_immediately", `True
    ; "idempotency_key", `String key
    ]
;;

let state (entry : R.entry) = A.state entry.actor |> protocol_ok
let field json name = Jsonaf.member_exn name json

let session_id json =
  field json "session_id" |> Jsonaf.string_exn |> P.Id.Session.of_string |> protocol_ok
;;

let%expect_test
    "native creator and nested ChatML creation use actual caller authority and stable \
     persisted IDs"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let source =
          {|<developer>Create children.</developer>
<tool name="agent_create"/><tool name="agent_status"/><tool name="run_chatml"/>
<tool name="read_file"><read id="data" path="${workspace}"/></tool><tool name="append_to_file"/>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="false" arbitrary_code="false" privilege_change="false"><read path="${workspace}"/></capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
  <audit format="none"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct" command="/bin/echo private-permission-command" result="stdout"/>|}
        in
        let prompt = Filename.concat root "parent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          source;
        let configuration =
          config ~profile:{ permission_profile with tool_default = Ask } root root prompt
        in
        let phase = ref "startup" in
        let started = ref (Eio.Time.now (Eio.Stdenv.clock env)) in
        let phases = Queue.create () in
        let last_state = ref None in
        let mark_phase next =
          if not (String.equal next !phase)
          then (
            phase := next;
            Queue.enqueue phases (next, Eio.Time.now (Eio.Stdenv.clock env) -. !started);
            if Queue.length phases > 16 then ignore (Queue.dequeue_exn phases : _))
        in
        let state entry =
          let current = state entry in
          let lifecycle =
            match current.lifecycle.observed with
            | Stopped -> "stopped"
            | Queued_for_slot -> "queued"
            | Starting -> "starting"
            | Recovering -> "recovering"
            | Idle -> "idle"
            | Running_turn _ -> "running"
            | Compacting _ -> "compacting"
            | Waiting_for_permission _ -> "waiting_permission"
            | Stopping -> "stopping"
            | Failed _ -> "failed"
          in
          let operation =
            Option.map current.active_operation ~f:(fun operation ->
              let status =
                match operation.P.Operation.state with
                | Starting -> "starting"
                | Running -> "running"
                | Cancelling -> "cancelling"
                | Completed -> "completed"
                | Failed _ -> "failed"
                | Cancelled -> "cancelled"
                | Interrupted _ -> "interrupted"
              in
              operation.id, status)
          in
          last_state
          := Some
               { session_id = current.identity.session_id
               ; revision = current.counters.revision
               ; desired = current.lifecycle.desired
               ; lifecycle
               ; operation
               ; pending_permissions =
                   List.count current.permissions ~f:(fun permission ->
                     P.Permission.equal_state permission.state Pending)
               ; invocations =
                   List.take current.invocations 8
                   |> List.map ~f:(fun invocation ->
                     let status =
                       match invocation.P.Invocation.status with
                       | Admitted -> "admitted"
                       | Dispatching -> "dispatching"
                       | Resolved _ -> "resolved"
                       | Published _ -> "published"
                     in
                     String.prefix invocation.context.tool_name 96, status)
               ; failure = Option.map current.failure ~f:(fun error -> error.P.Error.code)
               };
          current
        in
        let invocations = ref 0 in
        let queued = ref None
        and calls = ref 0 in
        let provider ~sw:_ ~inputs:_ =
          Int.incr calls;
          match !queued with
          | None -> Stdlib.Seq.empty
          | Some (name, arguments) ->
            queued := None;
            let open Openai.Responses.Response_stream in
            [ Output_item_added
                { item =
                    Function_call
                      { name
                      ; arguments = ""
                      ; call_id = sprintf "create-%d" !calls
                      ; _type = "function_call"
                      ; id = Some "create-item"
                      ; status = None
                      }
                ; output_index = 0
                ; type_ = "response.output_item.added"
                }
            ; Function_call_arguments_done
                { arguments = Jsonaf.to_string arguments
                ; item_id = "create-item"
                ; output_index = 0
                ; type_ = "response.function_call_arguments.done"
                }
            ; Output_item_done
                { item =
                    Function_call
                      { name
                      ; arguments = Jsonaf.to_string arguments
                      ; call_id = sprintf "create-%d" !calls
                      ; _type = "function_call"
                      ; id = Some "create-item"
                      ; status = Some "completed"
                      }
                ; output_index = 0
                ; type_ = "response.output_item.done"
                }
            ]
            |> Stdlib.List.to_seq
        in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let before_start = !calls in
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
                  }
                ()
              |> protocol_ok
            in
            [%test_eq: int] before_start !calls;
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                try
                  started := Eio.Time.now (Eio.Stdenv.clock env);
                  Queue.clear phases;
                  mark_phase "connect";
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                    let client = connection daemon (principal ()) in
                    Exn.protect
                      ~finally:(fun () -> Agent_client.Connection.close client)
                      ~f:(fun () ->
                        mark_phase "initialize connection";
                        initialize client;
                        mark_phase "workflow";
                        f sw daemon client))
                with
                | Eio.Time.Timeout ->
                  raise_s
                    [%sexp
                      "creator fixture timeout"
                    , (!phase : string)
                    , (Eio.Time.now (Eio.Stdenv.clock env) -. !started : float)
                    , (!calls : int)
                    , (!last_state : failure_state option)
                    , (Queue.to_list phases : (string * float) list)]))
        in
        let get daemon id = R.load (D.registry daemon) id |> protocol_ok in
        let invoke sw daemon client id name args =
          Int.incr invocations;
          let step label =
            mark_phase (sprintf "invoke %d %s: %s" !invocations name label)
          in
          step "load";
          let entry = get daemon id in
          let before = state entry in
          let before_calls = !calls in
          step "attach";
          let handle =
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
          queued := Some (name, args);
          step "submit";
          H.send_message
            handle
            { kind = Plain_text; text = "Run the requested workflow."; attachments = [] }
          |> protocol_ok
          |> ignore;
          step "await completion";
          let rec wait () =
            let current = state entry in
            match current.active_operation with
            | Some _ ->
              (* The test operator approves this caller's requested tool only.
                 A status call never approves the separate target child's request. *)
              List.iter current.permissions ~f:(fun permission ->
                match permission.P.Permission.state with
                | Pending ->
                  step "approve permission";
                  H.respond_permission
                    handle
                    ~permission_id:permission.id
                    ~permission_generation:permission.generation
                    ~choice:Approve_once
                    ~reason:None
                  |> protocol_ok
                  |> ignore
                | _ -> ());
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              wait ()
            | None -> current
          in
          let current = wait () in
          step "validate completed invocation";
          [%test_eq: int] (before_calls + 2) !calls;
          step "detach";
          H.close handle;
          let fresh =
            List.filter current.invocations ~f:(fun invocation ->
              not
                (List.exists before.invocations ~f:(fun old ->
                   P.Id.Invocation.equal invocation.P.Invocation.context.id old.context.id)))
          in
          let root_call =
            List.find_exn fresh ~f:(fun invocation ->
              P.Invocation.equal_origin invocation.context.origin Model)
          in
          let output_count state =
            List.count
              state.Agent_session.Session_state.conversation.canonical_history
              ~f:(fun entry -> P.History.equal_kind entry.P.History.kind Tool_output)
          in
          [%test_eq: int] 1 (output_count current - output_count before);
          root_call.status, fresh
        in
        let complete = function
          | P.Invocation.Published (Complete value), _ -> value
          | status, _ ->
            raise_s
              [%sexp
                "creator did not return a stored result", (status : P.Invocation.status)]
        in
        let child_source =
          {|<authoring_context policy="manual"/><developer>Child creator.</developer><config model="child-model" reasoning_effort="low"/>
<tool type="inherited" name="agent_create"/><tool type="inherited" name="agent_status"/><tool type="inherited" name="run_chatml"/><tool type="inherited" name="read_file"/><tool type="inherited" name="fixed_echo"/>|}
        in
        let child_request =
          request
            ~key:"retained-child"
            ~tools:
              [ "agent_create"; "agent_status"; "run_chatml"; "read_file"; "fixed_echo" ]
            child_source
        in
        let root_id, child_id, grandchild_id, revision =
          with_daemon (fun sw daemon client ->
            mark_phase "create parent";
            let parent, _ = create_session ~start_immediately:true client in
            let created =
              invoke sw daemon client parent.id "agent_create" child_request |> complete
            in
            let child_id = session_id created in
            let repeated =
              invoke sw daemon client parent.id "agent_create" child_request |> complete
            in
            assert (P.Id.Session.equal child_id (session_id repeated));
            let changed =
              request
                ~key:"retained-child"
                ~tools:
                  [ "agent_create"
                  ; "agent_status"
                  ; "run_chatml"
                  ; "read_file"
                  ; "fixed_echo"
                  ]
                (child_source ^ "<user>Different initial input.</user>")
            in
            (match invoke sw daemon client parent.id "agent_create" changed with
             | Published (Fail error), _ ->
               [%test_eq: string] "agent.create.conflict" error.code
             | _ -> failwith "native creator changed the retained request payload");
            let replaced_native =
              request
                ~key:"cannot-redeclare"
                ~tools:[ "read_file" ]
                {|<developer>Redefine an inherited reader.</developer><tool name="read_file"><read id="escape" path="/"/></tool>|}
            in
            (match invoke sw daemon client parent.id "agent_create" replaced_native with
             | Published (Fail error), _ ->
               [%test_eq: string] "agent.create.invalid_definition" error.code
             | _ -> failwith "native creator allowed a replacement resource binding");
            let records =
              Agent_store.Delegation_store.with_records
                (Agent_store.Session_store.delegations (D.store daemon))
                ~max_records:8
                ~max_bytes:1048576
                ~f:(fun records -> Ok records)
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            [%test_eq: int] 1 (List.length records);
            [%test_eq: int]
              2
              (List.length (Agent_store.Session_store.list_sessions (D.store daemon)));
            let denied =
              request
                ~key:"cannot-widen"
                ~tools:[ "append_to_file" ]
                {|<tool type="inherited" name="append_to_file"/>|}
            in
            (match invoke sw daemon client child_id "agent_create" denied with
             | Published (Fail error), _ ->
               [%test_eq: string] "capability.not_selected" error.code
             | _ -> failwith "child creator widened its tools");
            [%test_eq: int]
              2
              (List.length (Agent_store.Session_store.list_sessions (D.store daemon)));
            let grandchild_request =
              request
                ~key:"script-child"
                ~tools:[ "read_file"; "fixed_echo" ]
                {|<developer>Script-created grandchild.</developer><tool type="inherited" name="read_file"/><tool type="inherited" name="fixed_echo"/>|}
            in
            let script_request =
              `Object
                [ ( "source"
                  , `String
                      {|let main input =
  let* result = Tool.call("agent_create", input) in
  match result with
  | `Ok(child) -> Task.pure(child)
  | `Error(code) -> Task.fail(code)|}
                  )
                ; "input", grandchild_request
                ; ( "tools"
                  , `Array
                      [ `String "agent_create"
                      ; `String "read_file"
                      ; `String "fixed_echo"
                      ] )
                ]
            in
            let outcome, invocations =
              invoke sw daemon client child_id "run_chatml" script_request
            in
            let grandchild = complete (outcome, invocations) in
            let grandchild_id = session_id grandchild in
            let creator =
              List.find_exn invocations ~f:(fun invocation ->
                String.equal invocation.context.tool_name "agent_create")
            in
            assert (P.Invocation.equal_origin creator.context.origin Script);
            assert (P.Id.Session.equal creator.context.session_id child_id);
            let ledger = Agent_store.Session_store.delegations (D.store daemon) in
            let child_state = state (get daemon grandchild_id) in
            let record =
              Agent_store.Delegation_store.resolve
                ledger
                (Option.value_exn child_state.spec.delegation)
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            assert (P.Id.Session.equal record.key.parent_session_id child_id);
            let inspect caller target =
              invoke
                sw
                daemon
                client
                caller
                "agent_status"
                (`Object [ "session_id", `String (P.Id.Session.to_string target) ])
            in
            let before_status = state (get daemon grandchild_id) in
            let status = inspect child_id grandchild_id |> complete in
            [%test_eq: string] "idle" (field status "state" |> Jsonaf.string_exn);
            assert (
              Jsonaf.exactly_equal (field status "waiting_permissions") (`Number "0"));
            assert (Jsonaf.exactly_equal (field status "operation") `Null);
            assert (P.Id.Session.equal grandchild_id (session_id status));
            (match inspect parent.id grandchild_id with
             | Published (Fail error), _ ->
               [%test_eq: string] "agent.management.denied" error.code;
               assert (Jsonaf.exactly_equal error.details `Null)
             | _ -> failwith "ancestor acquired an unrecorded management relationship");
            (match inspect child_id parent.id with
             | Published (Fail error), _ ->
               [%test_eq: string] "agent.management.denied" error.code
             | _ -> failwith "child could inspect its parent by ID");
            [%test_eq: Sexp.t]
              (Agent_session.Session_state.sexp_of_t before_status)
              (Agent_session.Session_state.sexp_of_t (state (get daemon grandchild_id)));
            let script_status =
              `Object
                [ ( "source"
                  , `String
                      {|let main input =
  let* result = Tool.call("agent_status", input) in
  match result with
  | `Ok(status) -> Task.pure(status)
  | `Error(code) -> Task.fail(code)|}
                  )
                ; ( "input"
                  , `Object [ "session_id", `String (P.Id.Session.to_string child_id) ] )
                ; "tools", `Array [ `String "agent_status" ]
                ]
            in
            let inspected =
              invoke sw daemon client parent.id "run_chatml" script_status |> complete
            in
            assert (P.Id.Session.equal child_id (session_id inspected));
            let grandchild_handle =
              H.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection:client
                ~session_id:grandchild_id
                ~mode:Read_write
                ~subscribe:false
                ()
              |> protocol_ok
            in
            queued := Some ("fixed_echo", `Object []);
            mark_phase "submit shell permission request";
            H.send_message
              grandchild_handle
              { kind = Plain_text
              ; text = "Request the protected shell command."
              ; attachments = []
              }
            |> protocol_ok
            |> ignore;
            let rec await_permission () =
              let current = state (get daemon grandchild_id) in
              match
                List.find current.permissions ~f:(fun permission ->
                  P.Permission.equal_state permission.state Pending)
              with
              | Some permission -> permission
              | None ->
                (match current.active_operation with
                 | None ->
                   raise_s
                     [%sexp
                       "shell request finished without pending permission"
                     , (current.invocations : P.Invocation.t list)]
                 | Some _ ->
                   Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                   await_permission ())
            in
            mark_phase "await shell permission";
            let permission = await_permission () in
            mark_phase "inspect pending permission";
            let waiting = inspect child_id grandchild_id |> complete in
            [%test_eq: string]
              "waiting_for_permission"
              (field waiting "state" |> Jsonaf.string_exn);
            assert (
              Jsonaf.exactly_equal (field waiting "waiting_permissions") (`Number "1"));
            assert (
              not
                (String.is_substring
                   (Jsonaf.to_string waiting)
                   ~substring:"private-permission-command"));
            let still_waiting = state (get daemon grandchild_id) in
            let current_permission =
              List.find_exn still_waiting.permissions ~f:(fun current ->
                P.Id.Permission.equal current.id permission.id)
            in
            assert (P.Permission.equal_state current_permission.state Pending);
            assert (List.is_empty still_waiting.grants);
            mark_phase "stop grandchild";
            H.stop grandchild_handle ~mode:Cancel |> protocol_ok |> ignore;
            let rec await_stopped () =
              match (state (get daemon grandchild_id)).lifecycle.observed with
              | Stopped -> ()
              | _ ->
                Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                await_stopped ()
            in
            await_stopped ();
            mark_phase "inspect stopped grandchild";
            let stopped = inspect child_id grandchild_id |> complete in
            [%test_eq: string] "stopped" (field stopped "state" |> Jsonaf.string_exn);
            assert (
              Jsonaf.exactly_equal (field stopped "waiting_permissions") (`Number "0"));
            mark_phase "restart grandchild";
            H.start grandchild_handle ~queue_if_limited:false |> protocol_ok |> ignore;
            H.close grandchild_handle;
            [%test_eq: int]
              3
              (List.length (Agent_store.Session_store.list_sessions (D.store daemon)));
            parent.id, child_id, grandchild_id, field created "definition_revision")
        in
        with_daemon (fun sw daemon client ->
          let repeated =
            invoke sw daemon client root_id "agent_create" child_request |> complete
          in
          assert (P.Id.Session.equal child_id (session_id repeated));
          assert (Jsonaf.exactly_equal revision (field repeated "definition_revision"));
          let grandchild = state (get daemon grandchild_id) in
          assert (P.Session.equal_desired_state grandchild.lifecycle.desired Running);
          let inspected =
            invoke
              sw
              daemon
              client
              child_id
              "agent_status"
              (`Object [ "session_id", `String (P.Id.Session.to_string grandchild_id) ])
            |> complete
          in
          assert (P.Id.Session.equal grandchild_id (session_id inspected));
          let ledger = Agent_store.Session_store.delegations (D.store daemon) in
          let record =
            Agent_store.Delegation_store.resolve
              ledger
              (Option.value_exn grandchild.spec.delegation)
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
          in
          Agent_store.Delegation_store.revoke ledger record Authority_changed
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
          |> protocol_ok
          |> ignore;
          (match
             invoke
               sw
               daemon
               client
               child_id
               "agent_status"
               (`Object [ "session_id", `String (P.Id.Session.to_string grandchild_id) ])
           with
           | Published (Fail error), _ ->
             [%test_eq: string] "agent.management.denied" error.code;
             assert (Jsonaf.exactly_equal error.details `Null)
           | _ -> failwith "revoked management disclosed child state");
          [%test_eq: int]
            3
            (List.length (Agent_store.Session_store.list_sessions (D.store daemon))));
        Eio.Switch.run (fun sw ->
          let module E = Agent_server.Embedded in
          let embedded =
            E.start
              ~sw
              ~env
              ~daemon_options:
                { D.default_options with
                  qualify_chatml_extensions = true
                ; inference_policy =
                    Agent_server_test_support.inference_policy
                      ~default_model:"fixture-model"
                      ~post_stream:provider
                }
              { prompt_file = prompt
              ; workspace = root
              ; tool_dir = root
              ; home = root
              ; data_root = None
              ; start_immediately = true
              ; permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 128
              }
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> E.close embedded)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                let client = E.connection embedded in
                let id = E.session_id embedded in
                let handle =
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
                queued := Some ("agent_create", child_request);
                H.send_message
                  handle
                  { kind = Plain_text; text = "Try creating a child."; attachments = [] }
                |> protocol_ok
                |> ignore;
                let rec wait () =
                  let snapshot =
                    Agent_client.Connection.request
                      client
                      (Session_get { session_id = id; history = None })
                    |> protocol_ok
                    |> function
                    | P.Public.Result.Session_get snapshot ->
                      P.Public.Snapshot.fields snapshot
                    | _ -> failwith "unexpected snapshot"
                  in
                  match snapshot.session.active_operation with
                  | Some _ ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    wait ()
                  | None -> snapshot
                in
                let snapshot = wait () in
                assert (
                  P.Session.equal_persistence snapshot.session.spec.persistence Transient);
                let outputs =
                  List.filter snapshot.canonical_history.entries ~f:(fun entry ->
                    Option.exists
                      (P.Public.History.header entry)
                      ~f:(Transcript.Header.equal (Result Function)))
                in
                (match outputs with
                 | [ entry ] ->
                   let payload =
                     P.Public.History.full_payload entry |> Option.value_exn
                   in
                   let output =
                     match
                       History_entry.Payload.Semantic.view
                         (History_entry.Payload.semantic payload)
                     with
                     | Result { output = Text text; _ } -> text
                     | _ -> failwith "expected a neutral text tool result"
                   in
                   let outcome =
                     output
                     |> Jsonaf.of_string
                     |> P.Invocation.outcome_of_json
                     |> protocol_ok
                   in
                   (match outcome with
                    | Fail error -> [%test_eq: string] "capability_unavailable" error.code
                    | _ -> failwith "transient host silently created a durable child")
                 | _ -> failwith "expected one transient creator response");
                H.close handle)));
        print_endline
          "native and script creator outcomes publish once; replay retains child \
           ID/revision; child cannot widen tools; grandchild belongs to actual script \
           caller"));
  [%expect
    {| native and script creator outcomes publish once; replay retains child ID/revision; child cannot widen tools; grandchild belongs to actual script caller |}]
;;
