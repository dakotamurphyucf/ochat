open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module Res = Openai.Responses

let field = Jsonaf.member_exn
let text json name = field name json |> Jsonaf.string_exn

let helper_request =
  [%blob "chatml_extensibility_fixtures/x07-helper-session/request.chatml"]
;;

let helper_moderator =
  [%blob "chatml_extensibility_fixtures/x07-helper-session/moderator.chatml"]
;;

let helper_tools = [%blob "chatml_extensibility_fixtures/x07-helper-session/tools.chatmd"]
let helper_schema = [%blob "chatml_extensibility_fixtures/x07-helper-session/any.json"]
let watcher = [%blob "chatml_extensibility_fixtures/x06-response-watcher/watcher.chatml"]

let watch_probe =
  [%blob "chatml_extensibility_fixtures/x06-response-watcher/probe.chatml"]
;;

let watch_native =
  [%blob "chatml_extensibility_fixtures/x06-response-watcher/native-request.chatml"]
;;

let watch_input = [%blob "chatml_extensibility_fixtures/x06-response-watcher/input.json"]

let watch_tools =
  [%blob "chatml_extensibility_fixtures/x06-response-watcher/tools.chatmd"]
;;

let coordinator =
  helper_moderator
  ^ "\nlet helper_initial_state = initial_state\nlet helper_on_event = on_event\n"
  ^ watcher
  ^ {|
type coordinator_state = { helpers : request array; watches : response_watch array }
let initial_state : coordinator_state = { helpers = helper_initial_state; watches = watch_initial_state }
let on_event ctx state event =
  let state : coordinator_state = state in
  let* helpers = helper_on_event(ctx, state.helpers, event) in
  let* watches = watch_on_event(ctx, state.watches, event) in
  Task.pure({ helpers = helpers; watches = watches })
|}
;;

let state daemon id =
  let entry = R.load (D.registry daemon) id |> protocol_ok in
  A.state entry.actor |> protocol_ok
;;

let function_call name arguments =
  let open Res.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name
            ; arguments = ""
            ; call_id = "helper-call"
            ; _type = "function_call"
            ; id = Some "helper-item"
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = "helper-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let run env helper runner ~native_watch =
  Mirage_crypto_rng_unix.use_default ();
  let root = temporary_root env |> Caml_unix.realpath in
  let path file = Eio.Path.(Eio.Stdenv.fs env / file) in
  Exn.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (path root))
    ~f:(fun () ->
      let public = Filename.concat root "public" in
      Eio.Path.mkdir ~perm:0o700 (path public);
      let helper_path = Filename.concat public "helper" in
      let runner_path = Filename.concat public "runner" in
      List.iter
        [ helper, helper_path; runner, runner_path ]
        ~f:(fun (source, target) ->
          Eio.Path.save
            ~create:(`Exclusive 0o700)
            (path target)
            (Eio.Path.load (path source)));
      Caml_unix.putenv "OCHAT_SHELL_RESOURCE_RUNNER" runner_path;
      List.iter
        [ "helper-request.chatml", helper_request
        ; ( "authored-helper.chatmd"
          , "<developer>HELPER_CHILD authored specialist.</developer>" )
        ; "helper-moderator.chatml", coordinator
        ; "helper-any.json", helper_schema
        ; "watch-probe.chatml", watch_probe
        ; "watch-native.chatml", watch_native
        ; "watch-input.json", watch_input
        ]
        ~f:(fun (name, contents) ->
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            (path (Filename.concat root name))
            contents);
      let prompt = Filename.concat root "parent.chatmd" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        (path prompt)
        ({|<developer>HELPER_PARENT</developer>
<tool name="specialist" agent="authored-helper.chatmd" local persistence="persistent"/>
<tool name="run_chatml"/>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<shell_access id="helper" cwd="${workspace}">
  <capabilities sandbox="required" network="false" child_processes="true" arbitrary_code="true" privilege_change="false"><read path="${workspace}"/></capabilities>
  <environment inherit="selected"><set name="PATH" value="/usr/bin:/bin"/></environment>
  <policy default="allow"/>
  <limits wall_time="30s" idle_time="none"/>
  <audit format="none"/>
</shell_access>
<tool name="session_bridge" type="shell" mode="fixed" runtime="helper" command="./helper" stdin="required" result="stdout"/>
<tool name="session_view" type="shell" mode="fixed" runtime="helper" command="./helper" stdin="required" result="stdout"/>|}
         ^ helper_tools
         ^ watch_tools
         ^
         match native_watch with
         | false ->
           {|<tool name="watch_session_request" type="chatml" script="session_request_script" entrypoint="run" input_schema="helper-any.json" output_schema="helper-any.json"><uses tool="session_bridge"/></tool>|}
         | true ->
           {|<tool name="agent_wait"/><tool name="agent_read"/><tool name="agent_status"/>
<script id="watch_native_script" language="chatml" kind="tool" src="watch-native.chatml"/>
<tool name="watch_session_request" type="chatml" script="watch_native_script" entrypoint="run" input_schema="helper-any.json" output_schema="helper-any.json"><uses tool="agent_wait"/><uses tool="agent_read"/><uses tool="agent_status"/></tool>|}
        );
      let configuration = config root public prompt in
      let configuration =
        { configuration with
          workspaces =
            List.map configuration.workspaces ~f:(fun workspace ->
              { workspace with
                prompt_limits =
                  List.map workspace.prompt_limits ~f:(fun limit ->
                    { limit with max_root_agents = 2 })
              })
        }
      in
      let authorization_times = ref [] in
      let grant name allowed =
        Agent_session.Session_management_channel.grant
          ~policy_revision:"helper-fixture-v1"
          ~tool_name:name
          ~allowed
          ~limits:Shell_access.Request_channel.default_limits
          ~authorize:(fun context ->
            authorization_times
            := (name, Eio.Time.now (Eio.Stdenv.clock env)) :: !authorization_times;
            let caps = context.Shell_access.Context.capabilities in
            match
              String.equal context.executable.canonical_path helper_path
              && List.equal
                   String.equal
                   (List.map caps.read_roots ~f:(String.rstrip ~drop:(Char.equal '/')))
                   [ public ]
              && List.is_empty caps.write_roots
              && Array.for_all context.environment ~f:(fun entry ->
                List.mem
                  [ "PATH=/bin:/usr/bin"
                  ; "PAGER=cat"
                  ; "GIT_PAGER=cat"
                  ; "TERM=dumb"
                  ; "NO_COLOR=1"
                  ]
                  entry
                  ~equal:String.equal)
            with
            | true -> Ok ()
            | false -> Error "helper must retain the exact public-only fixture boundary")
        |> Result.ok_or_failwith
      in
      let grants =
        [ grant "session_bridge" [ Create; Send; Read; Status; Wait; Stop ]
        ; grant "session_view" [ Read; Status; Wait ]
        ]
      in
      let queued = ref None in
      let child_tool = ref None in
      let child_calls = ref 0 in
      let child_pause = ref None in
      let private_file = Filename.concat root "private-fixture.txt" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        (path private_file)
        "HELPER_PRIVATE_FIXTURE_CONTENT";
      let provider ~sw:_ ~inputs =
        let child =
          List.exists inputs ~f:(function
            | Res.Item.Input_message message ->
              let json = Res.Item.jsonaf_of_t (Input_message message) in
              String.equal (text json "role") "developer"
              && String.is_substring (Jsonaf.to_string json) ~substring:"HELPER_CHILD"
            | _ -> false)
        in
        (match child with
         | true ->
           Int.incr child_calls;
           Option.iter !child_pause ~f:Eio.Promise.await
         | false -> ());
        match child with
        | false ->
          (match !queued with
           | None -> Stdlib.Seq.empty
           | Some (name, args) ->
             queued := None;
             function_call name args)
        | true when Option.is_some !child_tool ->
          let arguments = Option.value_exn !child_tool in
          child_tool := None;
          function_call "read_file" arguments
        | true ->
          let answer = "persisted helper answer" in
          let message : Res.Output_message.t =
            { role = Assistant
            ; id = "helper-answer"
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
      let with_daemon ?(grants = grants) f =
        Eio.Switch.run (fun sw ->
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
                ; session_helpers = grants
                ; model_post_stream = Some provider
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    f sw daemon client))))
      in
      let invoke_status sw daemon client parent name arguments =
        let before = state daemon parent in
        let handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:parent
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        queued := Some (name, arguments);
        H.send_message
          handle
          { kind = Plain_text; text = "Run the helper workflow."; attachments = [] }
        |> protocol_ok
        |> ignore;
        await (fun () ->
          Option.is_none (state daemon parent).active_operation && Option.is_none !queued);
        H.close handle;
        let after = state daemon parent in
        let invocation =
          List.find_exn after.invocations ~f:(fun invocation ->
            P.Invocation.equal_origin invocation.context.origin Model
            && not
                 (List.exists before.invocations ~f:(fun old ->
                    P.Id.Invocation.equal old.context.id invocation.context.id)))
        in
        invocation.status
      in
      let invoke sw daemon client parent name arguments =
        match invoke_status sw daemon client parent name arguments with
        | Published (Complete (`String source)) -> source
        | status ->
          raise_s [%sexp "helper invocation failed", (status : P.Invocation.status)]
      in
      let bridge ?(name = "session_bridge") sw daemon client parent operation arguments =
        let envelope =
          `Object
            [ "version", `Number "1"
            ; "operation", `String operation
            ; "arguments", arguments
            ]
        in
        let source =
          invoke
            sw
            daemon
            client
            parent
            name
            (`Object [ "stdin", `String (Jsonaf.to_string envelope) ])
        in
        match Jsonaf.of_string source |> P.Invocation.outcome_of_json with
        | Ok outcome -> outcome
        | Error error -> failwith (error.message ^ ": " ^ source)
      in
      let complete = function
        | P.Invocation.Complete value -> value
        | outcome ->
          raise_s [%sexp "bridge application failed", (outcome : P.Invocation.outcome)]
      in
      let id value = text value "session_id" |> P.Id.Session.of_string |> protocol_ok in
      let check_notification daemon parent child =
        let current = state daemon parent in
        let delivery =
          match
            List.filter current.deliveries ~f:(fun delivery ->
              String.equal delivery.context.correlation "agent-helper-result")
          with
          | [ delivery ] -> delivery
          | deliveries ->
            raise_s
              [%sexp "expected one helper notification", (deliveries : P.Delivery.t list)]
        in
        (match delivery.context.completion with
         | Succeeded value -> assert (P.Id.Session.equal child (id value))
         | completion ->
           raise_s [%sexp "unexpected helper completion", (completion : P.Completion.t)]);
        let notifications =
          List.filter current.conversation.canonical_history ~f:(fun entry ->
            match entry.P.History.provenance with
            | Runtime_notification _ ->
              (match delivery.status with
               | Committed { history_id; _ } -> History_entry.Id.equal history_id entry.id
               | _ -> false)
            | _ -> false)
        in
        [%test_eq: int] 1 (List.length notifications);
        Agent_session.Notification_history.validate ~delivery (List.hd_exn notifications)
        |> protocol_ok
      in
      let child_request =
        `Object
          [ "version", `Number "1"
          ; "root_file", `String "child.chatmd"
          ; ( "sources"
            , `Array
                [ `Object
                    [ "path", `String "child.chatmd"
                    ; ( "text"
                      , `String
                          {|<authoring_context policy="manual"/><developer>HELPER_CHILD</developer><tool type="inherited" name="read_file"/>|}
                      )
                    ]
                ] )
          ; "tools", `Array [ `String "read_file" ]
          ; "start_immediately", `True
          ; "idempotency_key", `String "helper-child"
          ]
      in
      let watch_subscription daemon parent subscription_id =
        List.find_exn (state daemon parent).subscriptions ~f:(fun subscription ->
          P.Id.Subscription.equal subscription.context.id subscription_id)
      in
      let start_watch sw daemon client parent query =
        match
          invoke_status
            sw
            daemon
            client
            parent
            "notify_when_agent_responds"
            (`Object query)
        with
        | Published (Pending (Subscription id, _)) -> id
        | status ->
          raise_s
            [%sexp "watch did not return a subscription", (status : P.Invocation.status)]
      in
      let await_timer daemon parent subscription_id =
        await (fun () ->
          let subscription = watch_subscription daemon parent subscription_id in
          match subscription.result with
          | Some result ->
            raise_s
              [%sexp "watch ended before delayed response", (result : P.Completion.t)]
          | None -> Option.is_some subscription.timer_id)
      in
      let await_watch ?(require_wake = true) daemon parent subscription_id =
        await (fun () ->
          let current = state daemon parent in
          Option.is_none current.active_operation
          && List.exists current.deliveries ~f:(fun delivery ->
            match delivery.context.work, delivery.status, delivery.wake_disposition with
            | Some (Subscription id), Committed _, _ when not require_wake ->
              P.Id.Subscription.equal id subscription_id
            | Some (Subscription id), Committed _, Some (Accepted_wake _) ->
              P.Id.Subscription.equal id subscription_id
            | Some (Subscription id), Committed _, None
              when P.Completion.equal_wake delivery.context.wake No_wake ->
              P.Id.Subscription.equal id subscription_id
            | Some (Subscription id), _, Some (Discarded_wake reason)
              when P.Id.Subscription.equal id subscription_id ->
              failwith ("watch wake rejected: " ^ reason)
            | Some (Subscription id), Failed error, _
              when P.Id.Subscription.equal id subscription_id ->
              failwith ("watch delivery failed: " ^ error.message)
            | _ -> false));
        (watch_subscription daemon parent subscription_id).result |> Option.value_exn
      in
      let check_watch_notifications daemon parent =
        let current = state daemon parent in
        List.iter current.subscriptions ~f:(fun subscription ->
          let completion = Option.value_exn subscription.result in
          let deliveries =
            List.filter current.deliveries ~f:(fun delivery ->
              match delivery.context.work with
              | Some (Subscription id) ->
                P.Id.Subscription.equal id subscription.context.id
              | _ -> false)
          in
          [%test_eq: int] 1 (List.length deliveries);
          let delivery = List.hd_exn deliveries in
          assert (P.Completion.equal completion delivery.context.completion);
          let history_id =
            match delivery.status with
            | Committed { history_id; _ } -> history_id
            | status ->
              raise_s
                [%sexp "watch notification not committed", (status : P.Delivery.status)]
          in
          let entry =
            List.find_exn current.conversation.canonical_history ~f:(fun entry ->
              History_entry.Id.equal entry.id history_id)
          in
          Agent_session.Notification_history.validate ~delivery entry |> protocol_ok)
      in
      let authored_bridge sw daemon client parent foreign caller =
        let owner, name =
          match caller with
          | Agent_server_authored_helper_fixture.Owner -> parent, "session_bridge"
          | Read_only -> parent, "session_view"
          | Foreign -> foreign, "session_bridge"
        in
        bridge ~name sw daemon client owner
      in
      let parent_id, child_id, receipt, authored, foreign_id =
        with_daemon (fun sw daemon client ->
          let parent, _ = create_session ~start_immediately:true client in
          await (fun () -> Option.is_none (state daemon parent.id).active_operation);
          let entry = R.load (D.registry daemon) parent.id |> protocol_ok in
          Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
            let native = Option.value_exn runtime.native_runtime in
            let registry =
              Lazy.force native.capabilities
              |> Result.map_error ~f:(fun error ->
                error.Chat_response.Tool_capability.message)
              |> Result.ok_or_failwith
            in
            List.iter
              [ "agent_create"
              ; "agent_send"
              ; "agent_read"
              ; "agent_status"
              ; "agent_wait"
              ; "agent_stop"
              ]
              ~f:(fun name ->
                let present =
                  Result.is_ok (Chat_response.Tool_capability.find registry ~name)
                in
                [%test_eq: bool]
                  (native_watch
                   && List.mem
                        [ "agent_read"; "agent_wait"; "agent_status" ]
                        name
                        ~equal:String.equal)
                  present);
            Ok ())
          |> protocol_ok;
          let create_envelope =
            `Object
              [ "version", `Number "1"
              ; "operation", `String "create"
              ; "arguments", child_request
              ]
          in
          let created =
            authorization_times := [];
            let creation_started = Eio.Time.now (Eio.Stdenv.clock env) in
            let job_id =
              match
                invoke_status sw daemon client parent.id "manage_agent" create_envelope
              with
              | Published (Pending (Job job, _)) -> job
              | status ->
                raise_s
                  [%sexp "expected asynchronous helper", (status : P.Invocation.status)]
            in
            await (fun () ->
              let current = state daemon parent.id in
              List.iter current.jobs ~f:(fun job ->
                match job.status with
                | Failed _ | Cancelled | Interrupted _ ->
                  let authorization_checkpoints =
                    List.rev_map !authorization_times ~f:(fun (name, time) ->
                      name, time -. creation_started)
                  in
                  raise_s
                    [%sexp
                      "asynchronous helper failed"
                    , (authorization_checkpoints : (string * float) list)
                    , (job : P.Job.t)]
                | _ -> ());
              Option.is_none current.active_operation
              && List.exists current.deliveries ~f:(fun delivery ->
                match
                  delivery.context.work, delivery.status, delivery.wake_disposition
                with
                | Some (Job id), Committed _, Some (Accepted_wake _) ->
                  P.Id.Job.equal id job_id
                | _ -> false));
            let job =
              List.find_exn (state daemon parent.id).jobs ~f:(fun job ->
                P.Id.Job.equal job.id job_id)
            in
            match P.Job.terminal_completion job |> protocol_ok with
            | Some (Succeeded value) -> value
            | completion ->
              raise_s [%sexp "helper job failed", (completion : P.Completion.t option)]
          in
          let one_off_replay =
            invoke
              sw
              daemon
              client
              parent.id
              "run_chatml"
              (`Object
                  [ ( "source"
                    , `String
                        {|let main input =
  let* result = Tool.call("session_bridge", input) in
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(code) -> Task.fail(code)|}
                    )
                  ; ( "input"
                    , `Object [ "stdin", `String (Jsonaf.to_string create_envelope) ] )
                  ; "tools", `Array [ `String "session_bridge"; `String "read_file" ]
                  ])
            |> Jsonaf.of_string
            |> P.Invocation.outcome_of_json
            |> protocol_ok
            |> complete
          in
          let child = id created in
          assert (P.Id.Session.equal child (id one_off_replay));
          [%test_eq: string]
            (text created "session_id")
            (bridge sw daemon client parent.id "create" child_request
             |> complete
             |> fun value -> text value "session_id");
          let target = [ "session_id", P.Id.Session.to_json child ] in
          ignore
            (bridge
               ~name:"session_view"
               sw
               daemon
               client
               parent.id
               "status"
               (`Object target)
             |> complete);
          (match
             bridge
               ~name:"session_view"
               sw
               daemon
               client
               parent.id
               "send"
               (`Object
                   (target
                    @ [ "message", `String "denied"
                      ; "idempotency_key", `String "view-denied"
                      ]))
           with
           | Fail error -> [%test_eq: string] "agent.management.denied" error.code
           | _ -> failwith "readonly helper gained send authority");
          await (fun () -> Option.is_none (state daemon child).active_operation);
          let paused, release = Eio.Promise.create () in
          child_pause := Some paused;
          child_tool := Some (`Object [ "file", `String private_file ]);
          let sent =
            bridge
              sw
              daemon
              client
              parent.id
              "send"
              (`Object
                  (target
                   @ [ "message", `String "answer now"
                     ; "idempotency_key", `String "helper-message"
                     ]))
            |> complete
          in
          let receipt = text sent "receipt_id" in
          let query = target @ [ "receipt_id", `String receipt ] in
          let subscription_id = start_watch sw daemon client parent.id query in
          await_timer daemon parent.id subscription_id;
          assert (Option.is_some (state daemon child).active_operation);
          child_pause := None;
          Eio.Promise.resolve release ();
          (match await_watch daemon parent.id subscription_id with
           | Succeeded page ->
             assert (
               String.is_substring
                 (Jsonaf.to_string page)
                 ~substring:"persisted helper answer")
           | result -> raise_s [%sexp "watch failed", (result : P.Completion.t)]);
          let waited =
            bridge
              sw
              daemon
              client
              parent.id
              "wait"
              (`Object (query @ [ "timeout_ms", `Number "10000" ]))
            |> complete
          in
          [%test_eq: string] "receipt_terminal" (text waited "reason");
          assert (Option.is_none !child_tool);
          let child_results =
            (state daemon child).invocations
            |> List.filter_map ~f:(fun invocation ->
              match invocation.status with
              | Published (Complete value)
                when String.equal invocation.context.tool_name "read_file" ->
                Some (Jsonaf.to_string value)
              | _ -> None)
          in
          (match
             List.exists child_results ~f:(fun value ->
               String.is_substring value ~substring:"outside the configured read roots")
           with
           | true -> ()
           | false ->
             raise_s
               [%sexp
                 "missing inherited file denial"
               , (child_results : string list)
               , ((state daemon child).invocations : P.Invocation.t list)]);
          assert (
            List.for_all child_results ~f:(fun value ->
              not (String.is_substring value ~substring:"HELPER_PRIVATE_FIXTURE_CONTENT")));
          let output =
            bridge sw daemon client parent.id "read" (`Object query)
            |> complete
            |> Jsonaf.to_string
          in
          assert (String.is_substring output ~substring:"persisted helper answer");
          let snapshot =
            bridge sw daemon client parent.id "read" (`Object target) |> complete
          in
          assert (Jsonaf.bool_exn (field "caught_up" snapshot));
          let cursor_query = target @ [ "cursor", field "next_cursor" snapshot ] in
          let cursor_watch = start_watch sw daemon client parent.id cursor_query in
          await_timer daemon parent.id cursor_watch;
          let cancelled_watch = start_watch sw daemon client parent.id cursor_query in
          await_timer daemon parent.id cancelled_watch;
          let calls_before_cancel = !child_calls in
          [%test_eq: string]
            "cancelled"
            (invoke
               sw
               daemon
               client
               parent.id
               "cancel_response_watch"
               (`Object [ "subscription_id", P.Id.Subscription.to_json cancelled_watch ]));
          (match await_watch daemon parent.id cancelled_watch with
           | Cancelled _ -> ()
           | result ->
             raise_s [%sexp "watch cancellation failed", (result : P.Completion.t)]);
          [%test_eq: int] calls_before_cancel !child_calls;
          assert (
            P.Session.equal_desired_state (state daemon child).lifecycle.desired Running);
          ignore
            (bridge
               sw
               daemon
               client
               parent.id
               "send"
               (`Object
                   (target
                    @ [ "message", `String "future output"
                      ; "idempotency_key", `String "cursor-message"
                      ]))
             |> complete);
          (match await_watch daemon parent.id cursor_watch with
           | Succeeded page ->
             assert (
               String.is_substring
                 (Jsonaf.to_string page)
                 ~substring:"persisted helper answer");
             assert (
               not (String.equal (text page "next_cursor") (text snapshot "next_cursor")))
           | result -> raise_s [%sexp "cursor watch failed", (result : P.Completion.t)]);
          check_watch_notifications daemon parent.id;
          let foreign, _ =
            create_session ~start_immediately:true ~key:"foreign-parent" client
          in
          assert (not (P.Id.Session.equal parent.id foreign.id));
          (match bridge sw daemon client foreign.id "status" (`Object target) with
           | Fail _ -> ()
           | _ -> failwith "foreign parent accessed helper-created child");
          let foreign_watch = start_watch sw daemon client foreign.id query in
          (* An immediate denial may be delivered before this foreground ends;
             its retained error/history matters here, not another model turn. *)
          (match await_watch ~require_wake:false daemon foreign.id foreign_watch with
           | Failed error -> [%test_eq: string] "agent.management.denied" error.code
           | result ->
             raise_s
               [%sexp
                 "foreign response watcher was not denied", (result : P.Completion.t)]);
          check_watch_notifications daemon foreign.id;
          ignore
            (bridge
               sw
               daemon
               client
               parent.id
               "stop"
               (`Object
                   (target
                    @ [ "mode", `String "graceful"
                      ; "idempotency_key", `String "helper-stop"
                      ]))
             |> complete);
          check_notification daemon parent.id child;
          check_watch_notifications daemon parent.id;
          let named args =
            match invoke_status sw daemon client parent.id "specialist" args with
            | Published (Complete value) -> value
            | status ->
              raise_s
                [%sexp
                  "authored helper named call failed", (status : P.Invocation.status)]
          in
          let authored =
            Agent_server_authored_helper_fixture.before_restart
              ~state:(state daemon)
              ~named
              ~bridge:(authored_bridge sw daemon client parent.id foreign.id)
          in
          parent.id, child, receipt, authored, foreign.id)
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        (path (Filename.concat root "authored-helper.chatmd"))
        "<developer>Edited live authored specialist.</developer>";
      let cursor_watch, receipt_watch =
        with_daemon (fun sw daemon client ->
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:parent_id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          H.close handle;
          let before = !child_calls in
          Agent_server_authored_helper_fixture.after_restart
            ~state:(state daemon)
            ~bridge:(authored_bridge sw daemon client parent_id foreign_id)
            authored;
          [%test_eq: int] before !child_calls;
          let target = [ "session_id", P.Id.Session.to_json child_id ] in
          let replay =
            bridge sw daemon client parent_id "create" child_request |> complete
          in
          assert (P.Id.Session.equal child_id (id replay));
          let output =
            bridge
              sw
              daemon
              client
              parent_id
              "read"
              (`Object (target @ [ "receipt_id", `String receipt ]))
            |> complete
            |> Jsonaf.to_string
          in
          assert (String.is_substring output ~substring:"persisted helper answer");
          check_notification daemon parent_id child_id;
          check_watch_notifications daemon parent_id;
          let child_handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:child_id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          await (fun () -> Option.is_none (state daemon child_id).active_operation);
          let snapshot =
            bridge sw daemon client parent_id "read" (`Object target) |> complete
          in
          assert (Jsonaf.bool_exn (field "caught_up" snapshot));
          let paused, _release = Eio.Promise.create () in
          child_pause := Some paused;
          let sent =
            bridge
              sw
              daemon
              client
              parent_id
              "send"
              (`Object
                  (target
                   @ [ "message", `String "interrupted by restart"
                     ; "idempotency_key", `String "restart-message"
                     ]))
            |> complete
          in
          let receipt_watch =
            start_watch
              sw
              daemon
              client
              parent_id
              (target @ [ "receipt_id", field "receipt_id" sent ])
          in
          await_timer daemon parent_id receipt_watch;
          let cursor_watch =
            start_watch
              sw
              daemon
              client
              parent_id
              (target @ [ "cursor", field "next_cursor" snapshot ])
          in
          await_timer daemon parent_id cursor_watch;
          assert (Option.is_some (state daemon child_id).active_operation);
          H.close child_handle;
          cursor_watch, receipt_watch)
      in
      child_pause := None;
      with_daemon (fun sw daemon client ->
        let step name f =
          match
            Eio.Time.with_timeout (Eio.Stdenv.clock env) 10. (fun () -> Ok (f ()))
          with
          | Ok result -> result
          | Error _ ->
            let current = state daemon parent_id in
            let failures =
              List.filter current.moderator_executions ~f:(fun receipt ->
                match receipt.status with
                | Completed _ -> false
                | _ -> true)
            in
            let subscriptions =
              List.filter current.subscriptions ~f:(fun subscription ->
                P.Id.Subscription.equal subscription.context.id cursor_watch
                || P.Id.Subscription.equal subscription.context.id receipt_watch)
            in
            let jobs =
              List.map current.jobs ~f:(fun job -> job.id, job.status, job.delivery)
            in
            raise_s
              [%sexp
                (name : string)
              , (subscriptions : P.Subscription.t list)
              , (failures : P.Moderator_execution.t list)
              , (jobs : (P.Id.Job.t * P.Job.status * P.Job.delivery) list)]
        in
        let before_calls = !child_calls in
        assert (
          P.Session.equal_desired_state (state daemon parent_id).lifecycle.desired Running);
        (* Both recovered completions request a turn. The automatic follow-up
           budget may suppress a wake; it must never suppress their messages. *)
        (match
           step "cursor completion" (fun () ->
             await_watch ~require_wake:false daemon parent_id cursor_watch)
         with
         | Failed error -> [%test_eq: string] "agent.read.cursor_expired" error.code
         | result ->
           raise_s [%sexp "expired cursor was not reported", (result : P.Completion.t)]);
        (match
           step "receipt completion" (fun () ->
             await_watch ~require_wake:false daemon parent_id receipt_watch)
         with
         | Failed error -> [%test_eq: string] "watcher.target_failed" error.code
         | result ->
           raise_s
             [%sexp "interrupted receipt was not reported", (result : P.Completion.t)]);
        [%test_eq: int] before_calls !child_calls;
        check_notification daemon parent_id child_id;
        check_watch_notifications daemon parent_id;
        let pending_jobs () =
          List.filter (state daemon parent_id).jobs ~f:(fun job ->
            match job.status, job.delivery with
            | ( (Succeeded | Failed _ | Cancelled | Interrupted _)
              , (Delivered _ | Discarded _ | Not_required) ) -> false
            | _ -> true)
        in
        (match
           Eio.Time.with_timeout (Eio.Stdenv.clock env) 3. (fun () ->
             await (fun () -> List.is_empty (pending_jobs ()));
             Ok ())
         with
         | Ok () -> ()
         | Error _ ->
           raise_s
             [%sexp "watch recovery left pending jobs", (pending_jobs () : P.Job.t list)]);
        let child_handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:child_id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        step "stop child" (fun () ->
          H.stop child_handle ~mode:Graceful |> protocol_ok |> ignore);
        H.close child_handle);
      with_daemon
        ~grants:
          [ grant "session_bridge" [ Create; Send; Read; Status; Wait; Stop ]
          ; grant "session_view" [ Read; Status; Wait; Send ]
          ]
        (fun sw daemon client ->
           let handle =
             H.attach
               ~sw
               ~clock:(Eio.Stdenv.clock env)
               ~connection:client
               ~session_id:parent_id
               ~mode:Read_write
               ~subscribe:false
               ()
             |> protocol_ok
           in
           H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
           H.close handle;
           (* Stored status/history remain readable; reactivating the old resource
             binding must fail before any child provider/tool execution. *)
           ignore
             (bridge
                sw
                daemon
                client
                parent_id
                "status"
                (`Object [ "session_id", P.Id.Session.to_json child_id ])
              |> complete);
           let before = !child_calls in
           let child_handle =
             H.attach
               ~sw
               ~clock:(Eio.Stdenv.clock env)
               ~connection:client
               ~session_id:child_id
               ~mode:Read_write
               ~subscribe:false
               ()
             |> protocol_ok
           in
           (match H.start child_handle ~queue_if_limited:false with
            | Error _ -> ()
            | Ok _ -> await (fun () -> Option.is_some (state daemon child_id).failure));
           H.close child_handle;
           [%test_eq: int] before !child_calls);
      print_endline
        "real shell helper lifecycle without native registrations, scoped grants, \
         foreign denial and restart PASS")
;;

let () =
  let args = Sys.get_argv () in
  Eio_main.run (fun env ->
    List.iter [ false; true ] ~f:(fun native_watch ->
      run env (Caml_unix.realpath args.(1)) (Caml_unix.realpath args.(2)) ~native_watch))
;;
