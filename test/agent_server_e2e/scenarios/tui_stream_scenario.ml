open Core
module F = Support.Tui_fixture
module T = Tui_trace_scenario
module Temp = Support.Temporary_environment
module Config = Support.Config_fixture
module Provider = Support.Tui_stream_provider
module Client = Chat_tui.Agent_session_client
module Model = Chat_tui.Model
module Projection = Chat_tui.Agent_projection
module Process = Support.Process_manager

let endpoint port = sprintf "http://127.0.0.1:%d" port
let snapshot client = Projection.snapshot (Client.projection client)

let protocol_json entries =
  `Array (List.map entries ~f:Agent_protocol.History.entry_to_json) |> Jsonaf.to_string
;;

let fixture env temporary =
  let fixture = F.create env temporary "tui-stream" in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temp.path temporary (Config.prompt_path fixture))
    {|<developer>TUI streaming fixture</developer><tool name="fork"/>|};
  let config =
    Config.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(tool_default deny)"
         ~with_:"(tool_default ask)"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temp.path temporary (Config.config_path fixture))
    config;
  fixture
;;

let with_daemon env fixture port f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Support.Daemon_process.start_in_directory_with_environment_overrides
        ~sw
        ~env
        ~fixture
        ~cwd:(Temp.path (Config.environment fixture) (Config.physical_workspace fixture))
        ~environment_overrides:
          [ "API_URL", endpoint port; "OPENAI_API_KEY", "tui-local-test-key" ]
        ~config_path:(Config.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        (match Support.Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
         | Ok _ -> ()
         | Error error -> raise_s [%sexp (error : Support.Daemon_process.readiness_error)]);
        f sw)
      ~finally:(fun () ->
        ignore
          (Support.Daemon_process.stop daemon ~env ~grace_seconds:1.
           : Process.termination)))
;;

let with_connected env fixture port http f =
  with_daemon env fixture port (fun sw ->
    let connection = T.connected ~sw env fixture http in
    let observer =
      Support.Unix_driver.connect ~sw ~env ~socket_path:(Config.unix_socket fixture)
    in
    Exn.protect
      ~f:(fun () ->
        ignore
          (Support.Unix_driver.initialize observer |> F.ok
           : Agent_protocol.Initialize.Response.t);
        let client =
          Client.create
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            { prompt = "smoke"
            ; workspace = "physical"
            ; liveness = Detached
            ; permission_profile = None
            ; display_name = None
            ; labels = []
            ; mode = Read_write
            }
          |> F.ok
        in
        Exn.protect
          ~f:(fun () -> f client observer)
          ~finally:(fun () -> Client.close client))
      ~finally:(fun () ->
        Agent_client.Connection.close connection;
        Agent_client.Connection.close observer))
;;

let apply_until env client applier model predicate =
  try
    F.await env (fun () ->
      T.apply applier model (Client.projection client);
      if predicate () then Some () else None)
  with
  | Eio.Time.Timeout ->
    raise_s
      [%sexp
        "TUI trace checkpoint timed out"
      , (Model.messages model : (string * string) list)
      , (List.map
           (Projection.live_events (Client.projection client))
           ~f:(fun event -> event.operation_sequence, event.kind)
         : (int64 * Agent_protocol.Event.Recoverable.kind) list)]
;;

let row model role text =
  List.findi (Model.messages model) ~f:(fun _ (r, t) ->
    String.equal r role && String.equal t text)
;;

let assert_row model role text =
  match row model role text with
  | Some (idx, _) -> fst (Model.render_row_identity model ~idx |> Option.value_exn)
  | None ->
    raise_s
      [%sexp
        "missing exact TUI row"
      , (role : string)
      , (text : string)
      , (Model.messages model : (string * string) list)]
;;

let await_row env client applier model role text =
  apply_until env client applier model (fun () -> Option.is_some (row model role text));
  assert_row model role text
;;

let call model =
  List.find (Model.active_agent_calls model) ~f:(fun call ->
    String.equal (Model.agent_call_id call) "tui-fork")
;;

let progress model =
  Option.value_map (call model) ~default:[] ~f:(fun call ->
    Model.agent_call_progress_entries call |> List.map ~f:Model.progress_entry_text)
;;

let approve env client =
  let permission =
    try
      F.await env (fun () ->
        List.find (snapshot client).permissions ~f:(fun p ->
          Agent_protocol.Permission.equal_state p.state Pending))
    with
    | Eio.Time.Timeout ->
      raise_s
        [%sexp "permission never arrived", (snapshot client : Agent_protocol.Snapshot.t)]
  in
  F.require
    (String.equal permission.tool_name "fork")
    "unexpected tool requested permission";
  ignore
    (Client.respond_permission
       client
       ~permission_id:permission.id
       ~permission_generation:permission.generation
       ~choice:Approve_once
       ~reason:(Some "deterministic TUI fixture")
     |> F.ok
     : Agent_protocol.Permission.t)
;;

let begin_root env provider client applier model =
  ignore
    (Client.send_text client "stream-root" |> F.ok
     : Agent_protocol.Method_result.Send_message.t);
  let root = Provider.await_request provider env 0 in
  Provider.emit
    root
    [ Provider.added (Provider.reasoning "root-reason" "")
    ; Provider.reasoning_delta "root-reason" "root-think"
    ];
  let id = await_row env client applier model "reasoning" "root-think" in
  Provider.emit root [ Provider.done_ (Provider.reasoning "root-reason" "root-think") ];
  Provider.emit root (Provider.fork_call ());
  Provider.finish root;
  approve env client;
  id
;;

let nested_progress env provider client applier model =
  let nested = Provider.await_request provider env 1 in
  Provider.emit
    nested
    [ Provider.added (Provider.reasoning "nested-reason" "")
    ; Provider.reasoning_delta "nested-reason" "nested-think"
    ];
  apply_until env client applier model (fun () ->
    List.mem (progress model) "nested-think" ~equal:String.equal);
  Provider.emit
    nested
    [ Provider.done_ (Provider.reasoning "nested-reason" "nested-think")
    ; Provider.added (Provider.message "nested-message" "")
    ; Provider.text_delta "nested-message" "nested-a"
    ];
  apply_until env client applier model (fun () ->
    List.mem (progress model) "nested-a" ~equal:String.equal);
  F.require
    (Poly.equal (progress model) [ "nested-think"; "nested-a" ])
    "nested progress duplicated or reordered";
  nested
;;

let durable_during_progress env client applier model =
  let before = progress model in
  let sent = Client.send_text client "stream-deferred" |> F.ok in
  F.require
    (Agent_protocol.Method_result.Send_message.equal_disposition
       sent.disposition
       Deferred)
    "nested tool did not suspend the foreground turn";
  apply_until env client applier model (fun () ->
    List.exists (snapshot client).deferred_entries ~f:(fun entry ->
      Agent_protocol.History.Id.compare entry.id sent.history_id = 0));
  T.apply applier model (Client.projection client);
  F.require (Poly.equal before (progress model)) "durable revision replayed tool progress";
  sent
;;

let finish_nested env provider client applier model nested =
  Provider.emit nested [ Provider.text_delta "nested-message" "-b" ];
  apply_until env client applier model (fun () ->
    List.mem (progress model) "nested-a-b" ~equal:String.equal);
  Provider.emit nested [ Provider.done_ (Provider.message "nested-message" "nested-a-b") ];
  Provider.finish nested;
  let final = Provider.await_request provider env 2 in
  apply_until env client applier model (fun () ->
    Option.exists (call model) ~f:(fun call ->
      Poly.equal (Model.agent_call_outcome call) (Some Returned)));
  F.require
    (String.is_substring (Jsonaf.to_string (Provider.body final)) ~substring:"nested-a-b")
    "parent provider did not receive the nested result";
  final
;;

let assert_pair entries =
  let index, call =
    List.findi entries ~f:(fun _ entry ->
      Agent_protocol.History.equal_kind entry.Agent_protocol.History.kind Tool_call)
    |> Option.value_exn
  in
  let output = List.nth_exn entries (index + 1) in
  F.require
    (Agent_protocol.History.equal_kind output.kind Tool_output)
    "tool pair is not adjacent";
  List.iter [ call; output ] ~f:(fun entry ->
    F.require
      (Poly.equal
         (Jsonaf.member "call_id" entry.Agent_protocol.History.payload)
         (Some (`String "tui-fork")))
      "canonical tool pair has wrong call identity")
;;

let finish_text env client applier model final =
  Provider.emit
    final
    [ Provider.added (Provider.message "root-message" "")
    ; Provider.text_delta "root-message" "root-a"
    ];
  let id = await_row env client applier model "assistant" "root-a" in
  Provider.emit final [ Provider.text_delta "root-message" "-b" ];
  let continued = await_row env client applier model "assistant" "root-a-b" in
  F.require
    (Chat_tui.Projected_message.Id.equal id continued)
    "stream delta changed row identity";
  Provider.emit final [ Provider.done_ (Provider.message "root-message" "root-a-b") ];
  Provider.finish final;
  apply_until env client applier model (fun () ->
    Option.is_none (snapshot client).session.active_operation);
  id
;;

let assert_canonical model entries deferred =
  F.require
    (String.equal
       (protocol_json entries)
       (protocol_json
          (List.map
             (Model.history_items model)
             ~f:Agent_session.History_codec.to_protocol)))
    "final TUI history differs from server";
  let kinds = List.map entries ~f:(fun entry -> entry.Agent_protocol.History.kind) in
  F.require
    (List.equal
       Agent_protocol.History.equal_kind
       kinds
       [ Message; Message; Reasoning; Tool_call; Tool_output; Message; Message ])
    "child history leaked into the root or root entries were lost";
  F.require
    (List.count entries ~f:(fun entry ->
       Agent_protocol.History.Id.compare
         entry.id
         deferred.Agent_protocol.Method_result.Send_message.history_id
       = 0)
     = 1)
    "deferred message lost or duplicated";
  assert_pair entries
;;

let finish_root env provider client observer applier model final reason_id deferred =
  let id = finish_text env client applier model final in
  let authoritative =
    Agent_client.Admin.get_session observer (snapshot client).session.id |> F.ok
  in
  F.require (Option.is_none authoritative.failure) "streaming session failed";
  let entries = authoritative.canonical_history.entries in
  assert_canonical model entries deferred;
  List.iter
    [ "reasoning", "root-think", reason_id; "assistant", "root-a-b", id ]
    ~f:(fun (role, text, id) ->
      F.require
        (Chat_tui.Projected_message.Id.equal id (assert_row model role text))
        "finalization changed row identity");
  F.require (Provider.request_count provider = 3) "unexpected extra provider request";
  Model.messages model
;;

let exercise env provider client observer =
  let model = T.model "unsent-stream-draft" in
  let applier = Chat_tui.Agent_event_apply.create () in
  T.apply applier model (Client.projection client);
  let reason_id = begin_root env provider client applier model in
  let nested = nested_progress env provider client applier model in
  let deferred = durable_during_progress env client applier model in
  let final = finish_nested env provider client applier model nested in
  let messages =
    finish_root env provider client observer applier model final reason_id deferred
  in
  F.require
    (String.equal (Model.input_line model) "unsent-stream-draft")
    "live events overwrote draft";
  messages
;;

let overlay_script =
  {|
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end
    | `Item_appended(item) | `Pre_tool_call(tool_call) | `Post_tool_response(tool_result) ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Item_appended(item) ->
      let parts = Item.text_parts(item) in
      if Array.length(parts) == 0 then Task.pure(state)
      else if Array.get(parts, 0) == "stream-root" then
        Task.bind(Turn.replace_item(Item.id(item), Item.input_text_message("replacement", "user", "overlay-user")), fun ignored ->
        Task.bind(Turn.append_item(Item.output_text_message("inserted", "overlay-inserted")), fun ignored ->
        Task.bind(Turn.delete_item(Item.id(ctx.items[0])), fun ignored -> Task.pure(state))))
      else Task.pure(state)
    | _ -> Task.pure(state)
</script>
|}
;;

let authoritative env client observer =
  let value =
    Agent_client.Admin.get_session observer (snapshot client).session.id |> F.ok
  in
  F.await env (fun () ->
    if Int64.((snapshot client).latest_event_sequence >= value.latest_event_sequence)
    then Some value
    else None)
;;

let assert_overlay_rows model =
  List.iter
    [ "user", "overlay-user"; "assistant", "overlay-inserted" ]
    ~f:(fun (role, text) ->
      ignore (assert_row model role text : Chat_tui.Projected_message.Id.t));
  F.require
    (Option.is_none (row model "user" "stream-root"))
    "replacement still shows canonical text";
  F.require
    (not
       (List.exists (Model.messages model) ~f:(fun (_, text) ->
          String.equal text "TUI streaming fixture")))
    "deleted developer entry remains visible"
;;

let assert_overlay env client observer applier model =
  let server = authoritative env client observer in
  T.apply applier model (Client.projection client);
  assert_overlay_rows model;
  F.assert_user
    (List.filter server.canonical_history.entries ~f:(fun entry ->
       String.is_substring
         (Jsonaf.to_string entry.Agent_protocol.History.payload)
         ~substring:"stream-root"))
    "stream-root";
  let effective = Option.value_exn server.effective_history in
  F.require
    (String.equal
       (protocol_json effective.entries)
       (protocol_json (Option.value_exn (snapshot client).effective_history).entries))
    "subscribed overlay differs from authoritative effective history"
;;

let assert_terminal_agent client model =
  F.require
    (List.is_empty (snapshot client).deferred_entries)
    "adopted queue was not cleared";
  let call = Option.value_exn (call model) in
  F.require
    (Poly.equal (Model.agent_call_outcome call) (Some Returned))
    "terminal Agent-page state lost";
  F.require
    (Poly.equal (progress model) [ "nested-think"; "nested-a-b" ])
    "terminal progress duplicated";
  ignore
    (Chat_tui.Renderer_page_agent.render ~size:(90, 25) ~model : Notty.I.t * (int * int));
  F.require
    (String.equal (Model.input_line model) "overlay-draft")
    "Agent page changed draft"
;;

let exercise_overlays env provider client observer =
  let model = T.model "overlay-draft" in
  let applier = Chat_tui.Agent_event_apply.create () in
  let reason_id = begin_root env provider client applier model in
  let nested = nested_progress env provider client applier model in
  assert_overlay env client observer applier model;
  let deferred = durable_during_progress env client applier model in
  F.require
    (Option.is_none (row model "user" "stream-deferred"))
    "deferred input rendered as committed";
  let final = finish_nested env provider client applier model nested in
  let messages =
    finish_root env provider client observer applier model final reason_id deferred
  in
  assert_overlay env client observer applier model;
  assert_terminal_agent client model;
  messages
;;

let await_idle env client applier model =
  apply_until env client applier model (fun () ->
    Option.is_none (snapshot client).session.active_operation)
;;

let assert_draft model =
  F.require
    (String.equal (Model.input_line model) "advanced-draft"
     && Model.cursor_pos model = 3
     && Poly.equal (Model.selection_anchor model) (Some 1))
    "server update changed draft/cursor/selection"
;;

let assert_history env client observer applier model =
  let server = authoritative env client observer in
  T.apply applier model (Client.projection client);
  F.require
    (String.equal
       (protocol_json server.canonical_history.entries)
       (protocol_json
          (List.map
             (Model.history_items model)
             ~f:Agent_session.History_codec.to_protocol)))
    "TUI canonical history differs from authoritative snapshot";
  assert_draft model;
  server
;;

let assert_permission_rendering model =
  List.iter
    [ 40, 30; 100, 30; 140, 40 ]
    ~f:(fun size ->
      let image, _ = Chat_tui.Renderer.render_full ~size ~model in
      let buffer = Buffer.create 4096 in
      Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) size image;
      let rendered = Buffer.contents buffer in
      List.iter
        [ "Tool permission requested"; "Tool: fork"; "approve once"; "deny" ]
        ~f:(fun substring ->
          F.require
            (String.is_substring rendered ~substring)
            ("missing permission row: " ^ substring)))
;;

let permission_view env client model =
  let pending =
    F.await env (fun () ->
      Chat_tui.Agent_permission_view.sync model ~current:None (Client.projection client))
  in
  F.require (Option.is_some (Model.moderator_modal model)) "permission modal did not open";
  assert_permission_rendering model;
  F.require
    (Poly.equal (Model.active_page model) Model.Page_id.Shell_security)
    "permission did not select security page";
  let modal = Model.moderator_modal model in
  ignore
    (Chat_tui.Agent_permission_view.sync
       model
       ~current:(Some pending)
       (Client.projection client)
     : Agent_protocol.Permission.t option);
  F.require
    (phys_equal (Option.value_exn modal) (Option.value_exn (Model.moderator_modal model)))
    "repeated projection reset modal";
  pending
;;

let approve_nested env provider client applier model =
  ignore
    (Client.send_text client "approval-root" |> F.ok
     : Agent_protocol.Method_result.Send_message.t);
  let request = Provider.await_request provider env 0 in
  Provider.emit request (Provider.fork_call ());
  Provider.finish request;
  let pending = permission_view env client model in
  F.require (Provider.request_count provider = 1) "tool ran before approval";
  approve env client;
  let child = Provider.await_request provider env 1 in
  apply_until env client applier model (fun () ->
    Option.is_none
      (Chat_tui.Agent_permission_view.sync
         model
         ~current:(Some pending)
         (Client.projection client)));
  F.require
    (Option.is_none (Model.moderator_modal model))
    "resolved approval modal remained open";
  child
;;

let cancel_nested env provider client applier model =
  let child = approve_nested env provider client applier model in
  Provider.emit
    child
    [ Provider.added (Provider.message "cancel-child" "")
    ; Provider.text_delta "cancel-child" "cancel-partial"
    ];
  apply_until env client applier model (fun () ->
    List.mem (progress model) "cancel-partial" ~equal:String.equal);
  ignore (Client.cancel_active_operation client |> F.ok : Agent_protocol.Session.t);
  await_idle env client applier model;
  F.require
    (Option.exists (call model) ~f:(fun call ->
       Poly.equal (Model.agent_call_outcome call) (Some Cancelled)))
    "cancelled Agent-page call remained running";
  Provider.emit
    child
    [ Provider.done_ (Provider.message "cancel-child" "late-child-result") ];
  Provider.finish child
;;

let compact_success env provider client observer applier model =
  let before = assert_history env client observer applier model in
  ignore (Client.compact client |> F.ok : Agent_protocol.Session.t);
  let request = Provider.await_request provider env 2 in
  apply_until env client applier model (fun () ->
    Option.exists (snapshot client).session.active_operation ~f:(fun op ->
      Poly.equal op.kind Compaction));
  Provider.reply_summary request "TUI deterministic compacted summary";
  await_idle env client applier model;
  let after = assert_history env client observer applier model in
  F.require
    (not
       (String.equal
          (protocol_json before.canonical_history.entries)
          (protocol_json after.canonical_history.entries)))
    "compaction did not replace history";
  F.require
    (String.is_substring
       (protocol_json after.canonical_history.entries)
       ~substring:"TUI deterministic compacted summary")
    "compaction summary is absent";
  after
;;

let cancel_compaction env provider client applier model =
  ignore (Client.compact client |> F.ok : Agent_protocol.Session.t);
  let request = Provider.await_request provider env 3 in
  ignore (Client.cancel_active_operation client |> F.ok : Agent_protocol.Session.t);
  await_idle env client applier model;
  Provider.reply_summary request "REJECTED late summary"
;;

let continue_after_cancel env provider client applier model =
  ignore
    (Client.send_text client "after-cancellation" |> F.ok
     : Agent_protocol.Method_result.Send_message.t);
  let final = Provider.await_request provider env 4 in
  F.require
    (not
       (String.is_substring
          (Jsonaf.to_string (Provider.body final))
          ~substring:"REJECTED"))
    "cancelled summary reached next turn";
  ignore (finish_text env client applier model final : Chat_tui.Projected_message.Id.t)
;;

let assert_cancelled_history
      (before : Agent_protocol.Snapshot.t)
      (after : Agent_protocol.Snapshot.t)
  =
  let encoded = protocol_json after.canonical_history.entries in
  F.require
    (not
       (String.is_substring encoded ~substring:"REJECTED"
        || String.is_substring encoded ~substring:"late-child-result"))
    "cancelled worker published late history";
  F.require
    (List.is_prefix
       after.canonical_history.entries
       ~prefix:before.canonical_history.entries
       ~equal:(fun a b -> String.equal (protocol_json [ a ]) (protocol_json [ b ])))
    "cancelled compaction replaced history"
;;

let exercise_approval env provider client observer =
  let model = T.model "advanced-draft" in
  let applier = Chat_tui.Agent_event_apply.create () in
  cancel_nested env provider client applier model;
  let before = compact_success env provider client observer applier model in
  cancel_compaction env provider client applier model;
  continue_after_cancel env provider client applier model;
  let after = assert_history env client observer applier model in
  assert_cancelled_history before after;
  F.require
    (Provider.request_count provider = 5)
    "unexpected provider request after cancellation";
  Model.messages model
;;

let record_connection connection replies =
  Agent_client.Transport.create
    ~request:(fun command ->
      let result = Agent_client.Connection.request connection command in
      (match result with
       | Ok (Agent_protocol.Method_result.Session_attach response) ->
         replies := response.replay :: !replies
       | _ -> ());
      result)
    ~next_notification:(fun () -> Agent_client.Connection.next_notification connection)
    ~close:(fun () -> Agent_client.Connection.close connection)
  |> Agent_client.Connection.create
;;

let reconnect_terminal env provider original observer =
  let request = Provider.await_request provider env 0 in
  List.init 8 ~f:(fun i -> sprintf "offline-deferred-%d" i)
  |> List.iter ~f:(fun text ->
    let sent = Client.send_text original text |> F.ok in
    F.require
      (Poly.equal sent.disposition Deferred)
      "offline writer did not defer message");
  Provider.emit
    request
    [ Provider.text_delta "reconnect-message" "-finished"
    ; Provider.done_ (Provider.message "reconnect-message" "before-disconnect-finished")
    ];
  Provider.finish request;
  let followup = Provider.await_request provider env 1 in
  Provider.emit
    followup
    [ Provider.added (Provider.message "offline-followup" "")
    ; Provider.done_ (Provider.message "offline-followup" "offline-writer-followup")
    ];
  Provider.finish followup;
  F.await env (fun () ->
    let server =
      Agent_client.Admin.get_session observer (snapshot original).session.id |> F.ok
    in
    if Option.is_none server.session.active_operation then Some server else None)
;;

let verify_replay replies expired =
  match !replies with
  | Agent_protocol.Method_result.Attach.Events events :: _ when not expired ->
    F.require (not (List.is_empty events)) "reconnect replay contained no events"
  | Snapshot _ :: _ when expired -> ()
  | values ->
    raise_s
      [%sexp
        "wrong reconnect replay branch"
      , (expired : bool)
      , (values : Agent_protocol.Method_result.Attach.replay list)]
;;

let begin_reconnect env provider client applier model =
  ignore
    (Client.send_text client "reconnect-root" |> F.ok
     : Agent_protocol.Method_result.Send_message.t);
  let request = Provider.await_request provider env 0 in
  Provider.emit
    request
    [ Provider.added (Provider.message "reconnect-message" "")
    ; Provider.text_delta "reconnect-message" "before-disconnect"
    ];
  await_row env client applier model "assistant" "before-disconnect"
;;

let disconnect_reconnect env client model disconnect entered =
  disconnect ();
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    Eio.Promise.await entered);
  F.require
    (match (Client.status client).phase with
     | Reconnecting _ -> true
     | _ -> false)
    "TUI did not enter reconnecting state";
  F.require
    (Result.is_error (Client.send_text client "must-not-send-offline"))
    "offline send was accepted";
  assert_draft model
;;

let assert_reconnected env client observer applier model stable_id =
  let final_id = assert_row model "assistant" "before-disconnect-finished" in
  F.require
    (Chat_tui.Projected_message.Id.equal stable_id final_id)
    "reconnect changed canonical row identity";
  F.require
    (Option.is_none (row model "assistant" "before-disconnect"))
    "stale stream row survived reconnect";
  let final = assert_history env client observer applier model in
  F.require
    (not
       (String.is_substring
          (protocol_json final.canonical_history.entries)
          ~substring:"must-not-send-offline"))
    "failed offline send was replayed";
  F.require (Model.undo model && Model.redo model) "reconnect lost undo/redo";
  F.require
    (String.equal (Model.input_line model) "advanced-draft" && Model.cursor_pos model = 3)
    "undo/redo lost reconnect draft";
  Model.messages model
;;

let reconnect_checkpoint
      env
      provider
      original
      observer
      client
      disconnect
      entered
      resume
      replies
      expired
  =
  let model = T.model "advanced-draft" in
  let applier = Chat_tui.Agent_event_apply.create () in
  let stable_id = begin_reconnect env provider client applier model in
  disconnect_reconnect env client model disconnect entered;
  let server = reconnect_terminal env provider original observer in
  Eio.Promise.resolve resume ();
  apply_until env client applier model (fun () ->
    (match (Client.status client).phase with
     | Connected -> true
     | _ -> false)
    && Int64.((snapshot client).latest_event_sequence >= server.latest_event_sequence));
  verify_replay replies expired;
  assert_reconnected env client observer applier model stable_id
;;

let reconnect_transport ~sw env fixture http =
  if http
  then (
    let link, port =
      Support.Tui_manual_link.start_tcp ~sw ~env ~upstream_port:(Config.http_port fixture)
    in
    let connect () =
      Agent_transport_http.Client.connect
        ~sw
        ~env
        ~uri:(Uri.of_string (endpoint port))
        ~bearer_token:(Some (Config.admin_token fixture))
        ~notification_capacity:4096
      |> F.ok
    in
    ( connect
    , (fun _ -> Support.Tui_manual_link.cut link)
    , fun () -> Support.Tui_manual_link.resume link ))
  else (fun () -> T.connected ~sw env fixture false), Agent_client.Connection.close, Fn.id
;;

let exercise_reconnect env provider original observer fixture http expired =
  Eio.Switch.run (fun sw ->
    let replies = ref [] in
    let transport, disconnect, resume_link = reconnect_transport ~sw env fixture http in
    let connect () =
      transport () |> fun connection -> record_connection connection replies
    in
    let current = ref (connect ()) in
    let gate, resume = Eio.Promise.create () in
    let entered, notify = Eio.Promise.create () in
    let reconnect () =
      Eio.Promise.resolve notify ();
      Eio.Promise.await gate;
      resume_link ();
      current := connect ();
      Ok !current
    in
    let client =
      Client.attach
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~connection:!current
        ~reconnect:(Some reconnect)
        ~session_id:(snapshot original).session.id
        ~mode:Read_write
        ()
      |> F.ok
    in
    Exn.protect
      ~f:(fun () ->
        reconnect_checkpoint
          env
          provider
          original
          observer
          client
          (fun () -> disconnect !current)
          entered
          resume
          replies
          expired)
      ~finally:(fun () ->
        Client.close client;
        Agent_client.Connection.close !current))
;;

let child env selection =
  let journey, mode =
    match String.lsplit2 selection ~on:':' with
    | None -> "stream", selection
    | Some pair -> pair
  in
  let port = Sys.getenv "OCHAT_E2E_PROVIDER_PORT" |> Option.value_exn |> Int.of_string in
  F.require
    (Poly.equal (Sys.getenv "API_URL") (Some (endpoint port)))
    "fixture provider is not loopback pinned";
  F.require
    (Poly.equal (Sys.getenv "OPENAI_API_KEY") (Some "tui-local-test-key"))
    "fixture inherited a provider credential";
  Temp.with_ ~scenario:("tui-stream-" ^ mode) ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      let provider = Provider.start ~sw ~env ~port in
      let fixture = fixture env temporary in
      if String.equal journey "overlays"
      then (
        let path = Temp.path temporary (Config.prompt_path fixture) in
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          path
          (Eio.Path.load path ^ overlay_script));
      if String.equal journey "reconnect-snapshot"
      then (
        let path = Temp.path temporary (Config.config_path fixture) in
        let config =
          Eio.Path.load path
          |> String.substr_replace_first
               ~pattern:"(max_events_per_session 1000)"
               ~with_:"(max_events_per_session 4)"
        in
        Eio.Path.save ~create:(`Or_truncate 0o600) path config);
      let run client observer =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
          (match journey with
           | "reconnect" | "reconnect-snapshot" ->
             fun env provider client observer ->
               exercise_reconnect
                 env
                 provider
                 client
                 observer
                 fixture
                 (String.equal mode "http")
                 (String.equal journey "reconnect-snapshot")
           | "stream" -> exercise
           | "overlays" -> exercise_overlays
           | "approval" -> exercise_approval
           | _ -> failwith "unknown TUI journey")
            env
            provider
            client
            observer)
      in
      let messages =
        match mode with
        | "embedded" -> T.with_embedded_provider env fixture run
        | "unix" -> with_connected env fixture port false run
        | "http" -> with_connected env fixture port true run
        | _ -> failwith "unknown TUI stream transport"
      in
      Eio.Flow.copy_string
        (Sexp.to_string_mach ([%sexp_of: (string * string) list] messages))
        (Eio.Stdenv.stdout env)))
;;

let absolute path =
  if Filename.is_absolute path then path else Eio_posix.Low_level.realpath path
;;

let child_environment env temporary port =
  let server =
    Sys.getenv "OCHAT_E2E_SERVER_EXE"
    |> Option.value
         ~default:
           (Filename.concat
              (Eio.Path.native_exn (Eio.Stdenv.cwd env))
              "_build/default/bin/ochat_agent_server.exe")
    |> absolute
  in
  F.environment temporary
  |> Array.filter ~f:(fun entry ->
    not
      (List.exists
         [ "API_URL="; "OCHAT_E2E_PROVIDER_PORT="; "OCHAT_E2E_SERVER_EXE=" ]
         ~f:(fun prefix -> String.is_prefix entry ~prefix)))
  |> Fn.flip
       Array.append
       [| "API_URL=" ^ endpoint port
        ; "OCHAT_E2E_PROVIDER_PORT=" ^ Int.to_string port
        ; "OCHAT_E2E_SERVER_EXE=" ^ server
        ; "OPENAI_API_KEY=tui-local-test-key"
       |]
;;

let run_child env temporary mode =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let process =
      Process.spawn
        ~sw
        ~env
        ~cwd:(Temp.path temporary (Temp.roots temporary).workspaces)
        ~environment:(child_environment env temporary port)
        ~max_output_bytes:(1024 * 1024)
        [ absolute Sys_unix.executable_name
        ; "--scenario"
        ; "tui-stream-child"
        ; "--case"
        ; mode
        ]
    in
    let result =
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
        Process.await process)
    in
    if
      (not (Process.equal_exit result.exit (Exited 0)))
      || result.stdout.truncated
      || result.stderr.truncated
    then
      raise_s
        [%sexp "TUI streaming child failed", (mode : string), (result : Process.result)];
    result.stdout.contents)
;;

let run env temporary =
  let results = List.map [ "embedded"; "unix"; "http" ] ~f:(run_child env temporary) in
  F.require
    (List.for_all results ~f:(String.equal (List.hd_exn results)))
    "streamed TUI transcripts differ across transports"
;;

let run_journey journey env temporary =
  let results =
    List.map [ "embedded"; "unix"; "http" ] ~f:(fun mode ->
      run_child env temporary (journey ^ ":" ^ mode))
  in
  F.require
    (List.for_all results ~f:(String.equal (List.hd_exn results)))
    "advanced TUI transcripts differ across transports"
;;

let overlays = run_journey "overlays"
let approval = run_journey "approval"

let reconnect env temporary =
  List.iter [ "reconnect"; "reconnect-snapshot" ] ~f:(fun journey ->
    let results =
      List.map [ "unix"; "http" ] ~f:(fun mode ->
        run_child env temporary (journey ^ ":" ^ mode))
    in
    F.require
      (List.for_all results ~f:(String.equal (List.hd_exn results)))
      "reconnected TUI transcripts differ across transports")
;;
