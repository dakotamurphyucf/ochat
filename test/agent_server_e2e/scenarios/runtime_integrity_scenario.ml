open Core
module P = Permission_scenario
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Http_driver = Support.Http_driver
module Temporary_environment = Support.Temporary_environment
module Res = Openai.Responses

let require condition message =
  if not condition then raise_s [%sexp "runtime assertion failed", (message : string)]
;;

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "runtime protocol failure", (error : Agent_protocol.Error.t)]
;;

let request client command = (Http_driver.request client command |> ok).result
let key value = Agent_protocol.Idempotency_key.of_string value |> ok
let equal_json left right = String.equal (Jsonaf.to_string left) (Jsonaf.to_string right)

let snapshot client session_id =
  match request client (Session_get { session_id; history = None }) with
  | Session_get value -> value
  | _ -> failwith "unexpected session.get result"
;;

let failed_operations client session_id =
  match
    request
      client
      (Session_attach
         { session_id
         ; requested_mode = Read_only
         ; subscribe = false
         ; after_sequence = Some 0L
         ; reclaim_token = None
         ; idempotency_key = key "runtime:terminal-replay"
         })
  with
  | Session_attach { replay = Events events; _ } ->
    List.filter_map events ~f:(fun event ->
      match
        Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> ok
      with
      | Operation_failed { state = Failed error; _ } -> Some error
      | _ -> None)
  | _ -> failwith "expected retained operation event replay"
;;

let send client (session : P.session) text =
  let content =
    Agent_protocol.Session.Message_content.{ kind = Plain_text; text; attachments = [] }
  in
  match
    request
      client
      (Session_send_message
         { session_id = session.summary.id
         ; attachment_id = session.attachment.id
         ; content
         ; idempotency_key = key text
         })
  with
  | Session_send_message value -> value
  | _ -> failwith "unexpected send result"
;;

let fixture env environment name ~ask =
  P.configure_fixture
    env
    environment
    name
    ~profile:"unattended"
    ~tool_default:(if ask then "ask" else "allow")
    ()
;;

let marker fixture =
  Filename.concat (Config_fixture.physical_workspace fixture) "runtime-marker"
;;

let options fixture calls =
  { Agent_server.Daemon.default_options with
    model_post_stream =
      Some
        (fun ~sw ~inputs ->
          calls := inputs :: !calls;
          P.model_post_stream (marker fixture) ~sw ~inputs)
  }
;;

let entry_json entries = List.map entries ~f:Agent_protocol.History.entry_to_json
let encoded entries = `Array (entry_json entries) |> Jsonaf.to_string
let has_text entries text = String.is_substring (encoded entries) ~substring:text

let history_index entries id =
  List.findi entries ~f:(fun _ entry ->
    Agent_protocol.History.Id.compare entry.Agent_protocol.History.id id = 0)
  |> Option.value_exn
  |> fst
;;

let assert_pair entries =
  let call_index, call =
    List.findi entries ~f:(fun _ entry ->
      Agent_protocol.History.equal_kind entry.Agent_protocol.History.kind Tool_call)
    |> Option.value_exn
  in
  let output = List.nth_exn entries (call_index + 1) in
  require
    (Agent_protocol.History.equal_kind output.kind Tool_output)
    "deferred input split a tool pair";
  let call_id = function
    | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal "call_id"
    | _ -> failwith "tool payload was not an object"
  in
  require
    (equal_json (call_id call.payload) (call_id output.payload))
    "tool output matched a different call";
  call_index
;;

let assert_unique entries =
  let ids =
    List.map entries ~f:(fun entry ->
      Agent_protocol.History.Id.to_string entry.Agent_protocol.History.id)
  in
  require
    (Set.length (String.Set.of_list ids) = List.length ids)
    "canonical identity was duplicated"
;;

let assert_deferred sent =
  require
    (Agent_protocol.Method_result.Send_message.equal_disposition
       sent.Agent_protocol.Method_result.Send_message.disposition
       Deferred)
    "message was not deferred while permission suspended the tool";
  require
    (Option.is_none sent.operation_id)
    "deferred message allocated another operation"
;;

let assert_fifo
      (final : Agent_protocol.Snapshot.t)
      (first : Agent_protocol.Method_result.Send_message.t)
      (second : Agent_protocol.Method_result.Send_message.t)
      calls
  =
  let history = final.canonical_history.entries in
  let pair = assert_pair history in
  require
    (pair < history_index history first.history_id)
    "deferred message preceded completed pair";
  require
    (history_index history first.history_id < history_index history second.history_id)
    "FIFO order reversed";
  require (List.is_empty final.deferred_entries) "adopted entries remained deferred";
  assert_unique history;
  require (List.length !calls >= 2) "tool output never reached follow-up provider request";
  final
;;

let exercise_fifo env client session calls =
  ignore
    (send client session "initial-message" : Agent_protocol.Method_result.Send_message.t);
  let permission = P.await_pending_permission env client session.P.summary.id 250 in
  let first = send client session "deferred-first" in
  let second = send client session "deferred-second" in
  List.iter [ first; second ] ~f:assert_deferred;
  let pending = snapshot client session.summary.id in
  require
    (List.length pending.deferred_entries = 2)
    "pending FIFO did not contain both entries";
  P.respond_approve client session permission "fifo:approve";
  ignore
    (P.await_operation_end env client session.summary.id 250 : Agent_protocol.Session.t);
  assert_fifo (snapshot client session.summary.id) first second calls
;;

let restart_assert env fixture options before =
  Daemon_host.with_ env fixture ~options (fun sw _daemon ->
    P.with_client ~sw env fixture (fun client ->
      let after = snapshot client before.Agent_protocol.Snapshot.session.id in
      require
        (List.equal
           equal_json
           (entry_json before.canonical_history.entries)
           (entry_json after.canonical_history.entries))
        "restart changed canonical entries or IDs";
      require
        (List.is_empty after.deferred_entries)
        "restart resurrected consumed deferred messages"))
;;

let test_fifo env environment =
  let fixture = fixture env environment "runtime-fifo" ~ask:true in
  let calls = ref [] in
  let host_options = options fixture calls in
  let before = ref None in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    P.with_client ~sw env fixture (fun client ->
      let session = P.create_session client "unattended" "fifo:create" in
      let session = P.start_session client session "fifo:start" in
      before := Some (exercise_fifo env client session calls)));
  restart_assert env fixture host_options (Option.value_exn !before);
  require
    (String.equal
       (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / marker fixture))
       "\nexecuted")
    "tool executed more than once across restart"
;;

let moderator =
  {|
<script language="chatml" kind="moderator">
  type state = string
  type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end
    | `Item_appended(item) | `Pre_tool_call(tool_call) | `Post_tool_response(tool_result) ]
  let initial_state = ""
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Session_start -> Task.pure(state)
    | `Session_resume -> Task.pure(state)
    | `Item_appended(item) ->
      let parts = Item.text_parts(item) in
      if Array.length(parts) == 0 then Task.pure(state)
      else if Array.get(parts, 0) == "initial-message" then
        Task.bind(Turn.replace_item(Item.id(item), Item.input_text_message("replacement", "user", "moderated-message")), fun ignored ->
        Task.bind(Turn.append_item(Item.output_text_message("inserted", "inserted-overlay")), fun ignored ->
        Task.bind(Turn.delete_item(Item.id(ctx.items[0])), fun ignored -> Task.pure(state))))
      else Task.pure(state)
    | `Turn_start ->
      Task.bind(Turn.prepend_system("phase-turn-start"), fun ignored -> Task.pure(state ++ "start;"))
    | `Pre_tool_call(call) ->
      Task.bind(Tool.approve(), fun ignored -> Task.pure(state ++ "pre;"))
    | `Post_tool_response(result) ->
      Task.bind(Turn.append_item(Item.output_text_message("post", "phase-post-tool")), fun ignored -> Task.pure(state ++ "post;"))
    | `Turn_end ->
      if state == "start;pre;post;" then Task.pure(state ++ "end;")
      else Task.bind(Runtime.end_session(state ++ "end;"), fun ignored -> Task.pure(state))
</script>
|}
;;

let add_script environment fixture script =
  let path =
    Temporary_environment.path environment (Config_fixture.prompt_path fixture)
  in
  let prompt = Eio.Path.load path in
  Eio.Path.save ~create:(`Or_truncate 0o600) path (prompt ^ script)
;;

let assert_moderator_boundaries snapshot calls =
  Option.iter snapshot.Agent_protocol.Snapshot.failure ~f:(fun error ->
    raise_s [%sexp "moderator fixture failed", (error : Agent_protocol.Error.t)]);
  if not snapshot.Agent_protocol.Snapshot.halted
  then
    raise_s
      [%sexp
        "moderator halt was not applied"
      , (snapshot.session.observed_state : Agent_protocol.Session.observed_state)
      , (List.length !calls : int)];
  let reason = Option.value_exn snapshot.halt_reason in
  require
    (String.is_substring reason ~substring:"pre;post;")
    "pre/post tool safe points were reordered or omitted";
  require
    (String.is_prefix reason ~prefix:"start;" && String.is_suffix reason ~suffix:"end;")
    "turn boundary ordering was wrong";
  let canonical = snapshot.canonical_history.entries in
  require (has_text canonical "initial-message") "moderator replaced canonical input";
  require
    (not (has_text canonical "phase-post-tool"))
    "moderator overlay entered canonical history"
;;

let assert_moderated snapshot calls =
  assert_moderator_boundaries snapshot calls;
  let inputs =
    List.last_exn (List.rev !calls)
    |> List.map ~f:Res.Item.jsonaf_of_t
    |> fun values -> Jsonaf.to_string (`Array values)
  in
  require
    (String.is_substring inputs ~substring:"moderated-message")
    "replacement was absent from provider input";
  require
    (String.is_substring inputs ~substring:"phase-post-tool")
    "post-tool overlay missed the next request";
  require
    (not (String.is_substring inputs ~substring:"Invoke append_to_file exactly"))
    "deleted canonical item remained in effective provider input";
  require
    (String.is_substring inputs ~substring:"inserted-overlay")
    "inserted moderator item was absent from provider input"
;;

let test_moderator env environment =
  let fixture = fixture env environment "runtime-moderator" ~ask:false in
  add_script environment fixture moderator;
  let calls = ref [] in
  let before = ref None in
  let host_options = options fixture calls in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    P.with_client ~sw env fixture (fun client ->
      let session = P.create_session client "unattended" "moderator:create" in
      let session = P.start_session client session "moderator:start" in
      ignore
        (send client session "initial-message"
         : Agent_protocol.Method_result.Send_message.t);
      ignore
        (P.await_operation_end env client session.summary.id 250
         : Agent_protocol.Session.t);
      let final = snapshot client session.summary.id in
      let failures = failed_operations client session.summary.id in
      if not (List.is_empty failures)
      then
        raise_s
          [%sexp "moderator operation failed", (failures : Agent_protocol.Error.t list)];
      assert_moderated final calls;
      before := Some final));
  restart_assert env fixture host_options (Option.value_exn !before)
;;

let budget_script =
  {|
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Item_appended(item) ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Turn_end -> Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state + 1))
    | _ -> Task.pure(state)
</script>
|}
;;

let budget_stream count ~sw:_ ~inputs:_ =
  incr count;
  let item =
    Res.Response_stream.Item.Output_message
      { role = Assistant
      ; id = sprintf "budget-message-%d" !count
      ; content = [ { annotations = []; text = "budget"; _type = "output_text" } ]
      ; status = "completed"
      ; phase = None
      ; _type = "message"
      }
  in
  Stdlib.List.to_seq
    [ Res.Response_stream.Output_item_added
        { item; output_index = 0; type_ = "response.output_item.added" }
    ; Res.Response_stream.Output_item_done
        { item; output_index = 0; type_ = "response.output_item.done" }
    ]
;;

let assert_budget client session_id count =
  let final = snapshot client session_id in
  require
    (!count = 11)
    "self-triggered turns did not reach and stop at the configured stream budget";
  let failures = failed_operations client session_id in
  require
    (List.length failures = 1)
    "exhausted self-trigger budget did not produce one durable failure";
  require
    (String.is_substring
       (List.hd_exn failures).message
       ~substring:"maximum consecutive moderator-requested turns (10)")
    "automatic turns failed for a reason other than the budget";
  assert_unique final.canonical_history.entries
;;

let test_budget env environment =
  let fixture = fixture env environment "runtime-budget" ~ask:false in
  add_script environment fixture budget_script;
  let count = ref 0 in
  let host_options =
    { Agent_server.Daemon.default_options with
      model_post_stream = Some (budget_stream count)
    }
  in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    P.with_client ~sw env fixture (fun client ->
      let session = P.create_session client "unattended" "budget:create" in
      let session = P.start_session client session "budget:start" in
      ignore
        (send client session "initial-message"
         : Agent_protocol.Method_result.Send_message.t);
      ignore
        (P.await_operation_end env client session.summary.id 250
         : Agent_protocol.Session.t);
      assert_budget client session.summary.id count))
;;

let wake_script =
  {|
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Turn_start | `Item_appended(item)
    | `Pre_tool_call(tool_call) | `Post_tool_response(tool_result) | `Wake | `Turn_end ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Pre_tool_call(call) ->
      Task.bind(Schedule.after_ms(0, `Wake), fun ignored ->
      Task.bind(Tool.approve(), fun ignored -> Task.pure(state)))
    | `Wake ->
      Task.bind(Turn.append_item(Item.output_text_message("wake", "wake-at-safe-boundary")), fun ignored -> Task.pure(state + 1))
    | `Turn_end ->
      Task.bind(Runtime.end_session("wake-count:" ++ to_string(state)), fun ignored -> Task.pure(state))
    | _ -> Task.pure(state)
</script>
|}
;;

let rec await_scheduled_delivery env client session_id attempts =
  let current = snapshot client session_id in
  let delivered =
    List.exists current.schedules ~f:(fun schedule ->
      match schedule.Agent_protocol.Schedule.status with
      | Delivered -> true
      | _ -> false)
  in
  if delivered
  then current
  else if attempts = 0
  then failwith "scheduled wake was not queued while tool suspended"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_scheduled_delivery env client session_id (attempts - 1))
;;

let test_wake env environment =
  let fixture = fixture env environment "runtime-wake" ~ask:true in
  add_script environment fixture wake_script;
  Daemon_host.with_
    env
    fixture
    ~options:(options fixture (ref []))
    (fun sw _daemon ->
       P.with_client ~sw env fixture (fun client ->
         let session = P.create_session client "unattended" "wake:create" in
         let session = P.start_session client session "wake:start" in
         ignore
           (send client session "initial-message"
            : Agent_protocol.Method_result.Send_message.t);
         let permission = P.await_pending_permission env client session.summary.id 250 in
         let waiting = await_scheduled_delivery env client session.summary.id 250 in
         require (not waiting.halted) "queued wake drained while tool was suspended";
         P.respond_approve client session permission "wake:approve";
         ignore
           (P.await_operation_end env client session.summary.id 250
            : Agent_protocol.Session.t);
         let final = snapshot client session.summary.id in
         require
           (Option.equal String.equal final.halt_reason (Some "wake-count:1"))
           "queued wake was lost, duplicated, or missed post-tool safe point";
         ignore (assert_pair final.canonical_history.entries : int)))
;;

let cases =
  [ "history.tool-pair-deferred-restart", test_fifo
  ; "moderator.boundaries-overlays-halt", test_moderator
  ; "moderator.self-trigger-budget", test_budget
  ; "moderator.wakeup-during-tool", test_wake
  ; "generated.provider-settings-restart", Generated_provider_scenario.test
  ]
;;

let run env ~case =
  let selected =
    match case with
    | None -> cases
    | Some name -> [ name, List.Assoc.find_exn cases ~equal:String.equal name ]
  in
  Temporary_environment.with_ ~scenario:"runtime-integrity" ~env (fun environment ->
    List.iter selected ~f:(fun (name, test) ->
      test env environment;
      Eio.Flow.copy_string
        (Sexp.to_string_hum [%sexp "runtime integrity passed", (name : string)] ^ "\n")
        (Eio.Stdenv.stdout env)))
;;
