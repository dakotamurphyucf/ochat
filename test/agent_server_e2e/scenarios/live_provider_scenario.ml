open Core
module F = Support.Background_fixture
module Config = Support.Config_fixture
module Proxy = Support.Live_openai_proxy
module Report = Support.Load_report

let snapshot_reads_started = ref 0
let snapshot_reads_finished = ref 0

let await env description predicate =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 120. (fun () ->
    let rec loop () =
      match predicate () with
      | Some value -> value
      | None ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
        loop ()
    in
    ignore (description : string);
    loop ())
;;

let idle env client session =
  await env "live operation completion" (fun () ->
    Int.incr snapshot_reads_started;
    let state = F.snapshot client session in
    Int.incr snapshot_reads_finished;
    match state.session.active_operation with
    | Some _ -> None
    | None ->
      Option.iter state.failure ~f:(fun _ ->
        failwith "live session failed; inspect sanitized fixture diagnostics");
      Some state)
;;

let send client session key text =
  match
    F.request
      client
      (Session_send_message
         { session_id = session.F.summary.id
         ; attachment_id = session.attachment_id
         ; content = { kind = Plain_text; text; attachments = [] }
         ; idempotency_key = F.key key
         })
  with
  | Session_send_message result -> result
  | _ -> failwith "unexpected live send response"
;;

let configure env environment =
  let fixture =
    Config.create environment ~name:"live-openai" ~http_port:(F.reserve_port env)
  in
  let nested = Filename.concat (Config.physical_workspace fixture) "nested.chatmd" in
  F.save
    fixture
    nested
    "<config model=\"gpt-5.6-sol\" max_tokens=\"4096\" \
     reasoning_effort=\"low\"/><developer>Reply with LIVE-BACKGROUND-OK.</developer>";
  let payload =
    Jsonaf.to_string
      (`Object
          [ "prompt", `String nested; "input", `String "Reply now."; "is_local", `True ])
  in
  F.save
    fixture
    (Config.prompt_path fixture)
    (sprintf
       {|<config model="gpt-5.6-sol" max_tokens="4096" reasoning_effort="low"/>
<developer>Isolated integration fixture. Follow the user's tiny test instructions exactly. Do not call tools unless requested.</developer>
<tool name="append_to_file"><write path="${workspace}"/></tool>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Go
 | `Item_appended(item) | `Pre_tool_call(tool_call) | `Post_tool_response(tool_result)
 | `Model_job_succeeded(string, string, json) | `Model_job_failed(string, string, string) ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun ctx state event -> match event with
 | `Session_start -> Task.pure(state)
 | `Session_resume -> Task.pure(state)
 | `Turn_start -> Task.bind(Turn.prepend_system("Keep fixture replies concise."), fun ignored -> Task.pure(state))
 | `Go -> Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun ignored -> Task.pure(state))
 | `Model_job_succeeded(_, _, _) -> Task.pure(state + 1)
 | `Model_job_failed(_, _, _) -> Task.pure(state - 1)
 | `Turn_end -> Task.pure(state)
 | `Item_appended(_) -> Task.pure(state)
 | `Pre_tool_call(_) -> Task.pure(state)
 | `Post_tool_response(_) -> Task.pure(state)
</script>|}
       payload);
  F.save
    fixture
    (Config.config_path fixture)
    (Config.configuration fixture ()
     |> String.substr_replace_first
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)");
  fixture
;;

let tool_journey env fixture client session =
  let marker = Filename.concat (Config.physical_workspace fixture) "live-marker.txt" in
  ignore
    (send
       client
       session
       "live:tool"
       (sprintf
          "Call append_to_file exactly once to append LIVE-TOOL-OK to %s. Then reply \
           LIVE-FOREGROUND-OK."
          marker)
     : Agent_protocol.Method_result.Send_message.t);
  let state = idle env client session in
  let events = F.events client session "live:tool-events" in
  List.iter events ~f:(fun event ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> F.protocol_ok
    with
    | Operation_failed { state = Failed error; _ } ->
      failwith ("live operation failed: " ^ error.message)
    | _ -> ());
  F.require
    (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / marker))
    "live tool did not write its marker";
  let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / marker) in
  F.require
    (String.equal (String.strip contents) "LIVE-TOOL-OK")
    "live tool marker was duplicated or incorrect";
  F.require
    (List.exists state.canonical_history.entries ~f:(fun item ->
       Agent_protocol.History.equal_kind item.kind Tool_output))
    "live tool output missing from canonical history"
;;

let compact env client session =
  let before = F.snapshot client session in
  ignore
    (F.request
       client
       (Session_compact
          { session_id = session.F.summary.id
          ; attachment_id = session.attachment_id
          ; expected_revision = Some before.session.revision
          ; idempotency_key = F.key "live:compact"
          })
     : Agent_protocol.Method_result.t);
  let after = idle env client session in
  F.require
    (not
       (String.equal
          (Jsonaf.to_string
             (Agent_protocol.History.Window.to_json before.canonical_history))
          (Jsonaf.to_string
             (Agent_protocol.History.Window.to_json after.canonical_history))))
    "live compaction did not replace canonical history"
;;

let cancel env client session proxy =
  let before = Proxy.forwarded proxy in
  let sent =
    send
      client
      session
      "live:cancel"
      "Do not use tools. Write the integers 1 through 10000, one per line, without \
       skipping any."
  in
  let operation_id = Option.value_exn sent.operation_id in
  await env "upstream dispatch before cancellation" (fun () ->
    if Proxy.forwarded proxy > before then Some () else None);
  ignore
    (F.request
       client
       (Session_cancel_operation
          { session_id = session.F.summary.id
          ; attachment_id = session.attachment_id
          ; operation_id
          ; idempotency_key = F.key "live:cancel-operation"
          })
     : Agent_protocol.Method_result.t);
  ignore (idle env client session : Agent_protocol.Snapshot.t)
;;

let start_daemon sw env fixture port =
  let daemon =
    Support.Daemon_process.start_in_directory_with_environment_overrides
      ~sw
      ~env
      ~fixture
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / Config.physical_workspace fixture)
      ~environment_overrides:
        [ "API_URL", sprintf "http://127.0.0.1:%d" port
        ; "OPENAI_API_KEY", "live-relay-sentinel"
        ]
      ~config_path:(Config.config_path fixture)
  in
  match Support.Daemon_process.wait_ready daemon ~env ~timeout_seconds:10. with
  | Ok _ -> daemon
  | Error _ ->
    F.stop env daemon;
    failwith "live fixture daemon not ready"
;;

let background env fixture sw client session =
  ignore (F.schedule client session "live:background" "Go" 0 : Agent_protocol.Schedule.t);
  F.close_client client;
  F.with_client ~sw env fixture (fun observer ->
    let state =
      await env "live background delivery" (fun () ->
        let state = F.snapshot observer session in
        if
          List.exists state.jobs ~f:(fun job ->
            match job.status, job.delivery with
            | Succeeded, Delivered _ -> true
            | Failed _, _ -> failwith "live background job failed"
            | _ -> false)
        then Some state
        else None)
    in
    F.require (List.length state.jobs = 1) "live background work was duplicated")
;;

exception Journey_complete

let with_relay_switch f =
  try
    Eio.Switch.run (fun sw ->
      f sw;
      Eio.Switch.fail sw Journey_complete)
  with
  | Journey_complete -> ()
;;

let journey env environment report =
  let fixture = configure env environment in
  with_relay_switch (fun sw ->
    let port = F.reserve_port env in
    let proxy = Proxy.start ~sw ~env ~environment ~port in
    let daemon = start_daemon sw env fixture port in
    let monitoring = ref true in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while !monitoring do
        Report.record
          report
          env
          "stream-progress"
          (Proxy.metrics proxy
           @ [ "snapshot_reads_started", `Number (Int.to_string !snapshot_reads_started)
             ; "snapshot_reads_finished", `Number (Int.to_string !snapshot_reads_finished)
             ; ( "daemon_stderr_bytes"
               , `Number (Int.to_string (Support.Daemon_process.stderr daemon).bytes_seen)
               )
             ]);
        Eio.Time.sleep (Eio.Stdenv.clock env) 10.
      done;
      `Stop_daemon);
    Exn.protect
      ~finally:(fun () ->
        monitoring := false;
        Report.record report env "final-relay-metrics" (Proxy.metrics proxy);
        F.stop env daemon)
      ~f:(fun () ->
        F.with_client ~sw env fixture (fun client ->
          let session = F.create client "live:create" in
          tool_journey env fixture client session;
          Report.record report env "tool-passed" (Proxy.metrics proxy);
          cancel env client session proxy;
          Report.record report env "cancel-after-dispatch-passed" (Proxy.metrics proxy);
          compact env client session;
          Report.record report env "compaction-passed" (Proxy.metrics proxy);
          background env fixture sw client session);
        Report.record report env "background-passed" (Proxy.metrics proxy)))
;;

let run env =
  let report = Report.create env "live-openai" in
  try
    Support.Temporary_environment.with_ ~scenario:"live-openai" ~env (fun environment ->
      journey env environment report);
    let path = Report.finish report in
    Eio.Flow.copy_string
      ("live OpenAI journey passed: " ^ path ^ "\n")
      (Eio.Stdenv.stdout env)
  with
  | exn ->
    Report.fail report;
    raise exn
;;
