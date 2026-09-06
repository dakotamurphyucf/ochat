open Core
module F = Background_fixture
module Config = Config_fixture

let prompt payload =
  sprintf
    {|
<developer>MANUAL-ORCHESTRATION-READY — isolated deterministic fixture.</developer>
<script language="chatml" kind="moderator">
  type state = bool
  type event = [ `Session_start | `Session_resume | `Go | `Wake
    | `Model_job_succeeded(string, string, json)
    | `Model_job_failed(string, string, string) ]
  let initial_state = false
  let on_event : context -> state -> event -> state task =
    fun ctx state event -> match event with
    | `Go -> if state then Task.pure(state) else
      Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun id ->
      Task.bind(Turn.append_notice("MANUAL-BACKGROUND-PENDING"), fun ignored ->
      Task.pure(true)))
    | `Model_job_succeeded(id, recipe, result) ->
      Task.bind(Turn.append_notice("MANUAL-BACKGROUND-DONE: " ++ Json.stringify(result)),
        fun ignored -> Task.pure(state))
    | `Model_job_failed(id, recipe, message) ->
      Task.bind(Turn.append_notice("MANUAL-BACKGROUND-FAILED: " ++ message),
        fun ignored -> Task.pure(state))
    | `Wake -> Task.bind(Turn.append_notice("MANUAL-BACKGROUND-WAKE"),
        fun ignored -> Task.pure(state))
    | _ -> Task.pure(state)
</script>
|}
    payload
;;

let configure fixture =
  let nested = Filename.concat (Config.physical_workspace fixture) "nested.chatmd" in
  F.save fixture nested "<developer>Reply with background-result.</developer>";
  let payload =
    `Object
      [ "prompt", `String nested
      ; "input", `String "manual-background-input"
      ; "is_local", `True
      ]
    |> Jsonaf.to_string
  in
  F.save fixture (Config.prompt_path fixture) (prompt payload)
;;

let text_count snapshot text =
  let window =
    Option.value
      snapshot.Agent_protocol.Snapshot.effective_history
      ~default:snapshot.canonical_history
  in
  List.count window.entries ~f:(fun entry ->
    String.is_substring
      (Jsonaf.to_string entry.Agent_protocol.History.payload)
      ~substring:text)
;;

let has_text snapshot text = text_count snapshot text > 0

let completed snapshot =
  match snapshot.Agent_protocol.Snapshot.jobs with
  | [ { status = Succeeded; delivery = Delivered _; _ } ] ->
    text_count snapshot "MANUAL-BACKGROUND-DONE" = 1
    && has_text snapshot "background-result"
  | _ -> false
;;

let start_job ~sw env fixture provider =
  F.with_client ~sw env fixture (fun client ->
    let session = F.create client "manual-orchestration-create" in
    ignore (F.schedule client session "manual-go" "Go" 0 : Agent_protocol.Schedule.t);
    ignore
      (Tui_stream_provider.await_request provider env 0 : Tui_stream_provider.request);
    let snapshot = F.snapshot client session in
    F.require
      (List.exists snapshot.jobs ~f:(fun job ->
         match job.Agent_protocol.Job.status with
         | Running -> true
         | _ -> false))
      "background job was not running before client close";
    F.close_client client;
    session.summary)
;;

let self_check ~sw env fixture provider manual =
  let summary = start_job ~sw env fixture provider in
  Tui_manual_provider.respond manual ~env ~action:"background" ~index:0;
  F.with_client ~sw env fixture (fun client ->
    let session, _ = F.attach client summary "manual-background-reattach" in
    ignore
      (F.await_snapshot env client session "background delivery" completed
       : Agent_protocol.Snapshot.t);
    ignore (F.schedule client session "manual-wake" "Wake" 0 : Agent_protocol.Schedule.t);
    let snapshot =
      F.await_snapshot env client session "post-result wake" (fun s ->
        has_text s "MANUAL-BACKGROUND-WAKE")
    in
    F.require (not snapshot.halted) "orchestrator halted after background delivery";
    F.require
      (Poly.equal snapshot.session.observed_state Idle
       && Poly.equal snapshot.session.desired_state Running)
      "background completion stopped or started a root turn";
    F.require (text_count snapshot "MANUAL-BACKGROUND-WAKE" = 1) "wake duplicated";
    F.require (completed snapshot) "wake lost the delivered background result";
    F.require
      (Tui_stream_provider.request_count provider = 1)
      "background work duplicated")
;;
