open Core
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Daemon_process = Support.Daemon_process
module Fake_openai = Support.Fake_openai
module Http_driver = Support.Http_driver
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Res = Openai.Responses

type session =
  { summary : Agent_protocol.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let result_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "HTTP operation failed", (error : string)]
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let prompt =
  {|
<developer>Invoke append_to_file exactly once when asked.</developer>
<tool name="append_to_file">
  <write path="${workspace}"/>
</tool>
|}
;;

let save environment path contents =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment path)
    contents
;;

let permission_configuration fixture ~profile ~tool_default ~approval_timeout ~fallback =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_all ~pattern:"unattended" ~with_:profile
  |> String.substr_replace_first
       ~pattern:"(tool_default deny)"
       ~with_:(sprintf "(tool_default %s)" tool_default)
  |> String.substr_replace_first
       ~pattern:"(approval_timeout none)"
       ~with_:approval_timeout
  |> String.substr_replace_first
       ~pattern:"(approval_fallback deny)"
       ~with_:(sprintf "(approval_fallback %s)" fallback)
  |> String.substr_replace_first
       ~pattern:"(manifest_authorization deny)"
       ~with_:"(manifest_authorization assume_authorized)"
;;

let configure_fixture
      env
      environment
      name
      ~profile
      ~tool_default
      ?(approval_timeout = "(approval_timeout none)")
      ?(fallback = "deny")
      ()
  =
  let fixture = Config_fixture.create environment ~name ~http_port:(reserve_port env) in
  save environment (Config_fixture.prompt_path fixture) prompt;
  let configuration =
    permission_configuration fixture ~profile ~tool_default ~approval_timeout ~fallback
  in
  save environment (Config_fixture.config_path fixture) configuration;
  fixture
;;

let output_text text =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let function_call_stream marker =
  let item =
    Res.Response_stream.Item.Function_call
      { name = "append_to_file"
      ; arguments = ""
      ; call_id = "permission-call"
      ; _type = "function_call"
      ; id = Some "permission-item"
      ; status = Some "in_progress"
      }
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Function_call_arguments_done
      { arguments =
          Jsonaf.to_string
            (`Object [ "path", `String marker; "content", `String "executed" ])
      ; item_id = "permission-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let message_stream () =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = "permission-complete"
    ; content = [ output_text "done" ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Output_text_delta
      { item_id = message.id
      ; output_index = 0
      ; content_index = 0
      ; delta = "done"
      ; type_ = "response.output_text.delta"
      }
  ; Res.Response_stream.Output_item_done
      { item; output_index = 0; type_ = "response.output_item.done" }
  ]
;;

let has_tool_output inputs =
  List.exists inputs ~f:(function
    | Res.Item.Function_call_output _ -> true
    | _ -> false)
;;

let model_post_stream ?before_tool_call marker ~sw:_ ~inputs =
  let events =
    if has_tool_output inputs
    then message_stream ()
    else (
      Option.iter before_tool_call ~f:(fun wait -> wait ());
      function_call_stream marker)
  in
  Stdlib.List.to_seq events
;;

let options ?before_tool_call marker =
  { Agent_server.Daemon.default_options with
    model_post_stream = Some (model_post_stream ?before_tool_call marker)
  }
;;

let with_client ~sw env fixture f =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some (Config_fixture.admin_token fixture))
    |> result_ok
  in
  Exn.protect
    ~f:(fun () ->
      ignore (Http_driver.initialize client |> protocol_ok : _);
      f client)
    ~finally:(fun () -> Http_driver.shutdown client)
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let permissions client session_id state =
  let request =
    Agent_protocol.Permission.List_request.
      { session_id; page = page_request (); state = Some state }
  in
  match (Http_driver.request client (Permission_list request) |> protocol_ok).result with
  | Permission_list page -> page.items
  | _ -> fail "permission.list returned the wrong result"
;;

let jobs client session_id =
  let request =
    Agent_protocol.Job.List_request.
      { session_id; page = page_request (); status = None; kind = None }
  in
  match (Http_driver.request client (Job_list request) |> protocol_ok).result with
  | Job_list page -> page.items
  | _ -> fail "job.list returned the wrong result"
;;

let rec await_pending_permission env client session_id attempts =
  match permissions client session_id Pending with
  | permission :: _ -> permission
  | [] when attempts > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_pending_permission env client session_id (attempts - 1)
  | [] -> fail "session did not publish a pending permission"
;;

let catalog client =
  let prompts =
    Agent_protocol.Prompt.List_request.
      { page = page_request (); enabled = Some true; available = Some true }
  in
  let workspaces =
    Agent_protocol.Workspace.List_request.
      { page = page_request (); kind = None; access = None; available = Some true }
  in
  let prompt =
    match (Http_driver.request client (Prompt_list prompts) |> protocol_ok).result with
    | Prompt_list page -> List.hd_exn page.items
    | _ -> fail "prompt.list returned the wrong result"
  in
  let workspace =
    match
      (Http_driver.request client (Workspace_list workspaces) |> protocol_ok).result
    with
    | Workspace_list page -> List.hd_exn page.items
    | _ -> fail "workspace.list returned the wrong result"
  in
  prompt, workspace
;;

let session_spec prompt workspace profile =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.Agent_protocol.Prompt.id)
    ~workspace:(Configured workspace.Agent_protocol.Workspace.id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:profile
    ~start_immediately:false
    ~labels:[ "suite", "permission" ]
    ()
  |> protocol_ok
;;

let create_session client profile key =
  let prompt, workspace = catalog client in
  let spec = session_spec prompt workspace profile in
  let request =
    Agent_protocol.Session.Create_request.
      { spec
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_create request) |> protocol_ok).result with
  | Session_create created ->
    { summary = created.session
    ; attachment = (Option.value_exn created.attachment).attachment
    }
  | _ -> fail "session.create returned the wrong result"
;;

let start_session client session key =
  let request =
    Agent_protocol.Session.Start_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_start request) |> protocol_ok).result with
  | Session_start mutation -> { session with summary = mutation.session }
  | _ -> fail "session.start returned the wrong result"
;;

let send_message client session key =
  let request =
    Agent_protocol.Session.Send_message_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; content = { kind = Plain_text; text = "run permission probe"; attachments = [] }
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Session_send_message request) |> protocol_ok).result
  with
  | Session_send_message sent -> sent
  | _ -> fail "session.send_message returned the wrong result"
;;

let get_session client session_id =
  match
    (Http_driver.request client (Session_get { session_id; history = None })
     |> protocol_ok)
      .result
  with
  | Session_get snapshot -> snapshot.session
  | _ -> fail "session.get returned the wrong result"
;;

let rec await_operation_end env client session_id attempts =
  let session = get_session client session_id in
  if Option.is_none session.active_operation
  then session
  else if attempts = 0
  then fail "permission probe operation did not finish"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_operation_end env client session_id (attempts - 1))
;;

let marker_path environment fixture =
  Filename.concat (Config_fixture.physical_workspace fixture) "permission-marker.txt"
  |> Temporary_environment.path environment
;;

let run_probe env environment fixture profile host_options ~expected_marker =
  let marker = marker_path environment fixture in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client profile (profile ^ ":create") in
      let session = start_session client session (profile ^ ":start") in
      ignore (send_message client session (profile ^ ":send") : _);
      ignore (await_operation_end env client session.summary.id 250 : _);
      require
        (Bool.equal (Eio.Path.is_file marker) expected_marker)
        "permission profile produced the wrong tool execution result"))
;;

let test_profile env environment ~name ~tool_default ~expected_marker =
  let profile = String.tr name ~target:'.' ~replacement:'-' in
  let fixture = configure_fixture env environment name ~profile ~tool_default () in
  let marker = marker_path environment fixture in
  let marker_native = Eio.Path.native_exn marker in
  run_probe env environment fixture profile (options marker_native) ~expected_marker
;;

let test_deny env environment =
  test_profile
    env
    environment
    ~name:"profile.deny"
    ~tool_default:"deny"
    ~expected_marker:false
;;

let test_allow env environment =
  test_profile
    env
    environment
    ~name:"profile.allow"
    ~tool_default:"allow"
    ~expected_marker:true
;;

let respond_permission client session permission choice key =
  let request =
    Agent_protocol.Permission.Respond_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; permission_id = permission.Agent_protocol.Permission.id
      ; permission_generation = permission.generation
      ; choice
      ; reason = Some "E2E interactive approval"
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Permission_respond request) |> protocol_ok).result
  with
  | Permission_respond _ -> ()
  | _ -> fail "permission.respond returned the wrong result"
;;

let respond_approve client session permission key =
  respond_permission client session permission Approve_once key
;;

let test_ask_authorized env environment =
  let name = "profile.ask-authorized" in
  let profile = "profile-ask-authorized" in
  let fixture = configure_fixture env environment name ~profile ~tool_default:"ask" () in
  let marker = marker_path environment fixture in
  let host_options = options (Eio.Path.native_exn marker) in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client profile (name ^ ":create") in
      let session = start_session client session (name ^ ":start") in
      ignore (send_message client session (name ^ ":send") : _);
      let permission = await_pending_permission env client session.summary.id 250 in
      require (not (Eio.Path.is_file marker)) "tool ran before interactive approval";
      respond_approve client session permission (name ^ ":approve");
      ignore (await_operation_end env client session.summary.id 250 : _);
      require (Eio.Path.is_file marker) "approved interactive tool did not execute"))
;;

let close_logical_connection client =
  let response = Http_driver.close_connection client |> result_ok in
  require
    (response.status >= 200 && response.status < 300)
    "logical HTTP connection did not close successfully"
;;

let run_detached_probe env fixture profile name ~host_options ~gate_resolver ~observe =
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    let session_id =
      with_client ~sw env fixture (fun writer ->
        let session = create_session writer profile (name ^ ":create") in
        let session = start_session writer session (name ^ ":start") in
        ignore (send_message writer session (name ^ ":send") : _);
        close_logical_connection writer;
        Eio.Promise.resolve gate_resolver ();
        session.summary.id)
    in
    with_client ~sw env fixture (fun observer ->
      ignore (await_operation_end env observer session_id 250 : _);
      observe observer session_id))
;;

let detached_probe env fixture profile name ~host_options ~gate_resolver =
  let no_pending = ref false in
  run_detached_probe
    env
    fixture
    profile
    name
    ~host_options
    ~gate_resolver
    ~observe:(fun observer session_id ->
      no_pending := List.is_empty (permissions observer session_id Pending));
  !no_pending
;;

let test_ask_no_responder env environment =
  let name = "profile.ask-no-responder" in
  let profile = "profile-ask-no-responder" in
  let fixture = configure_fixture env environment name ~profile ~tool_default:"ask" () in
  let marker = marker_path environment fixture in
  let gate, gate_resolver = Eio.Promise.create () in
  let host_options =
    options
      ~before_tool_call:(fun () -> Eio.Promise.await gate)
      (Eio.Path.native_exn marker)
  in
  let no_pending = detached_probe env fixture profile name ~host_options ~gate_resolver in
  require (not (Eio.Path.is_file marker)) "ask fallback ran without a responder";
  require no_pending "no-responder fallback left a pending permission"
;;

let policy_options marker profile allowed =
  { (options marker) with
    policy_evaluator_resolver =
      Some
        (fun id ->
          if String.equal id profile
          then Some ("e2e-policy-v1", fun _invocation -> Ok allowed)
          else None)
  }
;;

let test_policy_decision env environment name allowed =
  let profile = String.tr name ~target:'.' ~replacement:'-' in
  let fixture =
    configure_fixture env environment name ~profile ~tool_default:"policy" ()
  in
  let marker = marker_path environment fixture in
  let host_options = policy_options (Eio.Path.native_exn marker) profile allowed in
  run_probe env environment fixture profile host_options ~expected_marker:allowed
;;

let test_policy_allow_deny env environment =
  test_policy_decision env environment "profile.policy-allow" true;
  test_policy_decision env environment "profile.policy-deny" false
;;

let timeout_fixture env environment name fallback =
  let profile = String.tr name ~target:'.' ~replacement:'-' in
  let fixture =
    configure_fixture
      env
      environment
      name
      ~profile
      ~tool_default:"ask"
      ~approval_timeout:"(approval_timeout_ms 50)"
      ~fallback
      ()
  in
  profile, fixture
;;

let require_permission_terminal client session_id state =
  require
    (Int.equal (List.length (permissions client session_id state)) 1)
    "timeout did not produce exactly one terminal permission"
;;

let run_timeout_probe
      env
      environment
      fixture
      profile
      host_options
      ~expected_marker
      ~expected_state
  =
  let marker = marker_path environment fixture in
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client profile (profile ^ ":create") in
      let session = start_session client session (profile ^ ":start") in
      ignore (send_message client session (profile ^ ":send") : _);
      ignore (await_operation_end env client session.summary.id 250 : _);
      require
        (Bool.equal (Eio.Path.is_file marker) expected_marker)
        "timeout fallback produced the wrong tool execution result";
      require_permission_terminal client session.summary.id expected_state))
;;

let test_timeout_disabled env environment =
  let name = "timeout.disabled" in
  let profile = "timeout-disabled" in
  let fixture = configure_fixture env environment name ~profile ~tool_default:"ask" () in
  let marker = marker_path environment fixture in
  Daemon_host.with_ env fixture ~options:(options (Eio.Path.native_exn marker))
  @@ fun sw _daemon ->
  with_client ~sw env fixture (fun client ->
    let session = create_session client profile (name ^ ":create") in
    let session = start_session client session (name ^ ":start") in
    ignore (send_message client session (name ^ ":send") : _);
    let permission = await_pending_permission env client session.summary.id 250 in
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.15;
    require
      (Int.equal (List.length (permissions client session.summary.id Pending)) 1)
      "disabled timeout resolved without a client response";
    require (not (Eio.Path.is_file marker)) "disabled timeout executed the tool";
    respond_permission client session permission Deny (name ^ ":cleanup");
    ignore (await_operation_end env client session.summary.id 250 : _))
;;

let test_timeout_fallback env environment name fallback expected_marker expected_state =
  let profile, fixture = timeout_fixture env environment name fallback in
  let marker = marker_path environment fixture |> Eio.Path.native_exn in
  run_timeout_probe
    env
    environment
    fixture
    profile
    (options marker)
    ~expected_marker
    ~expected_state
;;

let test_timeout_deny env environment =
  test_timeout_fallback env environment "timeout.deny" "deny" false Denied
;;

let test_timeout_allow env environment =
  test_timeout_fallback env environment "timeout.allow" "allow" true Approved
;;

let test_timeout_policy_decision env environment name allowed =
  let profile, fixture = timeout_fixture env environment name "allow_if_policy" in
  let marker = marker_path environment fixture |> Eio.Path.native_exn in
  run_timeout_probe
    env
    environment
    fixture
    profile
    (policy_options marker profile allowed)
    ~expected_marker:allowed
    ~expected_state:(if allowed then Approved else Denied)
;;

let test_timeout_allow_if_policy env environment =
  test_timeout_policy_decision env environment "timeout.policy-allow" true;
  test_timeout_policy_decision env environment "timeout.policy-deny" false
;;

let reviewer_options marker reviewer =
  let reviewer_id = Agent_session.Permission_reviewer.id reviewer in
  let reviewer_kind = Agent_session.Permission_reviewer.kind reviewer in
  let base = options marker in
  { base with
    reviewer_resolver =
      Some
        (fun kind id ->
          if
            Agent_session.Permission_reviewer.equal_kind kind reviewer_kind
            && String.equal id reviewer_id
          then Some reviewer
          else None)
  }
;;

let reviewer_fixture env environment name kind reviewer_id =
  let profile = String.tr name ~target:'.' ~replacement:'-' in
  let fallback =
    match kind with
    | Agent_session.Permission_reviewer.Model -> sprintf "(model_reviewer %s)" reviewer_id
    | External -> sprintf "(external_reviewer %s)" reviewer_id
  in
  let fixture =
    configure_fixture env environment name ~profile ~tool_default:"ask" ~fallback ()
  in
  profile, fixture
;;

let require_reviewer_job_redacted jobs marker =
  require (Int.equal (List.length jobs) 1) "reviewer did not create one durable job";
  let payload = (List.hd_exn jobs).Agent_protocol.Job.payload |> Jsonaf.to_string in
  require
    (not (String.is_substring payload ~substring:marker))
    "durable reviewer job leaked the invocation path";
  require
    (String.is_substring payload ~substring:"<redacted>")
    "durable reviewer job did not contain a redacted invocation"
;;

let require_failed_reviewer_job jobs marker =
  require_reviewer_job_redacted jobs marker;
  match (List.hd_exn jobs).Agent_protocol.Job.status with
  | Failed _ -> ()
  | Queued
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Cancelled
  | Interrupted _ -> fail "reviewer failure did not persist a failed job"
;;

let observed_reviewer reviewer_id kind decision observed =
  Agent_session.Permission_reviewer.create
    ~id:reviewer_id
    ~kind
    ~revision:"e2e-reviewer-v1"
    ~review:(fun request ->
      observed := Some request;
      Ok decision)
  |> protocol_ok
;;

let observe_reviewer_result marker allowed observer session_id =
  require_permission_terminal observer session_id (if allowed then Approved else Denied);
  require_reviewer_job_redacted (jobs observer session_id) (Eio.Path.native_exn marker)
;;

let require_reviewer_result marker observed allowed =
  let request : Agent_session.Permission_policy.invocation = Option.value_exn !observed in
  require
    (String.equal request.invocation_display "append_to_file(<redacted>)")
    "reviewer callback received an unredacted invocation";
  require
    (Bool.equal (Eio.Path.is_file marker) allowed)
    "reviewer decision produced the wrong tool execution result"
;;

let gated_model_options marker gate base =
  { (base : Agent_server.Daemon.options) with
    model_post_stream =
      Some (model_post_stream ~before_tool_call:(fun () -> Eio.Promise.await gate) marker)
  }
;;

let test_reviewer_decision env environment name kind decision allowed =
  let reviewer_id = String.tr name ~target:'.' ~replacement:'-' ^ "-reviewer" in
  let profile, fixture = reviewer_fixture env environment name kind reviewer_id in
  let marker = marker_path environment fixture in
  let observed = ref None in
  let reviewer = observed_reviewer reviewer_id kind decision observed in
  let gate, gate_resolver = Eio.Promise.create () in
  let marker_native = Eio.Path.native_exn marker in
  let host_options =
    gated_model_options marker_native gate (reviewer_options marker_native reviewer)
  in
  run_detached_probe
    env
    fixture
    profile
    name
    ~host_options
    ~gate_resolver
    ~observe:(observe_reviewer_result marker allowed);
  require_reviewer_result marker observed allowed
;;

let test_reviewer_kind env environment kind prefix =
  test_reviewer_decision
    env
    environment
    (prefix ^ "-approve")
    kind
    Agent_session.Permission_reviewer.Decision.Allow
    true;
  test_reviewer_decision
    env
    environment
    (prefix ^ "-deny")
    kind
    (Agent_session.Permission_reviewer.Decision.Deny "reviewer denied")
    false
;;

let test_model_reviewer env environment =
  test_reviewer_kind
    env
    environment
    Agent_session.Permission_reviewer.Model
    "reviewer.model"
;;

let test_external_reviewer env environment =
  test_reviewer_kind
    env
    environment
    Agent_session.Permission_reviewer.External
    "reviewer.external"
;;

let reviewer_failure_fixture env environment name reviewer =
  let reviewer_id = String.tr name ~target:'.' ~replacement:'-' ^ "-reviewer" in
  let profile, fixture =
    reviewer_fixture
      env
      environment
      name
      Agent_session.Permission_reviewer.External
      reviewer_id
  in
  let marker = marker_path environment fixture in
  let marker_native = Eio.Path.native_exn marker in
  let base =
    Option.value_map reviewer ~default:(options marker_native) ~f:(fun reviewer ->
      reviewer_options marker_native reviewer)
  in
  profile, fixture, marker, marker_native, base
;;

let run_reviewer_failure env environment name reviewer =
  let profile, fixture, marker, marker_native, base =
    reviewer_failure_fixture env environment name reviewer
  in
  let gate, gate_resolver = Eio.Promise.create () in
  let host_options = gated_model_options marker_native gate base in
  run_detached_probe
    env
    fixture
    profile
    name
    ~host_options
    ~gate_resolver
    ~observe:(fun observer session_id ->
      require_permission_terminal observer session_id Denied;
      require_failed_reviewer_job (jobs observer session_id) marker_native);
  require (not (Eio.Path.is_file marker)) "failed reviewer allowed tool execution"
;;

let failing_reviewer name review =
  Agent_session.Permission_reviewer.create
    ~id:(String.tr name ~target:'.' ~replacement:'-' ^ "-reviewer")
    ~kind:Agent_session.Permission_reviewer.External
    ~revision:"e2e-reviewer-failure-v1"
    ~review
  |> protocol_ok
;;

let test_reviewer_failures env environment =
  let malformed_name = "reviewer.failure-malformed" in
  let malformed =
    failing_reviewer malformed_name (fun _ ->
      Ok (Agent_session.Permission_reviewer.Decision.Deny ""))
  in
  run_reviewer_failure env environment malformed_name (Some malformed);
  let exception_name = "reviewer.failure-exception" in
  let exception_ = failing_reviewer exception_name (fun _ -> failwith "reviewer broke") in
  run_reviewer_failure env environment exception_name (Some exception_);
  run_reviewer_failure env environment "reviewer.failure-unavailable" None
;;

let rec await_running_reviewer_job env client session_id attempts =
  match jobs client session_id with
  | [ job ] ->
    (match job.Agent_protocol.Job.status with
     | Running -> job
     | (Queued | Waiting_permission _ | Waiting_completion _) when attempts > 0 ->
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
       await_running_reviewer_job env client session_id (attempts - 1)
     | Queued
     | Waiting_permission _
     | Waiting_completion _
     | Succeeded
     | Failed _
     | Cancelled
     | Interrupted _ -> fail "reviewer job did not enter running state")
  | [] when attempts > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_running_reviewer_job env client session_id (attempts - 1)
  | _ -> fail "reviewer cancellation did not expose exactly one job"
;;

let cancel_operation client session operation_id key =
  let request =
    Agent_protocol.Session.Cancel_operation_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; operation_id
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Session_cancel_operation request) |> protocol_ok).result
  with
  | Session_cancel_operation _ -> ()
  | _ -> fail "session.cancel_operation returned the wrong result"
;;

let require_reviewer_interrupted client session_id =
  match jobs client session_id with
  | [ job ] ->
    (match job.Agent_protocol.Job.status with
     | Interrupted _ -> ()
     | status ->
       raise_s
         [%sexp
           "cancelled reviewer job was not interrupted"
         , (status : Agent_protocol.Job.status)])
  | _ -> fail "reviewer cancellation did not retain exactly one job"
;;

let blocking_reviewer reviewer_id revision started =
  let never, _never_resolver = Eio.Promise.create () in
  Agent_session.Permission_reviewer.create
    ~id:reviewer_id
    ~kind:External
    ~revision
    ~review:(fun _ ->
      Option.iter started ~f:(fun resolver ->
        ignore (Eio.Promise.try_resolve resolver ());
        ());
      Eio.Promise.await never)
  |> protocol_ok
;;

let cancelled_reviewer_fixture env environment name reviewer_id profile =
  configure_fixture
    env
    environment
    name
    ~profile
    ~tool_default:"ask"
    ~approval_timeout:"(approval_timeout_ms 0)"
    ~fallback:(sprintf "(external_reviewer %s)" reviewer_id)
    ()
;;

let run_cancelled_reviewer env fixture profile name marker started host_options =
  Daemon_host.with_ env fixture ~options:host_options (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client profile (name ^ ":create") in
      let session = start_session client session (name ^ ":start") in
      let sent = send_message client session (name ^ ":send") in
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await started);
      ignore (await_running_reviewer_job env client session.summary.id 250 : _);
      cancel_operation
        client
        session
        (Option.value_exn sent.operation_id)
        (name ^ ":cancel");
      ignore (await_operation_end env client session.summary.id 250 : _);
      require_reviewer_interrupted client session.summary.id;
      require
        (List.is_empty (permissions client session.summary.id Pending))
        "cancelled reviewer left a pending permission";
      require (not (Eio.Path.is_file marker)) "cancelled reviewer allowed tool execution"))
;;

let test_reviewer_cancelled env environment =
  let name = "reviewer.cancelled" in
  let reviewer_id = "reviewer-cancelled" in
  let profile = "reviewer-cancelled-profile" in
  let fixture = cancelled_reviewer_fixture env environment name reviewer_id profile in
  let started, started_resolver = Eio.Promise.create () in
  let reviewer = blocking_reviewer reviewer_id "e2e-cancel-v1" (Some started_resolver) in
  let marker = marker_path environment fixture in
  let host_options = reviewer_options (Eio.Path.native_exn marker) reviewer in
  run_cancelled_reviewer env fixture profile name marker started host_options
;;

let create_claimed_reviewer_job env fixture profile name options started gate_resolver =
  let session_id = ref None in
  Daemon_host.with_ env fixture ~options (fun sw _daemon ->
    let id =
      with_client ~sw env fixture (fun writer ->
        let session = create_session writer profile (name ^ ":create") in
        let session = start_session writer session (name ^ ":start") in
        ignore (send_message writer session (name ^ ":send") : _);
        close_logical_connection writer;
        Eio.Promise.resolve gate_resolver ();
        session.summary.id)
    in
    with_client ~sw env fixture (fun client ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await started);
      ignore (await_running_reviewer_job env client id 250 : _);
      session_id := Some id));
  Option.value_exn !session_id
;;

let test_reviewer_restart env environment =
  let name = "reviewer.restart-claimed-job" in
  let reviewer_id = "reviewer-restart" in
  let profile, fixture = reviewer_fixture env environment name External reviewer_id in
  let marker = marker_path environment fixture in
  let started, started_resolver = Eio.Promise.create () in
  let gate, gate_resolver = Eio.Promise.create () in
  let reviewer = blocking_reviewer reviewer_id "e2e-restart-v1" (Some started_resolver) in
  let marker_native = Eio.Path.native_exn marker in
  let options =
    gated_model_options marker_native gate (reviewer_options marker_native reviewer)
  in
  let session_id =
    create_claimed_reviewer_job env fixture profile name options started gate_resolver
  in
  Daemon_host.with_ env fixture ~options (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      ignore (await_operation_end env client session_id 250 : _);
      require_reviewer_interrupted client session_id;
      require
        (List.is_empty (permissions client session_id Pending))
        "restart left the reviewer permission pending";
      require (not (Eio.Path.is_file marker)) "restart recovery executed a reviewed tool"))
;;

let respond_permission_result client session permission choice key =
  let request =
    Agent_protocol.Permission.Respond_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; permission_id = permission.Agent_protocol.Permission.id
      ; permission_generation = permission.generation
      ; choice
      ; reason = Some "E2E timeout race response"
      ; idempotency_key = idempotency_key key
      }
  in
  match Http_driver.request client (Permission_respond request) with
  | Ok { result = Permission_respond _; _ } -> ()
  | Error error when Agent_protocol.Error.equal_code error.code Already_resolved -> ()
  | Ok _ -> fail "permission race response returned the wrong result"
  | Error error ->
    raise_s [%sexp "permission race response failed", (error : Agent_protocol.Error.t)]
;;

let terminal_permissions client session_id =
  Agent_protocol.Permission.[ Approved; Denied; Expired; Cancelled ]
  |> List.concat_map ~f:(permissions client session_id)
;;

let require_race_result environment fixture client session_id =
  let terminal = terminal_permissions client session_id in
  require (Int.equal (List.length terminal) 1) "permission race was not single-winner";
  require
    (List.is_empty (permissions client session_id Pending))
    "permission race left a pending permission";
  let marker = marker_path environment fixture in
  match (List.hd_exn terminal).Agent_protocol.Permission.state with
  | Approved ->
    require (Eio.Path.is_file marker) "approved race winner did not execute the tool";
    require
      (Int.equal
         (List.length
            (String.substr_index_all
               (Eio.Path.load marker)
               ~may_overlap:false
               ~pattern:"executed"))
         1)
      "permission race resumed the tool more than once"
  | Denied | Expired | Cancelled ->
    require (not (Eio.Path.is_file marker)) "denied race winner executed the tool"
  | Pending -> assert false
;;

let run_human_timeout_race env environment fixture profile name =
  let marker = marker_path environment fixture |> Eio.Path.native_exn in
  Daemon_host.with_ env fixture ~options:(options marker) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client profile (name ^ ":create") in
      let session = start_session client session (name ^ ":start") in
      ignore (send_message client session (name ^ ":send") : _);
      let permission = await_pending_permission env client session.summary.id 250 in
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
      respond_permission_result client session permission Approve_once (name ^ ":approve");
      ignore (await_operation_end env client session.summary.id 250 : _);
      require_race_result environment fixture client session.summary.id))
;;

let test_human_timeout_race env environment =
  let name = "resolution.human-timeout-race" in
  let profile = "human-timeout-race" in
  let fixture =
    configure_fixture
      env
      environment
      name
      ~profile
      ~tool_default:"ask"
      ~approval_timeout:"(approval_timeout_ms 50)"
      ~fallback:"deny"
      ()
  in
  run_human_timeout_race env environment fixture profile name
;;

let require_daemon_ready daemon env =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok _ -> ()
  | Error error -> raise_s [%sexp "stock daemon did not become ready", (error : _)]
;;

let stop_daemon daemon env =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:2. : _)
;;

let start_stock_daemon ~sw env fixture provider_port =
  Daemon_process.start_with_environment_overrides
    ~sw
    ~env
    ~fixture
    ~environment_overrides:
      [ "API_URL", sprintf "http://127.0.0.1:%d" provider_port
      ; "OPENAI_API_KEY", "e2e-local-provider"
      ]
    ~config_path:(Config_fixture.config_path fixture)
;;

let start_stock_session ~sw env fixture profile name release_resolver =
  with_client ~sw env fixture (fun writer ->
    let session = create_session writer profile (name ^ ":create") in
    let session = start_session writer session (name ^ ":start") in
    ignore (send_message writer session (name ^ ":send") : _);
    close_logical_connection writer;
    Eio.Promise.resolve release_resolver ();
    session.summary.id)
;;

let observe_stock_failure ~sw env fixture marker session_id =
  with_client ~sw env fixture (fun observer ->
    ignore (await_operation_end env observer session_id 250 : _);
    require_permission_terminal observer session_id Denied;
    require_failed_reviewer_job (jobs observer session_id) (Eio.Path.native_exn marker);
    require
      (not (Eio.Path.is_file marker))
      "stock daemon executed a tool after reviewer resolution failed")
;;

let run_stock_probe env fixture profile name marker provider_port release release_resolver
  =
  Eio.Switch.run (fun sw ->
    Fake_openai.start
      ~sw
      ~env
      ~port:provider_port
      ~marker:(Eio.Path.native_exn marker)
      ~release;
    let daemon = start_stock_daemon ~sw env fixture provider_port in
    Exn.protect
      ~f:(fun () ->
        require_daemon_ready daemon env;
        let session_id =
          start_stock_session ~sw env fixture profile name release_resolver
        in
        observe_stock_failure ~sw env fixture marker session_id)
      ~finally:(fun () -> stop_daemon daemon env))
;;

let test_stock_missing_reviewer env environment =
  let name = "stock-daemon.missing-reviewer-fails-closed" in
  let profile = "stock-missing-reviewer" in
  let fixture =
    configure_fixture
      env
      environment
      name
      ~profile
      ~tool_default:"ask"
      ~fallback:"(external_reviewer missing-reviewer)"
      ()
  in
  let marker = marker_path environment fixture in
  let provider_port = reserve_port env in
  let release, release_resolver = Eio.Promise.create () in
  run_stock_probe env fixture profile name marker provider_port release release_resolver
;;

let cases =
  [ "profile.deny", test_deny
  ; "profile.allow", test_allow
  ; "profile.ask-authorized", test_ask_authorized
  ; "profile.ask-no-responder", test_ask_no_responder
  ; "profile.policy-allow-deny", test_policy_allow_deny
  ; "timeout.disabled", test_timeout_disabled
  ; "timeout.deny", test_timeout_deny
  ; "timeout.allow", test_timeout_allow
  ; "timeout.allow-if-policy", test_timeout_allow_if_policy
  ; "reviewer.model-approve-deny", test_model_reviewer
  ; "reviewer.external-approve-deny", test_external_reviewer
  ; "reviewer.failure-malformed-unavailable", test_reviewer_failures
  ; "reviewer.cancelled", test_reviewer_cancelled
  ; "reviewer.restart-claimed-job", test_reviewer_restart
  ; "resolution.human-timeout-race", test_human_timeout_race
  ; "stock-daemon.missing-reviewer-fails-closed", test_stock_missing_reviewer
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown permission case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"permission-reviewers" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn ->
        raise_s [%sexp "permission E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("permission-reviewers" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
