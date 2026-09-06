open Core
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Http_driver = Support.Http_driver
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Res = Openai.Responses

type session =
  { summary : Agent_protocol.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

type hashes =
  { source : string
  ; manifest : string
  ; principal : Agent_protocol.Id.Principal.t
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

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let save environment path contents =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment path)
    contents
;;

let prompt secret =
  sprintf
    {|
<developer>Invoke fixed_echo exactly once for every user message.</developer>
<shell_access id="direct" cwd="${workspace}" pipefail="false">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny"
      scopes="once,exact_session,durable_exact" durable="true"/>
  <secrets replacement="&lt;redacted&gt;"><literal value=%s/></secrets>
  <audit format="jsonl" path="${session_dir}/shell-audit.jsonl"
      content="redacted" failure="deny_start"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct"
    command="/bin/echo" result="stdout"/>
|}
    (Sexp.to_string_mach (Sexp.Atom secret))
;;

let profile_configuration fixture ~manifest_authorization =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_all ~pattern:"unattended" ~with_:"shell-security"
  |> String.substr_replace_first
       ~pattern:"(tool_default deny)"
       ~with_:"(tool_default ask)"
  |> String.substr_replace_first
       ~pattern:"(manifest_authorization deny)"
       ~with_:(sprintf "(manifest_authorization %s)" manifest_authorization)
;;

let grant_text hashes ~source ~manifest ~principal =
  sprintf
    {|
(manifest_grants
 (((id shell-security-exact)
   (prompt smoke)
   (workspaces (physical))
   (manifest_sha256 %s)
   (source_sha256 %s)
   (principals (%s)))))
|}
    manifest
    source
    (Agent_protocol.Id.Principal.to_string principal)
  |> fun grant ->
  String.substr_replace_first
    (profile_configuration hashes ~manifest_authorization:"require_grant")
    ~pattern:"(manifest_grants ())"
    ~with_:grant
;;

let fixture env environment name secret ~manifest_authorization =
  let fixture = Config_fixture.create environment ~name ~http_port:(reserve_port env) in
  save environment (Config_fixture.prompt_path fixture) (prompt secret);
  save
    environment
    (Config_fixture.config_path fixture)
    (profile_configuration fixture ~manifest_authorization);
  fixture
;;

let output_text text =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let tool_stream secret index =
  let item =
    Res.Response_stream.Item.Function_call
      { name = "fixed_echo"
      ; arguments = ""
      ; call_id = sprintf "shell-call-%d" index
      ; _type = "function_call"
      ; id = Some (sprintf "shell-item-%d" index)
      ; status = Some "in_progress"
      }
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Function_call_arguments_done
      { arguments = Jsonaf.to_string (`Object [ "arguments", `Array [ `String secret ] ])
      ; item_id = sprintf "shell-item-%d" index
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let message_stream index =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = sprintf "shell-complete-%d" index
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

let options secret =
  let calls = ref 0 in
  let model_post_stream ~sw:_ ~inputs:_ =
    let index = !calls in
    Int.incr calls;
    let events =
      if index mod 2 = 0 then tool_stream secret index else message_stream index
    in
    Stdlib.List.to_seq events
  in
  { Agent_server.Daemon.default_options with model_post_stream = Some model_post_stream }
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

let session_spec prompt workspace =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.Agent_protocol.Prompt.id)
    ~workspace:(Configured workspace.Agent_protocol.Workspace.id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"shell-security"
    ~start_immediately:false
    ~labels:[ "suite", "shell-security" ]
    ()
  |> protocol_ok
;;

let idempotency_key key = Agent_protocol.Idempotency_key.of_string key |> protocol_ok

let create_request client key =
  let prompt, workspace = catalog client in
  Agent_protocol.Session.Create_request.
    { spec = session_spec prompt workspace
    ; requested_mode = Some Read_write
    ; subscribe = false
    ; idempotency_key = idempotency_key key
    }
;;

let create_session client key =
  match
    (Http_driver.request client (Session_create (create_request client key))
     |> protocol_ok)
      .result
  with
  | Session_create created ->
    { summary = created.session
    ; attachment = (Option.value_exn created.attachment).attachment
    }
  | _ -> fail "session.create returned the wrong result"
;;

let create_is_denied client key =
  Http_driver.request client (Session_create (create_request client key))
  |> Result.is_error
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
      ; content = { kind = Plain_text; text = "run shell probe"; attachments = [] }
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Session_send_message request) |> protocol_ok).result
  with
  | Session_send_message sent -> sent
  | _ -> fail "session.send_message returned the wrong result"
;;

let permissions client session_id state =
  let request =
    Agent_protocol.Permission.List_request.
      { session_id; page = page_request (); state = Some state }
  in
  match (Http_driver.request client (Permission_list request) |> protocol_ok).result with
  | Permission_list page -> page.items
  | _ -> fail "permission.list returned the wrong result"
;;

let rec await_permission env client session_id count =
  match permissions client session_id Pending with
  | permission :: _ -> permission
  | [] when count > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_permission env client session_id (count - 1)
  | [] -> fail "shell invocation did not publish a permission"
;;

let session_get client session_id =
  match
    (Http_driver.request client (Session_get { session_id; history = None })
     |> protocol_ok)
      .result
  with
  | Session_get snapshot -> snapshot.session
  | _ -> fail "session.get returned the wrong result"
;;

let rec await_idle env client session_id count =
  let session = session_get client session_id in
  if Option.is_none session.active_operation
  then session
  else if count = 0
  then fail "shell operation did not finish"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_idle env client session_id (count - 1))
;;

let respond client session permission choice key =
  let request =
    Agent_protocol.Permission.Respond_request.
      { session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; permission_id = permission.Agent_protocol.Permission.id
      ; permission_generation = permission.generation
      ; choice
      ; reason = Some "shell E2E approval"
      ; idempotency_key = idempotency_key key
      }
  in
  match
    (Http_driver.request client (Permission_respond request) |> protocol_ok).result
  with
  | Permission_respond _ -> ()
  | _ -> fail "permission.respond returned the wrong result"
;;

let grants client session_id state =
  let request =
    Agent_protocol.Grant.List_request.
      { page = page_request ()
      ; session_id = Some session_id
      ; principal_id = None
      ; state = Some state
      }
  in
  match (Http_driver.request client (Grant_list request) |> protocol_ok).result with
  | Grant_list page -> page.items
  | _ -> fail "grant.list returned the wrong result"
;;

let shell_grants client session_id state =
  grants client session_id state
  |> List.filter ~f:(fun grant ->
    String.equal grant.Agent_protocol.Grant.tool_name "shell")
;;

let attach client summary key =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id = summary.Agent_protocol.Session.id
      ; requested_mode = Read_write
      ; subscribe = false
      ; after_sequence = Some summary.latest_event_sequence
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Session_attach request) |> protocol_ok).result with
  | Session_attach attached ->
    { summary = session_get client summary.id; attachment = attached.attachment }
  | _ -> fail "session.attach returned the wrong result"
;;

let revoke client session grant key =
  let request =
    Agent_protocol.Grant.Revoke_request.
      { grant_id = grant.Agent_protocol.Grant.id
      ; session_id = session.summary.id
      ; attachment_id = session.attachment.id
      ; reason = "shell E2E revocation"
      ; idempotency_key = idempotency_key key
      }
  in
  match (Http_driver.request client (Grant_revoke request) |> protocol_ok).result with
  | Grant_revoke result -> result.grant
  | _ -> fail "grant.revoke returned the wrong result"
;;

let prompt_hashes daemon fixture =
  let entry =
    Agent_session.Prompt_catalog.find_by_name (Agent_server.Daemon.prompts daemon) "smoke"
    |> Option.value_exn
  in
  let source, manifest =
    match entry.availability with
    | Ready revision ->
      let artifact = Agent_session.Prompt_revision.artifact revision in
      artifact.root_sha256, Option.value_exn artifact.shell_manifest_sha256
    | Disabled | Unavailable _ -> fail "shell prompt did not compile"
  in
  let principal =
    Agent_server.Daemon.authenticate_http_bearer
      daemon
      (Some (Config_fixture.admin_token fixture))
    |> protocol_ok
    |> fun principal -> principal.Agent_protocol.Principal.id
  in
  { source; manifest; principal }
;;

let inspect_hashes env fixture secret =
  let result = ref None in
  Daemon_host.with_ env fixture ~options:(options secret) (fun _sw daemon ->
    result := Some (prompt_hashes daemon fixture));
  Option.value_exn !result
;;

let install_grant environment fixture hashes ~source ~manifest ~principal =
  save
    environment
    (Config_fixture.config_path fixture)
    (grant_text fixture ~source ~manifest ~principal);
  hashes
;;

let manifest_case env environment name mutate ~expected_authorized =
  let secret = "shell-secret-" ^ name in
  Temporary_environment.register_secret environment secret;
  let fixture =
    fixture env environment name secret ~manifest_authorization:"assume_authorized"
  in
  let hashes = inspect_hashes env fixture secret in
  let source, manifest, principal = mutate hashes in
  ignore (install_grant environment fixture hashes ~source ~manifest ~principal : hashes);
  Daemon_host.with_ env fixture ~options:(options secret) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      require
        (Bool.equal
           (not (create_is_denied client (name ^ ":create")))
           expected_authorized)
        "operator manifest grant produced the wrong authorization result"))
;;

let test_manifest_no_grant env environment =
  let name = "manifest.no-grant" in
  let secret = "shell-secret-no-grant" in
  let fixture =
    fixture env environment name secret ~manifest_authorization:"require_grant"
  in
  Daemon_host.with_ env fixture ~options:(options secret) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      require (create_is_denied client (name ^ ":create")) "missing grant was accepted"))
;;

let test_manifest_exact env environment =
  manifest_case
    env
    environment
    "manifest.exact-operator-grant"
    (fun hashes -> hashes.source, hashes.manifest, hashes.principal)
    ~expected_authorized:true
;;

let test_source_mismatch env environment =
  manifest_case
    env
    environment
    "manifest.source-hash-mismatch"
    (fun hashes -> String.make 64 '0', hashes.manifest, hashes.principal)
    ~expected_authorized:false
;;

let test_manifest_mismatch env environment =
  manifest_case
    env
    environment
    "manifest.manifest-hash-mismatch"
    (fun hashes -> hashes.source, String.make 64 '0', hashes.principal)
    ~expected_authorized:false
;;

let test_principal_mismatch env environment =
  manifest_case
    env
    environment
    "manifest.principal-mismatch"
    (fun hashes ->
       let principal =
         Agent_protocol.Id.Principal.of_string "pri_shell_security_wrong" |> protocol_ok
       in
       hashes.source, hashes.manifest, principal)
    ~expected_authorized:false
;;

let run_shell_turn env client session choice key =
  ignore (send_message client session (key ^ ":send") : _);
  let permission = await_permission env client session.summary.id 250 in
  respond client session permission choice (key ^ ":respond");
  ignore (await_idle env client session.summary.id 250 : _);
  permission
;;

let with_approval_session env environment name f =
  let secret = "shell-secret-" ^ name in
  Temporary_environment.register_secret environment secret;
  let fixture =
    fixture env environment name secret ~manifest_authorization:"assume_authorized"
  in
  Daemon_host.with_ env fixture ~options:(options secret) (fun sw daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client (name ^ ":create") in
      let session = start_session client session (name ^ ":start") in
      f secret fixture daemon client session))
;;

let test_approve_once env environment =
  with_approval_session env environment "grant.approve-once" (fun _ _ _ client session ->
    ignore (run_shell_turn env client session Approve_once "approve-once:first" : _);
    require
      (List.is_empty (shell_grants client session.summary.id Active))
      "approve-once persisted a shell grant";
    ignore (send_message client session "approve-once:second-send" : _);
    ignore (await_permission env client session.summary.id 250 : _))
;;

let require_single_grant client session_id =
  require
    (Int.equal (List.length (shell_grants client session_id Active)) 1)
    "session approval did not persist exactly one grant"
;;

let test_session_persist_restart env environment =
  let name = "grant.session-persist-restart" in
  let secret = "shell-secret-session-persist" in
  let fixture =
    fixture env environment name secret ~manifest_authorization:"assume_authorized"
  in
  let session = ref None in
  Daemon_host.with_ env fixture ~options:(options secret) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let created =
        create_session client (name ^ ":create")
        |> fun value -> start_session client value (name ^ ":start")
      in
      ignore (run_shell_turn env client created Approve_session (name ^ ":first") : _);
      require_single_grant client created.summary.id;
      session := Some created.summary));
  Daemon_host.with_ env fixture ~options:(options secret) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let attached = attach client (Option.value_exn !session) (name ^ ":attach") in
      ignore (send_message client attached (name ^ ":second") : _);
      ignore (await_idle env client attached.summary.id 250 : _);
      require
        (List.is_empty (permissions client attached.summary.id Pending))
        "restored session grant requested another approval"))
;;

let test_revoke env environment =
  with_approval_session env environment "grant.revoke" (fun _ _ _ client session ->
    ignore (run_shell_turn env client session Approve_session "revoke:first" : _);
    let grant = List.hd_exn (shell_grants client session.summary.id Active) in
    let revoked = revoke client session grant "revoke:grant" in
    require
      (Agent_protocol.Grant.equal_state revoked.state Revoked)
      "grant revocation did not return revoked state";
    ignore (send_message client session "revoke:second" : _);
    ignore (await_permission env client session.summary.id 250 : _))
;;

let test_single_delegated env environment =
  with_approval_session env environment "shell.single-delegated-approval"
  @@ fun _ _ _ client session ->
  ignore (send_message client session "single-approval:send" : _);
  let permission = await_permission env client session.summary.id 250 in
  require
    (String.is_prefix permission.tool_name ~prefix:"shell:")
    "shell approval was replaced by the generic tool gate";
  require
    (Int.equal (List.length (permissions client session.summary.id Pending)) 1)
    "shell invocation created duplicate approval flows";
  respond client session permission Approve_once "single-approval:respond";
  ignore (await_idle env client session.summary.id 250 : _)
;;

let audit_entries client session_id =
  let request =
    Agent_protocol.Audit.Read_request.
      { page = page_request ()
      ; session_id = Some session_id
      ; principal_id = None
      ; minimum_level = None
      ; name_prefix = None
      }
  in
  match (Http_driver.request client (Audit_read request) |> protocol_ok).result with
  | Audit_read page -> page.items
  | _ -> fail "audit.read returned the wrong result"
;;

let history_json client session_id =
  let request =
    Agent_protocol.History.Window_request.
      { position = Tail 100; limit = 100; effective = false }
  in
  match
    (Http_driver.request client (Session_get { session_id; history = Some request })
     |> protocol_ok)
      .result
  with
  | Session_get snapshot -> Agent_protocol.Snapshot.to_json snapshot |> Jsonaf.to_string
  | _ -> fail "session history returned the wrong result"
;;

let require_redacted secret encoded value label =
  require (not (String.is_substring value ~substring:secret)) (label ^ " leaked secret");
  require
    (not (String.is_substring value ~substring:encoded))
    (label ^ " leaked encoded secret")
;;

let test_redaction env environment =
  with_approval_session env environment "redaction.events-jobs-audit-health-logs"
  @@ fun secret _fixture daemon client session ->
  let encoded = Base64.encode_exn secret in
  ignore (send_message client session "redaction:send" : _);
  let permission = await_permission env client session.summary.id 250 in
  require_redacted
    secret
    encoded
    (Agent_protocol.Permission.to_json permission |> Jsonaf.to_string)
    "permission";
  respond client session permission Approve_once "redaction:respond";
  ignore (await_idle env client session.summary.id 250 : _);
  require_redacted secret encoded (history_json client session.summary.id) "history";
  let audit =
    List.map (audit_entries client session.summary.id) ~f:Agent_protocol.Audit.to_json
  in
  require_redacted secret encoded (Jsonaf.to_string (`Array audit)) "audit";
  let health = Agent_server.Daemon.health daemon ~include_details:true in
  require_redacted
    secret
    encoded
    (Agent_protocol.Health.Response.to_json health |> Jsonaf.to_string)
    "health"
;;

let live_secret = "live-boundary-secret-4c982de7"

let split_payload secret =
  Jsonaf.to_string
    (`Object
        [ "arguments", `Array [ `String secret; `String (Base64.encode_exn secret) ] ])
;;

let live_tool_item name index =
  Res.Response_stream.Item.Function_call
    { name
    ; arguments = ""
    ; call_id = sprintf "live-call-%d" index
    ; _type = "function_call"
    ; id = Some (sprintf "live-item-%d" index)
    ; status = Some "in_progress"
    }
;;

let live_delta index delta =
  Res.Response_stream.Function_call_arguments_delta
    { item_id = sprintf "live-item-%d" index
    ; output_index = 0
    ; delta
    ; type_ = "response.function_call_arguments.delta"
    }
;;

let live_tool_stream name payload index =
  let item = live_tool_item name index in
  let deltas =
    String.to_list payload |> List.map ~f:(fun ch -> live_delta index (String.of_char ch))
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ]
  @ deltas
  @ [ Res.Response_stream.Function_call_arguments_done
        { arguments = payload
        ; item_id = sprintf "live-item-%d" index
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ]
;;

let live_options env nested =
  let calls = ref 0 in
  let model_post_stream ~sw:_ ~inputs:_ =
    let index = !calls in
    Int.incr calls;
    let events =
      match nested, index with
      | true, 0 ->
        live_tool_stream "fork" {|{"command":"run shell probe","arguments":[]}|} index
      | false, 0 | true, 1 ->
        live_tool_stream "fixed_echo" (split_payload live_secret) index
      | _ -> message_stream index
    in
    Stdlib.List.to_seq events
    |> Seq.map (fun event ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
      event)
  in
  { Agent_server.Daemon.default_options with model_post_stream = Some model_post_stream }
;;

let live_prompt () =
  prompt live_secret
  |> String.substr_replace_all
       ~pattern:{|<policy default="ask"/>|}
       ~with_:{|<policy default="allow"/>|}
  |> String.substr_replace_all
       ~pattern:"</secrets>"
       ~with_:
         (sprintf
            "<literal value=%s/></secrets>"
            (Sexp.to_string_mach (Sexp.Atom (Base64.encode_exn live_secret))))
;;

let live_fixture env environment name =
  let fixture =
    fixture env environment name live_secret ~manifest_authorization:"assume_authorized"
  in
  save environment (Config_fixture.prompt_path fixture) (live_prompt ());
  let config =
    profile_configuration fixture ~manifest_authorization:"assume_authorized"
    |> String.substr_replace_all
         ~pattern:"(tool_default ask)"
         ~with_:"(tool_default allow)"
  in
  save environment (Config_fixture.config_path fixture) config;
  Temporary_environment.register_secret environment live_secret;
  Temporary_environment.register_secret environment (Base64.encode_exn live_secret);
  fixture
;;

let next_live_frame env stream frames =
  match Http_driver.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5. with
  | Ok frame -> frame
  | Error message ->
    let kinds =
      List.take frames 16
      |> List.map ~f:(fun event -> event.Agent_protocol.Event.Recoverable.kind)
    in
    raise_s
      [%sexp
        "live SSE read failed"
      , (message : string)
      , (kinds : Agent_protocol.Event.Recoverable.kind list)]
;;

let is_nested_tool_start event =
  match event.Agent_protocol.Event.Recoverable.kind with
  | Tool_trace ->
    let trace = Jsonaf.member_exn "trace" event.payload in
    (match Jsonaf.member "type" trace, Jsonaf.member "name" trace with
     | Some (`String "tool_started"), Some (`String "fixed_echo") -> true
     | _ -> false)
  | _ -> false
;;

let rec collect_live env stream ~nested remaining frames =
  require (remaining > 0) "live event collection did not reach operation completion";
  let frame = next_live_frame env stream frames in
  match frame.event with
  | Some "session.live_event" ->
    let event =
      Jsonaf.of_string frame.data
      |> Agent_protocol.Event.Recoverable.of_json
      |> protocol_ok
    in
    if nested && is_nested_tool_start event
    then List.rev (event :: frames)
    else collect_live env stream ~nested (remaining - 1) (event :: frames)
  | Some "session.event" ->
    let event =
      Jsonaf.of_string frame.data |> Agent_protocol.Event.Durable.of_json |> protocol_ok
    in
    (match event.kind with
     | Operation_completed -> List.rev frames
     | Operation_failed | Operation_cancelled | Operation_interrupted ->
       fail "live probe operation failed"
     | _ -> collect_live env stream ~nested (remaining - 1) frames)
  | _ -> fail "unexpected SSE frame in live redaction probe"
;;

let capture_live ~sw env client session ~nested =
  let stream, response =
    Http_driver.open_session_events
      client
      ~sw
      ~session_id:session.summary.id
      ~after_sequence:session.summary.latest_event_sequence
      ~buffer_capacity:1024
      ()
    |> result_ok
  in
  require (response.status = 200) "live SSE subscription failed";
  Exn.protect
    ~finally:(fun () -> Http_driver.Sse.close stream)
    ~f:(fun () ->
      let frames, (_ : Agent_protocol.Method_result.Send_message.t) =
        Eio.Fiber.pair
          (fun () -> collect_live env stream ~nested 2048 [])
          (fun () -> send_message client session "live:redaction")
      in
      frames)
;;

let stream_delta event =
  match event.Agent_protocol.Event.Recoverable.kind with
  | Sourced_stream | History_correlated_stream ->
    let payload =
      Jsonaf.member_exn "event" event.payload |> Res.Response_stream.t_of_jsonaf
    in
    (match payload with
     | Function_call_arguments_delta delta -> Some (delta.item_id, delta.delta)
     | _ -> None)
  | _ -> None
;;

let require_live_deltas events =
  List.iter
    [ Agent_protocol.Event.Recoverable.Sourced_stream; History_correlated_stream ]
    ~f:(fun kind ->
      let deltas =
        List.filter events ~f:(fun (event : Agent_protocol.Event.Recoverable.t) ->
          Agent_protocol.Event.Recoverable.equal_kind event.kind kind)
        |> List.filter_map ~f:stream_delta
      in
      require
        (not (List.is_empty deltas))
        "live probe did not publish tool argument deltas";
      let groups = String.Table.create () in
      List.iter deltas ~f:(fun (id, delta) ->
        Hashtbl.add_multi groups ~key:id ~data:delta);
      Hashtbl.iter groups ~f:(fun chunks ->
        let payload = List.rev chunks |> String.concat in
        require
          (String.is_substring payload ~substring:"<redacted>")
          "completed tool arguments were dropped instead of redacted";
        require_redacted
          live_secret
          (Base64.encode_exn live_secret)
          payload
          "reassembled live arguments"))
;;

let require_live_tool_events events nested =
  let payloads =
    List.filter_map events ~f:(fun event ->
      match event.Agent_protocol.Event.Recoverable.kind, nested with
      | Tool_started, false -> Some event.payload
      | Tool_trace, true -> Some (Jsonaf.member_exn "trace" event.payload)
      | _ -> None)
  in
  let payloads =
    List.filter payloads ~f:(fun payload ->
      match Jsonaf.member "name" payload with
      | Some (`String "fixed_echo") -> true
      | _ -> false)
  in
  require
    (not (List.is_empty payloads))
    "live probe missed fixed_echo Started/Trace payload";
  List.iter payloads ~f:(fun payload ->
    require_redacted
      live_secret
      (Base64.encode_exn live_secret)
      (Jsonaf.to_string payload)
      "live Started/Trace payload")
;;

let test_live_redaction name nested check env environment =
  let fixture = live_fixture env environment name in
  Daemon_host.with_ env fixture ~options:(live_options env nested) (fun sw _daemon ->
    with_client ~sw env fixture (fun client ->
      let session = create_session client (name ^ ":create") in
      let session = start_session client session (name ^ ":start") in
      let events = capture_live ~sw env client session ~nested in
      check events;
      List.iter events ~f:(fun event ->
        require_redacted
          live_secret
          (Base64.encode_exn live_secret)
          (Agent_protocol.Event.Recoverable.to_json event |> Jsonaf.to_string)
          "live SSE frame")))
;;

let cases =
  [ "manifest.no-grant", test_manifest_no_grant
  ; "manifest.exact-operator-grant", test_manifest_exact
  ; "manifest.source-hash-mismatch", test_source_mismatch
  ; "manifest.manifest-hash-mismatch", test_manifest_mismatch
  ; "manifest.principal-mismatch", test_principal_mismatch
  ; "grant.approve-once", test_approve_once
  ; "grant.session-persist-restart", test_session_persist_restart
  ; "grant.revoke", test_revoke
  ; "shell.single-delegated-approval", test_single_delegated
  ; "redaction.events-jobs-audit-health-logs", test_redaction
  ; ( "redaction.live-split-deltas"
    , test_live_redaction "redaction.live-split-deltas" false require_live_deltas )
  ; ( "redaction.live-started"
    , test_live_redaction "redaction.live-started" false (fun events ->
        require_live_tool_events events false) )
  ; ( "redaction.live-trace"
    , test_live_redaction "redaction.live-trace" true (fun events ->
        require_live_tool_events events true) )
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown shell-security case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"shell-grants-redaction" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn ->
        raise_s [%sexp "shell-security E2E case failed", (name : string), (exn : Exn.t)]);
    let report =
      [%sexp
        { scenario = ("shell-grants-redaction" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }]
    in
    Eio.Flow.copy_string (Sexp.to_string_hum report ^ "\n") (Eio.Stdenv.stdout env))
;;
