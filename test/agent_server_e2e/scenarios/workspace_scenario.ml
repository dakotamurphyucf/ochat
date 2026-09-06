open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  Config_fixture.create environment ~name ~http_port:(reserve_port env)
;;

let diagnostic_prompt variable =
  sprintf
    {|
<developer>You are a deterministic workspace diagnostic agent.</developer>
<shell_access id="diagnostic" cwd="${%s}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false"/>
  <resolver allow_relative_search_path="false">
    <executable id="pwd" path="/bin/pwd" trusted="true"/>
  </resolver>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="deny">
    <rule id="allow-pwd" action="allow"><resolved_path value="/bin/pwd"/></rule>
  </policy>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="jsonl" path="${session_dir}/authority-audit.jsonl"
      content="full" failure="deny_start"/>
</shell_access>
<moderator_runtime shell_runtime="diagnostic"/>
<script id="workspace-probe" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Probe(string) ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Process.run("/bin/pwd", []), fun output ->
        Task.bind(Schedule.after_ms(60000, `Probe(output)), fun schedule_id ->
        Task.pure(state + 1)))
      | `Probe(output) -> Task.pure(state + 1)
</script>
|}
    variable
;;

let configure_fixture fixture variable =
  let environment = Config_fixture.environment fixture in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    (diagnostic_prompt variable);
  let configuration =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(manifest_authorization deny)"
         ~with_:"(manifest_authorization assume_authorized)"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.config_path fixture))
    configuration
;;

let configure_prompt fixture prompt =
  let environment = Config_fixture.environment fixture in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    prompt;
  let configuration =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(manifest_authorization deny)"
         ~with_:"(manifest_authorization assume_authorized)"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.config_path fixture))
    configuration
;;

let readiness_failure daemon error =
  raise_s
    [%sexp
      "daemon did not become ready"
    , { error : Daemon_process.readiness_error
      ; stdout = ((Daemon_process.stdout daemon).contents : string)
      ; stderr = ((Daemon_process.stderr daemon).contents : string)
      }]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let initialize connection = Unix_driver.initialize connection |> protocol_ok

let request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
;;

let with_daemon env environment fixture ~name f =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let launch_directory = Filename.concat roots.root (name ^ "-launch") in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment launch_directory);
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon_process.start_in_directory
        ~sw
        ~env
        ~fixture
        ~cwd:(Temporary_environment.path environment launch_directory)
        ~config_path:(Config_fixture.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        (match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
         | Ok _ -> ()
         | Error error -> readiness_failure daemon error);
        let connection =
          Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
        in
        Exn.protect
          ~f:(fun () ->
            ignore (initialize connection : Agent_protocol.Initialize.Response.t);
            f connection launch_directory)
          ~finally:(fun () -> Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let prompt_and_workspace connection =
  let prompts = Agent_client.Catalog.prompts connection |> protocol_ok in
  let workspaces = Agent_client.Catalog.workspaces connection |> protocol_ok in
  let prompt =
    List.find_exn prompts ~f:(fun prompt -> String.equal prompt.name "smoke")
  in
  let workspace =
    List.find_exn workspaces ~f:(fun workspace -> String.equal workspace.name "physical")
  in
  prompt, workspace
;;

let create_session_result connection =
  let prompt, workspace = prompt_and_workspace connection in
  let prompt_revision = Option.value_exn prompt.current_revision in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately:true
      ~labels:[ "suite", "workspace-e2e" ]
      ()
    |> protocol_ok
  in
  let idempotency_key =
    Agent_protocol.Idempotency_key.of_string "workspace-variable-create" |> protocol_ok
  in
  Agent_client.Connection.request
    connection
    (Session_create { spec; requested_mode = None; subscribe = false; idempotency_key })
  |> Result.map ~f:(fun result -> result, prompt_revision)
;;

let create_session connection =
  match create_session_result connection |> protocol_ok with
  | Session_create created, prompt_revision -> created.session.id, prompt_revision
  | _ -> fail "session.create returned the wrong result variant"
;;

let session_snapshot connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "session.get returned the wrong result variant"
;;

let rec await_schedule env connection session_id attempts =
  let snapshot = session_snapshot connection session_id in
  if not (List.is_empty snapshot.schedules)
  then List.hd_exn snapshot.schedules
  else if Option.is_some snapshot.failure
  then
    raise_s
      [%sexp
        "workspace diagnostic session failed"
      , (snapshot.failure : Agent_protocol.Error.t option)
      , (snapshot.session.observed_state : Agent_protocol.Session.observed_state)]
  else if attempts = 0
  then
    raise_s
      [%sexp
        "workspace diagnostic moderator did not create a schedule"
      , (snapshot.session.observed_state : Agent_protocol.Session.observed_state)
      , (snapshot.jobs : Agent_protocol.Job.t list)]
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_schedule env connection session_id (attempts - 1))
;;

let rec json_strings = function
  | `String value -> [ value ]
  | `Array values -> List.concat_map values ~f:json_strings
  | `Object fields -> List.concat_map fields ~f:(fun (_name, value) -> json_strings value)
  | `Null | `True | `False | `Number _ -> []
;;

let same_directory environment first second =
  let stat path =
    Eio.Path.stat ~follow:true (Temporary_environment.path environment path)
  in
  let first = stat first in
  let second = stat second in
  Poly.equal first.kind `Directory
  && Poly.equal second.kind `Directory
  && Int64.equal first.dev second.dev
  && Int64.equal first.ino second.ino
;;

type daemon_probe =
  { actual : string
  ; session_id : Agent_protocol.Id.Session.t
  ; prompt_revision : Agent_protocol.Id.Prompt_revision.t
  ; fixture : Config_fixture.t
  ; launch_directory : string
  }

let probe_value schedule =
  json_strings schedule.Agent_protocol.Schedule.payload
  |> List.map ~f:String.strip
  |> List.find_exn ~f:Filename.is_absolute
;;

let run_daemon_probe env environment ~name ~variable =
  let fixture = fixture env environment name in
  configure_fixture fixture variable;
  with_daemon env environment fixture ~name (fun connection launch_directory ->
    let session_id, prompt_revision = create_session connection in
    let schedule = await_schedule env connection session_id 250 in
    { actual = probe_value schedule
    ; session_id
    ; prompt_revision
    ; fixture
    ; launch_directory
    })
;;

let test_physical_workspace env environment =
  let probe =
    run_daemon_probe env environment ~name:"workspace-physical" ~variable:"workspace"
  in
  require
    (not
       (String.equal
          probe.launch_directory
          (Config_fixture.physical_workspace probe.fixture)))
    "daemon launch directory unexpectedly equals the physical workspace";
  require
    (same_directory
       environment
       probe.actual
       (Config_fixture.physical_workspace probe.fixture))
    "${workspace} did not resolve to the configured physical root"
;;

let session_directory probe =
  Filename.concat
    (Filename.concat (Config_fixture.data_dir probe.fixture) "sessions")
    (Agent_protocol.Id.Session.to_string probe.session_id)
;;

let prompt_directory probe =
  Filename.concat
    (Filename.concat
       (Filename.concat (Config_fixture.data_dir probe.fixture) "prompt-artifacts")
       (Agent_protocol.Id.Prompt_revision.to_string probe.prompt_revision))
    "tree"
;;

let test_prompt_session_cache_home env environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let probes =
    [ "tool_dir", (fun probe -> probe.launch_directory), "workspace-tool-dir"
    ; "prompt_dir", prompt_directory, "workspace-prompt-dir"
    ; "session_dir", session_directory, "workspace-session-dir"
    ; ( "cache_dir"
      , (fun probe -> Filename.concat (session_directory probe) "cache")
      , "workspace-cache-dir" )
    ; "home", (fun _probe -> roots.home), "workspace-home"
    ]
  in
  List.iter probes ~f:(fun (variable, expected, name) ->
    let probe = run_daemon_probe env environment ~name ~variable in
    let expected = expected probe in
    if not (same_directory environment probe.actual expected)
    then
      raise_s
        [%sexp
          "runtime variable resolved to the wrong directory"
        , { variable : string; actual = (probe.actual : string); expected : string }])
;;

let write_prompt environment fixture variable =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    (diagnostic_prompt variable)
;;

let embedded_permission_profile =
  let profile = Agent_server.Embedded.default_permission_profile in
  { profile with
    tool_default = Agent_server.Config.Permission_profile.Policy
  ; manifest_authorization = Assume_authorized
  }
;;

let test_local_workspace env environment =
  let fixture = fixture env environment "workspace-local" in
  write_prompt environment fixture "workspace";
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let tool_dir = Filename.concat roots.root "embedded-tool-dir" in
  let data_root = Filename.concat roots.data "embedded-workspace-data" in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment tool_dir);
  Eio.Switch.run (fun sw ->
    let options =
      Agent_server.Embedded.
        { prompt_file = Config_fixture.prompt_path fixture
        ; workspace = Config_fixture.physical_workspace fixture
        ; tool_dir
        ; home = roots.home
        ; data_root = Some data_root
        ; start_immediately = true
        ; permission_profile = embedded_permission_profile
        ; attachment_mode = Read_write
        ; event_capacity = 1_024
        }
    in
    let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
    Exn.protect
      ~f:(fun () ->
        let schedule =
          await_schedule
            env
            (Agent_server.Embedded.connection embedded)
            (Agent_server.Embedded.session_id embedded)
            250
        in
        let values = json_strings schedule.payload |> List.map ~f:String.strip in
        let actual = List.find_exn values ~f:Filename.is_absolute in
        require
          (same_directory environment actual (Config_fixture.physical_workspace fixture))
          "embedded ${workspace} did not resolve to the local workspace")
      ~finally:(fun () -> Agent_server.Embedded.close embedded))
;;

let imported_prompt = sprintf "<import src=\"parts/runtime.chatmd\"/>"

let write_imported_prompt environment fixture =
  let prompt_directory = Filename.dirname (Config_fixture.prompt_path fixture) in
  let parts_directory = Filename.concat prompt_directory "parts" in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment parts_directory);
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    imported_prompt;
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path
       environment
       (Filename.concat parts_directory "runtime.chatmd"))
    (diagnostic_prompt "source_dir")
;;

let test_imported_source_dir env environment =
  let name = "workspace-imported-source-dir" in
  let fixture = fixture env environment name in
  write_imported_prompt environment fixture;
  configure_prompt fixture imported_prompt;
  with_daemon env environment fixture ~name (fun connection _launch_directory ->
    let session_id, prompt_revision = create_session connection in
    let actual = await_schedule env connection session_id 250 |> probe_value in
    let prompt_directory =
      Filename.concat
        (Filename.concat
           (Filename.concat (Config_fixture.data_dir fixture) "prompt-artifacts")
           (Agent_protocol.Id.Prompt_revision.to_string prompt_revision))
        "tree"
    in
    let expected = Filename.concat prompt_directory "parts" in
    if not (same_directory environment actual expected)
    then
      raise_s
        [%sexp
          "imported ${source_dir} resolved to the wrong directory"
        , { actual : string; expected : string; prompt_directory : string }])
;;

let capability kind root =
  Option.value_map root ~default:"" ~f:(fun path -> sprintf "<%s path=\"%s\"/>" kind path)
;;

let authority_runtime ~read_root ~write_root =
  let read_capability = capability "read" read_root in
  let write_capability = capability "write" write_root in
  sprintf
    {|
<shell_access id="authority" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    %s
    %s
  </capabilities>
  <resolver allow_relative_search_path="false">
    <executable id="cat" path="/bin/cat" trusted="true"/>
    <executable id="cp" path="/bin/cp" trusted="true"/>
  </resolver>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="deny">
    <rule id="allow-cat" action="allow"><resolved_path value="/bin/cat"/></rule>
    <rule id="allow-cp" action="allow"><resolved_path value="/bin/cp"/></rule>
  </policy>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="none"/>
</shell_access>
<moderator_runtime shell_runtime="authority"/>
|}
    read_capability
    write_capability
;;

let filesystem_prompt ~read_root ~target =
  authority_runtime ~read_root ~write_root:None
  ^ sprintf
      {|
<script id="authority-probe" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Probe(string) ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Process.run("/bin/cat", ["%s"]), fun output ->
        Task.bind(Schedule.after_ms(60000, `Probe(output)), fun schedule_id ->
        Task.pure(state + 1)))
      | `Probe(output) -> Task.pure(state + 1)
</script>
|}
      target
;;

let copy_prompt ~source ~destination =
  authority_runtime
    ~read_root:(Some "${workspace}/allowed")
    ~write_root:(Some "${workspace}/allowed/output")
  ^ sprintf
      {|
<script id="authority-copy" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Copied(string) ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Process.run("/bin/cp", ["%s", "%s"]), fun output ->
        Task.bind(Schedule.after_ms(60000, `Copied(output)), fun schedule_id ->
        Task.pure(state + 1)))
      | `Copied(output) -> Task.pure(state + 1)
</script>
|}
      source
      destination
;;

let unknown_tool_prompt =
  {|
<developer>You are a deterministic authority diagnostic agent.</developer>
<script id="authority-probe" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Tool.call("undeclared-workspace-reader", `Null), fun result ->
        Task.pure(state + 1))
</script>
|}
;;

let save_workspace_file environment fixture relative contents =
  let path = Filename.concat (Config_fixture.physical_workspace fixture) relative in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path environment path)
    contents
;;

let failed_session_create connection =
  match create_session_result connection with
  | Error failure -> failure
  | Ok (result, _prompt_revision) ->
    raise_s
      [%sexp
        "session.create unexpectedly succeeded", (result : Agent_protocol.Method_result.t)]
;;

let test_no_filesystem_tool env environment =
  let name = "authority-no-filesystem" in
  let fixture = fixture env environment name in
  let secret = "workspace-secret-must-not-be-readable" in
  save_workspace_file environment fixture "sentinel.txt" secret;
  let target =
    Filename.concat (Config_fixture.physical_workspace fixture) "sentinel.txt"
  in
  configure_prompt fixture (filesystem_prompt ~read_root:None ~target);
  with_daemon env environment fixture ~name (fun connection _launch_directory ->
    let failure = failed_session_create connection in
    require
      (Agent_protocol.Error.equal_code failure.code Invalid_state)
      "undeclared filesystem access returned the wrong protocol error";
    require
      (String.is_substring failure.message ~substring:"capability violation")
      "undeclared filesystem access was not rejected by capability validation";
    require
      (not (String.is_substring failure.message ~substring:secret))
      "undeclared filesystem access leaked sentinel contents")
;;

let test_unknown_tool env environment =
  let name = "authority-unknown-tool" in
  let fixture = fixture env environment name in
  configure_prompt fixture unknown_tool_prompt;
  with_daemon env environment fixture ~name (fun connection _launch_directory ->
    let failure = failed_session_create connection in
    require
      (String.is_substring failure.message ~substring:"Tool.call is not configured")
      "an undeclared ChatML tool did not fail closed")
;;

let make_narrow_workspace environment fixture =
  let workspace = Config_fixture.physical_workspace fixture in
  List.iter [ "allowed"; "allowed/output" ] ~f:(fun relative ->
    Eio.Path.mkdir
      ~perm:0o700
      (Temporary_environment.path environment (Filename.concat workspace relative)));
  workspace
;;

let test_allowed_narrow_read env environment =
  let name = "authority-declared-narrow-allowed" in
  let fixture = fixture env environment name in
  let workspace = make_narrow_workspace environment fixture in
  let allowed_secret = "allowed-workspace-secret" in
  save_workspace_file environment fixture "allowed/sentinel.txt" allowed_secret;
  let source = Filename.concat workspace "allowed/sentinel.txt" in
  let destination = Filename.concat workspace "allowed/output/copied.txt" in
  configure_prompt fixture (copy_prompt ~source ~destination);
  with_daemon env environment fixture ~name (fun connection _launch_directory ->
    let session_id, _prompt_revision = create_session connection in
    ignore (await_schedule env connection session_id 250 : Agent_protocol.Schedule.t);
    let copied = Eio.Path.load (Temporary_environment.path environment destination) in
    require
      (String.equal copied allowed_secret)
      "declared narrow read authority did not copy its allowed sentinel")
;;

let test_denied_narrow_read env environment =
  let name = "authority-declared-narrow-denied" in
  let fixture = fixture env environment name in
  let workspace = make_narrow_workspace environment fixture in
  let denied_secret = "denied-workspace-secret" in
  save_workspace_file environment fixture "denied.txt" denied_secret;
  let source = Filename.concat workspace "denied.txt" in
  let destination = Filename.concat workspace "allowed/output/copied.txt" in
  configure_prompt fixture (copy_prompt ~source ~destination);
  with_daemon env environment fixture ~name (fun connection _launch_directory ->
    let failure = failed_session_create connection in
    require
      (String.is_substring failure.message ~substring:"capability violation")
      "narrow read authority did not reject a path outside its root";
    require
      (not (String.is_substring failure.message ~substring:denied_secret))
      "narrow read authority leaked denied sentinel contents")
;;

let test_declared_narrow_tool env environment =
  test_allowed_narrow_read env environment;
  test_denied_narrow_read env environment
;;

let cases =
  [ "variables.physical-workspace", test_physical_workspace
  ; "variables.local-workspace", test_local_workspace
  ; "variables.imported-source-dir", test_imported_source_dir
  ; "variables.prompt-session-cache-home", test_prompt_session_cache_home
  ; "authority.no-filesystem-tool", test_no_filesystem_tool
  ; "authority.unknown-tool", test_unknown_tool
  ; "authority.declared-narrow-tool", test_declared_narrow_tool
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown workspace case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"workspace-context" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("workspace-context" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
