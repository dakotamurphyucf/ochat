open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type access =
  | Shared_write
  | Exclusive

type overflow =
  | Reject
  | Queue

type prompt =
  { id : string
  ; path : string
  ; allowed_workspaces : string list
  ; source : string
  }

type workspace =
  { id : string
  ; access : access
  ; conflict_domain : string
  ; limits : (string * int * overflow) list
  }

type session =
  { id : Agent_protocol.Id.Session.t
  ; mutable attachment_id : Agent_protocol.Id.Attachment.t
  ; mutable revision : int64
  ; prompt_revision : Agent_protocol.Id.Prompt_revision.t
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok
let atom value = Sexp.to_string_mach (Sexp.Atom value)

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

let replace_section contents ~section ~next replacement =
  let start = String.substr_index_exn contents ~pattern:("(" ^ section) in
  let suffix = String.drop_prefix contents start in
  let finish = String.substr_index_exn suffix ~pattern:("\n(" ^ next) + start in
  String.prefix contents start ^ replacement ^ String.drop_prefix contents finish
;;

let access = function
  | Shared_write -> "shared_write"
  | Exclusive -> "exclusive"
;;

let overflow = function
  | Reject -> "reject"
  | Queue -> "queue"
;;

let prompt_limit (prompt, maximum, policy) =
  sprintf
    "((prompt %s) (max_root_agents %d) (overflow %s))"
    prompt
    maximum
    (overflow policy)
;;

let workspace_record fixture (workspace : workspace) =
  sprintf
    "((id %s) (source (physical %s)) (access %s) (conflict_domain %s) (prompt_limits \
     (%s)))"
    workspace.id
    (atom (Config_fixture.physical_workspace fixture))
    (access workspace.access)
    (atom workspace.conflict_domain)
    (String.concat ~sep:" " (List.map workspace.limits ~f:prompt_limit))
;;

let prompt_record (prompt : prompt) =
  sprintf
    "((id %s) (path %s) (description \"E2E quota prompt\") (allowed_workspaces (%s)) \
     (permission_profile unattended) (enabled true))"
    prompt.id
    (atom prompt.path)
    (String.concat ~sep:" " prompt.allowed_workspaces)
;;

let stable_prompt_source id =
  sprintf
    {|
<developer>You are the deterministic %s E2E agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start -> Task.pure(state + 1)
      | `Session_resume -> Task.pure(state + 1)
</script>
|}
    id
;;

let install_prompt_source fixture (prompt : prompt) =
  let environment = Config_fixture.environment fixture in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment prompt.path)
    prompt.source
;;

let configure fixture ~workspaces ~prompts =
  List.iter prompts ~f:(install_prompt_source fixture);
  let contents = Config_fixture.configuration fixture () in
  let workspace_section =
    sprintf
      "(workspaces\n (%s))"
      (String.concat ~sep:"\n  " (List.map workspaces ~f:(workspace_record fixture)))
  in
  let prompt_section =
    sprintf
      "(prompts\n (%s))"
      (String.concat ~sep:"\n  " (List.map prompts ~f:prompt_record))
  in
  let contents =
    replace_section contents ~section:"workspaces" ~next:"prompts" workspace_section
  in
  let contents =
    replace_section contents ~section:"prompts" ~next:"permission_profiles" prompt_section
  in
  let environment = Config_fixture.environment fixture in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.config_path fixture))
    contents
;;

let write_prompt environment path text =
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path environment path)
    text
;;

let smoke_prompt fixture allowed_workspaces =
  { id = "smoke"
  ; path = Config_fixture.prompt_path fixture
  ; allowed_workspaces
  ; source = stable_prompt_source "smoke"
  }
;;

let additional_prompt environment fixture ~id ~allowed_workspaces =
  let path =
    Filename.concat
      (Filename.dirname (Config_fixture.prompt_path fixture))
      (id ^ ".chatmd")
  in
  write_prompt
    environment
    path
    (sprintf "<developer>You are the deterministic %s E2E agent.</developer>" id);
  { id; path; allowed_workspaces; source = stable_prompt_source id }
;;

let failing_resume_prompt environment fixture ~id ~allowed_workspaces =
  let prompt = additional_prompt environment fixture ~id ~allowed_workspaces in
  { prompt with
    source =
      sprintf
        {|
<developer>You are the deterministic %s failure-injection agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start -> Task.pure(state + 1)
      | `Session_resume ->
        Task.bind(Process.run("/bin/false", []), fun output ->
        Task.pure(state + 1))
</script>
|}
        id
  }
;;

let wait_ready daemon env =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon did not become ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stdout daemon).contents : string)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let connect ~sw env fixture =
  let connection =
    Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  in
  ignore
    (Unix_driver.initialize connection |> protocol_ok
     : Agent_protocol.Initialize.Response.t);
  connection
;;

let with_daemon env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon_process.start
        ~sw
        ~env
        ~fixture
        ~config_path:(Config_fixture.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        wait_ready daemon env;
        let connection = connect ~sw env fixture in
        Exn.protect
          ~f:(fun () ->
            try f sw connection with
            | exn ->
              raise_s
                [%sexp
                  "quota daemon scenario failed"
                , (exn : Exn.t)
                , ((Daemon_process.stderr daemon).contents : string)])
          ~finally:(fun () -> Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let find_prompt connection name =
  Agent_client.Catalog.prompts connection
  |> protocol_ok
  |> List.find_exn ~f:(fun prompt -> String.equal prompt.name name)
;;

let find_workspace connection name =
  Agent_client.Catalog.workspaces connection
  |> protocol_ok
  |> List.find_exn ~f:(fun workspace -> String.equal workspace.name name)
;;

let create_session connection ~key ~prompt_name ~workspace_name =
  let prompt = find_prompt connection prompt_name in
  let workspace = find_workspace connection workspace_name in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately:false
      ~labels:[ "suite", "quota-queues-leases" ]
      ()
    |> protocol_ok
  in
  let command_request =
    Agent_protocol.Session.Create_request.
      { spec
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key (key ^ ":create")
      }
  in
  match request connection (Session_create command_request) with
  | Session_create created ->
    let attachment = Option.value_exn created.attachment in
    { id = created.session.id
    ; attachment_id = attachment.attachment.id
    ; revision = created.session.revision
    ; prompt_revision = Option.value_exn created.session.prompt_revision
    }
  | _ -> fail "session.create returned the wrong result variant"
;;

let attach connection session ~key =
  let command_request =
    Agent_protocol.Session.Attach_request.
      { session_id = session.id
      ; requested_mode = Read_write
      ; subscribe = false
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key = idempotency_key (key ^ ":attach")
      }
  in
  match request connection (Session_attach command_request) with
  | Session_attach attached -> session.attachment_id <- attached.attachment.id
  | _ -> fail "session.attach returned the wrong result variant"
;;

let get_session connection session =
  match request connection (Session_get { session_id = session.id; history = None }) with
  | Session_get snapshot ->
    session.revision <- snapshot.session.revision;
    snapshot.session
  | _ -> fail "session.get returned the wrong result variant"
;;

let start_result connection session ~key ~queue_if_limited =
  let command_request =
    Agent_protocol.Session.Start_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; queue_if_limited
      ; idempotency_key = idempotency_key (key ^ ":start")
      }
  in
  Agent_client.Connection.request connection (Session_start command_request)
;;

let start_session connection session ~key ~queue_if_limited =
  match start_result connection session ~key ~queue_if_limited |> protocol_ok with
  | Session_start started ->
    session.revision <- started.session.revision;
    started.session
  | _ -> fail "session.start returned the wrong result variant"
;;

let stop_session connection session ~key =
  let command_request =
    Agent_protocol.Session.Stop_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; mode = Graceful
      ; idempotency_key = idempotency_key (key ^ ":stop")
      }
  in
  match request connection (Session_stop command_request) with
  | Session_stop stopped ->
    session.revision <- stopped.session.revision;
    stopped.session
  | _ -> fail "session.stop returned the wrong result variant"
;;

let observed_is
      (state : Agent_protocol.Session.t)
      (expected : Agent_protocol.Session.observed_state)
  =
  let open Agent_protocol.Session in
  match state.Agent_protocol.Session.observed_state, expected with
  | Stopped, Stopped | Queued_for_slot, Queued_for_slot | Idle, Idle -> true
  | ( ( Stopped
      | Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _ )
    , _ ) -> false
;;

let rec await_observed env connection session expected attempts =
  let state = get_session connection session in
  if observed_is state expected
  then state
  else if attempts = 0
  then
    raise_s
      [%sexp
        "session did not reach expected state"
      , (session.id : Agent_protocol.Id.Session.t)
      , (expected : Agent_protocol.Session.observed_state)
      , (state.observed_state : Agent_protocol.Session.observed_state)]
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_observed env connection session expected (attempts - 1))
;;

let setup_single env environment name ~access ~maximum ~overflow =
  let fixture = fixture env environment name in
  let workspace =
    { id = "target"
    ; access
    ; conflict_domain = name ^ ":domain"
    ; limits = [ "smoke", maximum, overflow ]
    }
  in
  configure
    fixture
    ~workspaces:[ workspace ]
    ~prompts:[ smoke_prompt fixture [ "target" ] ];
  fixture
;;

let test_reject_limit env environment =
  let fixture =
    setup_single
      env
      environment
      "quota-reject"
      ~access:Shared_write
      ~maximum:1
      ~overflow:Reject
  in
  with_daemon env fixture (fun _sw connection ->
    let first =
      create_session
        connection
        ~key:"reject:first"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    let second =
      create_session
        connection
        ~key:"reject:second"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session connection first ~key:"reject:first" ~queue_if_limited:false
       : Agent_protocol.Session.t);
    let first_state = get_session connection first in
    require (observed_is first_state Idle) "first quota holder did not remain idle";
    (match
       start_result connection second ~key:"reject:second" ~queue_if_limited:false
     with
     | Error error ->
       require
         (Agent_protocol.Error.equal_code error.code Resource_limit)
         "quota rejection returned the wrong error"
     | Ok result ->
       raise_s
         [%sexp
           "second session bypassed the reject limit"
         , (first_state.observed_state : Agent_protocol.Session.observed_state)
         , (result : Agent_protocol.Method_result.t)]);
    let second_state = get_session connection second in
    require (observed_is second_state Stopped) "rejected session left the stopped state";
    ignore (stop_session connection first ~key:"reject:first" : Agent_protocol.Session.t);
    ignore
      (start_session connection second ~key:"reject:second:retry" ~queue_if_limited:false
       : Agent_protocol.Session.t))
;;

let test_queue_limit env environment =
  let fixture =
    setup_single
      env
      environment
      "quota-queue"
      ~access:Shared_write
      ~maximum:1
      ~overflow:Queue
  in
  with_daemon env fixture (fun _sw connection ->
    let first =
      create_session
        connection
        ~key:"queue:first"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    let second =
      create_session
        connection
        ~key:"queue:second"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session connection first ~key:"queue:first" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    require
      (observed_is (get_session connection first) Idle)
      "first queued-limit holder did not remain idle";
    let queued =
      start_session connection second ~key:"queue:second" ~queue_if_limited:true
    in
    require (observed_is queued Queued_for_slot) "limited start was not queued";
    ignore (stop_session connection first ~key:"queue:first" : Agent_protocol.Session.t);
    ignore (await_observed env connection second Idle 250 : Agent_protocol.Session.t))
;;

let test_fifo env environment =
  let fixture =
    setup_single
      env
      environment
      "queue-fifo"
      ~access:Shared_write
      ~maximum:1
      ~overflow:Queue
  in
  with_daemon env fixture (fun _sw connection ->
    let holder =
      create_session
        connection
        ~key:"fifo:holder"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    let first =
      create_session
        connection
        ~key:"fifo:first"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    let second =
      create_session
        connection
        ~key:"fifo:second"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session connection holder ~key:"fifo:holder" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    ignore
      (start_session connection first ~key:"fifo:first" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    ignore
      (start_session connection second ~key:"fifo:second" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    ignore (stop_session connection holder ~key:"fifo:holder" : Agent_protocol.Session.t);
    ignore (await_observed env connection first Idle 250 : Agent_protocol.Session.t);
    require
      (observed_is (get_session connection second) Queued_for_slot)
      "same-key FIFO was reordered";
    ignore (stop_session connection first ~key:"fifo:first" : Agent_protocol.Session.t);
    ignore (await_observed env connection second Idle 250 : Agent_protocol.Session.t))
;;

let test_shared_write env environment =
  let fixture =
    setup_single
      env
      environment
      "lease-shared"
      ~access:Shared_write
      ~maximum:2
      ~overflow:Reject
  in
  with_daemon env fixture (fun _sw connection ->
    let first =
      create_session
        connection
        ~key:"shared:first"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    let second =
      create_session
        connection
        ~key:"shared:second"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session connection first ~key:"shared:first" ~queue_if_limited:false
       : Agent_protocol.Session.t);
    ignore
      (start_session connection second ~key:"shared:second" ~queue_if_limited:false
       : Agent_protocol.Session.t);
    require
      (observed_is (get_session connection first) Idle)
      "first shared lease is not active";
    require
      (observed_is (get_session connection second) Idle)
      "second shared lease is not active")
;;

let test_exclusive_domain env environment =
  let fixture = fixture env environment "lease-exclusive" in
  let mk id =
    { id
    ; access = Exclusive
    ; conflict_domain = "shared-exclusive-domain"
    ; limits = [ "smoke", 2, Queue ]
    }
  in
  let names = [ "first"; "second" ] in
  configure
    fixture
    ~workspaces:(List.map names ~f:mk)
    ~prompts:[ smoke_prompt fixture names ];
  with_daemon env fixture (fun _sw connection ->
    let first =
      create_session
        connection
        ~key:"exclusive:first"
        ~prompt_name:"smoke"
        ~workspace_name:"first"
    in
    let second =
      create_session
        connection
        ~key:"exclusive:second"
        ~prompt_name:"smoke"
        ~workspace_name:"second"
    in
    ignore
      (start_session connection first ~key:"exclusive:first" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    require
      (observed_is (get_session connection first) Idle)
      "first exclusive holder did not remain idle";
    let queued =
      start_session connection second ~key:"exclusive:second" ~queue_if_limited:true
    in
    require
      (observed_is queued Queued_for_slot)
      "exclusive conflict domain allowed two holders";
    ignore
      (stop_session connection first ~key:"exclusive:first" : Agent_protocol.Session.t);
    ignore (await_observed env connection second Idle 250 : Agent_protocol.Session.t))
;;

let test_disconnect_survival env environment =
  let fixture =
    setup_single
      env
      environment
      "queue-disconnect"
      ~access:Exclusive
      ~maximum:2
      ~overflow:Queue
  in
  with_daemon env fixture (fun sw connection ->
    let holder =
      create_session
        connection
        ~key:"disconnect:holder"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session connection holder ~key:"disconnect:holder" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    let client = connect ~sw env fixture in
    let queued =
      create_session
        client
        ~key:"disconnect:queued"
        ~prompt_name:"smoke"
        ~workspace_name:"target"
    in
    ignore
      (start_session client queued ~key:"disconnect:queued" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    Agent_client.Connection.close client;
    ignore
      (stop_session connection holder ~key:"disconnect:holder" : Agent_protocol.Session.t);
    attach connection queued ~key:"disconnect:queued:reconnect";
    ignore (await_observed env connection queued Idle 250 : Agent_protocol.Session.t))
;;

let first_idle connection sessions =
  List.find sessions ~f:(fun session -> observed_is (get_session connection session) Idle)
;;

let rec await_first_idle env connection sessions attempts =
  match first_idle connection sessions with
  | Some session -> session
  | None when attempts > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_first_idle env connection sessions (attempts - 1)
  | None -> fail "no queued session acquired the released lease"
;;

let test_cross_key_fairness env environment =
  let fixture = fixture env environment "queue-fairness" in
  let workspace =
    { id = "target"
    ; access = Exclusive
    ; conflict_domain = "fairness-domain"
    ; limits = [ "smoke", 3, Queue; "other", 1, Queue ]
    }
  in
  let prompts =
    [ smoke_prompt fixture [ "target" ]
    ; additional_prompt environment fixture ~id:"other" ~allowed_workspaces:[ "target" ]
    ]
  in
  configure fixture ~workspaces:[ workspace ] ~prompts;
  with_daemon env fixture (fun _sw connection ->
    let make key prompt =
      create_session connection ~key ~prompt_name:prompt ~workspace_name:"target"
    in
    let holder = make "fair:holder" "smoke" in
    let first = make "fair:first" "smoke" in
    let second = make "fair:second" "smoke" in
    let other = make "fair:other" "other" in
    ignore
      (start_session connection holder ~key:"fair:holder" ~queue_if_limited:true
       : Agent_protocol.Session.t);
    List.iter [ first; second; other ] ~f:(fun session ->
      ignore
        (start_session
           connection
           session
           ~key:(Agent_protocol.Id.Session.to_string session.id)
           ~queue_if_limited:true
         : Agent_protocol.Session.t));
    ignore (stop_session connection holder ~key:"fair:holder" : Agent_protocol.Session.t);
    let active = await_first_idle env connection [ first; other ] 250 in
    ignore (stop_session connection active ~key:"fair:active" : Agent_protocol.Session.t);
    let expected =
      if Agent_protocol.Id.Session.compare active.id first.id = 0 then other else first
    in
    ignore (await_observed env connection expected Idle 250 : Agent_protocol.Session.t);
    let first_state = get_session connection first in
    let second_state = get_session connection second in
    let other_state = get_session connection other in
    if not (observed_is second_state Queued_for_slot)
    then
      raise_s
        [%sexp
          "one quota key took two turns before its peer"
        , (active.id : Agent_protocol.Id.Session.t)
        , (first_state.observed_state : Agent_protocol.Session.observed_state)
        , (second_state.observed_state : Agent_protocol.Session.observed_state)
        , (other_state.observed_state : Agent_protocol.Session.observed_state)];
    ignore
      (stop_session connection expected ~key:"fair:expected" : Agent_protocol.Session.t);
    ignore (await_observed env connection second Idle 250 : Agent_protocol.Session.t))
;;

let test_failed_start_release env environment =
  let fixture = fixture env environment "lease-failed-start" in
  let workspace =
    { id = "target"
    ; access = Exclusive
    ; conflict_domain = "failed-start-domain"
    ; limits = [ "broken", 1, Reject; "good", 1, Reject ]
    }
  in
  let good =
    additional_prompt environment fixture ~id:"good" ~allowed_workspaces:[ "target" ]
  in
  let broken =
    failing_resume_prompt
      environment
      fixture
      ~id:"broken"
      ~allowed_workspaces:[ "target" ]
  in
  configure fixture ~workspaces:[ workspace ] ~prompts:[ broken; good ];
  with_daemon env fixture (fun _sw connection ->
    let broken =
      create_session
        connection
        ~key:"failed:broken"
        ~prompt_name:"broken"
        ~workspace_name:"target"
    in
    let good =
      create_session
        connection
        ~key:"failed:good"
        ~prompt_name:"good"
        ~workspace_name:"target"
    in
    require
      (Result.is_error
         (start_result connection broken ~key:"failed:broken" ~queue_if_limited:false))
      "corrupt runtime unexpectedly started";
    ignore
      (start_session connection good ~key:"failed:good" ~queue_if_limited:false
       : Agent_protocol.Session.t))
;;

let test_restart_recovery env environment =
  let fixture =
    setup_single
      env
      environment
      "lease-restart"
      ~access:Exclusive
      ~maximum:2
      ~overflow:Queue
  in
  let holder, queued =
    with_daemon env fixture (fun _sw connection ->
      let holder =
        create_session
          connection
          ~key:"restart:holder"
          ~prompt_name:"smoke"
          ~workspace_name:"target"
      in
      let queued =
        create_session
          connection
          ~key:"restart:queued"
          ~prompt_name:"smoke"
          ~workspace_name:"target"
      in
      ignore
        (start_session connection holder ~key:"restart:holder" ~queue_if_limited:true
         : Agent_protocol.Session.t);
      ignore
        (start_session connection queued ~key:"restart:queued" ~queue_if_limited:true
         : Agent_protocol.Session.t);
      holder, queued)
  in
  with_daemon env fixture (fun _sw connection ->
    attach connection holder ~key:"restart:holder:recover";
    attach connection queued ~key:"restart:queued:recover";
    ignore (await_observed env connection holder Idle 250 : Agent_protocol.Session.t);
    require
      (observed_is (get_session connection queued) Queued_for_slot)
      "restart lost exclusive queued state";
    ignore
      (stop_session connection holder ~key:"restart:holder:recover"
       : Agent_protocol.Session.t);
    ignore (await_observed env connection queued Idle 250 : Agent_protocol.Session.t))
;;

let cases =
  [ "quota.reject-limit", test_reject_limit
  ; "quota.queue-limit", test_queue_limit
  ; "queue.fifo-per-key", test_fifo
  ; "queue.cross-key-fairness", test_cross_key_fairness
  ; "queue.client-disconnect-survival", test_disconnect_survival
  ; "lease.shared-write", test_shared_write
  ; "lease.exclusive-conflict-domain", test_exclusive_domain
  ; "lease.failed-start-release", test_failed_start_release
  ; "lease.restart-recovery", test_restart_recovery
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown quota case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"quota-queues-leases" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "quota E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("quota-queues-leases" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
