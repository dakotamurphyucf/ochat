open Core
module F = Support.Tui_fixture
module Temp = Support.Temporary_environment
module Config = Support.Config_fixture
module Provider = Support.Tui_stream_provider
module Manual = Support.Tui_manual_provider
module Daemon = Support.Daemon_process
module Connection = Agent_client.Connection

let say env message = Eio.Flow.copy_string (message ^ "\n") (Eio.Stdenv.stdout env)
let endpoint port = sprintf "http://127.0.0.1:%d" port

let quote value =
  "'" ^ String.substr_replace_all value ~pattern:"'" ~with_:"'\"'\"'" ^ "'"
;;

let port ~sw env =
  let reservation = Support.Port_reservation.create ~sw ~env in
  let value = Support.Port_reservation.port reservation in
  Support.Port_reservation.release reservation;
  value
;;

let fixture env temporary =
  let fixture = F.create env temporary "tui-manual" in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temp.path temporary (Config.prompt_path fixture))
    "<developer>MANUAL-TUI-READY — isolated deterministic fixture.</developer><tool \
     name=\"fork\"/>";
  let configuration =
    Config.configuration fixture ()
    |> String.substr_replace_first
         ~pattern:"(tool_default deny)"
         ~with_:"(tool_default ask)"
    |> String.substr_replace_first
         ~pattern:"(max_root_agents 2)"
         ~with_:"(max_root_agents 8)"
    |> String.substr_replace_first
         ~pattern:"(max_events_per_session 1000)"
         ~with_:"(max_events_per_session 64)"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temp.path temporary (Config.config_path fixture))
    configuration;
  fixture
;;

let launcher_environment temporary provider_port =
  let roots = Temp.roots temporary in
  [ "PATH=/usr/bin:/bin:/usr/sbin:/sbin"
  ; "TERM=xterm-256color"
  ; "LANG=en_US.UTF-8"
  ; "HOME=" ^ roots.home
  ; "XDG_CONFIG_HOME=" ^ roots.config
  ; "XDG_CACHE_HOME=" ^ roots.cache
  ; "XDG_DATA_HOME=" ^ roots.data
  ; "TMPDIR=" ^ roots.temporary
  ; "API_URL=" ^ endpoint provider_port
  ; "OPENAI_API_KEY=tui-local-test-key"
  ]
;;

let launcher env fixture provider_port name arguments =
  let temporary = Config.environment fixture in
  let path = Filename.concat (Temp.roots temporary).sockets (name ^ ".sh") in
  let command =
    [ "/usr/bin/env"; "-i" ]
    @ launcher_environment temporary provider_port
    @ [ F.tui_executable env; "--no-config" ]
    @ arguments
  in
  let contents =
    "#!/bin/sh\nset -eu\ncd "
    ^ quote (Config.physical_workspace fixture)
    ^ "\nexec "
    ^ String.concat ~sep:" " (List.map command ~f:quote)
    ^ " \"$@\"\n"
  in
  Eio.Path.save ~create:(`Exclusive 0o700) (Temp.path temporary path) contents;
  say env ("launcher." ^ name ^ "=" ^ path);
  path
;;

let launchers env fixture provider_port =
  let bearer = F.bearer_file fixture in
  let local =
    launcher
      env
      fixture
      provider_port
      "local"
      [ "--local"; "-file"; Config.prompt_path fixture ]
  in
  List.iter
    [ "unix", [ "--connect"; "unix://" ^ Config.unix_socket fixture ]
    ; ( "http"
      , [ "--connect"
        ; endpoint (Config.http_port fixture)
        ; "--bearer-token-file"
        ; bearer
        ] )
    ]
    ~f:(fun (name, connection) ->
      ignore
        (launcher
           env
           fixture
           provider_port
           (name ^ "-new")
           (connection
            @ [ "--new-daemon-session"
              ; "--prompt"
              ; "smoke"
              ; "--workspace"
              ; "physical"
              ; "--detached"
              ])
         : string);
      ignore (launcher env fixture provider_port (name ^ "-attach") connection : string));
  local
;;

let watch_requests ~sw env provider =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop index =
      match Provider.request_at provider index with
      | Some request ->
        let streaming =
          Poly.equal (Jsonaf.member "stream" (Provider.body request)) (Some `True)
        in
        say
          env
          (sprintf
             "provider.request index=%d mode=%s"
             index
             (if streaming then "stream" else "summary"));
        loop (index + 1)
      | None ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
        loop index
    in
    loop 0)
;;

let status env observer =
  let sessions = Agent_client.Admin.list_sessions observer |> F.ok in
  List.iter sessions ~f:(fun session ->
    let snapshot =
      Agent_client.Admin.get_session observer session.Agent_protocol.Session.id |> F.ok
    in
    say
      env
      (Sexp.to_string_hum
         [%sexp
           (Agent_protocol.Id.Session.to_string session.id : string)
         , (snapshot.session.observed_state : Agent_protocol.Session.observed_state)
         , (List.length snapshot.canonical_history.entries : int)
         , (List.count snapshot.permissions ~f:(fun p ->
              Agent_protocol.Permission.equal_state p.state Pending)
            : int)
         , (snapshot.latest_event_sequence : int64)]))
;;

let observe ~sw env observer =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let previous = ref [] in
    let rec loop () =
      let sessions = Agent_client.Admin.list_sessions observer |> F.ok in
      let counters =
        List.map sessions ~f:(fun s ->
          s.Agent_protocol.Session.id, s.latest_event_sequence)
      in
      if not (Poly.equal !previous counters)
      then (
        previous := counters;
        status env observer);
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.;
      loop ()
    in
    loop ())
;;

let operator env manual observer =
  let input = Eio.Buf_read.of_flow ~max_size:4096 (Eio.Stdenv.stdin env) in
  let rec loop () =
    match Eio.Buf_read.line input with
    | "quit" -> ()
    | line ->
      (try
         match
           String.split (String.strip line) ~on:' '
           |> List.filter ~f:(Fn.non String.is_empty)
         with
         | [ "status" ] -> status env observer
         | [ action; index ] ->
           Manual.respond manual ~env ~action ~index:(Int.of_string index);
           say env "operator.action-complete"
         | _ ->
           say
             env
             "Commands: status | reply/hold/finish/fork/summary/suggest/background INDEX \
              | quit"
       with
       | exn -> say env ("operator.error " ^ Exn.to_string exn));
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let self_check_approval env child provider manual =
  let module Pty = Support.Pty_process in
  let clock = Eio.Stdenv.clock env in
  Pty.send child "manual-fork\027\r";
  ignore (Provider.await_request provider env 1 : Provider.request);
  Manual.respond manual ~env ~action:"fork" ~index:1;
  List.iter
    [ "Tool permission requested"; "Tool: fork"; "approve once"; "deny" ]
    ~f:(Pty.await_text child ~clock);
  F.require (Provider.request_count provider = 2) "manual fork ran before approval";
  Pty.send child "\r";
  ignore (Provider.await_request provider env 2 : Provider.request);
  Manual.respond manual ~env ~action:"reply" ~index:2;
  ignore (Provider.await_request provider env 3 : Provider.request);
  Manual.respond manual ~env ~action:"reply" ~index:3
;;

let self_check_return_to_chat env child =
  let module Pty = Support.Pty_process in
  let offset = String.length (Pty.output child) in
  Pty.send child "\027";
  F.await env (fun () ->
    let fresh = String.drop_prefix (Pty.output child) offset in
    if String.is_substring fresh ~substring:"Ready for the next check."
    then Some ()
    else None)
;;

let self_check ~sw env fixture local provider manual =
  let module Pty = Support.Pty_process in
  let child =
    Pty.spawn
      ~sw
      ~env
      ~cwd:(Temp.path (Config.environment fixture) (Config.physical_workspace fixture))
      ~environment:[| "PATH=/usr/bin:/bin"; "TERM=xterm-256color" |]
      ~columns:100
      ~rows:30
      [ "/bin/sh"; local ]
  in
  let clock = Eio.Stdenv.clock env in
  Pty.await_text child ~clock "MANUAL-TUI-READY";
  Pty.send child "manual-smoke\027\r";
  ignore (Provider.await_request provider env 0 : Provider.request);
  Manual.respond manual ~env ~action:"reply" ~index:0;
  Pty.await_text child ~clock "Ready for the next check.";
  self_check_approval env child provider manual;
  self_check_return_to_chat env child;
  Pty.send child "\027";
  Pty.await_text child ~clock "NORMAL";
  Pty.send child ":q\r";
  F.require
    (Poly.equal (Pty.await_exit child ~clock) (`Exited 0))
    "manual launcher did not exit";
  Pty.assert_restored child ~clock;
  say env "manual.self-check=passed"
;;

let with_daemon ~sw env fixture provider_port f =
  let daemon =
    Daemon.start_in_directory_with_environment_overrides
      ~sw
      ~env
      ~fixture
      ~cwd:(Temp.path (Config.environment fixture) (Config.physical_workspace fixture))
      ~environment_overrides:
        [ "API_URL", endpoint provider_port; "OPENAI_API_KEY", "tui-local-test-key" ]
      ~config_path:(Config.config_path fixture)
  in
  Exn.protect
    ~f:(fun () ->
      (match Daemon.wait_ready daemon ~env ~timeout_seconds:5. with
       | Ok _ -> ()
       | Error error -> raise_s [%sexp (error : Daemon.readiness_error)]);
      let observer =
        Support.Unix_driver.connect ~sw ~env ~socket_path:(Config.unix_socket fixture)
      in
      Exn.protect
        ~f:(fun () ->
          ignore
            (Support.Unix_driver.initialize observer |> F.ok
             : Agent_protocol.Initialize.Response.t);
          f observer)
        ~finally:(fun () -> Connection.close observer))
    ~finally:(fun () ->
      ignore
        (Daemon.stop daemon ~env ~grace_seconds:1. : Support.Process_manager.termination))
;;

let observer_shutdown_self_check ~sw env observer =
  let started, resolver = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Exn.protect
      ~f:(fun () ->
        Eio.Promise.resolve resolver ();
        Eio.Fiber.await_cancel ())
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
          ignore
            (Support.Unix_driver.ping observer ~payload:None |> F.ok
             : Agent_protocol.Ping.Response.t);
          say env "manual.observer-shutdown-self-check=passed")));
  Eio.Promise.await started
;;

let delayed_suggestion ~sw env manual index =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 2.;
    Manual.respond manual ~env ~action:"suggest" ~index;
    `Stop_daemon)
;;

let automatic_suggestions ~sw env provider manual =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop index =
      match Provider.request_at provider index with
      | None ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
        loop index
      | Some request ->
        let body = Provider.body request in
        if
          Poly.equal (Jsonaf.member "stream" body) (Some `False)
          && Poly.equal (Jsonaf.member "model" body) (Some (`String "gpt-5.6-luna"))
        then delayed_suggestion ~sw env manual index;
        loop (index + 1)
    in
    loop 0)
;;

let check_delayed_preview env child =
  let module Pty = Support.Pty_process in
  let clock = Eio.Stdenv.clock env in
  Pty.send child "manual-typeahead-check";
  Eio.Time.sleep clock 0.5;
  let started = Eio.Time.now clock in
  Pty.send child "\000";
  Pty.await_text child ~clock "[suggesting]";
  Eio.Time.sleep clock 0.3;
  F.require
    (not (String.is_substring (Pty.output child) ~substring:"suggestion first line"))
    "manual suggestion arrived before the visual delay";
  Pty.await_text child ~clock "wide text";
  F.require
    Float.(Eio.Time.now clock -. started >= 2.)
    "manual suggestion delay was skipped";
  F.require
    (not (String.is_substring (Pty.output child) ~substring:"END-OF-SUGGESTION"))
    "long suggestion unexpectedly fits without scrolling";
  Pty.send child "\027[1;6C";
  Pty.await_text child ~clock "END-OF-SUGGESTION";
  Pty.send child "\027[1;6D"
;;

let typeahead_self_check ~sw env fixture local =
  let module Pty = Support.Pty_process in
  let child =
    Pty.spawn
      ~sw
      ~env
      ~cwd:(Temp.path (Config.environment fixture) (Config.physical_workspace fixture))
      ~environment:[| "PATH=/usr/bin:/bin"; "TERM=xterm-256color" |]
      ~columns:100
      ~rows:30
      [ "/bin/sh"; local; "--typeahead"; "manual" ]
  in
  let clock = Eio.Stdenv.clock env in
  Pty.await_text child ~clock "MANUAL-TUI-READY";
  check_delayed_preview env child;
  Pty.send child "\t";
  Eio.Time.sleep clock 0.15;
  Pty.send child "\027";
  Pty.await_text child ~clock "NORMAL";
  Pty.send child ":q\r";
  F.require
    (Poly.equal (Pty.await_exit child ~clock) (`Exited 0))
    "typeahead manual fixture failed to exit";
  Pty.assert_restored child ~clock;
  say env "manual.typeahead-self-check=passed"
;;

let run_case ~sw env fixture local provider manual observer = function
  | Some "headless-child" ->
    ignore (Eio_unix.run_in_systhread Core_unix.Terminal_io.setsid : int);
    self_check ~sw env fixture local provider manual;
    let has_terminal =
      try
        Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / "/dev/tty") (fun _ -> true)
      with
      | Eio.Io _ -> false
    in
    F.require (not has_terminal) "headless PTY owner acquired a controlling terminal"
  | Some "observer-shutdown-self-check" -> observer_shutdown_self_check ~sw env observer
  | Some "self-check" -> self_check ~sw env fixture local provider manual
  | Some "orchestration-self-check" ->
    Support.Tui_manual_orchestration.self_check ~sw env fixture provider manual;
    say env "manual.orchestration-self-check=passed"
  | Some "typeahead" ->
    automatic_suggestions ~sw env provider manual;
    watch_requests ~sw env provider;
    say
      env
      "manual.typeahead-ready — run a launcher with --typeahead manual or --typeahead \
       auto; suggestions arrive after 2s with 15 lines. Do not manually answer \
       automatically handled suggestion requests.";
    operator env manual observer
  | Some "typeahead-self-check" ->
    automatic_suggestions ~sw env provider manual;
    typeahead_self_check ~sw env fixture local
  | None | Some "orchestration" ->
    watch_requests ~sw env provider;
    observe ~sw env observer;
    say env "manual.ready — loopback provider and daemon, awaiting operator commands";
    operator env manual observer
  | Some _ -> failwith "unknown manual case"
;;

let run_fixture env ~case =
  Temp.with_ ~scenario:"tui-manual" ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      let provider_port = port ~sw env in
      let provider = Provider.start ~sw ~env ~port:provider_port in
      let manual = Manual.create provider in
      let fixture = fixture env temporary in
      if
        List.mem
          [ Some "orchestration"; Some "orchestration-self-check" ]
          case
          ~equal:Poly.equal
      then Support.Tui_manual_orchestration.configure fixture;
      let local = launchers env fixture provider_port in
      with_daemon ~sw env fixture provider_port (fun observer ->
        Eio.Switch.run (fun sw ->
          run_case ~sw env fixture local provider manual observer case))))
;;

let run env ~case =
  match case with
  | Some "headless-self-check" ->
    Eio.Switch.run (fun sw ->
      let child =
        Support.Process_manager.spawn
          ~sw
          ~env
          ~max_output_bytes:32768
          [ Sys_unix.executable_name
          ; "--scenario"
          ; "tui-manual"
          ; "--case"
          ; "headless-child"
          ]
      in
      let result =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
          Support.Process_manager.await child)
      in
      if not (Support.Process_manager.equal_exit result.exit (Exited 0))
      then
        raise_s [%sexp "headless PTY failed", (result : Support.Process_manager.result)];
      say env "manual.headless-self-check=passed")
  | _ -> run_fixture env ~case
;;
