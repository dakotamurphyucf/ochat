open Core
module F = Support.Tui_fixture
module Config = Support.Config_fixture
module Temp = Support.Temporary_environment
module Pty = Support.Pty_process

let spawn ~sw env fixture ~columns ~rows arguments =
  let temporary = Config.environment fixture in
  Pty.spawn
    ~sw
    ~env
    ~cwd:(Temp.path temporary (Config.physical_workspace fixture))
    ~environment:(F.environment temporary)
    ~columns
    ~rows
    (F.tui_executable env :: "--no-config" :: arguments)
;;

let ready env child =
  Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-fixture-ready"
;;

let escape_to_normal env child =
  let before_escape = String.length (Pty.output child) in
  Pty.send child "\027";
  F.await env (fun () ->
    let fresh = String.drop_prefix (Pty.output child) before_escape in
    Option.some_if (String.is_substring fresh ~substring:"NORMAL") ())
  |> ignore
;;

let inspect_work env child =
  escape_to_normal env child;
  Pty.send child ":work\r";
  Pty.await_text child ~clock:(Eio.Stdenv.clock env) "Session work";
  (* Escape followed immediately by i is an Alt-i terminal sequence. Wait for
     the new Chat frame before sending the separate Insert-mode key. *)
  escape_to_normal env child;
  Pty.send child "i"
;;

let submit child text = Pty.send child (text ^ "\027\r")

let quit env child =
  escape_to_normal env child;
  Pty.send child ":q\r";
  let status = Pty.await_exit child ~clock:(Eio.Stdenv.clock env) in
  F.require (Poly.equal status (`Exited 0)) "TUI did not exit successfully";
  Pty.assert_restored child ~clock:(Eio.Stdenv.clock env)
;;

let appended entries =
  List.concat_map entries ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> F.ok
    with
    | History_appended entries -> entries
    | _ -> [])
;;

let local_message env temporary child =
  let text = "pty-local-user-message" in
  submit child text;
  let entries =
    F.await env (fun () ->
      let entries = F.local_events temporary |> appended in
      if
        List.exists entries ~f:(fun entry ->
          Agent_protocol.History.equal_role entry.role User)
      then Some entries
      else None)
  in
  F.assert_user entries text;
  Pty.await_text child ~clock:(Eio.Stdenv.clock env) text
;;

let local env temporary ~columns ~rows ~message =
  let fixture = F.create env temporary "tui-local" in
  Eio.Switch.run (fun sw ->
    let child =
      spawn
        ~sw
        env
        fixture
        ~columns
        ~rows
        [ "--local"; "-file"; Config.prompt_path fixture ]
    in
    ready env child;
    inspect_work env child;
    if message then local_message env temporary child;
    quit env child);
  let remaining =
    Eio.Path.read_dir (Temp.path temporary (Temp.roots temporary).temporary)
  in
  F.require
    (not (List.exists remaining ~f:(String.is_prefix ~prefix:"ochat-embedded-")))
    "local process-bound host survived TUI exit"
;;

let connected_arguments fixture http =
  let connection =
    if http
    then
      [ "--connect"
      ; sprintf "http://127.0.0.1:%d" (Config.http_port fixture)
      ; "--bearer-token-file"
      ; F.bearer_file fixture
      ]
    else [ "--connect"; "unix://" ^ Config.unix_socket fixture ]
  in
  connection
  @ [ "--new-daemon-session"
    ; "--prompt"
    ; "smoke"
    ; "--workspace"
    ; "physical"
    ; "--detached"
    ]
;;

let only_session env connection =
  F.await env (fun () ->
    match Agent_client.Admin.list_sessions connection |> F.ok with
    | [ session ] -> Some session
    | [] -> None
    | _ -> failwith "TUI created more than one session")
;;

let connected_message env connection session_id child =
  let text = "pty-daemon-user-message" in
  submit child text;
  let snapshot =
    F.await env (fun () ->
      let snapshot = Agent_client.Admin.get_session connection session_id |> F.ok in
      if snapshot.halted then Some snapshot else None)
  in
  F.assert_user snapshot.canonical_history.entries text;
  Pty.await_text child ~clock:(Eio.Stdenv.clock env) text
;;

let connected http env temporary =
  let fixture = F.create env temporary "tui-connected" in
  F.with_daemon env fixture (fun sw connection ->
    let child =
      spawn ~sw env fixture ~columns:100 ~rows:30 (connected_arguments fixture http)
    in
    ready env child;
    inspect_work env child;
    let session = only_session env connection in
    F.require
      (Agent_protocol.Session.equal_desired_state session.desired_state Running)
      "TUI-created session was not running";
    connected_message env connection session.id child;
    quit env child;
    let after = Agent_client.Admin.get_session connection session.id |> F.ok in
    F.assert_user after.canonical_history.entries "pty-daemon-user-message";
    F.require
      (Agent_protocol.Session.equal_liveness after.session.spec.liveness Detached)
      "connected TUI changed detached liveness")
;;

let cases =
  [ "trace.messages-stable-ids", Tui_trace_scenario.messages
  ; "trace.reasoning-tools-progress", Tui_stream_scenario.run
  ; "trace.deferred-overlays-agent-page", Tui_stream_scenario.overlays
  ; "trace.approval-compaction-cancellation", Tui_stream_scenario.approval
  ; "trace.reconnect-draft-preservation", Tui_stream_scenario.reconnect
  ; "trace.presentation-state-client-local", Tui_trace_scenario.presentation
  ; ( "pty.local-start-message-quit"
    , fun env temporary -> local env temporary ~columns:100 ~rows:30 ~message:true )
  ; "pty.unix-connected-start-quit", connected false
  ; "pty.http-connected-start-quit", connected true
  ; ( "pty.terminal-restoration"
    , fun env temporary -> local env temporary ~columns:72 ~rows:20 ~message:false )
  ]
;;

let run env ~case =
  let selected =
    match case with
    | None -> cases
    | Some name -> [ name, List.Assoc.find_exn cases name ~equal:String.equal ]
  in
  List.iter selected ~f:(fun (name, test) ->
    Temp.with_ ~scenario:("tui-parity-" ^ name) ~env (fun temporary -> test env temporary);
    Eio.Flow.copy_string
      (Sexp.to_string_hum [%sexp "TUI check passed", (name : string)] ^ "\n")
      (Eio.Stdenv.stdout env))
;;
