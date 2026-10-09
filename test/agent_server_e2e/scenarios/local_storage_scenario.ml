open! Core
module P = Agent_protocol
module Embedded = Agent_server.Embedded
module Temp = Support.Temporary_environment
module Process = Support.Process_manager
module Pty = Support.Pty_process
module F = Support.Tui_fixture

let stopped = function
  | P.Session.Stopped -> true
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let command ~sw env temporary ~prompt ~root arguments =
  let child =
    Process.spawn
      ~sw
      ~env
      ~environment:(F.environment temporary)
      ~max_output_bytes:65536
      (F.tui_executable env
       :: "--no-config"
       :: "--local"
       :: "--data-root"
       :: root
       :: "-file"
       :: prompt
       :: arguments)
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () -> Process.await child)
;;

let require_success result =
  F.require
    (Process.equal_exit result.Process.exit (Exited 0))
    ("local command failed: " ^ result.stderr.contents);
  F.require
    (not (result.stdout.truncated || result.stderr.truncated))
    "truncated CLI output"
;;

let run env temporary =
  Mirage_crypto_rng_unix.use_default ();
  let roots = Temp.roots temporary in
  let root = Filename.concat roots.data "local-store" in
  let workspace = Filename.concat roots.workspaces "local-workspace" in
  let prompt = Filename.concat roots.root "local.chatmd" in
  let source = "<developer>LOCAL-CLI-READY</developer>" in
  Eio.Path.mkdir ~perm:0o700 (Temp.path temporary workspace);
  Eio.Path.save ~create:(`Exclusive 0o600) (Temp.path temporary prompt) source;
  let durable = Agent_server.Local_storage.Root.create ~path:root () |> F.ok in
  let options : Embedded.options =
    { prompt_file = prompt
    ; workspace
    ; tool_dir = roots.root
    ; home = Some roots.home
    ; storage = Durable durable
    ; start_immediately = false
    ; permission_profile = Embedded.default_permission_profile
    ; attachment_mode = Read_write
    ; event_capacity = 64
    }
  in
  let calls = ref 0 in
  let daemon_options =
    { Agent_server.Daemon.default_options with
      inference_policy =
        Agent_server_test_support.inference_policy
          ~default_model:"fixture-model"
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            Int.incr calls;
            failwith "CLI fixture activated provider")
    }
  in
  let id =
    Eio.Switch.run (fun sw ->
      let created = Embedded.start ~sw ~env ~daemon_options options |> F.ok in
      let id = Embedded.session_id created in
      Embedded.close created;
      id)
  in
  let id_text = P.Id.Session.to_string id in
  let saved_prompt = prompt ^ ".saved" in
  let saved_workspace = workspace ^ ".saved" in
  Eio.Path.rename (Temp.path temporary prompt) (Temp.path temporary saved_prompt);
  Eio.Path.rename (Temp.path temporary workspace) (Temp.path temporary saved_workspace);
  Eio.Switch.run (fun sw ->
    let listing =
      command ~sw env temporary ~prompt ~root [ "--list-sessions"; "--json" ]
    in
    require_success listing;
    F.require
      (String.is_substring listing.stdout.contents ~substring:id_text)
      "retained ID missing from CLI catalog";
    let inspection =
      command ~sw env temporary ~prompt ~root [ "--session-info"; id_text; "--json" ]
    in
    require_success inspection;
    let snapshot =
      P.Public.Snapshot.of_json (Jsonaf.of_string inspection.stdout.contents) |> F.ok
    in
    let fields = P.Public.Snapshot.fields snapshot in
    F.require
      (P.Id.Session.equal fields.session.id id && stopped fields.session.observed_state)
      "CLI inspect changed stopped identity";
    let out_file = Filename.concat roots.artifacts "retained.chatmd" in
    let exported =
      command
        ~sw
        env
        temporary
        ~prompt
        ~root
        [ "--export-session"; id_text; "--out"; out_file ]
    in
    require_success exported;
    F.require
      (String.is_substring
         (Eio.Path.load (Temp.path temporary out_file))
         ~substring:"LOCAL-CLI-READY")
      "CLI export lost pinned prompt";
    let unavailable = command ~sw env temporary ~prompt ~root [ "--session"; id_text ] in
    F.require
      (not (Process.equal_exit unavailable.exit (Exited 0)))
      "selection silently replaced missing workspace";
    let conflict =
      command ~sw env temporary ~prompt ~root [ "--transient"; "--list-sessions" ]
    in
    F.require
      (not (Process.equal_exit conflict.exit (Exited 0)))
      "conflicting root modes accepted");
  Eio.Path.rename (Temp.path temporary saved_prompt) (Temp.path temporary prompt);
  Eio.Path.rename (Temp.path temporary saved_workspace) (Temp.path temporary workspace);
  Eio.Switch.run (fun sw ->
    let child =
      Pty.spawn
        ~sw
        ~env
        ~cwd:(Temp.path temporary workspace)
        ~environment:(F.environment temporary)
        ~columns:100
        ~rows:30
        [ F.tui_executable env
        ; "--no-config"
        ; "--local"
        ; "--data-root"
        ; root
        ; "-file"
        ; prompt
        ; "--session"
        ; id_text
        ]
    in
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "LOCAL-CLI-READY";
    Pty.send child "\027";
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "NORMAL";
    Pty.send child ":q\r";
    (match Pty.await_exit child ~clock:(Eio.Stdenv.clock env) with
     | `Exited 0 -> ()
     | `Exited _ | `Signaled _ -> failwith "selected local TUI failed");
    Pty.assert_restored child ~clock:(Eio.Stdenv.clock env));
  Eio.Switch.run (fun sw ->
    Embedded.with_local_host ~sw ~env ~daemon_options options ~f:(fun host ->
      let open Result.Let_syntax in
      let connection = Embedded.host_connection host in
      let%bind sessions = Agent_client.Admin.list_sessions connection in
      let%map snapshot = Agent_client.Admin.get_session connection id in
      F.require (List.length sessions = 1) "local commands created replacement session";
      F.require
        (stopped (P.Public.Snapshot.fields snapshot).session.observed_state)
        "selection implicitly started stopped session";
      F.require (!calls = 0) "local catalog/selection requested provider")
    |> F.ok)
;;
