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

let configure_project temporary fixture =
  let root = Filename.dirname (Config.prompt_path fixture) in
  let save relative contents =
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      (Temp.path temporary (Filename.concat root relative))
      contents
  in
  Eio.Path.mkdir ~perm:0o700 (Temp.path temporary (Filename.concat root "modules"));
  save
    "chatml.project.json"
    {|{"schema_version":1,"namespace":"tui.project","source_dirs":["modules"],"environments":{"empty":{"modules":{}},"selected":{"modules":{"Banner":"self.Messages"}}},"default_environment":"empty"}|};
  save "modules/messages.chatml" {|let text = "TUI-project-captured"|};
  save "modules/messages.chatmli" "val text : string";
  save
    (Filename.basename (Config.prompt_path fixture))
    {|<developer>TUI-fixture-ready</developer>
<script language="chatml" kind="moderator" project="./chatml.project.json" environment="selected">
type state = int
type event = [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx state event ->
    match event with
    | `Session_start ->
      Task.bind(Turn.prepend_system(Banner.text), fun ignored -> Task.pure(state))
    | `Item_appended(_) ->
      Task.bind(Turn.prepend_system(Banner.text ++ ":submitted"), fun ignored ->
        Task.bind(Runtime.end_session("tui-input-accepted"), fun ignored -> Task.pure(state + 1)))
    | _ -> Task.pure(state)
</script>|};
  fun () -> save "modules/messages.chatml" {|let text = "TUI-project-edited"|}
;;

let local env temporary ~columns ~rows ~message ?(export = false) ?(project = false) () =
  let fixture = F.create env temporary "tui-local" in
  let edit_project =
    if project then Some (configure_project temporary fixture) else None
  in
  Support.Provider_fixture.provision ~env fixture;
  let definition = Temp.path temporary (Config.prompt_path fixture) in
  let original_definition = Eio.Path.load definition in
  let output_file =
    Filename.concat (Config.physical_workspace fixture) "conversation.chatmd"
  in
  Eio.Switch.run (fun sw ->
    let child =
      spawn
        ~sw
        env
        fixture
        ~columns
        ~rows
        ((if project
          then [ "--new-session"; "--no-persist" ]
          else [ "--local"; "--transient" ])
         @ [ "-file"; Config.prompt_path fixture ]
         @ if export then [ "--export-file"; output_file ] else [])
    in
    ready env child;
    Option.iter edit_project ~f:(fun edit ->
      Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-project-captured";
      edit ());
    inspect_work env child;
    if message then local_message env temporary child;
    if project
    then
      Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-project-captured:submitted";
    quit env child);
  F.require
    (String.equal (Eio.Path.load definition) original_definition)
    "TUI exit overwrote the authored definition";
  if export
  then (
    let transcript = Eio.Path.load (Temp.path temporary output_file) in
    F.require
      (String.is_substring transcript ~substring:"TUI-fixture-ready")
      "export omitted canonical starting history";
    F.require
      (String.is_substring transcript ~substring:"pty-local-user-message")
      "export omitted canonical submitted history";
    F.require
      (not (String.is_substring transcript ~substring:"<script"))
      "transcript export copied executable definition declarations";
    if project
    then
      (* The PTY above observes the captured module's effective overlay. The
         exported canonical transcript must not promote that overlay to history. *)
      F.require
        (not (String.is_substring transcript ~substring:"TUI-project-"))
        "transcript export promoted an effective overlay to canonical history");
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

let reject_definition_export env temporary =
  let fixture = F.create env temporary "tui-export-definition" in
  Support.Provider_fixture.provision ~env fixture;
  let definition_file = Config.prompt_path fixture in
  let definition = Temp.path temporary definition_file in
  let original = Eio.Path.load definition in
  let alias =
    Filename.concat
      (Filename.dirname definition_file)
      ("./" ^ Filename.basename definition_file)
  in
  Eio.Switch.run (fun sw ->
    let child =
      spawn
        ~sw
        env
        fixture
        ~columns:100
        ~rows:30
        [ "--local"; "--transient"; "-file"; definition_file; "--export-file"; alias ]
    in
    ready env child;
    escape_to_normal env child;
    Pty.send child ":q\r";
    (match Pty.await_exit child ~clock:(Eio.Stdenv.clock env) with
     | `Exited code -> F.require (code <> 0) "definition export unexpectedly succeeded"
     | `Signaled _ -> failwith "definition export rejection killed the TUI");
    F.require
      (String.is_substring (Pty.output child) ~substring:"Export destination must differ")
      "definition export failed for an unrelated reason";
    Pty.assert_restored child ~clock:(Eio.Stdenv.clock env));
  F.require
    (String.equal original (Eio.Path.load definition))
    "definition alias export changed authored source"
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
      let snapshot =
        Agent_client.Admin.get_session connection session_id
        |> F.ok
        |> Agent_protocol.Public.Snapshot.fields
      in
      if snapshot.halted then Some snapshot else None)
  in
  F.assert_public_user snapshot.canonical_history.entries text;
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
    let after =
      Agent_client.Admin.get_session connection session.id
      |> F.ok
      |> Agent_protocol.Public.Snapshot.fields
    in
    F.assert_public_user after.canonical_history.entries "pty-daemon-user-message";
    F.require
      (Agent_protocol.Session.equal_liveness after.session.spec.liveness Detached)
      "connected TUI changed detached liveness")
;;

(* Real terminal adoption and maintenance share the same durable embedded host.
   Transcript data is separate from the configured definition/module inventory. *)
let imported_project_rebuild env temporary =
  let fixture = F.create env temporary "tui-import-project" in
  Support.Provider_fixture.provision ~env fixture;
  let edit_project = configure_project temporary fixture in
  let roots = Temp.roots temporary in
  let root = Filename.concat roots.data "import-project-store" in
  let input = Filename.concat roots.artifacts "edited-transcript.chatmd" in
  let output = Filename.concat roots.artifacts "imported-export.chatmd" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temp.path temporary input)
    {|<developer>TUI-fixture-ready</developer>
<user ochat-history-id="3:old:41">IMPORTED-EDITED-HISTORY</user>|};
  let definition = Eio.Path.load (Temp.path temporary (Config.prompt_path fixture)) in
  Eio.Switch.run (fun sw ->
    let child =
      spawn
        ~sw
        env
        fixture
        ~columns:100
        ~rows:30
        [ "--local"
        ; "--data-root"
        ; root
        ; "-file"
        ; Config.prompt_path fixture
        ; "--import-transcript"
        ; input
        ; "--export-file"
        ; output
        ]
    in
    ready env child;
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "IMPORTED-EDITED-HISTORY";
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-project-captured";
    submit child "stop-after-import";
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-project-captured:submitted";
    quit env child);
  let exported = Eio.Path.load (Temp.path temporary output) in
  F.require
    (String.is_substring exported ~substring:"IMPORTED-EDITED-HISTORY")
    "native import/export lost edited transcript";
  F.require
    (String.equal
       definition
       (Eio.Path.load (Temp.path temporary (Config.prompt_path fixture))))
    "native adoption overwrote executable definition";
  let command arguments =
    Eio.Switch.run (fun sw ->
      let child =
        Support.Process_manager.spawn
          ~sw
          ~env
          ~cwd:(Temp.path temporary (Config.physical_workspace fixture))
          ~environment:(F.environment temporary)
          ~max_output_bytes:65536
          (F.tui_executable env
           :: "--no-config"
           :: "--local"
           :: "--data-root"
           :: root
           :: "-file"
           :: Config.prompt_path fixture
           :: arguments)
      in
      let result =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
          Support.Process_manager.await child)
      in
      F.require
        (Support.Process_manager.equal_exit result.exit (Exited 0))
        ("native project command failed: " ^ result.stderr.contents);
      F.require
        (not (result.stdout.truncated || result.stderr.truncated))
        "native project command output truncated";
      result.stdout.contents)
  in
  let session =
    match Jsonaf.of_string (command [ "--list-sessions"; "--json" ]) with
    | `Array [ json ] -> Agent_protocol.Session.of_json json |> F.ok
    | _ -> failwith "transcript adoption did not create exactly one conversation"
  in
  let id = Agent_protocol.Id.Session.to_string session.id in
  let inspect () =
    command [ "--session-info"; id; "--json" ]
    |> Jsonaf.of_string
    |> Agent_protocol.Public.Snapshot.of_json
    |> F.ok
    |> Agent_protocol.Public.Snapshot.fields
  in
  let before = inspect () in
  edit_project ();
  ignore (command [ "--rebuild-from-prompt"; id ] : string);
  let after = inspect () in
  F.require
    (Agent_protocol.Id.Session.equal before.session.id after.session.id)
    "explicit rebuild replaced conversation identity";
  F.require
    (after.session.generation > before.session.generation)
    "explicit rebuild did not advance generation";
  F.require
    (not
       (Option.equal
          Agent_protocol.Id.Prompt_revision.equal
          before.session.prompt_revision
          after.session.prompt_revision))
    "explicit rebuild retained the old captured program revision";
  F.require
    (not
       (List.exists after.canonical_history.entries ~f:(fun entry ->
          List.mem
            (Support.Public_view.history_text entry)
            "IMPORTED-EDITED-HISTORY"
            ~equal:String.equal)))
    "explicit rebuild unexpectedly retained imported conversation history";
  Eio.Switch.run (fun sw ->
    let child =
      spawn
        ~sw
        env
        fixture
        ~columns:100
        ~rows:30
        [ "--local"
        ; "--data-root"
        ; root
        ; "-file"
        ; Config.prompt_path fixture
        ; "--session"
        ; id
        ]
    in
    ready env child;
    submit child "run-current-code";
    Pty.await_text child ~clock:(Eio.Stdenv.clock env) "TUI-project-edited:submitted";
    quit env child)
;;

let cases =
  [ "local.durable-retained-commands", Local_storage_scenario.run
  ; "pty.local-transcript-import-current-rebuild", imported_project_rebuild
  ; "trace.messages-stable-ids", Tui_trace_scenario.messages
  ; "trace.reasoning-tools-progress", Tui_stream_scenario.run
  ; "trace.deferred-overlays-agent-page", Tui_stream_scenario.overlays
  ; "trace.approval-compaction-cancellation", Tui_stream_scenario.approval
  ; "trace.reconnect-draft-preservation", Tui_stream_scenario.reconnect
  ; "trace.presentation-state-client-local", Tui_trace_scenario.presentation
  ; ( "pty.local-start-message-quit"
    , fun env temporary -> local env temporary ~columns:100 ~rows:30 ~message:true () )
  ; ( "pty.local-explicit-transcript-export"
    , fun env temporary ->
        local env temporary ~columns:100 ~rows:30 ~message:true ~export:true () )
  ; ( "pty.local-configured-project-capture"
    , fun env temporary ->
        local
          env
          temporary
          ~columns:100
          ~rows:30
          ~message:true
          ~export:true
          ~project:true
          () )
  ; "pty.unix-connected-start-quit", connected false
  ; "pty.local-definition-export-rejected", reject_definition_export
  ; "pty.http-connected-start-quit", connected true
  ; ( "pty.terminal-restoration"
    , fun env temporary -> local env temporary ~columns:72 ~rows:20 ~message:false () )
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
