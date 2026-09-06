open! Core
module F = Support.Tui_fixture
module Temp = Support.Temporary_environment
module Config = Support.Config_fixture
module Pty = Support.Pty_process
module Provider = Support.Compaction_json_provider
module Process = Support.Process_manager

let environment temporary port =
  F.environment temporary
  |> Array.filter ~f:(fun entry -> not (String.is_prefix entry ~prefix:"API_URL="))
  |> Fn.flip
       Array.append
       [| sprintf "API_URL=http://127.0.0.1:%d" port
        ; "OPENAI_API_KEY=typeahead-fixture-key"
       |]
;;

let spawn ~sw env fixture port arguments =
  let temporary = Config.environment fixture in
  Pty.spawn
    ~sw
    ~env
    ~cwd:(Temp.path temporary (Config.physical_workspace fixture))
    ~environment:(environment temporary port)
    ~columns:100
    ~rows:30
    (F.tui_executable env :: "--no-config" :: arguments)
;;

let quit env child =
  let offset = String.length (Pty.output child) in
  Pty.send child "\027";
  F.await env (fun () ->
    if
      String.is_substring
        (String.drop_prefix (Pty.output child) offset)
        ~substring:"NORMAL"
    then Some ()
    else None);
  Pty.send child ":q\r";
  F.require
    (Poly.equal (Pty.await_exit child ~clock:(Eio.Stdenv.clock env)) (`Exited 0))
    "typeahead TUI did not exit cleanly";
  Pty.assert_restored child ~clock:(Eio.Stdenv.clock env)
;;

let request provider env index =
  let request = Provider.await_request provider ~env ~index in
  let body = Provider.body request in
  F.require
    (Poly.equal (Jsonaf.member "model" body) (Some (`String "gpt-5.6-luna")))
    "wrong suggestion model";
  F.require
    (Poly.equal (Jsonaf.member "tools" body) (Some (`Array [])))
    "suggestion declared tools";
  F.require (Poly.equal (Jsonaf.member "stream" body) (Some `False)) "suggestion streamed";
  request
;;

let check_body request expected =
  F.require
    (String.is_substring (Jsonaf.to_string (Provider.body request)) ~substring:expected)
    "suggestion request did not reflect the expected editor state"
;;

let check_redo env provider child =
  let clock = Eio.Stdenv.clock env in
  Pty.send child "u\018i\000";
  let restored = request provider env 2 in
  check_body restored "SECOND-LINE";
  Provider.release restored (Summary "REDO-VERIFIED");
  Pty.await_text child ~clock "REDO-VERIFIED";
  List.iter [ (); (); () ] ~f:(fun () ->
    Pty.send child "\027";
    Eio.Time.sleep clock 0.15)
;;

let exercise env provider child ~unchanged =
  let clock = Eio.Stdenv.clock env in
  Pty.await_text child ~clock "TYPEAHEAD-READY";
  unchanged ();
  Pty.send child "PRIVATE-DRAFT";
  Eio.Time.sleep clock 0.3;
  F.require (Provider.request_count provider = 0) "manual mode auto-transmitted draft";
  Pty.send child "\000";
  let first = request provider env 0 in
  check_body first "PRIVATE-DRAFT";
  Pty.await_text child ~clock "[suggesting]";
  F.require
    (not
       (String.is_substring
          (Jsonaf.to_string (Provider.body first))
          ~substring:"TYPEAHEAD-READY"))
    "default suggestion leaked history";
  Provider.release first (Summary " first-line\nSECOND-LINE");
  Pty.await_text child ~clock "SECOND-LINE";
  unchanged ();
  Pty.send child "\027";
  Eio.Time.sleep clock 0.15;
  Pty.send child "\027[Z";
  Pty.send child "\t";
  Eio.Time.sleep clock 0.3;
  F.require (Provider.request_count provider = 1) "acceptance auto-transmitted draft";
  unchanged ();
  Pty.send child "\000";
  let second = request provider env 1 in
  check_body second "SECOND-LINE";
  Provider.release second (Summary "DISMISS-ME");
  Pty.await_text child ~clock "DISMISS-ME";
  Pty.send child "\027";
  Eio.Time.sleep clock 0.15;
  Pty.send child "\027";
  Eio.Time.sleep clock 0.15;
  Pty.send child "\027";
  Pty.await_text child ~clock "NORMAL";
  check_redo env provider child;
  Pty.send child "ui";
  Pty.send child "\000";
  let failure = request provider env 3 in
  F.require
    (not
       (String.is_substring
          (Jsonaf.to_string (Provider.body failure))
          ~substring:"SECOND-LINE"))
    "undo failed to remove the accepted second line";
  Provider.release failure (Raw_json {|{"error":{"message":"PRIVATE_ERROR_CANARY"}}|});
  Pty.await_text child ~clock "typeahead unavailable";
  F.require
    (not (String.is_substring (Pty.output child) ~substring:"PRIVATE_ERROR_CANARY"))
    "provider error body leaked into terminal";
  unchanged ();
  Pty.send child "\000";
  ignore (request provider env 4 : Provider.request);
  Pty.send child "\027";
  Eio.Time.sleep clock 0.15;
  quit env child
;;

let rec assert_no_raw_logs path =
  List.iter (Eio.Path.read_dir path) ~f:(fun name ->
    let child = Eio.Path.(path / name) in
    if Eio.Path.is_directory child
    then assert_no_raw_logs child
    else (
      F.require
        (not
           (List.mem
              [ "type-ahead-out.txt"
              ; "raw-openai-response.txt"
              ; "raw-openai-streaming-response.txt"
              ]
              name
              ~equal:String.equal))
        "typeahead created a raw provider log";
      let contents = Eio.Path.load child in
      List.iter
        [ "PRIVATE-DRAFT"
        ; "PRIVATE_ERROR_CANARY"
        ; "SECOND-LINE"
        ; "OBSERVER-PRIVATE"
        ; "AUTO-DRAFT"
        ; "CURRENT-SUGGESTION"
        ]
        ~f:(fun canary ->
          F.require
            (not (String.is_substring contents ~substring:canary))
            "typeahead private canary persisted to disk")))
;;

let exercise_auto env provider child ~unchanged =
  let clock = Eio.Stdenv.clock env in
  Pty.await_text child ~clock "TYPEAHEAD-READY";
  unchanged ();
  Pty.send child "AUTO-DRAFT";
  let first = request provider env 0 in
  Pty.send child "x";
  let second = request provider env 1 in
  check_body second "AUTO-DRAFTx";
  Provider.release first (Summary "STALE-CANARY");
  Provider.release second (Summary "CURRENT-SUGGESTION");
  Pty.await_text child ~clock "CURRENT-SUGGESTION";
  F.require
    (not (String.is_substring (Pty.output child) ~substring:"STALE-CANARY"))
    "cancelled suggestion was rendered";
  Pty.send child "\t";
  Eio.Time.sleep clock 0.35;
  F.require
    (Provider.request_count provider = 2)
    "auto acceptance started another request";
  unchanged ();
  quit env child
;;

let connected_arguments fixture ~http =
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

let assert_no_local_user temporary =
  List.iter (F.local_events temporary) ~f:(fun event ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> F.ok
    with
    | History_appended entries ->
      F.require
        (not
           (List.exists entries ~f:(fun entry ->
              Agent_protocol.History.equal_role entry.role User)))
        "draft mutated local history"
    | _ -> ())
;;

let run_host env name =
  let host = Option.value (String.chop_prefix name ~prefix:"auto-") ~default:name in
  let automatic = not (String.equal name host) in
  Temp.with_ ~scenario:("typeahead-" ^ name) ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      let port = Support.Port_reservation.create ~sw ~env in
      let number = Support.Port_reservation.port port in
      Support.Port_reservation.release port;
      let provider = Provider.start ~sw ~env ~port:number in
      let fixture = F.create env temporary ("typeahead-" ^ name) in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        (Temp.path temporary (Config.prompt_path fixture))
        "<developer>TYPEAHEAD-READY</developer>";
      let start arguments unchanged =
        let child =
          spawn
            ~sw
            env
            fixture
            number
            (arguments @ [ "--typeahead"; (if automatic then "auto" else "manual") ])
        in
        (if automatic then exercise_auto else exercise) env provider child ~unchanged
      in
      (match host with
       | "legacy" ->
         start
           [ "--new-session"
           ; "--no-persist"
           ; "-file"
           ; Config.prompt_path fixture
           ; "--export-file"
           ; Filename.concat (Temp.roots temporary).root "legacy-export.chatmd"
           ]
           (fun () -> ())
       | "local" ->
         start
           [ "--local"; "-file"; Config.prompt_path fixture ]
           (fun () -> assert_no_local_user temporary)
       | _ ->
         F.with_daemon env fixture (fun _ connection ->
           let checked_peer = ref false in
           start
             (connected_arguments fixture ~http:(String.equal host "http"))
             (fun () ->
                let sessions = Agent_client.Admin.list_sessions connection |> F.ok in
                let session = List.hd_exn sessions in
                let snapshot =
                  Agent_client.Admin.get_session connection session.id |> F.ok
                in
                F.require
                  (List.length snapshot.canonical_history.entries = 1)
                  "draft mutated daemon history";
                if not !checked_peer
                then (
                  checked_peer := true;
                  let calls = Provider.request_count provider in
                  let peer =
                    spawn
                      ~sw
                      env
                      fixture
                      number
                      [ "--connect"
                      ; "unix://" ^ Config.unix_socket fixture
                      ; "--session"
                      ; Agent_protocol.Id.Session.to_string session.id
                      ; "--read-only"
                      ; "--typeahead"
                      ; "auto"
                      ]
                  in
                  Pty.await_text peer ~clock:(Eio.Stdenv.clock env) "TYPEAHEAD-READY";
                  Pty.send peer "OBSERVER-PRIVATE";
                  Pty.send peer "\000";
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.4;
                  F.require
                    (Provider.request_count provider = calls)
                    "read-only client requested suggestion";
                  F.require
                    (not
                       (String.is_substring (Pty.output peer) ~substring:"PRIVATE-DRAFT"))
                    "another client saw private editor state";
                  quit env peer))));
      assert_no_raw_logs (Temp.path temporary (Temp.roots temporary).root)));
  Eio.Flow.copy_string ("typeahead." ^ name ^ " passed\n") (Eio.Stdenv.stdout env)
;;

let cli ~sw env temporary arguments =
  let child =
    Process.spawn
      ~sw
      ~env
      ~environment:(F.environment temporary)
      ~max_output_bytes:16384
      (F.tui_executable env :: arguments)
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () -> Process.await child)
;;

let configuration env =
  Temp.with_ ~scenario:"typeahead-config" ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      let path = Filename.concat (Temp.roots temporary).config "typeahead.args" in
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        (Temp.path temporary path)
        "--typeahead auto\n--typeahead-debounce-ms 300\n";
      List.iter
        [ [ "--config"; path; "--typeahead"; "off"; "--list-sessions" ]
        ; [ "--no-config"; "--list-sessions" ]
        ]
        ~f:(fun arguments ->
          F.require
            (Process.equal_exit (cli ~sw env temporary arguments).exit (Exited 0))
            "typeahead config precedence/no-config failed");
      List.iter
        [ [ "--typeahead"; "manual" ]
        ; [ "--typeahead-history-messages"; "4" ]
        ; [ "--typeahead-debounce-ms"; "0" ]
        ; [ "--typeahead-max-output-tokens"; "513" ]
        ]
        ~f:(fun flags ->
          let result =
            cli
              ~sw
              env
              temporary
              ([ "--no-config"; "--local"; "-file"; "/must-not-read" ] @ flags)
          in
          F.require
            (not (Process.equal_exit result.exit (Exited 0)))
            "invalid typeahead config accepted";
          F.require
            (String.is_substring
               (result.stdout.contents ^ result.stderr.contents)
               ~substring:"typeahead"
             || String.is_substring
                  (result.stdout.contents ^ result.stderr.contents)
                  ~substring:"Typeahead")
            "configuration validation did not precede prompt/session startup")));
  Eio.Flow.copy_string "typeahead.configuration passed\n" (Eio.Stdenv.stdout env)
;;

let run env ~case =
  if Option.is_none case then configuration env;
  List.iter
    (match case with
     | None ->
       [ "legacy"; "local"; "daemon"; "http"; "auto-legacy"; "auto-local"; "auto-daemon" ]
     | Some name -> [ name ])
    ~f:(run_host env)
;;
