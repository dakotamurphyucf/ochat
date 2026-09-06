open Core

let require condition message = if not condition then failwith message

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let prompt =
  {|
<developer>TUI-fixture-ready</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Item_appended(_) ->
        Task.bind(Runtime.end_session("tui-input-accepted"), fun ignored -> Task.pure(state + 1))
      | _ -> Task.pure(state)
</script>
|}
;;

let create env environment name =
  let http_port =
    Eio.Switch.run (fun sw ->
      let reserved = Port_reservation.create ~sw ~env in
      let port = Port_reservation.port reserved in
      Port_reservation.release reserved;
      port)
  in
  let fixture = Config_fixture.create environment ~name ~http_port in
  Config_fixture.grant_public_all_scopes fixture;
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.prompt_path fixture))
    prompt;
  fixture
;;

let environment temporary =
  Temporary_environment.child_environment temporary ~base:(Core_unix.environment ())
  |> Array.filter ~f:(fun entry ->
    not
      (List.exists
         [ "TERM="; "API_URL="; "OPENAI_BASE_URL="; "OPENAI_API_KEY=" ]
         ~f:(fun prefix -> String.is_prefix entry ~prefix)))
  |> Fn.flip Array.append [| "TERM=xterm-256color"; "API_URL=http://127.0.0.1:1" |]
;;

let tui_executable env =
  let path =
    Sys.getenv "OCHAT_E2E_TUI_EXE"
    |> Option.value
         ~default:
           (Filename.concat
              (Eio.Path.native_exn (Eio.Stdenv.cwd env))
              "_build/default/bin/chat_tui.exe")
  in
  if Filename.is_absolute path then path else Eio_posix.Low_level.realpath path
;;

let with_daemon env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon_process.start_in_directory_with_environment_overrides
        ~sw
        ~env
        ~fixture
        ~cwd:
          (Temporary_environment.path
             (Config_fixture.environment fixture)
             (Config_fixture.physical_workspace fixture))
        ~environment_overrides:[ "API_URL", "http://127.0.0.1:1" ]
        ~config_path:(Config_fixture.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        (match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
         | Ok _ -> ()
         | Error error -> raise_s [%sexp (error : Daemon_process.readiness_error)]);
        let connection =
          Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
        in
        Exn.protect
          ~f:(fun () ->
            ignore
              (Unix_driver.initialize connection |> ok
               : Agent_protocol.Initialize.Response.t);
            f sw connection)
          ~finally:(fun () -> Agent_client.Connection.close connection))
      ~finally:(fun () ->
        ignore
          (Daemon_process.stop daemon ~env ~grace_seconds:1.
           : Process_manager.termination)))
;;

let bearer_file fixture =
  let temporary = Config_fixture.environment fixture in
  let path =
    Filename.concat (Temporary_environment.roots temporary).config "tui-bearer"
  in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path temporary path)
    (Config_fixture.admin_token fixture ^ "\n");
  path
;;

let await env f =
  let rec loop () =
    match f () with
    | Some value -> value
    | None ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      loop ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. loop
;;

let rec journal_files path depth =
  if depth = 0
  then []
  else
    Eio.Path.read_dir path
    |> List.concat_map ~f:(fun name ->
      let child = Eio.Path.(path / name) in
      match Eio.Path.kind ~follow:false child with
      | `Directory -> journal_files child (depth - 1)
      | `Regular_file
        when String.equal (Filename.basename (Eio.Path.native_exn path)) "journal"
             && Result.is_ok (Agent_store.Journal_segment.Id.of_filename name) ->
        [ child ]
      | _ -> [])
;;

let frame_events contents =
  let rec loop offset acc =
    if offset = String.length contents
    then List.rev acc |> List.concat
    else (
      match
        Agent_store.Frame.decode ~max_payload_length:(16 * 1024 * 1024) ~contents ~offset
      with
      | Ok (Incomplete_tail _) -> List.rev acc |> List.concat
      | Ok (Complete { frame; next_offset }) ->
        let transaction =
          Agent_store.Frame.payload frame |> Agent_store.Transaction.decode
        in
        let events =
          match transaction with
          | Ok transaction ->
            Agent_session.Session_persistence.durable_events transaction
            |> Result.map_error ~f:(fun error ->
              Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
            |> Result.ok_or_failwith
          | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
        in
        loop next_offset (events :: acc)
      | Error error -> raise_s [%sexp (error : Agent_store.Frame.error)])
  in
  loop 0 []
;;

let local_events temporary =
  let root =
    Temporary_environment.path temporary (Temporary_environment.roots temporary).temporary
  in
  journal_files root 8
  |> List.concat_map ~f:(fun file -> Eio.Path.load file |> frame_events)
;;

let assert_user entries text =
  let users =
    List.filter entries ~f:(fun (entry : Agent_protocol.History.entry) ->
      Agent_protocol.History.equal_role entry.role User)
  in
  match users with
  | [ actual ] ->
    let expected =
      Agent_session.History_codec.user_text ~id:actual.id text
      |> Agent_session.History_codec.to_protocol
    in
    require
      (Sexp.equal
         ([%sexp_of: Agent_protocol.History.entry] actual)
         ([%sexp_of: Agent_protocol.History.entry] expected))
      "TUI submitted different content"
  | _ ->
    raise_s
      [%sexp
        "TUI did not submit exactly one canonical user entry"
      , (users : Agent_protocol.History.entry list)]
;;

module Offline_network = struct
  type t = unit

  type tag =
    [ `Generic
    | `Unix
    ]

  let denied () = failwith "embedded TUI trace forbids network access"
  let listen () ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _ = denied ()
  let connect () ~sw:_ _ = denied ()
  let datagram_socket () ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ = denied ()
  let getaddrinfo () ~service:_ _ = denied ()
  let getnameinfo () _ = denied ()
end

let offline_environment (env : Eio_unix.Stdenv.base) : Eio_unix.Stdenv.base =
  let net = Eio.Resource.T ((), Eio.Net.Pi.network (module Offline_network)) in
  object
    method net = net
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method fs = env#fs
    method cwd = env#cwd
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method domain_mgr = env#domain_mgr
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
