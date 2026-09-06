open Core

type t =
  { process : Process_manager.t
  ; fixture : Config_fixture.t
  }

type readiness_error =
  | Timeout
  | Exited_before_ready of Process_manager.result
[@@deriving sexp]

let pid t = Process_manager.pid t.process

let absolute _env path =
  if Filename.is_absolute path then path else Eio_posix.Low_level.realpath path
;;

let fallback_executable env =
  Filename.concat
    (Eio.Path.native_exn (Eio.Stdenv.cwd env))
    "_build/default/bin/ochat_agent_server.exe"
;;

let executable env =
  let candidate =
    Sys.getenv "OCHAT_E2E_SERVER_EXE"
    |> Option.value ~default:(fallback_executable env)
    |> absolute env
  in
  if Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / candidate)
  then candidate
  else
    raise_s [%sexp "ochat-agent-server executable is unavailable", (candidate : string)]
;;

let apply_environment_overrides environment overrides =
  let keys = overrides |> List.map ~f:fst |> String.Set.of_list in
  let inherited =
    Array.filter environment ~f:(fun entry ->
      match String.lsplit2 entry ~on:'=' with
      | Some (key, _) -> not (Set.mem keys key)
      | None -> true)
  in
  let overrides =
    overrides |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value) |> Array.of_list
  in
  Array.append inherited overrides
;;

let child_environment fixture cwd environment_overrides =
  let environment =
    Temporary_environment.child_environment
      (Config_fixture.environment fixture)
      ~base:(Core_unix.environment ())
  in
  let environment =
    match cwd with
    | None -> environment
    | Some cwd ->
      let pwd = Eio.Path.native_exn cwd in
      let environment =
        Array.filter environment ~f:(Fn.non (String.is_prefix ~prefix:"PWD="))
      in
      Array.append environment [| "PWD=" ^ pwd |]
  in
  apply_environment_overrides environment environment_overrides
;;

let spawn ~sw ~env ~fixture ?cwd ?(environment_overrides = []) arguments =
  Process_manager.spawn
    ~sw
    ~env
    ?cwd
    ~environment:(child_environment fixture cwd environment_overrides)
    ~max_output_bytes:(1024 * 1024)
    (executable env :: arguments)
;;

let run_cli ~sw ~env ~fixture ~arguments =
  spawn ~sw ~env ~fixture arguments |> Process_manager.await
;;

let start ~sw ~env ~fixture ~config_path =
  { process = spawn ~sw ~env ~fixture [ "-config"; config_path ]; fixture }
;;

let start_with_environment_overrides ~sw ~env ~fixture ~environment_overrides ~config_path
  =
  { process = spawn ~sw ~env ~fixture ~environment_overrides [ "-config"; config_path ]
  ; fixture
  }
;;

let start_in_directory ~sw ~env ~fixture ~cwd ~config_path =
  { process = spawn ~sw ~env ~fixture ~cwd [ "-config"; config_path ]; fixture }
;;

let start_in_directory_with_environment_overrides
      ~sw
      ~env
      ~fixture
      ~cwd
      ~environment_overrides
      ~config_path
  =
  { process =
      spawn ~sw ~env ~fixture ~cwd ~environment_overrides [ "-config"; config_path ]
  ; fixture
  }
;;

let health t ~env ~token =
  Http_probe.get_health env ~port:(Config_fixture.http_port t.fixture) ~token
;;

let rec wait_loop t env =
  match Process_manager.poll_result t.process with
  | Some result -> Error (Exited_before_ready result)
  | None ->
    (match health t ~env ~token:(Config_fixture.admin_token t.fixture) with
     | Ok response when response.ready -> Ok response
     | Ok _ | Error _ ->
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
       wait_loop t env)
;;

let wait_ready t ~env ~timeout_seconds =
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) timeout_seconds (fun () ->
      Ok (wait_loop t env))
  with
  | Ok result -> result
  | Error `Timeout -> Error Timeout
;;

let stop t ~env ~grace_seconds =
  Process_manager.terminate t.process ~clock:(Eio.Stdenv.clock env) ~grace_seconds
;;

let result t = Process_manager.poll_result t.process
let signal t signal = Process_manager.signal t.process signal
let stdout t = Process_manager.stdout t.process
let stderr t = Process_manager.stderr t.process
