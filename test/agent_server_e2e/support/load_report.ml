open Core
module F = Background_fixture

type t =
  { path : Eio.Fs.dir_ty Eio.Path.t
  ; pending : Eio.Fs.dir_ty Eio.Path.t
  ; started : float
  ; mutex : Eio.Mutex.t
  ; mutable samples : Jsonaf.t list
  }

let workspace env =
  match Sys.getenv "DUNE_SOURCEROOT" with
  | Some path -> Eio.Path.(Eio.Stdenv.fs env / path)
  | None -> Eio.Stdenv.cwd env
;;

let report_root env =
  match Sys.getenv "OCHAT_E2E_REPORT_ROOT" with
  | Some path ->
    F.require (Filename.is_absolute path) "report root must be absolute";
    Eio.Path.(Eio.Stdenv.fs env / path)
  | None -> Eio.Path.(workspace env / "_build" / "agent-e2e-reports")
;;

let create env name =
  let root = report_root env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  let id =
    Agent_protocol.Id.Operation.create () |> Agent_protocol.Id.Operation.to_string
  in
  { path = Eio.Path.(root / (name ^ "-" ^ id ^ ".json"))
  ; pending = Eio.Path.(root / (name ^ "-" ^ id ^ ".next"))
  ; started = Load_fixture.now env
  ; mutex = Eio.Mutex.create ()
  ; samples = []
  }
;;

let save t status =
  let contents =
    Jsonaf.to_string
      (`Object
          [ "status", `String status
          ; "started_at", `Number (Float.to_string t.started)
          ; "samples", `Array (List.rev t.samples)
          ])
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) t.pending (contents ^ "\n");
  Eio.Path.rename t.pending t.path
;;

let record t env label fields =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.samples
    <- `Object
         ([ "label", `String label
          ; ( "elapsed_seconds"
            , `Number (Float.to_string (Load_fixture.now env -. t.started)) )
          ]
          @ fields)
       :: t.samples;
    save t "running")
;;

let rec disk path =
  match Eio.Path.kind ~follow:false path with
  | `Directory ->
    Eio.Path.read_dir path
    |> List.fold ~init:0L ~f:(fun total name ->
      Int64.(total + disk Eio.Path.(path / name)))
  | `Regular_file -> (Eio.Path.stat ~follow:false path).size |> Optint.Int63.to_int64
  | _ -> 0L
;;

let disk_fields fixture =
  let root =
    Temporary_environment.path
      (Config_fixture.environment fixture)
      (Config_fixture.data_dir fixture)
  in
  let categories =
    Eio.Path.read_dir root
    |> List.map ~f:(fun name ->
      name, `Number (Int64.to_string (disk Eio.Path.(root / name))))
  in
  [ "disk_categories", `Object categories
  ; "store_bytes", `Number (Int64.to_string (disk root))
  ]
;;

let process_output env argv =
  Eio.Process.parse_out (Eio.Stdenv.process_mgr env) Eio.Buf_read.take_all argv
;;

let resources env daemon =
  let pid = Int.to_string (Daemon_process.pid daemon) in
  let rss =
    process_output env [ "/bin/ps"; "-o"; "rss="; "-p"; pid ]
    |> String.strip
    |> Int.of_string
  in
  let descriptors =
    process_output env [ "/usr/sbin/lsof"; "-a"; "-p"; pid; "-Ff" ]
    |> String.split_lines
    |> List.count ~f:(fun line ->
      String.is_prefix line ~prefix:"f"
      && Option.is_some (Int.of_string_opt (String.drop_prefix line 1)))
  in
  rss, descriptors
;;

let sample t env fixture daemon client label fields =
  let rss, descriptors = resources env daemon in
  let number value = `Number (Int.to_string value) in
  record
    t
    env
    label
    ([ "fixture_pid", number (Daemon_process.pid daemon)
     ; "rss_kib", number rss
     ; "descriptors", number descriptors
     ; "loaded_actors", number (Load_fixture.loaded client)
     ]
     @ disk_fields fixture
     @ fields);
  rss, descriptors
;;

let finish t =
  save t "passed";
  Eio.Path.native_exn t.path
;;

let fail t = save t "failed"
