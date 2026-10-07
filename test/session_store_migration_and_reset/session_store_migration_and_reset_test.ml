open Core

let with_temp_home f =
  Eio_main.run (fun env ->
    let root =
      Eio.Process.parse_out
        (Eio.Stdenv.process_mgr env)
        Eio.Buf_read.take_all
        [ "mktemp"
        ; "-d"
        ; Filename.concat Filename.temp_dir_name "ochat-session-store.XXXXXX"
        ]
      |> String.strip
    in
    let previous = Sys.getenv "HOME" in
    Core_unix.putenv ~key:"HOME" ~data:root;
    Exn.protect
      ~f:(fun () -> f env)
      ~finally:(fun () ->
        (match previous with
         | Some data -> Core_unix.putenv ~key:"HOME" ~data
         | None -> Core_unix.unsetenv "HOME");
        Eio.Cancel.protect (fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))))
;;

let with_quiet_env env f =
  Eio.Switch.run (fun sw ->
    let source, sink = Eio_unix.pipe sw in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Flow.copy source (Eio.Flow.buffer_sink (Buffer.create 128)));
    let quiet =
      object
        method fs = env#fs
        method cwd = env#cwd
        method stdin = env#stdin
        method stdout = sink
        method stderr = sink
        method net = env#net
        method domain_mgr = env#domain_mgr
        method process_mgr = env#process_mgr
        method clock = env#clock
        method mono_clock = env#mono_clock
        method secure_random = env#secure_random
        method debug = env#debug
        method backend_id = env#backend_id
      end
    in
    Fun.protect (fun () -> f quiet) ~finally:(fun () -> Eio.Flow.close sink))
;;

let snapshot_path ~env id = Eio.Path.(Session_store.ensure_dir ~env id / "snapshot.bin")

let reasoning id =
  Openai.Responses.Item.Reasoning { summary = []; _type = "reasoning"; id; status = None }
;;

let%expect_test "production store preserves an unreadable snapshot" =
  with_temp_home
  @@ fun env ->
  let snapshot = snapshot_path ~env "corrupt" in
  let contents = "not-bin-prot" in
  Eio.Path.save ~create:(`Or_truncate 0o600) snapshot contents;
  let failed =
    match
      Or_error.try_with (fun () ->
        Session_store.load_or_create ~env ~prompt_file:"unused" ~id:"corrupt" ())
    with
    | Error _ -> true
    | Ok _ -> false
  in
  print_s
    [%sexp
      { failed : bool
      ; preserved = (String.equal contents (Eio.Path.load snapshot) : bool)
      }];
  [%expect {| ((failed true) (preserved true)) |}]
;;

let%expect_test "legacy save returns lock contention without exiting" =
  with_temp_home
  @@ fun env ->
  let session = Session.create ~id:"locked" ~prompt_file:"prompt" () in
  let directory = Session_store.ensure_dir ~env session.id in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(directory / "snapshot.bin.lock")
    "held";
  let failed = Result.is_error (Session_store.save ~env session) in
  print_s [%sexp { failed : bool; process_continued = (true : bool) }];
  [%expect {| ((failed true) (process_continued true)) |}]
;;

let%test_unit "store reset modes preserve the allocator watermark" =
  with_temp_home
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"reset" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let entry =
    Openai.Responses_history.create ~allocator (reasoning "r") |> Result.ok_or_failwith
  in
  let session =
    Session.create
      ~id:"reset"
      ~prompt_file:"prompt"
      ~history:[ entry ]
      ~next_history_sequence:(History_entry.Allocator.next_sequence allocator)
      ()
  in
  Session_store.save_exn ~env session;
  with_quiet_env env (fun env ->
    Session_store.reset_session ~env ~id:"reset" ~keep_history:true ());
  let retained = Session_store.load_or_create ~env ~prompt_file:"unused" ~id:"reset" () in
  with_quiet_env env (fun env ->
    Session_store.reset_session ~env ~id:"reset" ~keep_history:false ());
  let cleared = Session_store.load_or_create ~env ~prompt_file:"unused" ~id:"reset" () in
  [%test_eq: int] (List.length retained.history) 1;
  [%test_eq: int] retained.next_history_sequence 1;
  [%test_eq: int] (List.length cleared.history) 0;
  [%test_eq: int] cleared.next_history_sequence 1
;;

let%test_unit "save replaces the snapshot instead of truncating its inode" =
  with_temp_home
  @@ fun env ->
  let original = Session.create ~id:"atomic" ~prompt_file:"before" () in
  Session_store.save_exn ~env original;
  let path = snapshot_path ~env "atomic" in
  Eio.Path.with_open_in path (fun old_flow ->
    let old_bytes = Eio.Path.load path in
    Session_store.save_exn ~env { original with prompt_file = "after" };
    let still_old = Eio.Buf_read.(parse_exn take_all) old_flow ~max_size:1_000_000 in
    [%test_eq: string] still_old old_bytes);
  let loaded = Session_store.read_current_file path |> Or_error.ok_exn in
  [%test_eq: string] loaded.prompt_file "after";
  let files = Eio.Path.read_dir (Session_store.path ~env "atomic") in
  assert (not (List.exists files ~f:(String.is_suffix ~suffix:".tmp")))
;;

let%test_unit "failed snapshot rename cleans temporary and lock files" =
  with_temp_home
  @@ fun env ->
  let session = Session.create ~id:"rename-failure" ~prompt_file:"prompt" () in
  let dir = Session_store.ensure_dir ~env session.id in
  let destination = Eio.Path.(dir / "snapshot.bin") in
  Eio.Path.mkdir ~perm:0o700 destination;
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(destination / "keep") "preserved";
  assert (Result.is_error (Session_store.save ~env session));
  [%test_eq: string] (Eio.Path.load Eio.Path.(destination / "keep")) "preserved";
  [%test_eq: string list] (Eio.Path.read_dir dir) [ "snapshot.bin" ]
;;

let%expect_test "directory identity mismatch fails without redirecting a save" =
  with_temp_home
  @@ fun env ->
  let snapshot = snapshot_path ~env "requested" in
  Session.Io.File.write snapshot (Session.create ~id:"different" ~prompt_file:"prompt" ());
  let bytes = Eio.Path.load snapshot in
  let rejected =
    Or_error.try_with (fun () ->
      Session_store.load_or_create ~env ~prompt_file:"prompt" ~id:"requested" ())
    |> Result.is_error
  in
  print_s
    [%sexp
      { rejected : bool
      ; original_intact = (String.equal bytes (Eio.Path.load snapshot) : bool)
      ; no_other_directory =
          (not (Eio.Path.is_directory (Session_store.path ~env "different")) : bool)
      }];
  [%expect {| ((rejected true) (original_intact true) (no_other_directory true)) |}]
;;

let%expect_test "invalid authored values fail before making storage directories" =
  with_temp_home
  @@ fun env ->
  let invalid =
    { (Session.create ~id:"invalid" ~prompt_file:"prompt" ()) with
      next_history_sequence = -1
    }
  in
  let rejected = Session_store.save ~env invalid |> Result.is_error in
  print_s
    [%sexp
      { rejected : bool
      ; no_directory =
          (not (Eio.Path.is_directory (Session_store.path ~env "invalid")) : bool)
      }];
  [%expect {| ((rejected true) (no_directory true)) |}]
;;

let%expect_test "reset changes prompt through a new immutable copy" =
  with_temp_home
  @@ fun env ->
  let directory = Session_store.ensure_dir ~env "prompt-reset" in
  let old_prompt = Eio.Path.(directory / "prompt.chatmd") in
  let replacement = Eio.Path.(directory / "replacement.chatmd") in
  Eio.Path.save ~create:(`Exclusive 0o600) old_prompt "old prompt";
  Eio.Path.save ~create:(`Exclusive 0o600) replacement "new prompt";
  let session =
    Session.create
      ~id:"prompt-reset"
      ~prompt_file:"original"
      ~local_prompt_copy:"prompt.chatmd"
      ()
  in
  Session_store.save_exn ~env session;
  with_quiet_env env (fun env ->
    Session_store.reset_session
      ~env
      ~id:session.id
      ~prompt_file:(Eio.Path.native_exn replacement)
      ());
  let restored = Session_store.read_existing ~env ~id:session.id |> Option.value_exn in
  let copy = Option.value_exn restored.local_prompt_copy in
  print_s
    [%sexp
      { old_prompt_intact = (String.equal (Eio.Path.load old_prompt) "old prompt" : bool)
      ; different_copy = (not (String.equal copy "prompt.chatmd") : bool)
      ; new_prompt = (Eio.Path.load Eio.Path.(directory / copy) : string)
      }];
  [%expect
    {| ((old_prompt_intact true) (different_copy true) (new_prompt "new prompt")) |}]
;;

let%expect_test "unknown-bearing reset fails before archive or snapshot mutation" =
  with_temp_home
  @@ fun env ->
  let base =
    Session.create
      ~id:"future-reset"
      ~prompt_file:"prompt"
      ~moderator_snapshot:
        (Session.Moderator_snapshot.create ~script_id:"mod" ~script_source_hash:"hash" ())
      ()
  in
  let unwrap = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
  in
  let rec extend json = function
    | [] ->
      (match json with
       | `Object fields -> `Object (fields @ [ "future", `True ])
       | _ -> assert false)
    | name :: rest ->
      (match json with
       | `Object fields ->
         `Object
           (List.map fields ~f:(fun (key, value) ->
              key, if String.equal key name then extend value rest else value))
       | _ -> assert false)
  in
  let document =
    Session.Document.encode base
    |> unwrap
    |> Document_schema.Document.json
    |> fun json ->
    extend json [ "payload"; "moderator_state"; "legacy_snapshot" ]
    |> Document_schema.Document.inspect ~limits:Document_schema.Limits.default
    |> unwrap
  in
  let session = Session.Document.decode document |> unwrap in
  Session_store.save_exn ~env session;
  let directory = Session_store.path ~env session.id in
  let snapshot = Eio.Path.(directory / "snapshot.bin") in
  let before = Eio.Path.load snapshot in
  with_quiet_env env (fun env -> Session_store.reset_session ~env ~id:session.id ());
  print_s
    [%sexp
      { unchanged = (String.equal before (Eio.Path.load snapshot) : bool)
      ; no_archive = (not (Eio.Path.is_directory Eio.Path.(directory / "archive")) : bool)
      ; no_lock =
          (not (Eio.Path.is_file Eio.Path.(directory / "snapshot.bin.lock")) : bool)
      }];
  [%expect {| ((unchanged true) (no_archive true) (no_lock true)) |}]
;;
