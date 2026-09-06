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

let snapshot_path ~env id = Eio.Path.(Session_store.ensure_dir ~env id / "snapshot.bin")

let reasoning id =
  Openai.Responses.Item.Reasoning { summary = []; _type = "reasoning"; id; status = None }
;;

let%expect_test "production store loads V0 through V3 fixtures" =
  with_temp_home
  @@ fun env ->
  let history = [ reasoning "same"; reasoning "same" ] in
  let write module_ id value =
    Bin_prot_utils_eio.write_bin_prot module_ (snapshot_path ~env id) value
  in
  write
    (module Session.Legacy.V0)
    "v0"
    Session.Legacy.V0.
      { id = "v0"
      ; prompt_file = "prompt"
      ; history
      ; tasks = []
      ; kv_store = []
      ; vfs_root = "vfs"
      };
  write
    (module Session.Legacy.V1)
    "v1"
    Session.Legacy.V1.
      { version = 1
      ; id = "v1"
      ; prompt_file = "prompt"
      ; history
      ; tasks = []
      ; kv_store = []
      ; vfs_root = "vfs"
      };
  write
    (module Session.Legacy.V2)
    "v2"
    Session.Legacy.V2.
      { version = 2
      ; id = "v2"
      ; prompt_file = "prompt"
      ; local_prompt_copy = None
      ; history
      ; tasks = []
      ; kv_store = []
      ; vfs_root = "vfs"
      };
  write
    (module Session.Legacy.V3)
    "v3"
    Session.Legacy.V3.
      { version = 3
      ; id = "v3"
      ; prompt_file = "prompt"
      ; local_prompt_copy = None
      ; history
      ; tasks = []
      ; moderator_snapshot = None
      ; kv_store = []
      ; vfs_root = "vfs"
      };
  let loaded =
    List.map [ "v0"; "v1"; "v2"; "v3" ] ~f:(fun id ->
      Session_store.load_or_create ~env ~prompt_file:"unused" ~id ())
  in
  print_s
    [%sexp
      (List.map loaded ~f:(fun session ->
         session.Session.id, List.length session.history, session.next_history_sequence)
       : (string * int * int) list)];
  [%expect {| ((v0 2 2) (v1 2 2) (v2 2 2) (v3 2 2)) |}]
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
  let entry = History_entry.create ~allocator (reasoning "r") |> Result.ok_or_failwith in
  let session =
    Session.create
      ~id:"reset"
      ~prompt_file:"prompt"
      ~history:[ entry ]
      ~next_history_sequence:(History_entry.Allocator.next_sequence allocator)
      ()
  in
  Session_store.save_exn ~env session;
  let silence f =
    let previous = Caml_unix.dup Caml_unix.stdout in
    let sink = Caml_unix.openfile "/dev/null" [ Caml_unix.O_WRONLY ] 0o600 in
    Caml_unix.dup2 sink Caml_unix.stdout;
    Exn.protect ~f ~finally:(fun () ->
      Caml_unix.dup2 previous Caml_unix.stdout;
      Caml_unix.close sink;
      Caml_unix.close previous)
  in
  silence (fun () -> Session_store.reset_session ~env ~id:"reset" ~keep_history:true ());
  let retained = Session_store.load_or_create ~env ~prompt_file:"unused" ~id:"reset" () in
  silence (fun () -> Session_store.reset_session ~env ~id:"reset" ~keep_history:false ());
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
