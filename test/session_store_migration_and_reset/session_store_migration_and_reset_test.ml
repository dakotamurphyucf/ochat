open Core

let with_temp_home f =
  let root =
    Filename.concat
      Filename.temp_dir_name
      ("ochat-session-store-" ^ Int.to_string (Random.int 1_000_000))
  in
  let previous = Sys.getenv "HOME" in
  Core_unix.mkdir_p root;
  Core_unix.putenv ~key:"HOME" ~data:root;
  Exn.protect
    ~f:(fun () -> Eio_main.run f)
    ~finally:(fun () ->
      Core_unix.putenv ~key:"HOME" ~data:(Option.value previous ~default:""))
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
  Session_store.save ~env session;
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
