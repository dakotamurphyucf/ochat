open Core

(* -------------------------------------------------------------------------- *)
(*  Session persistence – round-trip and legacy upgrade tests                  *)
(* -------------------------------------------------------------------------- *)

let%expect_test "session round-trip save/load" =
  let tmp_root =
    Filename.concat
      Filename.temp_dir_name
      ("session_rt_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_root;
  (* Construct a fresh session value. *)
  let session = Session.create ~prompt_file:"prompt.chatmd" () in
  Eio_main.run
  @@ fun env ->
  let snapshot_file = Filename.concat tmp_root "snapshot.bin" in
  let snapshot_path = Eio.Path.(env#fs / snapshot_file) in
  (* Persist to disk, then load back. *)
  Session.Io.File.write snapshot_path session;
  let loaded = Session.Io.File.read snapshot_path in
  let same = Sexp.equal (Session.sexp_of_t session) (Session.sexp_of_t loaded) in
  print_s [%sexp (same : bool)];
  [%expect {| true |}]
;;

let%expect_test "legacy V0 → latest upgrade" =
  let legacy : Session.Legacy.V0.t =
    { id = "legacy"
    ; prompt_file = "prompt.chatmd"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let upgraded = Session.Legacy.upgrade_v0 legacy in
  let version_ok = Int.equal upgraded.version Session.current_version
  and id_ok = String.equal upgraded.id legacy.id in
  print_s [%sexp { version_ok : bool; id_ok : bool }];
  [%expect {| ((version_ok true) (id_ok true)) |}]
;;

let%expect_test "legacy V1 → latest upgrade" =
  let legacy : Session.Legacy.V1.t =
    { version = 1
    ; id = "legacy-v1"
    ; prompt_file = "prompt.chatmd"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let upgraded = Session.Legacy.upgrade_v1 legacy in
  let version_ok = Int.equal upgraded.version Session.current_version
  and id_ok = String.equal upgraded.id legacy.id
  and prompt_copy_none = Option.is_none upgraded.local_prompt_copy in
  print_s [%sexp { version_ok : bool; id_ok : bool; prompt_copy_none : bool }];
  [%expect {| ((version_ok true) (id_ok true) (prompt_copy_none true)) |}]
;;
