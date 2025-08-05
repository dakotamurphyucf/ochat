let () = Mirage_crypto_rng_unix.use_default ()
let info = Irmin_git_unix.info

let path =
  if Array.length Sys.argv = 2
  then Sys.argv.(1)
  else "git://github.com/dakotamurphyucf/ochat.git"
;;

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync.Make (Store)

let root = "/tmp/irmin/test"

let init () =
  let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
  Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook
;;

let test () =
  init ();
  let config = Irmin_git.config root in
  let repo = Store.Repo.v config in
  let t = Store.of_branch repo "main" in
  let upstream = Store.remote path () in
  let _ = Sync.pull t upstream `Set in
  let readme = Store.get t [ "Readme.md" ] in
  let tree = Store.get_tree t [] in
  let tree = Store.Tree.add tree [ "BAR.md" ] "Hoho!" in
  let tree = Store.Tree.add tree [ "FOO.md" ] "Hihi!" in
  Store.set_tree_exn t ~info:(info "merge") [] tree;
  Printf.printf "%s\n%!" readme
;;

let () =
  Eio_main.run @@ fun env -> Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> test ()
;;
