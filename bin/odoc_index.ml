open Core
open Eio

let root_dir = ref ""
let out_dir = ref ".odoc_index"

let speclist =
  [ ( "--root"
    , Arg.Set_string root_dir
    , "Root directory ($ODOC_ROOT/default/var/cache/odig/html)" )
  ; "--out", Arg.Set_string out_dir, "Output directory for index (default .odoc_index)"
  ]
;;

let usage = "odoc-index --root <path> [--out <dir>]"

let main env =
  Mirage_crypto_rng_unix.use_default ();
  Arg.parse speclist (fun _ -> ()) usage;
  if String.is_empty !root_dir
  then (
    Printf.eprintf "%s\n" usage;
    exit 1);
  let root_path = Path.(env#fs / !root_dir) in
  let out_path = Path.(env#fs / !out_dir) in
  (match Path.is_directory out_path with
   | true -> ()
   | false -> Path.mkdirs ~perm:0o700 out_path);
  Odoc_indexer.index_packages
    ~skip_pkgs:
      [ "ocaml"; "ocaml_intrinsics_kernel"; "ocaml-compiler-libs"; "ocamlgraph"; "tls" ]
    ~env
    ~root:root_path
    ~output:out_path
    ~net:env#net
    ();
  Printf.printf "Indexing completed in %s\n%!" !out_dir
;;

let () = Eio_main.run main
