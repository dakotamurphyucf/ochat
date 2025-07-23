open Core
open Eio

(* -------------------------------------------------------------------------- *)
(* CLI for building a Markdown vector index                                     *)
(* -------------------------------------------------------------------------- *)

let root_dir = ref ""
let index_name = ref "docs"
let description = ref "Markdown documentation index"
let out_dir = ref ".md_index"

let speclist =
  [ "--root", Arg.Set_string root_dir, "Root directory to crawl recursively"
  ; "--name", Arg.Set_string index_name, "Logical index name (default: docs)"
  ; ( "--desc"
    , Arg.Set_string description
    , "One-line description (default: Markdown documentation index)" )
  ; "--out", Arg.Set_string out_dir, "Output directory for vector DB (default: .md_index)"
  ]
;;

let usage = "md-index --root <path> [--name docs] [--desc text] [--out .md_index]"

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
  Markdown_indexer.index_directory
    ~vector_db_root:!out_dir
    ~env
    ~index_name:!index_name
    ~description:!description
    ~root:root_path;
  Printf.printf
    "Markdown indexing completed. Index name: %s – stored under %s\n%!"
    !index_name
    (Path.native_exn out_path)
;;

let () = Eio_main.run main
