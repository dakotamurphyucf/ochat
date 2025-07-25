(** Odoc-index – command-line tool to build a local search corpus from
    ODoc-generated HTML documentation.

    Given the root directory produced by
    {ul
    {- [dune build @doc] or [odig odoc] (commonly
       [$ODOC_ROOT/default/var/cache/odig/html])}}
    the program traverses every first-level package directory, slices the
    HTML files into small Markdown snippets and turns them into:

    • dense vector embeddings (for nearest-neighbour search)
    • a BM-25 lexical index
    • raw Markdown files (one per snippet)

    All heavy-lifting is delegated to {!Odoc_indexer.index_packages}.  This
    wrapper only parses a minimal set of CLI flags and orchestrates the
    call.

    Invocation pattern:
    {[ odoc-index --root <html-doc-root> [--out <output-dir>] ]}

    Both [--root] and [--out] accept relative or absolute paths.  The
    default output directory is [.odoc_index].  The command aborts with a
    non-zero exit status if [--root] is missing or does not exist.
*)

open Core
open Eio

(* -------------------------------------------------------------------------- *)
(* Command-line flags *)
(* -------------------------------------------------------------------------- *)

let root_dir : string ref = ref ""
let out_dir : string ref = ref ".odoc_index"

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
    ~filter:
      (Odoc_indexer.Update
         ( Exclude
             [ "ocaml"
             ; "ocaml_intrinsics_kernel"
             ; "ocaml-compiler-libs"
             ; "ocamlgraph"
             ; "tls"
             ]
         , [ "ochat" ] ))
    ~env
    ~root:root_path
    ~output:out_path
    ~net:env#net
    ();
  Printf.printf "Indexing completed in %s\n%!" !out_dir
;;

let () = Eio_main.run main
