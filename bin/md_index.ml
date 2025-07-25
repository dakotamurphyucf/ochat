(** Build a persistent *vector index* out of a directory tree of Markdown
    files.

    The binary is installed as [md-index] (and aliased by the umbrella
    executable [ochat md-index]).  It is a *thin wrapper* around
    {!Markdown_indexer.index_directory} that adds a command-line interface
    and a few convenience checks.

    {1 Overview}

    1. Validate and parse CLI flags (see {!cmdline}).
    2. Ensure the output directory exists (created recursively with
       {!Eio.Path.mkdirs}).
    3. Delegate the heavy lifting to {!Markdown_indexer.index_directory},
       which slices the Markdown files into overlapping windows, fetches
       OpenAI embeddings and persists them with {!Vector_db}.

    All blocking I/O is performed through Eio so the program remains fully
    cooperative and cancellation-safe.  A call to
    {!Mirage_crypto_rng_unix.use_default} initialises the CSPRNG required
    by HTTPS requests.

    {1:cmdline Command-line flags}

    [md-index] understands the following options (all in "GNU style"):

    {ul
      {- {{!val-root_dir} [--root PATH]} – *mandatory*.  Top-level
         directory that will be crawled recursively.}
      {- {{!val-index_name} [--name NAME]} – Logical identifier of the
         index (default ["docs"]).  Acts as the sub-folder name under
         [--out] and as the primary key in {!Md_index_catalog}.}
      {- {{!val-description} [--desc TEXT]} – One-line description shown
         by UIs (default "Markdown documentation index").}
      {- {{!val-out_dir} [--out DIR]} – Directory that stores *all* vector
         databases (default [".md_index"]).  The actual index lives in
         [DIR/NAME].}}

    {1 Exit codes}

    * **0** – Success.
    * **1** – Invalid or missing arguments.
*)

open Core
open Eio

(* -------------------------------------------------------------------------- *)
(* Implementation                                                               *)
(* -------------------------------------------------------------------------- *)

(** Value populated by [--root].  Must point to an existing directory.
    An empty string means the flag was omitted. *)
let root_dir = ref ""

(** Value populated by [--name]. *)
let index_name = ref "docs"

(** Value populated by [--desc]. *)
let description = ref "Markdown documentation index"

(** Parent directory for all indexes (from [--out]). *)
let out_dir = ref "./.md_index"

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

(** Entry point executed by {!Eio_main.run}.

    The steps are:

    1. Seed the random number generator used by HTTPS (required by
       [cohttp-eio] and therefore {!Embed_service}).
    2. Parse the CLI; on missing [--root] print usage and exit with code
       1.
    3. Convert the user-provided strings into *capability-style* paths
       relative to {{!Eio.Stdenv.fs} [env#fs]}.
    4. Ensure the output directory hierarchy exists (similar to
       [mkdir -p]).
    5. Delegate to {!Markdown_indexer.index_directory}. *)
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

let () =
  (* Run the {!main} fibre in the default runtime. *)
  Eio_main.run main
;;
