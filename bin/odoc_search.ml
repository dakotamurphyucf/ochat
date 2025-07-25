(** Semantic search over an on-disk ODoc vector index.

    {b odoc-search} is a small command-line tool shipping with the Ochat
    tooling.  It turns a free-form natural-language query into relevant OCaml
    API fragments by combining dense vector search with a classical lexical
    BM25 score.

    The executable expects the documentation of each OPAM package to be
    pre-processed by {{!page:Odoc_index odoc-index}} into a directory layout
    such as:

    {v
    .odoc_index/
      ├─ core/
      │   ├─ vectors.binio       (* dense embeddings *)
      │   ├─ bm25.binio          (* BM25 inverted index *)
      │   └─ <doc-id>.md         (* rendered snippet bodies *)
      ├─ eio/
      └─ ...
    v}

    Each call to [odoc-search] performs the following high-level steps:

    1. The query string is embedded through the OpenAI embeddings API and
       L₂-normalised (via the {{!module:Owl}Owl} linear-algebra library).
    2. A coarse package filter is applied: if [--package] is not supplied the
       {!module:Package_index} coarse index is queried to keep the most
       similar packages, reducing the amount of data loaded in step&nbsp;3.
    3. For every selected package the dense document embeddings are loaded
       from [vectors.binio] and concatenated into one big in-memory corpus.
    4. {!Vector_db.query} or, when BM25 data is present,
       {!Vector_db.query_hybrid} is run to retrieve the top-[k] candidates
       (cosine similarity linearly interpolated with BM25; see [--beta]).
    5. Results are printed to stdout in a simple Markdown format consisting of
       the package name, document identifier and full snippet body.

    A typical invocation searches the entire index for the best 10 matches:

    {[
      odoc-search --query "generate random uuid" -k 10
    ]}

    {1 Command-line flags}

    • [--query STRING] – mandatory search text.
    • [--package PKG]  – restrict search to a single OPAM package.
    • [--index DIR]    – root directory of the index (default [.odoc_index]).
    • [-k INT]         – maximum number of hits to return (default 5).
    • [--beta FLOAT]   – interpolation factor between cosine and BM25
                         (0 ⇒ dense only; 1 ⇒ lexical only; default 0.25).

    {1 Limitations}

    * The process keeps the whole embedding matrix in RAM; huge corpora may
      exceed typical workstation memory.
    * A valid [OPENAI_API_KEY] must be present in the environment so that
      embeddings can be generated.
    * The tool does not build the index itself – run [odoc-index] first.
*)

open Core
open Eio
open Owl

let query = ref ""
let package = ref ""
let index_dir = ref ".odoc_index"
let k = ref 5
let beta = ref 0.25

let speclist =
  [ "--query", Arg.Set_string query, "Query string"
  ; "--package", Arg.Set_string package, "Package to search (optional)"
  ; "--index", Arg.Set_string index_dir, "Index directory (default .odoc_index)"
  ; "-k", Arg.Set_int k, "Number of results per package (default 5)"
  ; "--beta", Arg.Set_float beta, "Hybrid weight 0-1 (default 0.25)"
  ]
;;

let usage =
  "odoc-search --query <text> [--package pkg] [--index dir] [-k 5] [--beta 0.25]"
;;

(** [mat_of_array arr] converts a float [arr] to a column matrix whose number
    of rows equals [Array.length arr].  The resulting matrix is compatible
    with {!Vector_db.query} which expects an [n × 1] embedding. *)
let mat_of_array arr = Mat.of_array arr (Array.length arr) 1

(** Internal helper kept for backwards-compatibility with an older
    implementation.  It loaded a single package on demand and performed query
    time hybrid retrieval directly.  Currently unused but retained because
    external scripts may still link against it. *)

let _load_package pkg_dir pkg query_mat =
  let vec_file = Path.(pkg_dir / "vectors.binio") in
  let bm25_file = Path.(pkg_dir / "bm25.binio") in
  match
    ( Result.try_with (fun () -> Vector_db.Vec.read_vectors_from_disk vec_file)
    , Result.try_with (fun () -> Bm25.read_from_disk bm25_file) )
  with
  | Ok vecs, Ok bm25 ->
    let db = Vector_db.create_corpus vecs in
    let idxs =
      Vector_db.query_hybrid db ~bm25 ~beta:!beta ~embedding:query_mat ~text:!query ~k:!k
    in
    idxs
    |> Array.to_list
    |> List.map ~f:(fun idx ->
      let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
      let text = Io.load_doc ~dir:pkg_dir (id ^ ".md") in
      pkg, id, text)
  | _ -> []
;;

(** Load the dense document embeddings stored in [pkg_dir/vectors.binio].
    
    Returns [[].] when the file is missing or cannot be deserialised. *)

let load_package_vectors pkg_dir =
  let vec_file = Path.(pkg_dir / "vectors.binio") in
  match Result.try_with (fun () -> Vector_db.Vec.read_vectors_from_disk vec_file) with
  | Ok vecs -> Array.to_list vecs
  | _ -> []
;;

(** [get_results vecs query_mat index_path pkgs] runs a pure dense retrieval
    against the concatenated [vecs] corpus and resolves each document id to
    its snippet markdown, returning [(package, id, body)] triples.
    
    The function relies on the fact that document identifiers are globally
    unique across packages – a property guaranteed by {!odoc_index}. *)

let get_results vecs query_mat index_path pkgs =
  let db = Vector_db.create_corpus vecs in
  let idxs = Vector_db.query db query_mat !k in
  idxs
  |> Array.to_list
  |> List.map ~f:(fun idx ->
    let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
    let pkg, text =
      List.hd_exn
      @@ Eio.Fiber.List.filter_map
           (fun pkg ->
              let pkg_dir = Path.(index_path / pkg) in
              if Path.is_file Path.(pkg_dir / (id ^ ".md"))
              then Some (pkg, Io.load_doc ~dir:pkg_dir (id ^ ".md"))
              else None)
           pkgs
    in
    pkg, id, text)
;;

(** Entrypoint wired into {!Eio_main.run}.
    
    [env] is the root Eio environment in which the event-loop is running.  The
    function never returns – it terminates the process via {!Stdlib.exit} when
    argument parsing fails. *)

let main env =
  Mirage_crypto_rng_unix.use_default ();
  Arg.parse speclist (fun _ -> ()) usage;
  if String.is_empty !query
  then (
    Printf.eprintf "%s\n" usage;
    exit 1);
  (* 1. Embed query *)
  let embed_resp = Openai.Embeddings.post_openai_embeddings env#net ~input:[ !query ] in
  let query_vec = Array.of_list (List.hd_exn embed_resp.data).embedding in
  let query_mat = mat_of_array query_vec in
  let index_path = Path.(env#fs / !index_dir) in
  let pkgs =
    if String.( = ) !package ""
    then (
      match Package_index.load ~dir:index_path with
      | Some idx ->
        let cand = Package_index.query idx ~embedding:query_vec ~k:5 in
        if List.is_empty cand then Path.read_dir index_path else cand
      | None -> Path.read_dir index_path)
    else [ !package ]
  in
  let vectors =
    Array.of_list
    @@ List.concat
    @@ Eio.Fiber.List.map
         (fun pkg ->
            let pkg_dir = Path.(index_path / pkg) in
            if Path.is_directory pkg_dir then load_package_vectors pkg_dir else [])
         pkgs
  in
  (* 2. Search each package *)
  let results =
    if Array.is_empty vectors
    then (
      Printf.printf
        "No vectors found in index directory: %s\n"
        (Path.native_exn index_path);
      [])
    else get_results vectors query_mat index_path pkgs
  in
  (* List.concat
    @@ Eio.Fiber.List.map
         (fun pkg ->
            let pkg_dir = Path.(index_path / pkg) in
            if Path.is_directory pkg_dir then load_package pkg_dir pkg query_mat else [])
         pkgs
  in *)
  (* 3. Print *)
  List.iteri results ~f:(fun i (pkg, id, text) ->
    let preview = text in
    Printf.printf "[%d] [%s] %s:\n%s\n\n\n---\n\n\n" (i + 1) pkg id preview);
  ()
;;

let () = Eio_main.run main
