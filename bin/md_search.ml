(** Semantic search over Markdown snippet indexes (CLI).

    The [md-search] executable queries one or more vector indexes
    produced by {!md-index}.  For a natural-language input it:

    1. Creates an OpenAI *embedding* for the query string.
    2. Selects candidate snippet indexes (either the one passed with
       {!--index} or – when the special value {!all} is used – the
       five indexes whose *centroid* is closest to the query).
    3. Loads the vectors of every snippet contained in the selected
       indexes and builds an in-memory {!Vector_db} corpus.
    4. Performs cosine-similarity search and prints up to [k] Markdown
       previews to [stdout].

    The program is Unix-only (depends on {!Eio_main}).  All blocking
    effects – network access, filesystem reads – run inside an Eio
    fibre so the binary remains responsive even on slow I/O.

    {1  Command-line interface}

    Usage: [md-search --query TEXT [--index NAME|all] [--index-dir DIR] [-k INT]]

    The executable accepts the following flags:

    {v
      --query <text>     Natural-language search query (mandatory)
      --index <name|all> Index to search.  Use [all] to auto-select the
                         five closest ones (default: all).
      --index-dir <dir>  Directory that contains the Markdown indexes
                         as sub-folders (default: .md_index)
      -k <int>           Maximum number of snippets to display (default: 5)
    v}

    Exit code is [0] on success, non-zero on errors (invalid arguments,
    missing indexes, network failures, …).
*)

open Core
open Eio
open Owl

let query = ref ""
let index_name = ref "all"
let index_dir = ref ".md_index"
let k = ref 5

let speclist =
  [ "--query", Arg.Set_string query, "Natural-language search query (required)"
  ; "--index", Arg.Set_string index_name, "Index name to query (default: all)"
  ; ( "--index-dir"
    , Arg.Set_string index_dir
    , "Directory holding indices (default: .md_index)" )
  ; "-k", Arg.Set_int k, "Maximum number of results (default: 5)"
  ]
;;

let usage = "md-search --query <text> [--index name|all] [--index-dir dir] [-k 5]"

(** [mat_of_array arr] returns a column matrix view of the one-dimensional
    float array [arr].  The resulting value has shape *n x 1* where
    *n* is [Array.length arr].  No copy is performed – the matrix shares
    memory with the array. *)
let mat_of_array arr = Mat.of_array arr (Array.length arr) 1

(** [dot_product a b] computes the Euclidean dot product of two float arrays
    assumed to have identical length.  Complexity is O(n) where [n] is the
    vector dimension. *)
let dot_product a b =
  let acc = ref 0. in
  for i = 0 to Array.length a - 1 do
    acc := !acc +. (a.(i) *. b.(i))
  done;
  !acc
;;

(** Program entry-point – do **not** call directly.

    [main env] runs the CLI inside an {!Eio_main.run} context and exits
    the process when finished.  All side-effects live inside the given
    Eio environment [env]. *)
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
  (* 2. Determine candidate indexes *)
  let base_dir = Path.(env#fs / !index_dir) in
  let candidate_indexes =
    if String.( = ) !index_name "all"
    then (
      match Md_index_catalog.load ~dir:base_dir with
      | Some catalog ->
        (* rank by dot product with centroid vector *)
        catalog
        |> Array.to_list
        |> List.map ~f:(fun ({ Md_index_catalog.Entry.name; vector; _ } as _e) ->
          dot_product query_vec vector, name)
        |> List.sort ~compare:(fun (s1, _) (s2, _) -> Float.compare s2 s1)
        |> (fun l -> List.take l 5)
        |> List.map ~f:snd
      | None ->
        (* fallback: list all subdirs *)
        List.filter (Eio.Path.read_dir base_dir) ~f:(fun d ->
          try Eio.Path.is_directory Eio.Path.(base_dir / d) with
          | _ -> false))
    else [ !index_name ]
  in
  if List.is_empty candidate_indexes
  then (
    Printf.printf "No candidate indexes found under %s\n" !index_dir;
    exit 0);
  (* 3. Aggregate vectors from all selected indexes *)
  let vecs_with_idx : (string * Vector_db.Vec.t) list =
    List.concat_map candidate_indexes ~f:(fun idx_name ->
      let idx_dir = Eio.Path.(base_dir / idx_name) in
      if Path.is_directory idx_dir
      then (
        let vec_path = Eio.Path.(idx_dir / "vectors.binio") in
        match
          Result.try_with (fun () -> Vector_db.Vec.read_vectors_from_disk vec_path)
        with
        | Ok vecs -> Array.to_list vecs |> List.map ~f:(fun v -> idx_name, v)
        | Error _ -> [])
      else [])
  in
  if List.is_empty vecs_with_idx
  then (
    Printf.printf "No vectors found in selected indexes.\n";
    exit 0);
  let only_vecs = Array.of_list (List.map vecs_with_idx ~f:snd) in
  let db = Vector_db.create_corpus only_vecs in
  let idxs = Vector_db.query db query_mat !k in
  let results =
    idxs
    |> Array.to_list
    |> List.mapi ~f:(fun rank idx ->
      let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
      (* find which index has this id *)
      let idx_name =
        List.find_map_exn vecs_with_idx ~f:(fun (n, v) ->
          if String.equal v.Vector_db.Vec.id id then Some n else None)
      in
      rank + 1, idx_name, id)
  in
  (* 4. Display snippets *)
  List.iter results ~f:(fun (rank, idx_name, id) ->
    let text =
      try Io.load_doc ~dir:Path.(base_dir / idx_name / "snippets") (id ^ ".md") with
      | _ -> "(snippet file missing)"
    in
    let preview_len = 8000 in
    let preview =
      if String.length text > preview_len
      then String.sub text ~pos:0 ~len:preview_len ^ " …"
      else text
    in
    Printf.printf "[%d] [%s] %s\n%s\n\n---\n\n" rank idx_name id preview);
  ()
;;

let () = Eio_main.run main
