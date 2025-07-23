open Core
open Eio
open Owl

(* -------------------------------------------------------------------------- *)
(* CLI for semantic search over Markdown indices                                *)
(* -------------------------------------------------------------------------- *)

let query = ref ""
let index_name = ref "all"
let index_dir = ref ".md_index"
let k = ref 5

let speclist =
  [ "--query", Arg.Set_string query, "Natural-language search query (required)";
    "--index", Arg.Set_string index_name, "Index name to query (default: all)";
    "--index-dir", Arg.Set_string index_dir, "Directory holding indices (default: .md_index)";
    "-k", Arg.Set_int k, "Maximum number of results (default: 5)" ]

let usage =
  "md-search --query <text> [--index name|all] [--index-dir dir] [-k 5]"

let mat_of_array arr = Mat.of_array arr (Array.length arr) 1

let dot_product a b =
  let acc = ref 0. in
  for i = 0 to Array.length a - 1 do
    acc := !acc +. (a.(i) *. b.(i))
  done;
  !acc

let main env =
  Mirage_crypto_rng_unix.use_default ();
  Arg.parse speclist (fun _ -> ()) usage;

  if String.is_empty !query then (
    Printf.eprintf "%s\n" usage;
    exit 1);

  (* 1. Embed query *)
  let embed_resp = Openai.Embeddings.post_openai_embeddings env#net ~input:[ !query ] in
  let query_vec = Array.of_list (List.hd_exn embed_resp.data).embedding in
  let query_mat = mat_of_array query_vec in

  (* 2. Determine candidate indexes *)
  let base_dir = Path.(env#fs / !index_dir) in

  let candidate_indexes =
    if String.( = ) !index_name "all" then (
      match Md_index_catalog.load ~dir:base_dir with
      | Some catalog ->
        (* rank by dot product with centroid vector *)
        catalog
        |> Array.to_list
        |> List.map ~f:(fun ({ Md_index_catalog.Entry.name; vector; _ } as _e) ->
               (dot_product query_vec vector, name))
        |> List.sort ~compare:(fun (s1, _) (s2, _) -> Float.compare s2 s1)
        |> (fun l -> List.take l 5)
        |> List.map ~f:snd
      | None ->
        (* fallback: list all subdirs *)
        List.filter (Eio.Path.read_dir base_dir) ~f:(fun d ->
            try Eio.Path.is_directory Eio.Path.(base_dir / d) with _ -> false)
    ) else [ !index_name ]
  in

  if List.is_empty candidate_indexes then (
    Printf.printf "No candidate indexes found under %s\n" !index_dir;
    exit 0);

  (* 3. Aggregate vectors from all selected indexes *)
  let vecs_with_idx : (string * Vector_db.Vec.t) list =
    List.concat_map candidate_indexes ~f:(fun idx_name ->
        let idx_dir = Eio.Path.(base_dir / idx_name) in
        if Path.is_directory idx_dir then (
          let vec_path = Eio.Path.(idx_dir / "vectors.binio") in
          match Result.try_with (fun () -> Vector_db.Vec.read_vectors_from_disk vec_path) with
          | Ok vecs -> Array.to_list vecs |> List.map ~f:(fun v -> (idx_name, v))
          | Error _ -> []
        ) else [] )
  in

  if List.is_empty vecs_with_idx then (
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
        (rank + 1, idx_name, id))
  in

  (* 4. Display snippets *)
  List.iter results ~f:(fun (rank, idx_name, id) ->
      let text =
        try Io.load_doc ~dir:Path.(base_dir / idx_name / "snippets") (id ^ ".md")
        with _ -> "(snippet file missing)"
      in
      let preview_len = 8000 in
      let preview = if String.length text > preview_len then String.sub text ~pos:0 ~len:preview_len ^ " …" else text in
      Printf.printf "[%d] [%s] %s\n%s\n\n---\n\n" rank idx_name id preview);

  ()

let () = Eio_main.run main

