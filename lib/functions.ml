open Core
open Io

let add_line_numbers str =
  let lines = String.split_lines str in
  let numbered_lines =
    List.mapi ~f:(fun i line -> Printf.sprintf "%d. %s" (i + 1) line) lines
  in
  String.concat ~sep:"\n" numbered_lines
;;

let get_contents ~dir : Gpt_function.t =
  let f path =
    match Io.load_doc ~dir path with
    | res -> res
    | exception ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Get_contents) f
;;

let get_url_content ~net : Gpt_function.t =
  let f url =
    let host = Net.get_host url in
    let path = Net.get_path url in
    print_endline host;
    print_endline path;
    let headers = Http.Header.of_list [ "Accept", "*/*"; "Accept-Encoding", "gzip" ] in
    let res = Net.get Net.Default ~net ~host path ~headers in
    let decompressed = Option.value ~default:res @@ Result.ok (Ezgzip.decompress res) in
    let soup = Soup.parse decompressed in
    String.concat ~sep:"\n"
    @@ List.filter ~f:(fun s -> not @@ String.equal "" s)
    @@ List.map ~f:(fun s -> String.strip s)
    @@ Soup.texts soup
  in
  Gpt_function.create_function (module Definitions.Get_url_content) f
;;

let index_ocaml_code ~dir ~dm ~net : Gpt_function.t =
  let f (folder_to_index, vector_db_folder) =
    Eio.Switch.run
    @@ fun sw ->
    Indexer.index ~sw ~dir ~dm ~net ~vector_db_folder ~folder_to_index;
    "code has been indexed"
  in
  Gpt_function.create_function (module Definitions.Index_ocaml_code) f
;;

let query_vector_db ~dir ~net : Gpt_function.t =
  let f (vector_db_folder, query, num_results, index) =
    let vf = dir / vector_db_folder in
    let index =
      Option.value ~default:"" @@ Option.map ~f:(fun index -> "." ^ index) index
    in
    let file = String.concat [ "vectors"; index; ".binio" ] in
    let vec_file = String.concat [ vector_db_folder; "/"; file ] in
    let bm25_file = String.concat [ vector_db_folder; "/bm25"; index; ".binio" ] in
    let vecs = Vector_db.Vec.read_vectors_from_disk (dir / vec_file) in
    let corpus = Vector_db.create_corpus vecs in
    let bm25 =
      try Bm25.read_from_disk (dir / bm25_file) with
      | _ -> Bm25.create []
    in
    let response = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
    let query_vector =
      Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
      |> Owl.Mat.transpose
    in
    let top_indices =
      Vector_db.query_hybrid
        corpus
        ~bm25
        ~beta:0.4
        ~embedding:query_vector
        ~text:query
        ~k:num_results
    in
    let docs = Vector_db.get_docs vf corpus top_indices in
    let results =
      List.map ~f:(fun doc -> sprintf "\n**Result:**\n```ocaml\n%s\n```\n" doc) docs
    in
    String.concat ~sep:"\n" results
  in
  Gpt_function.create_function (module Definitions.Query_vector_db) f
;;

let apply_patch ~dir : Gpt_function.t =
  let split path =
    Eio.Path.split (dir / path)
    |> Option.map ~f:(fun ((_, dirname), basename) -> dirname, basename)
  in
  let f patch =
    let open_fn path = Io.load_doc ~dir path in
    let write_fn path s =
      match split path with
      | Some (dirname, basename) ->
        print_endline "dirname";
        print_endline dirname;
        print_endline path;
        print_endline basename;
        (match Io.is_dir ~dir dirname with
         | true -> Io.save_doc ~dir path s
         | false ->
           Io.mkdir ~exists_ok:true ~dir dirname;
           Io.save_doc ~dir path s)
      | None -> Io.save_doc ~dir path s
    in
    let remove_fn path = Io.delete_doc ~dir path in
    match Apply_patch.process_patch ~text:patch ~open_fn ~write_fn ~remove_fn with
    | _ -> sprintf "git patch successful"
    | exception ex -> Fmt.str "error running apply_patch: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Apply_patch) f
;;

let read_dir ~dir : Gpt_function.t =
  let f path =
    match Io.directory ~dir path with
    | res -> String.concat ~sep:"\n" res
    | exception ex -> Fmt.str "error running read_directory: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Read_directory) f
;;

let mkdir ~dir : Gpt_function.t =
  let f path =
    match Io.mkdir ~exists_ok:true ~dir path with
    | () -> sprintf "Directory %s created successfully." path
    | exception ex -> Fmt.str "error running mkdir: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Make_dir) f
;;

(* -------------------------------------------------------------------------- *)
(* ODoc search – vector-based snippet retrieval                                 *)
(* -------------------------------------------------------------------------- *)

let odoc_search ~dir ~net : Gpt_function.t =
  (*────────────────────────  Simple in-memory caches  ───────────────────────*)
  let module Odoc_cache = struct
    open Core

    module S = struct
      type t = string [@@deriving compare, hash, sexp]
    end

    let embed_tbl : (string, float array) Hashtbl.t = Hashtbl.create (module S)
    let vec_tbl : (string, Vector_db.Vec.t array) Hashtbl.t = Hashtbl.create (module S)
    let mu = Eio.Mutex.create ()

    let get_embed ~net query =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find embed_tbl query in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let resp = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
        let vec = Array.of_list (List.hd_exn resp.data).embedding in
        Eio.Mutex.lock mu;
        Hashtbl.set embed_tbl ~key:query ~data:vec;
        Eio.Mutex.unlock mu;
        vec
    ;;

    let get_vectors vec_file_path path_t =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find vec_tbl vec_file_path in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let vecs =
          try Vector_db.Vec.read_vectors_from_disk path_t with
          | _ -> [||]
        in
        Eio.Mutex.lock mu;
        Hashtbl.set vec_tbl ~key:vec_file_path ~data:vecs;
        Eio.Mutex.unlock mu;
        vecs
    ;;
  end
  in
  let f (query, k_opt, index_opt, package) =
    let open Eio.Path in
    let k = Option.value k_opt ~default:5 in
    let index_dir = Option.value index_opt ~default:".odoc_index" in
    (* 1. Embed the query (cached) *)
    let query_vec = Odoc_cache.get_embed ~net query in
    let query_mat = Owl.Mat.of_array query_vec (Array.length query_vec) 1 in
    let index_path = dir / index_dir in
    (* 2. Determine candidate packages *)
    let pkgs =
      if String.equal package "all"
      then (
        match Package_index.load ~dir:index_path with
        | Some idx ->
          (match Package_index.query idx ~embedding:query_vec ~k:5 with
           | l when List.is_empty l -> Eio.Path.read_dir index_path
           | l -> l)
        | None -> Eio.Path.read_dir index_path)
      else [ package ]
    in
    (* 3. Aggregate vectors from selected packages *)
    let vectors_for_pkg pkg =
      let pkg_dir = index_path / pkg in
      if Eio.Path.is_directory pkg_dir
      then (
        let vec_path = pkg_dir / "vectors.binio" in
        let vec_key = Eio.Path.native_exn vec_path in
        let vecs = Odoc_cache.get_vectors vec_key vec_path in
        Array.to_list vecs |> List.map ~f:(fun v -> pkg, v))
      else []
    in
    let vecs_with_pkg = List.concat_map pkgs ~f:vectors_for_pkg in
    if List.is_empty vecs_with_pkg
    then Printf.sprintf "No vectors found in index directory %s" index_dir
    else (
      let only_vecs = Array.of_list (List.map vecs_with_pkg ~f:snd) in
      let db = Vector_db.create_corpus only_vecs in
      let idxs = Vector_db.query db query_mat k in
      (* 4. Fetch snippets *)
      let results =
        Array.to_list idxs
        |> List.mapi ~f:(fun rank idx ->
          let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
          (* find which package contains this id *)
          let pkg_opt =
            List.find_map vecs_with_pkg ~f:(fun (pkg, v) ->
              if String.equal v.Vector_db.Vec.id id then Some pkg else None)
          in
          match pkg_opt with
          | None -> None
          | Some pkg ->
            (match
               Or_error.try_with (fun () ->
                 Io.load_doc ~dir:index_path (pkg ^ "/" ^ id ^ ".md"))
             with
             | Ok text ->
               let preview_len = 8000 in
               let preview =
                 if String.length text > preview_len
                 then String.sub text ~pos:0 ~len:preview_len ^ " …"
                 else text
               in
               Some (rank + 1, pkg, id, preview)
             | Error _ -> None))
        |> List.filter_map ~f:Fn.id
      in
      if List.is_empty results
      then "No matching snippets found"
      else
        results
        |> List.map ~f:(fun (rank, pkg, id, preview) ->
          Printf.sprintf "[%d] [%s] %s\n%s" rank pkg id preview)
        |> String.concat ~sep:"\n\n---\n\n")
  in
  Gpt_function.create_function (module Definitions.Odoc_search) ~strict:false f
;;

(* -------------------------------------------------------------------------- *)
(* Webpage → Markdown tool                                                     *)
(* -------------------------------------------------------------------------- *)

let webpage_to_markdown ~env ~dir ~net : Gpt_function.t =
  Webpage_markdown.Tool.register ~env ~dir ~net
;;

(* -------------------------------------------------------------------------- *)
(*  Fork stub – placeholder implementation                                     *)
(* -------------------------------------------------------------------------- *)

let fork : Gpt_function.t =
  let impl (_ : Definitions.Fork.input) =
    "[fork-tool placeholder – should never be called directly]"
  in
  Gpt_function.create_function (module Definitions.Fork) impl
;;

(* -------------------------------------------------------------------------- *)
(* Markdown indexing – build vector store                                      *)
(* -------------------------------------------------------------------------- *)

let index_markdown_docs ~env ~dir : Gpt_function.t =
  let f (root, index_name, description, vector_db_root_opt) =
    let root_path = Eio.Path.(dir / root) in
    let vector_db_root = Option.value vector_db_root_opt ~default:".md_index" in
    try
      Markdown_indexer.index_directory
        ~vector_db_root
        ~env
        ~index_name
        ~description
        ~root:root_path;
      "Markdown documents have been indexed successfully."
    with
    | ex -> Fmt.str "error indexing markdown docs: %a" Eio.Exn.pp ex
  in
  Gpt_function.create_function (module Definitions.Index_markdown_docs) f
;;

(* -------------------------------------------------------------------------- *)
(* Markdown search – semantic retrieval                                        *)
(* -------------------------------------------------------------------------- *)

let markdown_search ~dir ~net : Gpt_function.t =
  (*────────────────────────  Simple in-memory caches  ───────────────────────*)
  let module Md_cache = struct
    open Core

    module S = struct
      type t = string [@@deriving compare, hash, sexp]
    end

    let embed_tbl : (string, float array) Hashtbl.t = Hashtbl.create (module S)
    let vec_tbl : (string, Vector_db.Vec.t array) Hashtbl.t = Hashtbl.create (module S)
    let mu = Eio.Mutex.create ()

    let get_embed ~net query =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find embed_tbl query in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let resp = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
        let vec = Array.of_list (List.hd_exn resp.data).embedding in
        Eio.Mutex.lock mu;
        Hashtbl.set embed_tbl ~key:query ~data:vec;
        Eio.Mutex.unlock mu;
        vec
    ;;

    let get_vectors vec_file_path path_t =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find vec_tbl vec_file_path in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let vecs =
          try Vector_db.Vec.read_vectors_from_disk path_t with
          | _ -> [||]
        in
        Eio.Mutex.lock mu;
        Hashtbl.set vec_tbl ~key:vec_file_path ~data:vecs;
        Eio.Mutex.unlock mu;
        vecs
    ;;
  end
  in
  let f (query, k_opt, index_name_opt, vector_db_root_opt) =
    let open Eio.Path in
    let k = Option.value k_opt ~default:5 in
    let vector_db_root = Option.value vector_db_root_opt ~default:".md_index" in
    let index_dir = dir / vector_db_root in
    (* 1. Embed query *)
    let query_vec = Md_cache.get_embed ~net query in
    let query_mat = Owl.Mat.of_array query_vec (Array.length query_vec) 1 in
    (* 2. Determine candidate indexes *)
    let indexes =
      match index_name_opt with
      | Some name when not (String.equal name "all") -> [ name ]
      | _ ->
        (match Md_index_catalog.load ~dir:index_dir with
         | Some catalog ->
           (* Compute similarity and sort *)
           let scores =
             Array.map catalog ~f:(fun { Md_index_catalog.Entry.name; vector; _ } ->
               let score =
                 Array.fold2_exn query_vec vector ~init:0.0 ~f:(fun acc q v ->
                   acc +. (q *. v))
               in
               score, name)
           in
           scores
           |> Array.to_list
           |> List.sort ~compare:(fun (s1, _) (s2, _) -> Float.compare s2 s1)
           |> (fun l -> List.take l 5)
           |> List.map ~f:snd
         | None ->
           (* fallback list all dirs *)
           List.filter (Eio.Path.read_dir index_dir) ~f:(fun entry ->
             Eio.Path.is_directory (index_dir / entry)))
    in
    if List.is_empty indexes
    then Printf.sprintf "No Markdown indices found under %s" vector_db_root
    else (
      (* 3. Aggregate vectors from selected indexes *)
      let vecs_with_index =
        List.concat_map indexes ~f:(fun idx_name ->
          let idx_dir = index_dir / idx_name in
          if is_directory idx_dir
          then (
            let vec_path = idx_dir / "vectors.binio" in
            let vec_key = native_exn vec_path in
            let vecs = Md_cache.get_vectors vec_key vec_path in
            Array.to_list vecs |> List.map ~f:(fun v -> idx_name, v))
          else [])
      in
      if List.is_empty vecs_with_index
      then Printf.sprintf "No vectors found in selected indices"
      else (
        let only_vecs = Array.of_list (List.map vecs_with_index ~f:snd) in
        let db = Vector_db.create_corpus only_vecs in
        let idxs = Vector_db.query db query_mat k in
        let results =
          Array.to_list idxs
          |> List.mapi ~f:(fun rank idx ->
            let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
            (* which index has this id *)
            let idx_opt =
              List.find_map vecs_with_index ~f:(fun (idx_name, v) ->
                if String.equal v.Vector_db.Vec.id id then Some idx_name else None)
            in
            match idx_opt with
            | None -> None
            | Some idx_name ->
              (match
                 Or_error.try_with (fun () ->
                   Io.load_doc ~dir:index_dir (idx_name ^ "/snippets/" ^ id ^ ".md"))
               with
               | Ok text ->
                 let preview_len = 8000 in
                 let preview =
                   if String.length text > preview_len
                   then String.sub text ~pos:0 ~len:preview_len ^ " …"
                   else text
                 in
                 Some (rank + 1, idx_name, id, preview)
               | Error _ -> None))
          |> List.filter_map ~f:Fn.id
        in
        if List.is_empty results
        then "No matching snippets found"
        else
          results
          |> List.map ~f:(fun (rank, idx_name, id, preview) ->
            Printf.sprintf "[%d] [%s] %s\n%s" rank idx_name id preview)
          |> String.concat ~sep:"\n\n---\n\n"))
  in
  Gpt_function.create_function (module Definitions.Markdown_search) ~strict:false f
;;
