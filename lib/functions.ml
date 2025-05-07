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
    let vecs = Vector_db.Vec.read_vectors_from_disk (dir / vec_file) in
    let corpus = Vector_db.create_corpus vecs in
    let response = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
    let query_vector =
      Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
      |> Owl.Mat.transpose
    in
    let top_indices = Vector_db.query corpus query_vector num_results in
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
