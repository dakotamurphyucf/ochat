open Core
open Eio
open Io

let collect_ocaml_files path directory =
  Printf.printf "module: %s" directory;
  match Ocaml_parser.collect_ocaml_files path directory with
  | Ok module_infos -> module_infos
  | Error msg -> failwith ("Error collecting OCaml files: " ^ msg)
;;

let parse_module_info ~dir docs =
  match to_res (fun () -> Ocaml_parser.parse_module_info dir docs) with
  | Ok module_infos -> module_infos
  | Error msg ->
    print_endline ("Error collecting OCaml files: " ^ msg);
    let path = docs.module_path in
    Printf.printf "module: %s" path;
    None, None
;;

let handle_job traverse_input =
  List.map ~f:(fun input -> Ocaml_parser.traverse input) traverse_input
  |> List.concat
  |> List.map ~f:Ocaml_parser.format_parse_result
  |> List.map ~f:(fun (metadata, doc) ->
    Doc.hash_string_md5 (metadata ^ "\n" ^ doc), doc, metadata)
;;

let chunk n = List.groupi ~break:(fun i _ _ -> i mod n = 0)

let get_vectors ~net docs =
  let tbl = Hashtbl.create (module Int) in
  List.iteri
    ~f:(fun i (id, doc, meta) -> Hashtbl.add_exn tbl ~key:i ~data:(id, doc, meta))
    docs;
  let response =
    Openai.Embeddings.post_openai_embeddings
      net
      ~input:(List.map ~f:(fun (_id, doc, _meta) -> doc) docs)
  in
  List.map response.data ~f:(fun item ->
    let id, doc, meta = Hashtbl.find_exn tbl item.index in
    meta ^ "\n" ^ doc, Vector_db.Vec.{ id; vector = Array.of_list item.embedding })
;;

let index ~sw ~dir ~dm ~net ~vector_db_folder ~folder_to_index =
  let vf = dir / vector_db_folder in
  let module Pool =
    Task_pool (struct
      type input = Ocaml_parser.traverse_input list
      type output = (string * string * string) list

      let dm = dm
      let stream = Eio.Stream.create 0
      let sw = sw
      let handler = handle_job
    end)
  in
  let save (doc, v) =
    save_doc ~dir:vf v.Vector_db.Vec.id doc;
    v
  in
  let f (iface, imple) info =
    match info with
    | None, None -> iface, imple
    | Some mli, Some ml -> mli :: iface, ml :: imple
    | Some mli, None -> mli :: iface, imple
    | None, Some ml -> iface, ml :: imple
  in
  let task thunks =
    traceln "Client  submitting job...";
    chunk 50 @@ Pool.submit thunks
    |> Fiber.List.map (get_vectors ~net)
    |> List.concat
    |> Fiber.List.map save
  in
  List.iter ~f:Pool.spawn [ "A"; "B"; "C" ];
  let modules = collect_ocaml_files dir folder_to_index in
  let mli_input, ml_input =
    Fiber.List.map (parse_module_info ~dir) modules |> List.fold ~init:([], []) ~f
  in
  let mli_vecs =
    chunk 25 mli_input |> Fiber.List.map task |> List.concat |> Array.of_list
  in
  let ml_vecs =
    chunk 25 ml_input |> Fiber.List.map task |> List.concat |> Array.of_list
  in
  let mli_vec_file = String.concat [ vector_db_folder; "/"; "vectors.mli.binio" ] in
  let ml_vec_file = String.concat [ vector_db_folder; "/"; "vectors.ml.binio" ] in
  Vector_db.Vec.write_vectors_to_disk mli_vecs mli_vec_file;
  Vector_db.Vec.write_vectors_to_disk ml_vecs ml_vec_file
;;
