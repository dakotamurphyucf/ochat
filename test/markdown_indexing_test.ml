open Core

(* Utility: simple vector dot product *)
let dot a b =
  Array.fold2_exn a b ~init:0.0 ~f:(fun acc x y -> acc +. (x *. y))

(* -------------------------------------------------------------------------- *)
(* 1. Chunking test                                                          *)
(* -------------------------------------------------------------------------- *)

let%expect_test "markdown_snippet.chunking" =
  let tiki_path = "./out-cl100k_base.tikitoken.txt" in
  (* Build a markdown document with many paragraphs to trigger multiple
     slices. 20 paragraphs × 100 tokens ≈ 2 000 tokens. *)
  let para = String.concat ~sep:" " (List.init 100 ~f:(fun _ -> "token")) in
  let body = String.concat ~sep:"\n\n" (List.init 20 ~f:(fun _ -> para)) in
  let markdown = "# Title\n\n" ^ body in
  let slices =
    Markdown_snippet.slice
      ~index_name:"test"
      ~doc_path:"doc.md"
      ~markdown
      ~tiki_token_bpe:tiki_path
      ()
  in
  let num = List.length slices in
  (* Expect at least 4 snippets within token bounds (64-320) *)
  assert (num >= 4);
  print_endline "ok";
  [%expect {| ok |}]
;;

(* -------------------------------------------------------------------------- *)
(* 2. Vector-DB round-trip test                                              *)
(* -------------------------------------------------------------------------- *)

let%expect_test "vector_db.roundtrip" =
  let open Vector_db in
  (* Two dummy vectors – orthogonal unit vectors. *)
  let v1 = [| 1.0; 0.0; 0.0 |] in
  let v2 = [| 0.0; 1.0; 0.0 |] in
  let vecs =
    [| Vec.{ id = "doc1"; len = 10; vector = v1 }; Vec.{ id = "doc2"; len = 10; vector = v2 } |]
  in
  let db = Vector_db.create_corpus vecs in
  let query = Owl.Mat.of_array v1 3 1 in
  let idxs = Vector_db.query db query 1 in
  let top_idx = idxs.(0) in
  let id, _len = Hashtbl.find_exn db.Vector_db.index top_idx in
  print_endline id;
  [%expect {| doc1 |}]
;;

(* -------------------------------------------------------------------------- *)
(* 3. Catalogue lookup test                                                 *)
(* -------------------------------------------------------------------------- *)

let%expect_test "md_index_catalog.lookup" =
  (* Prepare a temporary directory. *)
  let tmp_dir =
    Filename.concat
      Filename.temp_dir_name
      ("md_catalog_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_dir;
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Path.(env#fs / tmp_dir) in
  (* Add two dummy indexes *)
  let v1 = [| 1.0; 0.0; 0.0 |] in
  let v2 = [| 0.0; 1.0; 0.0 |] in
  Md_index_catalog.add_or_update ~dir ~name:"alpha" ~description:"first" ~vector:v1;
  Md_index_catalog.add_or_update ~dir ~name:"beta" ~description:"second" ~vector:v2;
  (* Load catalogue and compute best match for a query aligned with v2. *)
  match Md_index_catalog.load ~dir with
  | None -> print_endline "catalog empty"
  | Some catalog ->
    let query = v2 in
    let scores =
      Array.map catalog ~f:(fun { Md_index_catalog.Entry.name; vector; _ } ->
        name, dot query vector)
    in
    let best =
      Array.max_elt scores ~compare:(fun (_, s1) (_, s2) -> Float.compare s1 s2)
      |> Option.value_exn
    in
    print_endline (fst best);
  [%expect {| beta |}]
;;
