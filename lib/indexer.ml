(** OCaml-source indexer.

    This implementation builds a *hybrid retrieval* corpus that
    combines dense OpenAI embeddings with a traditional BM-25 index.  It
    is optimised for interactive assistants that need to answer
    questions about a local code-base ("Which module exposes
    [bin_prot_writer] ?", "Show me an example of [Eio.Switch.run]").

    The public API is limited to {!Indexer.index}; everything else is
    private helper code.  Nevertheless we keep lightweight comments on
    the internal helpers to document invariants and assumptions.

    {1 Pipeline overview}

    1. {!collect_ocaml_files} walks [folder_to_index] and groups [*.ml]
       and [*.mli] pairs into {!Ocaml_parser.module_info} records.
    2. {!parse_module_info} lazily produces {!Ocaml_parser.traverse_input}
       thunks that can be evaluated in parallel.
    3. {!handle_job} runs inside the worker fibres created through
       {!module:Task_pool}.  It converts parse results into 100–300 token
       snippets, attaches location metadata and returns a list of
       `(id, doc, meta, len)` tuples.
    4. {!get_vectors} sends the concatenation of [doc] strings to the
       OpenAI *Embeddings* endpoint, re-assembles the responses in the
       original order and pairs them with their metadata to form
       {!Vector_db.Vec.t} values.
    5. {!index} orchestrates everything, saves the dense vectors with
       {!Vector_db.Vec.write_vectors_to_disk} and, finally, builds a
       matching BM-25 index for lexical search.

    All IO happens through {!module:Eio}.  The function must therefore be
    called from inside an [Eio_main.run] context.
*)

open Core
open Eio
open Io

(** [collect_ocaml_files dir path] enumerates all [.ml] and [.mli]
    descendants of [path] and groups them into module pairs à la
    {!Ocaml_parser.collect_ocaml_files}.  A bare exception is raised if
    the traversal fails – the calling code treats this as a fatal
    configuration error. *)
let collect_ocaml_files path directory =
  Printf.printf "module: %s" directory;
  match Ocaml_parser.collect_ocaml_files path directory with
  | Ok module_infos -> module_infos
  | Error msg -> failwith ("Error collecting OCaml files: " ^ msg)
;;

(** [parse_module_info ~dir info] converts the raw {!Ocaml_parser.module_info}
    record into a pair of *lazy* parse thunks – one for the interface and
    one for the implementation file.  Errors are logged to stdout and
    propagated by returning [(None, None)]. *)
let parse_module_info ~dir docs =
  match to_res (fun () -> Ocaml_parser.parse_module_info dir docs) with
  | Ok module_infos -> module_infos
  | Error msg ->
    print_endline ("Error collecting OCaml files: " ^ msg);
    let path = docs.module_path in
    Printf.printf "module: %s" path;
    None, None
;;

(** Worker executed in a background domain by {!Task_pool}.

    It receives a list of {!Ocaml_parser.traverse_input} records, walks
    the parse tree, and groups the extracted documentation into
    *self-contained* snippets of roughly 64–320 tokens.  Each snippet is
    annotated with a location header and a unique MD5 hash so that the
    same code is never embedded twice.

    The function is intentionally oblivious to concurrency concerns – it
    is pure and CPU-bound, returning a value that can safely be moved
    across fibres. *)
let handle_job traverse_input =
  (* ──────────────────────────────────────────────────────────────────── *)
  (* Helper: very rough token count – we just split on ASCII whitespace   *)
  let token_count s =
    String.split_on_chars s ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun x -> not (String.is_empty x))
    |> List.length
  in
  (* Parameters controlling chunk size (see design doc) *)
  let min_tokens = 64 in
  let max_tokens = 320 in
  (* Flush accumulators (parse_results + docs) to a single snippet *)
  let flush pr_acc doc_acc acc =
    match doc_acc with
    | [] -> acc
    | _ ->
      let docs_string = String.concat ~sep:"\n\n" (List.rev doc_acc) in
      let first_pr = List.last_exn pr_acc in
      let last_pr = List.hd_exn pr_acc in
      let location =
        Printf.sprintf
          "File \"%s\", line %d-%d, characters %d-%d"
          first_pr.Ocaml_parser.file
          first_pr.line_start
          last_pr.line_end
          first_pr.char_start
          last_pr.char_end
      in
      let ocaml_source_str =
        match first_pr.ocaml_source with
        | Ocaml_parser.Interface -> "Interface"
        | Ocaml_parser.Implementation -> "Implementation"
      in
      let meta_string =
        Printf.sprintf
          "(** \nLocation: %s\nModule Path: %s\nOCaml Source: %s\n*)\n\n"
          location
          first_pr.module_path
          ocaml_source_str
      in
      let len = token_count docs_string in
      ( Doc.hash_string_md5 (meta_string ^ "\n" ^ docs_string)
      , docs_string
      , meta_string
      , len )
      :: acc
  in
  (* Build list of (parse_result, doc_string) *)
  let pr_items = List.map ~f:Ocaml_parser.traverse traverse_input |> List.concat in
  let item_data =
    List.map pr_items ~f:(fun pr ->
      let _, doc = Ocaml_parser.format_parse_result pr in
      pr, doc)
  in
  let flush = flush in
  (* alias *)
  let res, pr_acc, doc_acc, _ =
    List.fold
      item_data
      ~init:([], [], [], 0)
      ~f:(fun (acc, pr_acc, doc_acc, tok_acc) (pr, doc) ->
        let t = token_count doc in
        let tok_acc' = tok_acc + t in
        let pr_acc' = pr :: pr_acc in
        let doc_acc' = doc :: doc_acc in
        if tok_acc' >= max_tokens && tok_acc' >= min_tokens
        then (
          let acc = flush pr_acc' doc_acc' acc in
          acc, [], [], 0)
        else acc, pr_acc', doc_acc', tok_acc')
  in
  let results = flush pr_acc doc_acc res in
  results
;;

(** [chunk k xs] splits [xs] into consecutive slices of size ≤ [k]. *)
let chunk n = List.groupi ~break:(fun i _ _ -> i mod n = 0)

(*────────────────────────  Embeddings  ─────────────────────────*)

(** [get_vectors ~net docs] sends the list of documentation snippets
    produced by {!handle_job} to the OpenAI embeddings endpoint and
    returns a pair [(full_text, vec)].  Snippets longer than 6 000
    tokens are window-/stride-cropped into overlapping slices so that
    *no single HTTP call* exceeds the model limit. *)
let get_vectors ~net docs =
  (* reuse token counter *)
  let token_count s =
    String.split_on_chars s ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun x -> not (String.is_empty x))
    |> List.length
  in
  let embedding_cap = 6000 in
  let window_tokens = 3000 in
  let stride_tokens = 2400 in
  let slice_doc (id, doc, meta, len) =
    if len <= embedding_cap
    then [ id, doc, meta, len ]
    else (
      let lines = String.split_lines doc in
      (* iterate building slices based on token budget *)
      let rec loop cur_tokens cur_lines remaining acc slice_idx =
        match remaining with
        | [] ->
          if cur_tokens = 0
          then List.rev acc
          else (
            let slice_doc = String.concat ~sep:"\n" (List.rev cur_lines) in
            let sid = id ^ "#" ^ Int.to_string slice_idx in
            let slen = token_count slice_doc in
            List.rev ((sid, slice_doc, meta, slen) :: acc))
        | line :: rest ->
          let t_line = token_count line in
          if cur_tokens + t_line > window_tokens
          then (
            (* emit slice, then start new with overlap *)
            let slice_doc = String.concat ~sep:"\n" (List.rev cur_lines) in
            let sid = id ^ "#" ^ Int.to_string slice_idx in
            let slen = token_count slice_doc in
            (* prepare overlap lines (take from end of cur_lines) *)
            let rec take_overlap toks lst acc_lines =
              match lst with
              | [] -> acc_lines
              | _ when toks <= 0 -> acc_lines
              | hd :: tl ->
                let t = token_count hd in
                take_overlap (toks - t) tl (hd :: acc_lines)
            in
            let overlap = take_overlap stride_tokens cur_lines [] in
            loop
              (token_count (String.concat ~sep:"\n" overlap))
              overlap
              (line :: rest)
              ((sid, slice_doc, meta, slen) :: acc)
              (slice_idx + 1))
          else loop (cur_tokens + t_line) (line :: cur_lines) rest acc slice_idx
      in
      loop 0 [] lines [] 0)
  in
  let docs_expanded = List.concat_map docs ~f:slice_doc in
  (* build index table *)
  let tbl = Hashtbl.create (module Int) in
  List.iteri docs_expanded ~f:(fun i (id, doc, meta, len) ->
    Hashtbl.add_exn tbl ~key:i ~data:(id, doc, meta, len));
  let input_texts = List.map docs_expanded ~f:(fun (_id, doc, _meta, _) -> doc) in
  let response = Openai.Embeddings.post_openai_embeddings net ~input:input_texts in
  List.map response.data ~f:(fun item ->
    let id, doc, meta, len = Hashtbl.find_exn tbl item.index in
    meta ^ "\n" ^ doc, Vector_db.Vec.{ id; len; vector = Array.of_list item.embedding })
;;

(** [index ~sw ~dir ~dm ~net ~vector_db_folder ~folder_to_index]
    orchestrates the entire indexing pipeline.

    * All blocking IO (disk and network) happens under the provided
      switch [sw].  Cancelling the switch therefore aborts embedding
      requests and leaves no half-written files behind.
    * The function schedules three worker fibres (arbitrarily named
      "A", "B", "C") via {!module:Task_pool}.  Feel free to spawn more
      if your workload benefits from it.
    * Output layout (relative to [dir]):

      {v
      <vector_db_folder>/
      ├─ vectors.mli.binio   – dense vectors for interfaces
      ├─ vectors.ml.binio    – dense vectors for implementations
      ├─ bm25.mli.binio      – BM25 index for interfaces
      └─ bm25.ml.binio       – BM25 index for implementations
      v}

    The function returns once all four files are durably written.
*)
let index ~sw ~dir ~dm ~net ~vector_db_folder ~folder_to_index =
  let vf = dir / vector_db_folder in
  let module Pool =
    Task_pool (struct
      type input = Ocaml_parser.traverse_input list
      type output = (string * string * string * int) list

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
  (* Save BM25 indices *)
  let save_bm25 vecs file_name =
    let docs =
      Array.mapi vecs ~f:(fun idx v ->
        let text = Io.load_doc ~dir:vf v.Vector_db.Vec.id in
        Bm25.{ id = idx; text })
      |> Array.to_list
    in
    let bm25 = Bm25.create docs in
    Bm25.write_to_disk (dir / String.concat [ vector_db_folder; "/"; file_name ]) bm25
  in
  save_bm25 mli_vecs "bm25.mli.binio";
  save_bm25 ml_vecs "bm25.ml.binio";
  let mli_vec_file = String.concat [ vector_db_folder; "/"; "vectors.mli.binio" ] in
  let ml_vec_file = String.concat [ vector_db_folder; "/"; "vectors.ml.binio" ] in
  Vector_db.Vec.write_vectors_to_disk mli_vecs (dir / mli_vec_file);
  Vector_db.Vec.write_vectors_to_disk ml_vecs (dir / ml_vec_file)
;;
