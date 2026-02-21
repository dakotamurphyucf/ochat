open! Core
open Eio

[@@@ocaml.warning "-32-27-16"]

(** {1 Overview}

    High-level pipeline that turns a directory tree of Markdown documents
    into a vector database that can be queried with semantic similarity
    search.  The entry-point {!index_directory} is intentionally
    single-purpose – you pass it the {e root} directory and get an on-disk
    index at [vector_db_root/index_name].  All heavy-lifting is delegated to
    specialised helper modules and external services:

    • {!Markdown_crawler} – bounded-concurrency file traversal.
    • {!Markdown_snippet} – cut documents into ~64-320 token windows.
    • {!Embed_service} – batched, rate-limited calls to OpenAI embeddings.
    • {!Vector_db} – binary serialisation of float vectors.
    • {!Md_index_catalog} – global registry of indexes + centroid vectors.

    The implementation follows Eio’s structured-concurrency model and is
    cancel-safe.  All I/O happens through the supplied [env] argument.  The
    function is idempotent: snippets are identified by a stable hash so
    re-running the indexer only uploads new or changed fragments. *)

module M = struct
  module MS = Markdown_snippet
  module Vec = Vector_db.Vec

  (*────────────────────────  Helpers  ─────────────────────────────*)

  (* Compute centroid – simple arithmetic mean across all vectors. *)
  let centroid (vecs : Vec.t array) : float array =
    match vecs with
    | [||] -> failwith "centroid: empty array"
    | _ ->
      let dims = Array.length vecs.(0).vector in
      let sum = Array.create ~len:dims 0.0 in
      Array.iter vecs ~f:(fun v ->
        Array.iteri v.vector ~f:(fun i x -> sum.(i) <- sum.(i) +. x));
      Array.map sum ~f:(fun x -> x /. Float.of_int (Array.length vecs))
  ;;

  (*──────────────────  Public entry-point  ───────────────────────*)

  let index_directory
        ?(vector_db_root = ".md_index")
        ~(env : Eio_unix.Stdenv.base)
        ~(index_name : string)
        ~(description : string)
        ~(root : _ Path.t)
    : unit
    =
    Log.with_span ~ctx:[ "index", `String index_name ] "md_indexer"
    @@ fun () ->
    (* Ensure output folder exists. *)
    let out_dir = Path.(env#fs / vector_db_root / index_name) in
    (match Path.is_directory out_dir with
     | true -> ()
     | false -> Path.mkdirs ~perm:0o700 out_dir);
    (* Load BPE table and codec once. *)
    let tiki_token_bpe = Tiktoken_data.o200k_base in
    let tiki_codec = Tikitoken.create_codec tiki_token_bpe in
    (* Accumulate snippets. *)
    let snippets_ref = ref [] in
    (* 1. Crawl directory – slice markdown files on the fly. *)
    Markdown_crawler.crawl ~root ~f:(fun ~doc_path ~markdown ->
      let slices = MS.slice ~index_name ~doc_path ~markdown ~tiki_token_bpe () in
      snippets_ref := slices @ !snippets_ref);
    let snippets = List.rev !snippets_ref in
    if List.is_empty snippets
    then (
      Log.emit `Info ~ctx:[] "md_indexer_empty";
      exit 0);
    (* 2. Batch embeddings – use shared Embed_service. *)
    Switch.run
    @@ fun sw ->
    let embed =
      let rate = 1000 in
      Embed_service.create
        ~sw
        ~clock:env#clock
        ~net:env#net
        ~codec:tiki_codec
        ~rate_per_sec:rate
        ~get_id:(fun (m : MS.Meta.t) -> m.id)
    in
    let batches = List.chunks_of snippets ~length:300 in
    let vecs_acc = ref [] in
    List.iter batches ~f:(fun batch ->
      let res = embed batch in
      vecs_acc := res @ !vecs_acc);
    let vecs =
      !vecs_acc
      |> List.rev
      |> List.map ~f:(fun (_meta, _text, vec) -> vec)
      |> Array.of_list
    in
    (* 3. Persist vectors & markdown snippets. *)
    Vector_db.Vec.write_vectors_to_disk vecs Path.(out_dir / "vectors.binio");
    (* Write markdown body for each snippet under [snippets/]. *)
    let snip_dir = Path.(out_dir / "snippets") in
    (match Path.is_directory snip_dir with
     | true -> ()
     | false -> Path.mkdirs ~perm:0o700 snip_dir);
    List.iter !vecs_acc ~f:(fun (meta, text, _vec) ->
      Io.save_doc ~dir:snip_dir (meta.id ^ ".md") text);
    (* 4. Update index catalogue *)
    let centroid_vec = centroid vecs in
    let catalog_dir = Path.(env#fs / vector_db_root) in
    Md_index_catalog.add_or_update
      ~dir:catalog_dir
      ~name:index_name
      ~description
      ~vector:centroid_vec
  ;;
end

include M
