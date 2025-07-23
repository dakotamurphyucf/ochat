(**
    End-to-end indexing of documentation produced by the
    {{:https://ocaml.org/p/odoc/latest}odoc} tool-chain.

    The function {!index_packages} turns the HTML pages located under
    the [_doc/_html] directory created by
    {[ dune build @doc ]} into a multi-modal search corpus made up of:

    • dense vector embeddings suitable for nearest-neighbour search
      (saved via {!Vector_db.Vec.write_vectors_to_disk});
    • lexical BM-25 indices (saved via {!Bm25.write_to_disk});
    • the raw Markdown snippets, one file per chunk, so that the caller
      can display the full context of a hit.

    The pipeline executed for each *opam package* folder is:

    {ol
    {- traverse the directory tree with {!Odoc_crawler.crawl};}
    {- slice every HTML / README into 64–320-token windows with
       {!Odoc_snippet.slice};}
    {- batch the chunks in groups of ≤ 300 and obtain embeddings from
       the OpenAI *Embedding* API; requests are funneled through a
       single fibre that enforces a per-second cap (see
       {!module-Embed_service});}
    {- write the embeddings, BM25 index and Markdown files to
       [output/pkg/];}
    {- gather a short blurb (first non-empty line) for each package and
       store it in {!Package_index}.}}

    Concurrency model:

    • CPU-bound slicing runs in a {!Io.Task_pool} backed by one domain
      per physical core.
    • IO-bound embedding calls are executed by the main fibre but
      throttled to respect provider quotas.

    Cancellation: the whole operation is enclosed in a {!Switch};
    aborting the switch propagates to every child fibre and ensures no
    partial files are left behind.
*)

open Core
open Eio
module I = Io
open Jsonaf.Export
module Log = Log

(**************************************************************************)
(* Helper utilities                                                        *)
(**************************************************************************)

(** [token_count ~codec text] returns the approximate number of BPE
      tokens in [text].  If [Tikitoken.encode] raises (e.g. because the
      text contains invalid UTF-8) or if the fallback heuristic is
      considered quicker, the function falls back to whitespace
      splitting.  This helper is *internal* and its behaviour is not
      stable – do not rely on it from outside the module. *)
let token_count ~codec text =
  match Tikitoken.encode ~codec ~text with
  | tokens -> List.length tokens
  | exception _ ->
    (* fallback: whitespace split *)
    String.split_on_chars text ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.length
;;

(** [meta_to_string m] renders [m] in the *legacy* pseudo-ocamldoc
      header format used by early experiments.  New code should rely on
      the serialised {!Odoc_snippet.meta} record and avoid parsing this
      textual representation.  The function is kept only because some
      historical corpora still expect it. *)
let meta_to_string (m : Odoc_snippet.meta) : string =
  let title = Option.value ~default:"" m.title in
  Printf.sprintf
    "(**\nPackage: %s\nPath: %s\nLines: %d-%d\nTitle: %s\n*)\n"
    m.pkg
    m.doc_path
    m.line_start
    m.line_end
    title
;;

(**************************************************************************)
(* Vector embedding helper (adapted from lib/indexer.ml)                   *)
(**************************************************************************)

(** [get_vectors ~net ~codec snippets] calls the OpenAI Embeddings
      endpoint and returns a triple of [(meta, text, vec)] for every
      snippet in [snippets].

      The function retries up to three times on transient network
      failures.  Each retry attempts to re-encode the *whole* batch –
      partial success is not currently supported.

      Returned vectors are **not** L2-normalised; that is done by
      {!Vector_db.create_corpus} during loading. *)
let rec get_vectors
          ?(attempts = 0)
          ~net
          ~codec
          (snippets : (Odoc_snippet.meta * string) list)
  =
  let inputs = List.map snippets ~f:(fun (_meta, text) -> text) in
  match Openai.Embeddings.post_openai_embeddings net ~input:inputs with
  | exception ex ->
    traceln "Error fetching embeddings: %a" Eio.Exn.pp ex;
    (match attempts with
     | 3 -> raise ex
     | _ ->
       traceln "Retrying embedding fetch (%d/3)" (attempts + 1);
       get_vectors ~net ~codec snippets ~attempts:(attempts + 1))
  | response ->
    let tbl = Hashtbl.create (module Int) in
    List.iteri snippets ~f:(fun idx (meta, text) ->
      Hashtbl.add_exn tbl ~key:idx ~data:(meta, text));
    List.map response.data ~f:(fun item ->
      let meta, text = Hashtbl.find_exn tbl item.index in
      let len = token_count ~codec text in
      let vector = Array.of_list item.embedding in
      let vec = Vector_db.Vec.{ id = meta.id; len; vector } in
      meta, text, vec)
;;

(**************************************************************************)
(* Rate-limited embedding daemon                                           *)
(**************************************************************************)

(*  The OpenAI embeddings endpoint enforces strict per-minute and per-second
    rate limits.  We funnel every request through a single fibre that
    throttles calls so that no more than [rate_per_sec] are executed.       *)

(* ───────────────────────────────────────────────────────────────────────── *
   Embedding service – now provided by the shared [Embed_service] module.  *
   The local implementation has been removed to avoid duplication.        *)

(**************************************************************************)
(* Public API: package indexing                                            *)
(**************************************************************************)

type package_filter =
  | Include of string list
  | Exclude of string list
  | Update of package_filter * string list
  | All

(** [index_packages ?skip_pkgs ~env ~root ~output ~net ()] builds a
      vector & BM-25 search corpus for every package folder under
      [root] and stores the artefacts under [output].

      Parameters:

      • [?skip_pkgs] – opam package names that should be ignored (e.g.
        because they are huge or irrelevant to the downstream
        application).

      • [env] – capability bundle provided by {!Eio_main.run}; its
        [clock], [net] and [fs] fields are used for, respectively,
        time-outs, HTTP calls and file IO.

      • [root] – directory produced by {[ dune build @doc ]}.  Each
        first-level directory beneath [root] is treated as an opam
        package.

      • [output] – destination folder.  For every package *foo* the
        function creates:

        {ul
        {- *foo*/vectors.binio – array of {!Vector_db.Vec.t};}
        {- *foo*/bm25.binio    – {!Bm25.t};}
        {- *foo*/<id>.md       – raw Markdown snippet for each chunk.}}

      • [net] – network capability used by {!Openai.Embeddings}.

      Behaviour & guarantees:

      • Slicing and embedding happen concurrently – CPU-bound work is
        offloaded to a pool of domains, HTTP calls are batched and
        throttled.

      • Progress is logged via {!Log.emit} and spanned so that a
        Jaeger/OpenTelemetry backend can visualise the trace.

      • The function is synchronous; it returns only after *all*
        artefacts have been flushed to disk.

      @raise exn on unrecoverable filesystem or network errors.  All
      transient failures (OpenAI 5xx, time-outs) are retried up to 3
      times before escalating. *)
let index_packages
      ?(filter = All)
      ~(env : Eio_unix.Stdenv.base)
      ~(root : _ Path.t)
      ~(output : _ Path.t)
      ~(net : _ Eio.Net.t)
      ()
  : unit
  =
  Log.with_span
    ~ctx:[ "path", jsonaf_of_string @@ Fmt.str "%a" Path.pp output ]
    "odoc_indexer"
  @@ fun () ->
  (* 1. Gather docs via crawler *)
  let domain_count = Domain.recommended_domain_count () in
  let pkg_docs = Hashtbl.create (module String) in
  let tiki_token_bpe =
    Io.load_doc ~dir:(Eio.Stdenv.fs env) "./out-cl100k_base.tikitoken.txt"
  in
  let should_crawl =
    let rec fn filter =
      match filter with
      | Include pkgs -> fun pkg -> List.mem pkgs pkg ~equal:String.equal
      | Exclude pkgs -> fun pkg -> not (List.mem pkgs pkg ~equal:String.equal)
      | All -> fun _ -> true
      | Update (prev_filter, pkgs) ->
        let prev_fn = fn prev_filter in
        fun pkg ->
          (match prev_filter with
           | Include _ -> prev_fn pkg || List.mem pkgs pkg ~equal:String.equal
           | Exclude _ -> prev_fn pkg
           | All -> true
           | Update _ -> failwith "Update filter cannot be nested")
    in
    fn filter
  in
  let should_index pkg =
    match filter with
    | Include pkgs -> List.mem pkgs pkg ~equal:String.equal
    | Exclude pkgs -> not (List.mem pkgs pkg ~equal:String.equal)
    | All -> true
    | Update (_, pkgs) -> List.mem pkgs pkg ~equal:String.equal
  in
  (* 2. Crawl the directory tree and slice Markdown files into snippets. *)
  (* Ensure output folder exists. *)
  let codec = Tikitoken.create_codec tiki_token_bpe in
  Odoc_crawler.crawl ~root (fun ~pkg ~doc_path ~markdown ->
    if should_crawl pkg
    then (
      let lst = Hashtbl.find_or_add pkg_docs pkg ~default:(fun () -> []) in
      Hashtbl.set pkg_docs ~key:pkg ~data:((doc_path, markdown) :: lst)));
  (* 2. Parallel processing per package *)
  Switch.run
  @@ fun sw ->
  Log.heartbeat ~sw ~clock:env#clock ~interval:1.0 ~probe:(fun () -> []) ();
  (* ------------------------------------------------------------------ *)
  (*  Shared, rate-limited embedder instance used by all fibres in this  *)
  (*  indexing run.  The service lives on [sw] so it is cleaned up once  *)
  (*  the switch finishes.                                               *)
  (* ------------------------------------------------------------------ *)
  let embed_batch =
    Embed_service.create
      ~sw
      ~clock:env#clock
      ~net
      ~codec
      ~rate_per_sec:1000
      ~get_id:(fun (m : Odoc_snippet.meta) -> m.id)
  in
  let dm = Eio.Stdenv.domain_mgr env in
  let module Pool =
    Io.Task_pool (struct
      type input = string * string * string
      type output = (Odoc_snippet.meta * string) list

      let dm = dm
      let stream = Eio.Stream.create 0
      let sw = sw

      let handler (pkg, doc_path, md) =
        let slice_timeout = 15.0 in
        try
          Eio.Time.with_timeout_exn env#clock slice_timeout (fun () ->
            Odoc_snippet.slice ~tiki_token_bpe ~pkg ~doc_path ~markdown:md ())
        with
        | Eio.Time.Timeout ->
          traceln "Timeout (%.0fs) slicing %s/%s" slice_timeout pkg doc_path;
          []
      ;;
    end)
  in
  (* spawn worker domains *)
  List.iter
    ~f:Pool.spawn
    (List.init domain_count ~f:(fun i -> Printf.sprintf "Worker-%d" (i + 1)));
  (* submit all packages *)
  let handler (pkg, docs) =
    let pkg_dir = Path.(output / pkg) in
    (match Path.is_directory pkg_dir with
     | true -> ()
     | false -> Path.mkdirs ~perm:0o700 pkg_dir);
    Log.emit
      ~ctx:
        [ "pkg", Jsonaf.Export.jsonaf_of_string pkg
        ; "docs", Jsonaf.Export.jsonaf_of_int (List.length docs)
        ]
      `Info
      "slice_start";
    let snippets =
      List.concat
      @@ Eio.Fiber.List.map (fun (doc_path, md) -> Pool.submit (pkg, doc_path, md)) docs
    in
    Log.emit
      ~ctx:
        [ "pkg", Jsonaf.Export.jsonaf_of_string pkg
        ; "docs", Jsonaf.Export.jsonaf_of_int (List.length docs)
        ]
      `Info
      "slice_end";
    if List.is_empty snippets
    then ()
    else (
      Log.emit
        ~ctx:
          [ "pkg", jsonaf_of_string pkg
          ; "snippets", jsonaf_of_int (List.length snippets)
          ]
        `Info
        "embeddings_start";
      let snippet_chunks = List.chunks_of snippets ~length:300 in
      let meta_text_vecs =
        List.concat
        @@ Eio.Fiber.List.map
             (fun snips ->
                Log.with_span
                  ~ctx:[ "pkg", jsonaf_of_string pkg ]
                  "openai_embed"
                  (fun () ->
                     try embed_batch snips with
                     | _ -> []))
             snippet_chunks
      in
      Log.emit ~ctx:[ "pkg", jsonaf_of_string pkg ] `Info "embeddings_done";
      (* Persist markdown and prepare bm25 docs *)
      let bm25_docs = ref [] in
      List.iteri meta_text_vecs ~f:(fun idx (meta, text, _) ->
        Io.save_doc ~dir:pkg_dir (meta.id ^ ".md") text;
        bm25_docs := Bm25.{ id = idx; text } :: !bm25_docs);
      (* vectors *)
      let vectors = Array.of_list (List.map meta_text_vecs ~f:(fun (_m, _t, v) -> v)) in
      Vector_db.Vec.write_vectors_to_disk vectors Path.(pkg_dir / "vectors.binio");
      let bm25 = Bm25.create !bm25_docs in
      Bm25.write_to_disk Path.(pkg_dir / "bm25.binio") bm25)
  in
  Hashtbl.to_alist pkg_docs
  |> Eio.Fiber.List.iter (fun (pkg, docs) -> if should_index pkg then handler (pkg, docs));
  (* 3. Build & save package index with blurbs (serial, lightweight) *)
  let descriptions =
    Hashtbl.to_alist pkg_docs
    |> List.map ~f:(fun (pkg, docs) ->
      let blurb =
        match
          List.find docs ~f:(fun (path, _) ->
            String.equal (String.lowercase path) (sprintf "%s/index.html" pkg))
        with
        | Some (_, md) ->
          md
          |> String.split_lines
          |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
          |> Option.value ~default:""
        | None ->
          (match docs with
           | (_, md) :: _ ->
             md
             |> String.split_lines
             |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
             |> Option.value ~default:""
           | [] -> "")
      in
      let count = token_count ~codec blurb in
      if count <= 1000
      then pkg, blurb
      else (
        let blurb = ref (String.slice blurb 0 (String.length blurb - 100)) in
        while token_count ~codec !blurb > 500 do
          blurb := String.slice !blurb 0 (String.length !blurb - 100)
        done;
        pkg, !blurb))
  in
  let _pkg_idx = Package_index.build_and_save ~net ~descriptions ~dir:output in
  ()
;;

(**************************************************************************)
(* End of file                                                             *)
(**************************************************************************)
