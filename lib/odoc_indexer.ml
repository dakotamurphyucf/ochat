open Core
open Eio
module I = Io
open Jsonaf.Export
module Log = Log

(**************************************************************************)
(* Helper utilities                                                        *)
(**************************************************************************)

let token_count ~codec text =
  match Tikitoken.encode ~codec ~text with
  | tokens -> List.length tokens
  | exception _ ->
    (* fallback: whitespace split *)
    String.split_on_chars text ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.length
;;

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

module Embed_service = struct
  open Eio

  type request =
    { snippets : (Odoc_snippet.meta * string) list
    ; resolver :
        ((Odoc_snippet.meta * string * Vector_db.Vec.t) list, exn) result Promise.u
    }

  let create ~sw ~clock ~net ~codec ~rate_per_sec ()
    :  (Odoc_snippet.meta * string) list
    -> (Odoc_snippet.meta * string * Vector_db.Vec.t) list
    =
    let stream : request Stream.t = Stream.create 100 in
    (* Helper: run OpenAI call with up to 3 retries *)
    let rec fetch_with_retries attempts snippets
      : (Odoc_snippet.meta * string * Vector_db.Vec.t) list
      =
      try get_vectors ~net ~codec snippets with
      | exn when attempts < 3 ->
        traceln "embed retry %d/3 due to %a" (attempts + 1) Eio.Exn.pp exn;
        (* back-off a little before retrying *)
        Time.sleep clock 1.0;
        fetch_with_retries (attempts + 1) snippets
      | exn -> raise exn
    in
    (* Daemon fibre that enforces the rate limit *)
    Fiber.fork_daemon ~sw (fun () ->
      let last_call = ref 0.0 in
      let min_interval = 1.0 /. Float.of_int rate_per_sec in
      let rec loop () =
        let { snippets; resolver } = Stream.take stream in
        let now = Time.now clock in
        (*   If the last call was less than [min_interval] seconds ago, sleep
             so that we honour the rate cap.                                *)
        let elapsed = now -. !last_call in
        if Float.(elapsed < min_interval)
        then (
          let to_sleep = min_interval -. elapsed in
          if Float.(to_sleep > 0.0) then Time.sleep clock to_sleep);
        last_call := Time.now clock;
        Fiber.fork ~sw (fun () ->
          (* 1. Fetch embeddings with retries *)
          let result =
            try Ok (fetch_with_retries 0 snippets) with
            | ex -> Error ex
          in
          Promise.resolve resolver result);
        loop ()
      in
      loop ());
    (* Returned [embed] function: enqueue and wait for the promise. *)
    fun snippets ->
      let promise, resolver = Promise.create () in
      Stream.add stream { snippets; resolver };
      match Promise.await promise with
      | Ok res -> res
      | Error ex -> raise ex
  ;;
end

(**************************************************************************)
(* Public API: package indexing                                            *)
(**************************************************************************)

let index_packages
      ?(skip_pkgs = [])
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
  let codec = Tikitoken.create_codec tiki_token_bpe in
  let skip_tbl =
    Hashtbl.of_alist_exn (module String) (List.map skip_pkgs ~f:(fun p -> p, ()))
  in
  Odoc_crawler.crawl ~root ~f:(fun ~pkg ~doc_path ~markdown ->
    if not (Hashtbl.mem skip_tbl pkg)
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
    Embed_service.create ~sw ~clock:env#clock ~net ~codec ~rate_per_sec:1000 ()
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
  |> Eio.Fiber.List.iter (fun (pkg, docs) -> handler (pkg, docs));
  (* 3. Build & save package index with blurbs (serial, lightweight) *)
  let descriptions =
    Hashtbl.to_alist pkg_docs
    |> List.filter ~f:(fun (pkg, _) -> not (Hashtbl.mem skip_tbl pkg))
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
