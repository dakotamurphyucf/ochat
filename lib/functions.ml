open Core
open Io
module Output = Openai.Responses.Tool_output.Output

let add_line_numbers str =
  let lines = String.split_lines str in
  let numbered_lines =
    List.mapi ~f:(fun i line -> Printf.sprintf "%d. %s" (i + 1) line) lines
  in
  String.concat ~sep:"\n" numbered_lines
;;

let is_text_char = function
  | ' ' .. '~' (* ASCII printable *)
  | '\n' | '\r' | '\t' (* common whitespace: LF, CR, TAB *) -> true
  | c ->
    (* Treat non-ASCII bytes as “text” (UTF-8 payload or legacy encodings). *)
    Char.to_int c >= 0x80
;;

let has_nul s = String.exists s ~f:(fun c -> Char.to_int c = 0x00)

let is_utf8 s =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop () =
    match Uutf.decode dec with
    | `Uchar _ | `Await -> loop ()
    | `End -> true
    | `Malformed _ -> false
  in
  loop ()
;;

let is_text s =
  (* NUL is a strong binary signal; reject early *)
  if has_nul s then false else String.for_all s ~f:is_text_char && is_utf8 s
;;

(* prevent path for binary files like gif/image/ect *)
let is_binary_file ~dir path =
  (* Fallback: try to guess from file content (very basic) *)
  let content = Io.load_doc ~dir path in
  not (is_text content)
;;

type read_file_root =
  { id : string
  ; path : Eio.Fs.dir_ty Eio.Path.t
  ; description : string option
  }

type read_file_input =
  { file : string
  ; root : string option
  ; offset : int option
  ; line_count : int option
  }

let read_file_root ~id ~path ?description () = { id; path; description }

let read_file_input_of_string value =
  let json = Jsonaf.of_string value in
  let file =
    Option.first_some (Jsonaf.member "file" json) (Jsonaf.member "path" json)
    |> Option.map ~f:Jsonaf.string_exn
    |> Option.value ~default:""
  in
  { file
  ; root = Option.map (Jsonaf.member "root" json) ~f:Jsonaf.string_exn
  ; offset = Option.map (Jsonaf.member "offset" json) ~f:Jsonaf.int_exn
  ; line_count = Option.map (Jsonaf.member "line_count" json) ~f:Jsonaf.int_exn
  }
;;

let read_file_parameters roots =
  let root_property =
    match roots with
    | [] -> []
    | roots ->
      [ ( "root"
        , `Object
            [ "type", `String "string"
            ; "enum", `Array (List.map roots ~f:(fun root -> `String root.id))
            ; ( "description"
              , `String
                  "Named allowed root. Omit to resolve file relative to ochat's launch \
                   directory; the resulting path must still be inside an allowed root." )
            ] )
      ]
  in
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          ([ ( "file"
             , `Object
                 [ "type", `String "string"
                 ; ( "description"
                   , `String
                       "Path to an existing regular UTF-8 text file. When root is \
                        provided, this path must be relative to that root." )
                 ] )
           ; ( "offset"
             , `Object
                 [ "type", `String "integer"
                 ; "minimum", `Number "0"
                 ; ( "description"
                   , `String
                       "Optional 0-based line offset. Omit it to start at the first line."
                   )
                 ] )
           ; ( "line_count"
             , `Object
                 [ "type", `String "integer"
                 ; "minimum", `Number "0"
                 ; ( "description"
                   , `String
                       "Optional maximum number of lines to return. Omit it to return \
                        the rest of the file, subject to internal output limits." )
                 ] )
           ]
           @ root_property) )
    ; "required", `Array [ `String "file" ]
    ; "additionalProperties", `False
    ]
;;

let root_description root =
  let suffix =
    Option.value_map root.description ~default:"" ~f:(fun value -> " — " ^ value)
  in
  sprintf "- %s: %s%s" root.id (Eio.Path.native_exn root.path) suffix
;;

let read_file_description roots custom =
  let base =
    "Reads a UTF-8 text file and returns a line-range header followed by its contents.\n\
     Relative file paths without root are resolved against the directory where ochat was \
     launched.\n\n\
     Parameters:\n\
     - file: Path to the file to read.\n\
     - offset: Optional 0-based line offset. Omit it to start at line 0.\n\
     - line_count: Optional maximum number of lines to return. Omit it to return the \
     rest of the file, subject to internal output limits."
  in
  let roots =
    match roots with
    | [] -> ""
    | roots ->
      "\n\nAllowed roots:\n"
      ^ String.concat ~sep:"\n" (List.map roots ~f:root_description)
      ^ "\n\n\
         To select a named root, pass root and a relative file path. Absolute paths are \
         accepted only when they remain inside an allowed root. Paths resolved from the \
         launch directory must also remain inside an allowed root."
  in
  base ^ roots ^ Option.value_map custom ~default:"" ~f:(fun value -> "\n\n" ^ value)
;;

let is_binary_path path =
  Eio.Path.with_open_in path
  @@ fun flow ->
  Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all flow |> is_text |> not
;;

let read_file_contents ~display_path path offset line_count =
  let read () =
    if is_binary_path path
    then
      failwith
        (Printf.sprintf "Refusing to read binary file: %s" (Eio.Path.native_exn path));
    Eio.Path.with_open_in path
    @@ fun flow ->
    try
      let r = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
      let total_bytes_limit = 380_928 in
      let taken_bytes = ref 0 in
      let truncated = ref false in
      let start_line_0 = Option.value ~default:0 offset in
      let requested_count = line_count in
      let current_line_0 = ref 0 in
      let returned_lines = ref 0 in
      let total_lines_in_file = ref 0 in
      let collected = ref [] in
      let push_line s =
        collected := s :: !collected;
        incr returned_lines
      in
      (* Read the whole file as a line stream once so we can compute total lines
           and also return the requested slice, while still honoring the byte cap. *)
      let lines = r |> Eio.Buf_read.(seq line) in
      Seq.iter
        (fun s ->
           (* Always count total lines, even if we don't return them. *)
           incr total_lines_in_file;
           let this_line_0 = !current_line_0 in
           incr current_line_0;
           let should_consider =
             this_line_0 >= start_line_0
             &&
             match requested_count with
             | None -> true
             | Some n -> !returned_lines < Int.max 0 n
           in
           if should_consider
           then
             if
               (* Enforce byte limit while building returned payload *)
               !taken_bytes < total_bytes_limit
             then (
               taken_bytes := !taken_bytes + String.length s;
               if !taken_bytes <= total_bytes_limit
               then push_line s
               else truncated := true)
             else truncated := true)
        lines;
      let result_body = List.rev !collected |> String.concat ~sep:"\n" in
      let header =
        (* ripgrep-like header: file:START-END:
             Keep the first line in "file:line:..." shape so terminals can linkify it.
             Put extra metadata on a second line that won't interfere with link selection. *)
        let start_1 = start_line_0 + 1 in
        let end_1 =
          (* If we returned 0 lines, show an empty range as START-START (1-based). *)
          if !returned_lines = 0 then start_1 else start_line_0 + !returned_lines
        in
        Printf.sprintf
          "%s:%d-%d:\n[total_lines=%d]\n"
          display_path
          start_1
          end_1
          !total_lines_in_file
      in
      let result = header ^ result_body in
      if !truncated
      then (
        let remaining_lines =
          Int.max 0 (!total_lines_in_file - (start_line_0 + !returned_lines))
        in
        Printf.sprintf
          "%s\n\n---\n[File truncated: %d more lines not shown]"
          result
          remaining_lines)
      else result
    with
    | Eio.Exn.Io _ as ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
  in
  match read () with
  | res -> res
  | exception ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
;;

let create_read_file ~description ~parameters resolve =
  let module M : Ochat_function.Def with type input = read_file_input = struct
    type input = read_file_input

    let name = "read_file"
    let type_ = "function"
    let description = Some description
    let parameters = parameters
    let input_of_string = read_file_input_of_string
  end
  in
  Ochat_function.create_function
    (module M)
    ~strict:false
    (fun input ->
       let input_error =
         if Option.exists input.offset ~f:(fun value -> value < 0)
         then Some "offset must be non-negative"
         else if Option.exists input.line_count ~f:(fun value -> value < 0)
         then Some "line_count must be non-negative"
         else None
       in
       match input_error with
       | Some message -> Output.Text ("error running read_file: " ^ message)
       | None ->
         (match resolve input with
          | Error message -> Output.Text ("error running read_file: " ^ message)
          | Ok (display_path, path) ->
            Output.Text
              (read_file_contents ~display_path path input.offset input.line_count)))
;;

let get_contents ~dir : Ochat_function.t =
  let resolve input = Ok (input.file, Eio.Path.(dir / input.file)) in
  create_read_file
    ~description:(read_file_description [] None)
    ~parameters:(read_file_parameters [])
    resolve
;;

type canonical_read_root =
  { configured : read_file_root
  ; canonical_path : string
  }

let absolute_native_path path =
  let native = Eio.Path.native_exn path in
  if Filename.is_relative native
  then Filename.concat (Stdlib.Sys.getcwd ()) native
  else native
;;

let canonical_read_root ~fs configured =
  { configured
  ; canonical_path =
      Shell_access.Path_util.canonical ~fs (absolute_native_path configured.path)
  }
;;

let path_is_under ~root path =
  String.equal root path
  || String.is_prefix
       path
       ~prefix:(if String.equal root "/" then "/" else root ^ Filename.dir_sep)
;;

let candidate_path ~fs ~dir roots input =
  match input.root with
  | None ->
    Ok
      (if Filename.is_relative input.file
       then Eio.Path.(dir / input.file)
       else Eio.Path.(fs / input.file))
  | Some id ->
    (match List.find roots ~f:(fun root -> String.equal root.configured.id id) with
     | None -> Error (sprintf "unknown root %S" id)
     | Some root when not (Filename.is_relative input.file) ->
       Error "file must be relative when root is provided"
     | Some root -> Ok Eio.Path.(root.configured.path / input.file))
;;

let regular_file path =
  match Eio.Path.kind ~follow:true path with
  | `Regular_file -> Ok ()
  | `Not_found -> Error "file does not exist"
  | ( `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Symbolic_link
    | `Socket ) as kind ->
    Error (Fmt.str "refusing non-regular file (%a)" Eio.File.Stat.pp_kind kind)
;;

let scoped_resolver ~fs ~dir roots input =
  if String.is_empty input.file
  then Error "file must not be empty"
  else if String.mem input.file '\000'
  then Error "file must not contain NUL"
  else
    Result.bind (candidate_path ~fs ~dir roots input) ~f:(fun candidate ->
      Result.try_with (fun () ->
        Shell_access.Path_util.canonical ~fs (absolute_native_path candidate))
      |> Result.map_error ~f:Exn.to_string
      |> Result.bind ~f:(fun canonical_path ->
        if
          not
            (List.exists roots ~f:(fun root ->
               path_is_under ~root:root.canonical_path canonical_path))
        then Error "requested file is outside the configured read roots"
        else (
          let path = Eio.Path.(fs / canonical_path) in
          Result.map (regular_file path) ~f:(fun () -> input.file, path))))
;;

let get_contents_scoped ~fs ~dir ~roots ?description () =
  if List.is_empty roots then invalid_arg "get_contents_scoped: roots must not be empty";
  List.iter roots ~f:(fun root ->
    if String.is_empty (String.strip root.id)
    then invalid_arg "get_contents_scoped: root ids must not be empty");
  (match
     List.find_a_dup (List.map roots ~f:(fun root -> root.id)) ~compare:String.compare
   with
   | None -> ()
   | Some id -> invalid_argf "get_contents_scoped: duplicate root id %S" id ());
  let canonical_roots = List.map roots ~f:(canonical_read_root ~fs) in
  let description_roots =
    List.map canonical_roots ~f:(fun root ->
      { root.configured with path = Eio.Path.(fs / root.canonical_path) })
  in
  create_read_file
    ~description:(read_file_description description_roots description)
    ~parameters:(read_file_parameters roots)
    (scoped_resolver ~fs ~dir canonical_roots)
;;

let append_to_file ~dir : Ochat_function.t =
  let f (path, content) =
    try
      Io.append_doc ~dir path ("\n" ^ content);
      Printf.sprintf "Content appended to %s successfully." path
    with
    | ex -> Fmt.str "error running append_to_file: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Append_to_file)
    (fun args -> Output.Text (f args))
;;

let find_and_replace ~dir : Ochat_function.t =
  let f (path, search, replace, all) =
    try
      let content = Io.load_doc ~dir path in
      let match_idexes =
        String.substr_index_all ~may_overlap:false ~pattern:search content
      in
      match all, match_idexes with
      | _, [] -> Printf.sprintf "No occurrences of '%s' found in %s." search path
      | false, _ :: [] ->
        let new_content =
          String.substr_replace_first content ~pattern:search ~with_:replace
        in
        Io.save_doc ~dir path new_content;
        Printf.sprintf
          "Replaced first occurrence of '%s' with '%s' in %s successfully."
          search
          replace
          path
      | false, _ :: _ ->
        sprintf
          "Error Found multiple occurrences of '%s' in %s, but all set to false. Use \
           apply_patch instead."
          search
          path
      | true, _ ->
        let new_content =
          String.substr_replace_all content ~pattern:search ~with_:replace
        in
        Io.save_doc ~dir path new_content;
        Printf.sprintf "Replaced '%s' with '%s' in %s successfully." search replace path
    with
    | ex -> Fmt.str "error running find_and_replace: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Find_and_replace)
    (fun args -> Output.Text (f args))
;;

let get_url_content ~net : Ochat_function.t =
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
  Ochat_function.create_function
    (module Definitions.Get_url_content)
    (fun args -> Output.Text (f args))
;;

let index_ocaml_code ~env ~dir ~net : Ochat_function.t =
  let f (folder_to_index, vector_db_folder) =
    Eio.Switch.run
    @@ fun sw ->
    let pool =
      Eio.Executor_pool.create
        ~sw
        (Eio.Stdenv.domain_mgr env)
        ~domain_count:(Domain.recommended_domain_count () - 1)
    in
    Indexer.index ~dir ~pool ~net ~vector_db_folder ~folder_to_index;
    "code has been indexed"
  in
  Ochat_function.create_function
    (module Definitions.Index_ocaml_code)
    (fun args -> Output.Text (f args))
;;

let query_vector_db ~dir ~net : Ochat_function.t =
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
  Ochat_function.create_function
    (module Definitions.Query_vector_db)
    (fun args -> Output.Text (f args))
;;

let apply_patch ~dir : Ochat_function.t =
  let split path =
    Eio.Path.split (dir / path)
    |> Option.map ~f:(fun ((_, dirname), basename) -> dirname, basename)
  in
  let f patch =
    let open_fn path = Io.load_doc ~dir path in
    let write_fn path s =
      match split path with
      | Some (dirname, _) ->
        (match Io.is_dir ~dir dirname with
         | true -> Io.save_doc ~dir path s
         | false ->
           Io.mkdir ~exists_ok:true ~dir dirname;
           Io.save_doc ~dir path s)
      | None -> Io.save_doc ~dir path s
    in
    let remove_fn path = Io.delete_doc ~dir path in
    match Apply_patch.process_patch ~text:patch ~open_fn ~write_fn ~remove_fn with
    | _, snippets ->
      let format_snippet (path, snip) =
        let header =
          Printf.sprintf
            "┏━[ %s ]%s"
            path
            (String.concat @@ List.init 70 ~f:(fun _ -> "-"))
        in
        let footer = String.concat @@ List.init 42 ~f:(fun _ -> "") in
        String.concat ~sep:"\n" [ header; snip; footer ]
      in
      let snippets_text =
        String.concat ~sep:"\n\n" (List.map ~f:format_snippet snippets)
      in
      Printf.sprintf "✅ Patch applied successfully!\n\n%s" snippets_text
    | exception Apply_patch.Diff_error err -> Apply_patch.error_to_string err
    | exception ex -> Fmt.str "error running apply_patch: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Apply_patch)
    (fun args -> Output.Text (f args))
;;

let read_dir ~dir : Ochat_function.t =
  let f path =
    match Io.directory ~dir path with
    | res -> String.concat ~sep:"\n" res
    | exception ex -> Fmt.str "error running read_directory: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Read_directory)
    (fun args -> Output.Text (f args))
;;

let mkdir ~dir : Ochat_function.t =
  let f path =
    match Io.mkdir ~exists_ok:true ~dir path with
    | () -> sprintf "Directory %s created successfully." path
    | exception ex -> Fmt.str "error running mkdir: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Make_dir)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Meta-prompting – recursive refinement tool                                 *)
(* -------------------------------------------------------------------------- *)

let meta_refine ~env : Ochat_function.t =
  let f (prompt_raw, task) =
    let open Meta_prompting in
    let action =
      match String.is_empty prompt_raw with
      | true -> Context.Generate
      | false -> Context.Update
    in
    Mp_flow.first_flow ~env ~prompt:prompt_raw ~task ~action ()
  in
  Ochat_function.create_function
    (module Definitions.Meta_refine)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* ODoc search – vector-based snippet retrieval                                 *)
(* -------------------------------------------------------------------------- *)

let odoc_search ~dir ~net : Ochat_function.t =
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
      if String.equal package "all" then Eio.Path.read_dir index_path else [ package ]
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
  Ochat_function.create_function
    (module Definitions.Odoc_search)
    ~strict:false
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Webpage → Markdown tool                                                     *)
(* -------------------------------------------------------------------------- *)

let webpage_to_markdown ~env ~dir ~net : Ochat_function.t =
  Webpage_markdown.Tool.register ~env ~dir ~net
;;

(* -------------------------------------------------------------------------- *)
(*  Fork stub – placeholder implementation                                     *)
(* -------------------------------------------------------------------------- *)

let fork : Ochat_function.t =
  let impl (_ : Definitions.Fork.input) =
    "[fork-tool placeholder – should never be called directly]"
  in
  Ochat_function.create_function
    (module Definitions.Fork)
    (fun args -> Output.Text (impl args))
;;

(* -------------------------------------------------------------------------- *)
(* Markdown indexing – build vector store                                      *)
(* -------------------------------------------------------------------------- *)

let index_markdown_docs ~env ~dir : Ochat_function.t =
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
  Ochat_function.create_function
    (module Definitions.Index_markdown_docs)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Markdown search – semantic retrieval                                        *)
(* -------------------------------------------------------------------------- *)

let markdown_search ~dir ~net : Ochat_function.t =
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
      | Some "all" | None ->
        (* fallback list all dirs *)
        List.filter (Eio.Path.read_dir index_dir) ~f:(fun entry ->
          Eio.Path.is_directory (index_dir / entry))
      | Some name -> [ name ]
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
  Ochat_function.create_function
    (module Definitions.Markdown_search)
    ~strict:false
    (fun args -> Output.Text (f args))
;;

let import_image ~dir : Ochat_function.t =
  let f image_path =
    let open Eio.Path in
    let img_full_path = dir / image_path in
    if not (is_file img_full_path)
    then Output.Text (Printf.sprintf "Image file %s does not exist." image_path)
    else (
      let image_url = Io.Base64.file_to_data_uri ~dir image_path in
      Output.Content [ Input_image { image_url; detail = Some Auto } ])
  in
  Ochat_function.create_function (module Definitions.Import_image) f
;;
