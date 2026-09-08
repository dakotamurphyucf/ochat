open! Core

module Diagnostic = struct
  type t =
    { code : string
    ; message : string
    ; source : string option
    }
  [@@deriving compare, equal, sexp]
end

let diagnostic ?source code message = Diagnostic.{ code; message; source }

let store_error error =
  diagnostic
    "prompt.artifact_store"
    (Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
;;

let exception_error ~source exn =
  diagnostic ~source "prompt.build_failed" (Exn.to_string exn)
;;

let canonical_source ~env path =
  let eio_path = Eio.Path.(Eio.Stdenv.fs env / path) in
  let stat = Eio.Path.stat ~follow:true eio_path in
  let native = Option.value (Eio.Path.native eio_path) ~default:path in
  sprintf "%s#%Ld:%Ld" native stat.dev stat.ino
;;

let revision_id manifest =
  let raw = Digestif.SHA256.(digest_string manifest |> to_raw_string) in
  let generator =
    Agent_protocol.Id.Generator.create ~bytes:(fun length -> String.prefix raw length)
  in
  Agent_protocol.Id.Prompt_revision.create_with generator
;;

let source_values captures =
  Hashtbl.to_alist captures
  |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
;;

let artifact_sources captures =
  source_values captures
  |> List.map ~f:(fun (relative_path, contents) ->
    Agent_store.Prompt_artifact_store.Source.create ~relative_path ~contents)
  |> Result.all
;;

let shell_digest inspection = inspection.Chat_response.Agent_runtime.manifest.sha256

let build_manifest definition ~canonical_source ~root ~sources ~shell_manifest_sha256 =
  [%sexp
    { schema = (4 : int)
    ; prompt_definition_id =
        (definition.Prompt_definition.id : Agent_protocol.Id.Prompt_definition.t)
    ; canonical_source : string
    ; root_sha256 = (Chatmd_shell_spec.Source_ref.digest root : string)
    ; sources =
        (List.map sources ~f:(fun (path, contents) ->
           path, Chatmd_shell_spec.Source_ref.digest contents)
         : (string * string) list)
    ; shell_manifest_sha256 : string
    }]
  |> Sexp.to_string_mach
;;

let source_capture_loader root_path root captures dependencies =
  let captured_bytes = ref (String.length root) in
  if !captured_bytes > 8 * 1024 * 1024 then failwith "prompt source byte limit exceeded";
  Source_loader.filesystem ~root:root_path
  |> Source_loader.with_observer ~f:(fun source contents ->
    let path = Source_loader.relative_path source in
    if
      Filename.is_absolute (Source_loader.file_name source)
      || List.mem (String.split path ~on:'/') ".." ~equal:String.equal
    then
      failwith
        "captured imports and scripts must remain beneath the root prompt directory";
    (match Hashtbl.find captures path with
     | Some previous when not (String.equal previous contents) ->
       failwith "prompt dependency changed during capture"
     | None when Hashtbl.length captures >= 255 ->
       failwith "prompt dependency file count limit exceeded"
     | _ -> ());
    let previous_bytes =
      Hashtbl.find captures path |> Option.value_map ~default:0 ~f:String.length
    in
    captured_bytes := !captured_bytes - previous_bytes + String.length contents;
    if !captured_bytes > 8 * 1024 * 1024 then failwith "prompt source byte limit exceeded";
    Hashtbl.set captures ~key:path ~data:contents)
  |> Source_loader.with_agent_observer ~f:(fun source ->
    Queue.enqueue dependencies source)
;;

let capture_nested_sources loader root_path root_name dependencies =
  let visited = Hash_set.create (module String) in
  Hash_set.add visited root_name;
  while not (Queue.is_empty dependencies) do
    let source = Queue.dequeue_exn dependencies in
    let path = Source_loader.relative_path source in
    if not (Hash_set.mem visited path)
    then (
      if Hash_set.length visited >= 256
      then failwith "nested prompt source limit exceeded";
      Hash_set.add visited path;
      let contents = Source_loader.read loader source |> Result.ok_or_failwith in
      ignore
        (Prompt.Chat_markdown.parse_chat_inputs
           ~source:path
           ~source_loader:loader
           ~dir:root_path
           contents
         : Prompt.Chat_markdown.top_level_elements list))
  done
;;

let parse_live ~env definition root =
  let root_directory = Filename.dirname definition.Prompt_definition.root_file in
  let root_name = Filename.basename definition.root_file in
  let root_path = Eio.Path.(Eio.Stdenv.fs env / root_directory) in
  let captures = Hashtbl.create (module String) in
  let dependencies = Queue.create () in
  let loader = source_capture_loader root_path root captures dependencies in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~source:root_name
      ~source_loader:loader
      ~dir:root_path
      root
  in
  capture_nested_sources loader root_path root_name dependencies;
  let inspection =
    Chat_response.Agent_runtime.inspect_shell
      ~env
      ~platform:(Chat_response.Agent_runtime.platform ())
      ~prompt_elements:elements
  in
  Result.map inspection ~f:(fun inspection -> elements, captures, inspection)
;;

let inspection_diagnostics diagnostics =
  List.map diagnostics ~f:(fun value ->
    let source =
      Option.map value.Chat_response.Agent_runtime.source ~f:(fun source -> source.file)
    in
    diagnostic ?source value.code value.message)
;;

let install artifact_store ~transaction_id artifact =
  if
    Agent_store.Prompt_artifact_store.exists
      artifact_store
      artifact.Agent_store.Prompt_artifact_store.Artifact.revision_id
  then Ok ()
  else Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id artifact
;;

(* Use the same declaration/import semantics as restoration, without starting
   executable preprocessing during the closure-version preflight. *)
let validate_parser_elements ~parser_version elements =
  List.iter elements ~f:(function
    | Prompt.Chat_markdown.Authoring_help _ when parser_version < 4 ->
      failwith "authoring help declarations require prompt parser schema version 4"
    | Prompt.Chat_markdown.Tool (Inherited _) when parser_version < 3 ->
      failwith "inherited tool references require prompt parser schema version 3"
    | (Extension_script _ | Tool (Extension _) | Authoring_context _)
      when parser_version < 2 ->
      failwith "extension declarations require prompt parser schema version 2"
    | _ -> ())
;;

let validate_parser_closure ~parser_version ~dir loader root_source =
  let pending = Queue.create ()
  and visited = Hash_set.create (module String) in
  let loader = Source_loader.with_agent_observer loader ~f:(Queue.enqueue pending) in
  Queue.enqueue pending root_source;
  let bytes = ref 0 in
  while not (Queue.is_empty pending) do
    let source = Queue.dequeue_exn pending in
    let path = Source_loader.relative_path source in
    if not (Hash_set.mem visited path)
    then (
      if Hash_set.length visited >= 256
      then failwith "prompt source closure limit exceeded";
      Hash_set.add visited path;
      let contents =
        Source_loader.read_bounded ~max_bytes:((8 * 1024 * 1024) - !bytes) loader source
        |> Result.ok_or_failwith
      in
      bytes := !bytes + String.length contents;
      Prompt.Chat_markdown.parse_chat_inputs_without_preprocessing
        ~source:path
        ~source_loader:loader
        ~dir
        contents
      |> validate_parser_elements ~parser_version)
  done
;;

let parse_artifact artifact_store artifact =
  let parser_version =
    artifact.Agent_store.Prompt_artifact_store.Artifact.parser_schema_version
  in
  if (parser_version < 1 || parser_version > 4) || artifact.runtime_schema_version <> 1
  then failwith "unsupported prompt parser/runtime schema version";
  Agent_store.Prompt_artifact_store.verify_materialized_tree artifact_store artifact
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
  |> Result.ok_or_failwith;
  let tree =
    Agent_store.Prompt_artifact_store.materialized_tree
      artifact_store
      artifact.Agent_store.Prompt_artifact_store.Artifact.revision_id
  in
  let sources =
    (artifact.root_relative_path, artifact.root_chatmd)
    :: List.map artifact.sources ~f:(fun source -> source.relative_path, source.contents)
  in
  let loader = Source_loader.captured_filesystem ~root:tree ~sources in
  let root_source =
    Source_loader.root loader ~file:artifact.root_relative_path |> Result.ok_or_failwith
  in
  if parser_version < 4
  then validate_parser_closure ~parser_version ~dir:tree loader root_source;
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~source:artifact.root_relative_path
      ~source_loader:loader
      ~dir:tree
      artifact.root_chatmd
  in
  validate_parser_elements ~parser_version elements;
  tree, elements
;;

let restore ~artifact_store (definition : Prompt_definition.t) revision_id =
  match Agent_store.Prompt_artifact_store.load artifact_store revision_id with
  | Error error -> Error [ store_error error ]
  | Ok artifact ->
    (try
       let tree, elements = parse_artifact artifact_store artifact in
       Ok (Prompt_revision.create ~definition ~artifact ~materialized_tree:tree ~elements)
     with
     | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
     | exn -> Error [ exception_error ~source:definition.root_file exn ])
;;

let build
      ~env
      ~artifact_store
      ~transaction_id
      ~created_at
      (definition : Prompt_definition.t)
  =
  try
    let root = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / definition.root_file) in
    match parse_live ~env definition root with
    | Error diagnostics -> Error (inspection_diagnostics diagnostics)
    | Ok (_elements, captures, inspection) ->
      let sources = source_values captures in
      let canonical_source = canonical_source ~env definition.root_file in
      let shell_manifest_sha256 = shell_digest inspection in
      let manifest =
        build_manifest definition ~canonical_source ~root ~sources ~shell_manifest_sha256
      in
      let revision_id = revision_id manifest in
      let open Result.Let_syntax in
      let result =
        let%bind sources = artifact_sources captures |> Result.map_error ~f:List.return in
        let%bind artifact =
          Agent_store.Prompt_artifact_store.Artifact.create
            ~revision_id
            ~prompt_definition_id:definition.id
            ~canonical_source
            ~root_relative_path:(Filename.basename definition.root_file)
            ~root_chatmd:root
            ~sources
            ~parser_schema_version:4
            ~runtime_schema_version:1
            ~shell_manifest_sha256
            ~created_at
            ()
          |> Result.map_error ~f:(fun error -> [ error ])
        in
        let%bind () =
          install artifact_store ~transaction_id artifact
          |> Result.map_error ~f:List.return
        in
        Ok artifact
      in
      (match result with
       | Error errors -> Error (List.map errors ~f:store_error)
       | Ok artifact ->
         let tree, elements = parse_artifact artifact_store artifact in
         Ok
           (Prompt_revision.create
              ~definition
              ~artifact
              ~materialized_tree:tree
              ~elements))
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error [ exception_error ~source:definition.root_file exn ]
;;
