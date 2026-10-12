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

let artifact_sources ?(checkpoint = Eio.Fiber.check) sources =
  sources
  |> List.map ~f:(fun (relative_path, contents) ->
    checkpoint ();
    Agent_store.Prompt_artifact_store.Source.create ~relative_path ~contents)
  |> Result.all
;;

let shell_digest (inspection : Chat_response.Agent_runtime.resource_inspection) =
  inspection.manifest.sha256
;;

let build_manifest
      ?(checkpoint = Eio.Fiber.check)
      definition
      ~canonical_source
      ~root
      ~sources
      ~shell_manifest_sha256
      ~programs_sha256
  =
  [%sexp
    { schema = (6 : int)
    ; prompt_definition_id =
        (definition.Prompt_definition.id : Agent_protocol.Id.Prompt_definition.t)
    ; canonical_source : string
    ; root_sha256 = (Chatmd_shell_spec.Source_ref.digest root : string)
    ; sources =
        (List.map sources ~f:(fun (path, contents) ->
           checkpoint ();
           path, Chatmd_shell_spec.Source_ref.digest contents)
         : (string * string) list)
    ; shell_manifest_sha256 : string
    ; programs_sha256 : string
    }]
  |> Sexp.to_string_mach
;;

let source_capture_loader source_loader root captures dependencies ~private_bytes =
  let captured_bytes = ref (String.length root) in
  if !captured_bytes > 8 * 1024 * 1024 then failwith "prompt source byte limit exceeded";
  let loader =
    source_loader
    |> Source_loader.with_read_limit ~max_bytes:(fun source ->
      match Hashtbl.find captures (Source_loader.relative_path source) with
      | Some contents -> String.length contents
      | None -> (8 * 1024 * 1024) - !captured_bytes - !private_bytes)
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
      if !captured_bytes > (8 * 1024 * 1024) - !private_bytes
      then failwith "prompt source byte limit exceeded";
      Hashtbl.set captures ~key:path ~data:contents)
    |> Source_loader.with_agent_observer ~f:(fun source ->
      Queue.enqueue dependencies source)
  in
  loader, captured_bytes
;;

module Capture = struct
  type t =
    { root : Prompt.Chat_markdown.Parsed.t
    ; agents : (string * Prompt.Chat_markdown.Parsed.t) list
    ; sources : (string * string) list
    ; shell_manifest_sha256 : string
    }

  let root t = t.root
  let agents t = t.agents
  let sources t = t.sources
  let shell_manifest_sha256 t = t.shell_manifest_sha256
end

let inspection_diagnostics diagnostics =
  List.map diagnostics ~f:(fun value ->
    let source =
      Option.map value.Chat_response.Agent_runtime.source ~f:(fun source -> source.file)
    in
    diagnostic ?source value.code value.message)
;;

let capture_loader ~loader:root_loader ~root_name ~env ~preprocess ~limits ~checkpoint =
  let poll () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let root_path = Source_loader.root_dir root_loader in
  let root_source =
    Source_loader.root root_loader ~file:root_name |> Result.ok_or_failwith
  in
  poll ();
  let authored =
    Source_loader.read_bounded
      ~max_bytes:
        (Int.min (Prompt.Chat_markdown.Limits.input_bytes limits) (8 * 1024 * 1024))
      root_loader
      root_source
    |> Result.ok_or_failwith
  in
  poll ();
  let captures = Hashtbl.create (module String) in
  let dependencies = Queue.create () in
  let private_bytes = ref 0 in
  let loader, authored_bytes =
    source_capture_loader root_loader authored captures dependencies ~private_bytes
  in
  let private_inputs = Hashtbl.create (module String) in
  let retain_effective parsed =
    let input = Prompt.Chat_markdown.Parsed.root_input parsed in
    let effective = Chatmd_parser_input.effective input in
    if not (Chatmd_parser_input.authored_matches input effective ~checkpoint:poll)
    then (
      let digest = Chatmd_parser_input.effective_sha256 input in
      match Hashtbl.find private_inputs digest with
      | Some previous ->
        if not (Chatmd_parser_input.effective_matches previous effective ~checkpoint:poll)
        then failwith "effective prompt digest collision"
      | None ->
        let length = Chatmd_parser_input.effective_byte_length input in
        if length > (8 * 1024 * 1024) - !authored_bytes - !private_bytes
        then failwith "prompt source byte limit exceeded";
        private_bytes := !private_bytes + length;
        Hashtbl.set private_inputs ~key:digest ~data:input);
    parsed
  in
  let prepare source contents =
    Prompt.Chat_markdown.prepare_chat_inputs
      ~source
      ~source_loader:loader
      ~dir:root_path
      ~preprocess
      ~limits
      ~checkpoint:poll
      contents
    |> retain_effective
  in
  let root = prepare root_name authored in
  let visited = Hash_set.create (module String) in
  Hash_set.add visited root_name;
  let agents = ref [] in
  while not (Queue.is_empty dependencies) do
    poll ();
    let source = Queue.dequeue_exn dependencies in
    let path = Source_loader.relative_path source in
    if not (Hash_set.mem visited path)
    then (
      if Hash_set.length visited >= 256
      then failwith "nested prompt source limit exceeded";
      Hash_set.add visited path;
      let contents =
        Source_loader.read_bounded
          ~max_bytes:
            (Int.min (Prompt.Chat_markdown.Limits.input_bytes limits) (8 * 1024 * 1024))
          loader
          source
        |> Result.ok_or_failwith
      in
      poll ();
      let parsed = prepare path contents in
      agents := (path, parsed) :: !agents)
  done;
  let inspection =
    Chat_response.Agent_runtime.inspect_resources
      ~env
      ~platform:(Chat_response.Agent_runtime.platform ())
      ~prompt_elements:(Prompt.Chat_markdown.Parsed.elements root)
  in
  poll ();
  Result.map_error inspection ~f:inspection_diagnostics
  |> Result.map ~f:(fun inspection ->
    let result : Capture.t =
      { root
      ; agents = List.rev !agents
      ; sources = source_values captures
      ; shell_manifest_sha256 = shell_digest inspection
      }
    in
    result)
;;

let capture_with_loader
      ~loader
      ~entry
      ~env
      ~preprocess
      ~limits
      ~checkpoint
      (_definition : Prompt_definition.t)
  =
  capture_loader
    ~loader
    ~root_name:(Source.Portable_path.to_string entry)
    ~env
    ~preprocess
    ~limits
    ~checkpoint
;;

let capture ~env ~preprocess ~limits ~checkpoint (definition : Prompt_definition.t) =
  let root_directory = Filename.dirname definition.root_file in
  let root_name = Filename.basename definition.root_file in
  let root_path = Eio.Path.(Eio.Stdenv.fs env / root_directory) in
  capture_loader
    ~loader:(Source_loader.filesystem ~root:root_path)
    ~root_name
    ~env
    ~preprocess
    ~limits
    ~checkpoint
;;

let capture_limits () =
  Prompt.Chat_markdown.Limits.create
    ~max_input_bytes:(8 * 1024 * 1024)
    ~max_parse_calls:1024
    ~max_tokens:100_000
    ~max_depth:128
  |> Or_error.ok_exn
;;

let install artifact_store ~transaction_id artifact =
  let module Store = Agent_store.Prompt_artifact_store in
  let open Result.Let_syntax in
  let%bind () =
    match Store.exists artifact_store artifact.Store.Artifact.revision_id with
    | true -> Ok ()
    | false -> Store.install artifact_store ~transaction_id artifact
  in
  let%bind retained = Store.load artifact_store artifact.revision_id in
  (* Revision IDs are content-derived, while the manifest includes the original
     creation timestamp. Catalog rebuilds must return the installed manifest,
     not a candidate with a new timestamp under the same revision ID. Verify all
     other candidate metadata before reusing that immutable artifact. *)
  match Store.Artifact.same_content artifact retained with
  | true -> Ok retained
  | false ->
    Error
      (Agent_store.Store_error.Corrupt
         "existing prompt revision differs from captured source")
;;

(* Use the same declaration/import semantics as restoration, without starting
   executable preprocessing during the closure-version preflight. *)
let validate_parser_elements ~parser_version elements =
  List.iter elements ~f:(function
    | Prompt.Chat_markdown.Tool (Persistent_agent _) when parser_version < 5 ->
      failwith "persistent agent declarations require prompt parser schema version 5"
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

module Content_input = struct
  type t =
    | Stored of Chatmd_prompt_content.Limits.t option
    | Fresh of Prompt_content_custody.Prepared.t
end

(* Pure captured replay establishes final artifact-relative source directories
   even before installation. Public restoration verifies the tree first. *)
let replay_custody ?(checkpoint = Eio.Fiber.check) ~content_input tree artifact =
  checkpoint ();
  let content_policy =
    match content_input with
    | Content_input.Stored limits -> Option.is_some limits
    | Fresh _ -> true
  in
  if
    Option.is_some artifact.Agent_store.Prompt_artifact_store.Artifact.content_bundle
    && ((not content_policy) || Option.is_none artifact.program_bundle)
  then failwith "Content custody requires explicit policy and retained parser inputs";
  let parser_version = artifact.parser_schema_version in
  if (parser_version < 1 || parser_version > 5) || artifact.runtime_schema_version <> 1
  then failwith "unsupported prompt parser/runtime schema version";
  let sources =
    (artifact.root_relative_path, artifact.root_chatmd)
    :: List.map artifact.sources ~f:(fun source -> source.relative_path, source.contents)
  in
  let loader = Source_loader.captured_filesystem ~root:tree ~sources in
  let root_source =
    Source_loader.root loader ~file:artifact.root_relative_path |> Result.ok_or_failwith
  in
  if parser_version < 5
  then validate_parser_closure ~parser_version ~dir:tree loader root_source;
  match artifact.program_bundle with
  | None ->
    let elements =
      Prompt.Chat_markdown.parse_chat_inputs
        ~source:artifact.root_relative_path
        ~source_loader:loader
        ~dir:tree
        artifact.root_chatmd
    in
    validate_parser_elements ~parser_version elements;
    tree, elements, None
  | Some storage ->
    let authored =
      Prompt_program_custody.authored ~root_file:artifact.root_relative_path ~sources
      |> Or_error.ok_exn
    in
    let custody =
      Prompt_program_custody.admit storage ~authored ~checkpoint
      |> Result.map_error ~f:(fun _ -> "Invalid retained parser/program custody")
      |> Result.ok_or_failwith
    in
    let parsed =
      Prompt_program_custody.replay
        ~checkpoint
        (Chatml_prompt_programs.root_input custody)
        ~authored
        ~dir:tree
      |> Or_error.ok_exn
    in
    Chatml_prompt_programs.validate_selection
      custody
      ~parsed
      ~limits:(Prompt_program_custody.limits ())
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> "Invalid retained active binding")
    |> Result.ok_or_failwith;
    let elements = Prompt.Chat_markdown.Parsed.elements parsed in
    validate_parser_elements ~parser_version elements;
    let content =
      match artifact.content_bundle, content_input with
      | None, Content_input.Stored _ ->
        if Prompt_content_custody.requires_captured_documents parsed ~checkpoint
        then failwith "Owned captured documents require retained content custody";
        None
      | Some storage, Stored (Some limits) ->
        let restored =
          Prompt_content_custody.restore
            storage
            ~programs:custody
            ~parsed
            ~limits
            ~checkpoint
          |> Result.map_error ~f:(fun _ -> "Invalid retained document custody")
          |> Result.ok_or_failwith
        in
        Some restored
      | Some _, Fresh prepared ->
        let restored =
          Prompt_content_custody.replay prepared ~parsed ~checkpoint
          |> Result.map_error ~f:(fun _ -> "Fresh document replay exceeds allowance")
          |> Result.ok_or_failwith
        in
        Some restored
      | None, Fresh _ -> failwith "Fresh content is absent from artifact"
      | Some _, Stored None -> assert false
    in
    tree, elements, Some (parsed, custody, content)
;;

let parse_tree ?(checkpoint = Eio.Fiber.check) ?content_limits tree artifact =
  checkpoint ();
  Agent_store.Prompt_artifact_store.verify_tree ~root:tree artifact
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
  |> Result.ok_or_failwith;
  replay_custody
    ~checkpoint
    ~content_input:(Content_input.Stored content_limits)
    tree
    artifact
;;

let prepared_revision
      ?(checkpoint = Eio.Fiber.check)
      ~definition
      ~artifact
      (tree, elements, custody)
  =
  checkpoint ();
  let revision =
    Prompt_revision.create ~definition ~artifact ~materialized_tree:tree ~elements
  in
  match custody with
  | None -> revision
  | Some (parser_context, programs, content) ->
    let revision =
      Prompt_revision.with_custody revision ~parser_context ~programs ~checkpoint
      |> Or_error.ok_exn
    in
    (match content with
     | None -> revision
     | Some restored ->
       Prompt_revision.with_content
         revision
         ~content:(Prompt_content_custody.Restored.content restored)
         ~materialized:(Prompt_content_custody.Restored.materialized restored)
         ~checkpoint
       |> Or_error.ok_exn)
;;

let parse_artifact ?checkpoint ?content_limits artifact_store artifact =
  parse_tree
    ?checkpoint
    ?content_limits
    (Agent_store.Prompt_artifact_store.materialized_tree
       artifact_store
       artifact.Agent_store.Prompt_artifact_store.Artifact.revision_id)
    artifact
;;

let reparse
      ?(checkpoint = Eio.Fiber.check)
      ?content_limits
      ~definition
      ~artifact
      ~materialized_tree
      ()
  =
  Prompt_parser_boundary.run
    ~checkpoint
    ~on_parser_error:(fun exn ->
      Error [ exception_error ~source:definition.Prompt_definition.root_file exn ])
    ~f:(fun checkpoint ->
      Ok
        (prepared_revision
           ~checkpoint
           ~definition
           ~artifact
           (parse_tree ~checkpoint ?content_limits materialized_tree artifact)))
;;

let restore ?content_limits ~artifact_store (definition : Prompt_definition.t) revision_id
  =
  match Agent_store.Prompt_artifact_store.load artifact_store revision_id with
  | Error error -> Error [ store_error error ]
  | Ok artifact ->
    (try
       Ok
         (prepared_revision
            ~definition
            ~artifact
            (parse_artifact ?content_limits artifact_store artifact))
     with
     | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
     | exn -> Error [ exception_error ~source:definition.root_file exn ])
;;

let publish_capture
      ?(checkpoint = Eio.Fiber.check)
      ?prepare_programs
      ?prepare_content
      ~source_receipt
      ~canonical_source
      ~artifact_store
      ~transaction_id
      ~created_at
      (definition : Prompt_definition.t)
      captured
  =
  let open Result.Let_syntax in
  (* Host preparation failures/exceptions retain their original identity. Its
     closure may own capabilities; only the immutable Capture is passed here. *)
  let%bind supplied =
    match prepare_programs with
    | None -> Ok None
    | Some prepare -> Result.map (prepare captured) ~f:Option.some
  in
  let%bind supplied_content =
    match prepare_content with
    | None -> Ok None
    | Some prepare -> Result.map (prepare captured) ~f:Option.some
  in
  Prompt_parser_boundary.run
    ~checkpoint
    ~on_parser_error:(fun exn ->
      Error [ exception_error ~source:definition.root_file exn ])
    ~f:(fun checkpoint ->
      checkpoint ();
      let root_input = Prompt.Chat_markdown.Parsed.root_input (Capture.root captured) in
      let root = Chatmd_parser_input.authored root_input in
      let sources = Capture.sources captured in
      let root_relative_path = Chatmd_parser_input.path root_input in
      let%bind authored =
        Prompt_program_custody.authored
          ~root_file:root_relative_path
          ~sources:((root_relative_path, root) :: sources)
        |> Result.map_error ~f:(fun e ->
          [ diagnostic "prompt.custody" (Error.to_string_hum e) ])
      in
      let%bind programs =
        Prompt_program_custody.prepare
          authored
          ~parsed_contexts:
            (Capture.root captured :: List.map (Capture.agents captured) ~f:snd)
          ~root:root_input
          ~supplied
          ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          [ diagnostic
              "prompt.custody"
              "Preparation custody does not match actual capture"
          ])
      in
      let contexts = Capture.root captured :: List.map (Capture.agents captured) ~f:snd in
      let%bind prepared_content =
        match supplied_content with
        | None ->
          let captured_slot =
            List.exists contexts ~f:(fun parsed ->
              Prompt_content_custody.requires_captured_documents parsed ~checkpoint)
          in
          if captured_slot
          then
            Error
              [ diagnostic
                  "prompt.content"
                  "Captured documents require completed source acquisition"
              ]
          else Ok None
        | Some supplied ->
          let limits = Chatmd_content_capture.limits supplied in
          let%map prepared =
            Prompt_content_custody.prepare
              ~parsed_contexts:contexts
              ~programs
              ~supplied:(Some supplied)
              ~limits
              ~checkpoint
            |> Result.map_error ~f:(fun _ ->
              [ diagnostic
                  "prompt.content"
                  "Document custody does not match actual capture or allowance"
              ])
          in
          Some prepared
      in
      let content =
        Option.map prepared_content ~f:Prompt_content_custody.Prepared.content
      in
      let canonical_source = canonical_source () in
      let shell_manifest_sha256 = Capture.shell_manifest_sha256 captured in
      let%bind programs_sha256 =
        Prompt_program_custody.fingerprint programs ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          [ diagnostic
              "prompt.custody"
              "Complete program manifest exceeds artifact policy"
          ])
      in
      let manifest =
        build_manifest
          ~checkpoint
          definition
          ~canonical_source
          ~root
          ~sources
          ~shell_manifest_sha256
          ~programs_sha256
      in
      let%bind manifest =
        match content with
        | None -> Ok manifest
        | Some content ->
          let%map digest =
            Prompt_content_custody.fingerprint content ~checkpoint
            |> Result.map_error ~f:(fun _ ->
              [ diagnostic
                  "prompt.content"
                  "Complete content manifest exceeds artifact policy"
              ])
          in
          manifest ^ "#document-content.v1:" ^ digest
      in
      let manifest =
        match source_receipt with
        | None -> manifest
        | Some receipt ->
          manifest
          ^ "#source-selection:"
          ^ Agent_store.Prompt_source_receipt.fingerprint receipt
      in
      let revision_id = revision_id manifest in
      let%bind source_values =
        artifact_sources ~checkpoint sources
        |> Result.map_error ~f:(fun error -> [ store_error error ])
      in
      let%bind artifact =
        Agent_store.Prompt_artifact_store.Artifact.create
          ~revision_id
          ~prompt_definition_id:definition.id
          ?source_receipt
          ~canonical_source
          ~root_relative_path
          ~root_chatmd:root
          ~sources:source_values
          ~parser_schema_version:5
          ~runtime_schema_version:1
          ~shell_manifest_sha256
          ~program_bundle:(Chatml_prompt_programs.to_storage programs)
          ?content_bundle:(Option.map content ~f:Chatmd_prompt_content.to_storage)
          ~created_at
          ()
        |> Result.map_error ~f:(fun e -> [ store_error e ])
      in
      let fresh_replay =
        Option.map prepared_content ~f:(fun prepared ->
          replay_custody
            ~checkpoint
            ~content_input:(Content_input.Fresh prepared)
            (Agent_store.Prompt_artifact_store.materialized_tree
               artifact_store
               revision_id)
            artifact)
      in
      checkpoint ();
      let%bind artifact =
        install artifact_store ~transaction_id artifact
        |> Result.map_error ~f:(fun e -> [ store_error e ])
      in
      let replayed =
        match fresh_replay with
        | None -> parse_artifact ~checkpoint artifact_store artifact
        | Some ((tree, _, _) as replayed) ->
          Agent_store.Prompt_artifact_store.verify_tree ~root:tree artifact
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
          |> Result.ok_or_failwith;
          replayed
      in
      let revision = prepared_revision ~checkpoint ~definition ~artifact replayed in
      checkpoint ();
      Ok revision)
;;

let build
      ?prepare_programs
      ?prepare_content
      ~env
      ~artifact_store
      ~transaction_id
      ~created_at
      (definition : Prompt_definition.t)
  =
  let captured =
    try
      capture
        ~env
        ~preprocess:Meta_prompting.Preprocessor.preprocess
        ~limits:(capture_limits ())
        ~checkpoint:Eio.Fiber.check
        definition
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn -> Error [ exception_error ~source:definition.root_file exn ]
  in
  Result.bind captured ~f:(fun captured ->
    publish_capture
      ?prepare_programs
      ?prepare_content
      ~source_receipt:None
      ~canonical_source:(fun () -> canonical_source ~env definition.root_file)
      ~artifact_store
      ~transaction_id
      ~created_at
      definition
      captured)
;;

let publish_selected_capture
      ?checkpoint
      ?prepare_programs
      ?prepare_content
      ~capture
      ~source_receipt
      ~root_member
      ~artifact_store
      ~transaction_id
      ~created_at
      (definition : Prompt_definition.t)
  =
  let module Receipt = Agent_store.Prompt_source_receipt in
  let root_input = Prompt.Chat_markdown.Parsed.root_input (Capture.root capture) in
  if
    (not
       (Agent_protocol.Id.Prompt_definition.equal
          definition.id
          (Receipt.definition_id source_receipt)))
    || (not (String.equal definition.root_file (Receipt.root_file source_receipt)))
    || not (String.equal (Chatmd_parser_input.path root_input) root_member)
  then
    Error
      [ diagnostic
          "prompt.source_selection"
          "Captured root does not match admitted original selection"
      ]
  else
    publish_capture
      ?checkpoint
      ?prepare_programs
      ?prepare_content
      ~source_receipt:(Some source_receipt)
      ~canonical_source:(fun () -> Receipt.root_file source_receipt)
      ~artifact_store
      ~transaction_id
      ~created_at
      definition
      capture
;;

let build_captured
      ?prepare_programs
      ?prepare_content
      ~capture
      ~source_receipt
      ~artifact_store
      ~transaction_id
      ~created_at
      (definition : Prompt_definition.t)
  =
  publish_selected_capture
    ?prepare_programs
    ?prepare_content
    ~capture
    ~source_receipt
    ~root_member:(Filename.basename definition.root_file)
    ~artifact_store
    ~transaction_id
    ~created_at
    definition
;;

let build_captured_entry
      ?checkpoint
      ?prepare_programs
      ?prepare_content
      ~entry
      ~capture
      ~artifact_store
      ~transaction_id
      ~created_at
      ()
  =
  publish_selected_capture
    ?checkpoint
    ?prepare_programs
    ?prepare_content
    ~capture
    ~source_receipt:(Prompt_source_entry.receipt entry)
    ~root_member:(Source.Portable_path.to_string (Prompt_source_entry.member entry))
    ~artifact_store
    ~transaction_id
    ~created_at
    (Prompt_source_entry.definition entry)
;;
