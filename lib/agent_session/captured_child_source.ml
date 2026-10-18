open Core
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module Store = Agent_store.Prompt_artifact_store

type t =
  { parent_revision_id : P.Id.Prompt_revision.t
  ; parent_manifest_sha256 : string
  ; root_relative_path : string
  ; parent_artifact : Store.Artifact.t
  ; root_chatmd : string
  ; sources : Store.Source.t list
  ; selected_source_bytes : int
  ; programs : Chatml_prompt_programs.t option
  ; programs_sha256 : string option
  ; content : Chatmd_prompt_content.t option
  ; content_sha256 : string option
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; parser_context : CM.Parsed.t option
  ; content_projection : Prompt_content_custody.Restored.t option
  }

let root_relative_path t = t.root_relative_path
let selected_source_bytes t = t.selected_source_bytes
let programs_sha256 t = t.programs_sha256
let content_sha256 t = t.content_sha256
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let legacy_agent_member tree ~declaration =
  let open Option.Let_syntax in
  let%bind root = Eio.Path.native tree in
  let%bind reference = String.chop_prefix declaration.CM.agent ~prefix:(root ^ "/") in
  (* Authored imports preserve paths such as definitions/../agents/child.chatmd.
     Normalize only the portion beneath the verified artifact tree; never consult
     realpath or follow an external live-file alias to decide source identity. *)
  let loader = Source_loader.confined_filesystem ~root:tree in
  Source_loader.root loader ~file:reference
  |> Result.ok
  |> Option.map ~f:Source_loader.relative_path
;;

let capture_member ~parent ~relative_path ~content_limits ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let parent_artifact = Prompt_revision.artifact parent in
  let%bind root_source =
    Store.Source.create
      ~relative_path:parent_artifact.root_relative_path
      ~contents:parent_artifact.root_chatmd
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let all_sources = root_source :: parent_artifact.sources in
  let tree = Prompt_revision.materialized_tree parent in
  let%bind selected =
    List.find all_sources ~f:(fun source ->
      checkpoint ();
      Option.value_map
        relative_path
        ~default:false
        ~f:(String.equal source.Store.Source.relative_path))
    |> Result.of_option
         ~error:
           (P.Error.create
              Permission_denied
              ~message:"captured child is not in the parent's captured source closure"
              ~retryable:false
              ())
  in
  let sources =
    List.filter all_sources ~f:(fun source ->
      checkpoint ();
      not (String.equal source.Store.Source.relative_path selected.relative_path))
  in
  let%bind programs, child_parsed =
    match parent_artifact.program_bundle with
    | None -> Ok (None, None)
    | Some storage ->
      let%bind parent_authored =
        Prompt_program_custody.authored
          ~root_file:parent_artifact.root_relative_path
          ~sources:
            (List.map all_sources ~f:(fun s ->
               checkpoint ();
               s.Store.Source.relative_path, s.contents))
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained authored inventory"
            ~retryable:false
            ())
      in
      let%bind custody =
        Prompt_program_custody.admit storage ~authored:parent_authored ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained program custody"
            ~retryable:false
            ())
      in
      let%bind root_input =
        List.find (Chatml_prompt_programs.parser_inputs custody) ~f:(fun input ->
          checkpoint ();
          Chatmd_parser_input.Role.equal (Chatmd_parser_input.role input) Root
          && String.equal (Chatmd_parser_input.path input) selected.relative_path)
        |> Result.of_option
             ~error:
               (P.Error.create
                  Prompt_unavailable
                  ~message:"Retained child has no captured effective Root"
                  ~retryable:false
                  ())
      in
      let%bind authored =
        Prompt_program_custody.authored
          ~root_file:selected.relative_path
          ~sources:
            (List.map all_sources ~f:(fun s ->
               checkpoint ();
               s.Store.Source.relative_path, s.contents))
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained child inventory"
            ~retryable:false
            ())
      in
      let%bind parsed =
        Prompt_program_custody.replay ~checkpoint root_input ~authored ~dir:tree
        |> Result.map_error ~f:(fun error ->
          P.Error.create
            Prompt_unavailable
            ~message:(Error.to_string_hum error)
            ~retryable:false
            ())
      in
      let%map child =
        Chatml_prompt_programs.reroot
          custody
          ~authored
          ~parsed
          ~programs:[]
          ~limits:(Prompt_program_custody.limits ())
          ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained child selection"
            ~retryable:false
            ())
      in
      Some child, Some parsed
  in
  let%bind programs_sha256 =
    match programs with
    | None -> Ok None
    | Some programs ->
      Prompt_program_custody.fingerprint programs ~checkpoint
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun _ ->
        P.Error.create
          Prompt_unavailable
          ~message:"Complete child program manifest exceeds artifact policy"
          ~retryable:false
          ())
  in
  let%bind content, content_sha256, content_limits, content_projection =
    match parent_artifact.content_bundle, programs, child_parsed, content_limits with
    | None, _, parsed, _ ->
      if
        Option.exists parsed ~f:(fun parsed ->
          Prompt_content_custody.requires_captured_documents parsed ~checkpoint)
      then
        Error
          (P.Error.create
             Prompt_unavailable
             ~message:"Owned child captured documents require retained content custody"
             ~retryable:false
             ())
      else Ok (None, None, None, None)
    | Some storage, Some programs, Some parsed, Some limits ->
      let%bind restored =
        Prompt_content_custody.restore storage ~programs ~parsed ~limits ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained child content or content allowance"
            ~retryable:false
            ())
      in
      let content = Prompt_content_custody.Restored.content restored in
      let%map digest =
        Prompt_content_custody.fingerprint content ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Complete child content manifest exceeds artifact policy"
            ~retryable:false
            ())
      in
      Some content, Some digest, Some limits, Some restored
    | Some _, _, _, _ ->
      Error
        (P.Error.create
           Prompt_unavailable
           ~message:
             "Retained child content requires parser custody and explicit content limits"
           ~retryable:false
           ())
  in
  let selected_source_bytes =
    List.fold sources ~init:(String.length selected.contents) ~f:(fun bytes source ->
      checkpoint ();
      bytes + String.length source.Store.Source.contents)
  in
  Ok
    { parent_revision_id = parent_artifact.revision_id
    ; parent_manifest_sha256 = parent_artifact.manifest_sha256
    ; root_relative_path = selected.relative_path
    ; parent_artifact
    ; root_chatmd = selected.contents
    ; sources
    ; selected_source_bytes
    ; programs
    ; programs_sha256
    ; content
    ; content_sha256
    ; content_limits
    ; parser_context = child_parsed
    ; content_projection
    }
;;

let authored_sources t =
  Prompt_program_custody.authored
    ~root_file:t.root_relative_path
    ~sources:
      ((t.root_relative_path, t.root_chatmd)
       :: List.map t.sources ~f:(fun s -> s.Store.Source.relative_path, s.contents))
  |> Or_error.ok_exn
;;

let with_programs t incoming ~checkpoint =
  let open Result.Let_syntax in
  let%bind previous =
    match t.programs with
    | Some previous -> Ok previous
    | None -> denied "Legacy source has no retained effective child Root"
  in
  let expected = Chatml_prompt_programs.root_input previous
  and actual = Chatml_prompt_programs.root_input incoming in
  let%bind () =
    if
      Chatmd_parser_input.Role.equal (Chatmd_parser_input.role actual) Root
      && String.equal (Chatmd_parser_input.path actual) t.root_relative_path
      && Chatmd_parser_input.authored_matches
           expected
           (Chatmd_parser_input.authored actual)
           ~checkpoint
      && Chatmd_parser_input.effective_matches
           expected
           (Chatmd_parser_input.effective actual)
           ~checkpoint
    then Ok ()
    else denied "Incoming custody does not select this retained child's exact Root"
  in
  let%bind programs =
    Chatml_prompt_programs.adopt
      incoming
      ~previous
      ~authored:(authored_sources t)
      ~limits:(Prompt_program_custody.limits ())
      ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Prompt_unavailable
        ~message:"Incoming custody changed retained source inventory or unknown ownership"
        ~retryable:false
        ())
  in
  let%map programs_sha256 =
    Prompt_program_custody.fingerprint programs ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Prompt_unavailable
        ~message:"Complete child program manifest exceeds artifact policy"
        ~retryable:false
        ())
  in
  { t with programs = Some programs; programs_sha256 = Some programs_sha256 }
;;

let artifact t ~revision_id ~created_at =
  Store.Artifact.create
    ~revision_id
    ?source_receipt:t.parent_artifact.source_receipt
    ~root_relative_path:t.root_relative_path
    ~root_chatmd:t.root_chatmd
    ~sources:t.sources
    ~parser_schema_version:t.parent_artifact.parser_schema_version
    ~runtime_schema_version:t.parent_artifact.runtime_schema_version
    ?program_bundle:(Option.map t.programs ~f:Chatml_prompt_programs.to_storage)
    ?content_bundle:(Option.map t.content ~f:Chatmd_prompt_content.to_storage)
    ~created_at
    ()
;;

let resource_revision t ~parent ~revision_id ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    match
      P.Id.Prompt_revision.equal (Prompt_revision.id parent) t.parent_revision_id
      && String.equal
           (Prompt_revision.artifact parent).manifest_sha256
           t.parent_manifest_sha256
    with
    | true -> Ok ()
    | false -> denied "authored resource preparation requires its original source owner"
  in
  let%bind artifact =
    artifact t ~revision_id ~created_at:t.parent_artifact.created_at
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  match t.parser_context, t.programs with
  | Some parsed, Some programs ->
    let tree = Prompt_revision.materialized_tree parent in
    let%bind () =
      Store.verify_tree ~root:tree artifact
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    let revision =
      Prompt_revision.create
        ~definition:(Prompt_revision.definition parent)
        ~artifact
        ~materialized_tree:tree
        ~elements:(CM.Parsed.elements parsed)
    in
    let unavailable error =
      P.Error.create
        Prompt_unavailable
        ~message:(Error.to_string_hum error)
        ~retryable:false
        ()
    in
    let%bind revision =
      Prompt_revision.with_custody revision ~parser_context:parsed ~programs ~checkpoint
      |> Result.map_error ~f:unavailable
    in
    (match t.content_projection with
     | None -> Ok revision
     | Some restored ->
       Prompt_revision.with_content
         revision
         ~content:(Prompt_content_custody.Restored.content restored)
         ~materialized:(Prompt_content_custody.Restored.materialized restored)
         ~checkpoint
       |> Result.map_error ~f:unavailable)
  | None, None | Some _, None | None, Some _ ->
    Prompt_revision_builder.reparse
      ~checkpoint
      ?content_limits:t.content_limits
      ~definition:(Prompt_revision.definition parent)
      ~artifact
      ~materialized_tree:(Prompt_revision.materialized_tree parent)
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      P.Error.create
        Prompt_unavailable
        ~message:
          (List.map diagnostics ~f:(fun diagnostic ->
             diagnostic.Prompt_revision_builder.Diagnostic.message)
           |> String.concat ~sep:"\n")
        ~retryable:false
        ())
;;

let validate_owned_reference parent reference ~checkpoint =
  match Prompt_revision.parser_context parent with
  | None -> denied "captured child requires its defining owned parser"
  | Some parsed ->
    let artifact = Prompt_revision.artifact parent in
    let input = CM.Parsed.root_input parsed in
    checkpoint ();
    if
      (not (Chatmd_parser_input.Role.equal (Chatmd_parser_input.role input) Root))
      || (not (String.equal (Chatmd_parser_input.path input) artifact.root_relative_path))
      || not (Chatmd_parser_input.authored_matches input artifact.root_chatmd ~checkpoint)
    then denied "captured child parser does not belong to the parent Root"
    else (
      let matches =
        List.fold (CM.Parsed.agent_references parsed) ~init:0 ~f:(fun count owned ->
          checkpoint ();
          if phys_equal reference owned then count + 1 else count)
      in
      if matches <> 1
      then denied "captured child requires one exact defining parser reference"
      else (
        match CM.Agent_reference.queued_key reference with
        | None -> denied "captured child reference has no retained queued member"
        | Some key -> Ok key))
;;

let capture ~parent ~reference ~content_limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind relative_path = validate_owned_reference parent reference ~checkpoint in
  let%bind () =
    match
      Prompt_revision.programs parent, (Prompt_revision.artifact parent).program_bundle
    with
    | Some _, Some _ -> Ok ()
    | None, _ | _, None ->
      denied "owned child source requires matching retained parser/program custody"
  in
  capture_member ~parent ~relative_path:(Some relative_path) ~content_limits ~checkpoint
;;

let capture_selected_member
      ~parent
      ~member
      ~content_limits
      ~max_input_bytes
      ~max_work_steps
      ~checkpoint
      ~on_acquire
  =
  let limit message =
    Error (P.Error.create Resource_limit ~message ~retryable:false ())
  in
  if max_input_bytes <= 0 || max_work_steps <= 0
  then limit "Captured Model source allowance exhausted"
  else
    let module Abort = struct
      exception Work_limit
      exception Byte_limit
    end
    in
    let used = ref 0 in
    let poll () =
      if !used >= max_work_steps then raise Abort.Work_limit;
      incr used;
      checkpoint ()
    in
    try
      let open Result.Let_syntax in
      let artifact = Prompt_revision.artifact parent in
      let selected = Source.Portable_path.to_string member in
      let%bind () =
        match Prompt_revision.programs parent, artifact.program_bundle with
        | Some _, Some _ -> Ok ()
        | None, _ | _, None ->
          denied "Captured Model member requires complete retained parser/program custody"
      in
      let bytes = ref (String.length artifact.root_chatmd) in
      if !bytes > max_input_bytes then raise Abort.Byte_limit;
      let present = ref (String.equal artifact.root_relative_path selected) in
      List.iter artifact.sources ~f:(fun source ->
        poll ();
        let length = String.length source.Store.Source.contents in
        if length > max_input_bytes - !bytes then raise Abort.Byte_limit;
        bytes := !bytes + length;
        if String.equal source.relative_path selected then present := true);
      let%bind () =
        if !present
        then Ok ()
        else denied "Captured Model member is absent from retained authored inventory"
      in
      poll ();
      on_acquire ();
      let%bind source =
        capture_member
          ~parent
          ~relative_path:(Some selected)
          ~content_limits
          ~checkpoint:poll
      in
      if Int.equal (selected_source_bytes source) !bytes
      then Ok source
      else denied "Captured Model source inventory changed during selection"
    with
    | Abort.Work_limit -> limit "Captured Model source work allowance exhausted"
    | Abort.Byte_limit -> limit "Captured Model source bytes exceed remaining allowance"
;;

let capture_legacy_tool ~parent ~declaration ~content_limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    match Prompt_revision.parser_context parent with
    | Some _ -> denied "owned child source cannot use legacy path compatibility"
    | None -> Ok ()
  in
  let matches =
    List.fold (Prompt_revision.elements parent) ~init:0 ~f:(fun count element ->
      checkpoint ();
      match element with
      | CM.Tool (Agent candidate | Persistent_agent (candidate, _)) ->
        if phys_equal candidate declaration then count + 1 else count
      | _ -> count)
  in
  if matches <> 1 || not declaration.CM.is_local
  then denied "legacy captured child requires one actual local tool declaration"
  else (
    let relative_path =
      legacy_agent_member (Prompt_revision.materialized_tree parent) ~declaration
    in
    capture_member ~parent ~relative_path ~content_limits ~checkpoint)
;;
