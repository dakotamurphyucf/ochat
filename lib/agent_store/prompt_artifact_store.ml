open Core

let sha256 contents = Digestif.SHA256.(digest_string contents |> to_hex)
let valid_relative_path = Prompt_manifest.valid_relative_path

module Source = struct
  type t =
    { relative_path : string
    ; contents : string
    ; sha256 : string
    }

  let create ~relative_path ~contents =
    if valid_relative_path relative_path
    then Ok { relative_path; contents; sha256 = sha256 contents }
    else Error (Store_error.Corrupt ("invalid prompt source path: " ^ relative_path))
  ;;
end

module Manifest = Prompt_manifest
module Publication = Prompt_manifest_document.Publication
module Program_bundle = Prompt_program_bundle

module Artifact = struct
  type t =
    { revision_id : Agent_protocol.Id.Prompt_revision.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; source_receipt : Prompt_source_receipt.t option
    ; canonical_source : string option
    ; root_relative_path : string
    ; root_chatmd : string
    ; root_sha256 : string
    ; sources : Source.t list
    ; parser_schema_version : int
    ; runtime_schema_version : int
    ; shell_manifest_sha256 : string option
    ; program_bundle : Program_bundle.t option
    ; content_bundle : Program_bundle.t option
    ; manifest_sha256 : string
    ; manifest_publication : Publication.t
    ; created_at : Agent_protocol.Timestamp.t
    }

  let manifest (artifact : t) : Manifest.t =
    { version = 1
    ; revision_id = artifact.revision_id
    ; prompt_definition_id = artifact.prompt_definition_id
    ; source_receipt = artifact.source_receipt
    ; canonical_source = artifact.canonical_source
    ; root_relative_path = artifact.root_relative_path
    ; root_sha256 = artifact.root_sha256
    ; sources =
        List.map artifact.sources ~f:(fun (source : Source.t) ->
          Manifest.Source.{ relative_path = source.relative_path; sha256 = source.sha256 })
    ; parser_schema_version = artifact.parser_schema_version
    ; runtime_schema_version = artifact.runtime_schema_version
    ; shell_manifest_sha256 = artifact.shell_manifest_sha256
    ; program_bundle = Option.map artifact.program_bundle ~f:Program_bundle.manifest
    ; content_bundle = Option.map artifact.content_bundle ~f:Program_bundle.manifest
    ; created_at = artifact.created_at
    }
  ;;

  let of_publication publication ~root_chatmd ~sources ~program_bundle ~content_bundle =
    let known = Prompt_manifest_document.value (Publication.document publication) in
    let artifact =
      { revision_id = known.revision_id
      ; prompt_definition_id = known.prompt_definition_id
      ; source_receipt = known.source_receipt
      ; canonical_source = known.canonical_source
      ; root_relative_path = known.root_relative_path
      ; root_chatmd
      ; root_sha256 = known.root_sha256
      ; sources
      ; parser_schema_version = known.parser_schema_version
      ; runtime_schema_version = known.runtime_schema_version
      ; shell_manifest_sha256 = known.shell_manifest_sha256
      ; program_bundle
      ; content_bundle
      ; manifest_sha256 = Publication.sha256 publication
      ; manifest_publication = publication
      ; created_at = known.created_at
      }
    in
    if
      Manifest.equal known (manifest artifact)
      && String.equal known.root_sha256 (sha256 root_chatmd)
      && List.for_all sources ~f:(fun source ->
        String.equal source.Source.sha256 (sha256 source.contents))
    then Ok artifact
    else
      Error (Store_error.Corrupt "prompt bytes differ from original manifest inventory")
  ;;

  let with_identity t ~revision_id ~created_at =
    let open Result.Let_syntax in
    let%bind publication =
      Publication.with_identity t.manifest_publication ~revision_id ~created_at
      |> Document_fields.store
    in
    of_publication
      publication
      ~root_chatmd:t.root_chatmd
      ~sources:t.sources
      ~program_bundle:t.program_bundle
      ~content_bundle:t.content_bundle
  ;;

  let same_content (a : t) (b : t) =
    Manifest.equal (manifest a) { (manifest b) with created_at = a.created_at }
  ;;

  let check_payload_budget ~root_chatmd ~sources ~program_bundle ~content_bundle =
    let module B = Program_bundle in
    let open Result.Let_syntax in
    let%bind () =
      if String.length root_chatmd > B.max_payload_bytes
      then
        Error
          (Store_error.Document (Document_schema.Error.Limit_exceeded "root prompt bytes"))
      else Ok ()
    in
    let invalid () =
      Error
        (Store_error.Document
           (Document_schema.Error.Limit_exceeded "prompt source inventory"))
    in
    let%bind initial_count, initial_bytes =
      List.fold_result
        [ program_bundle; content_bundle ]
        ~init:(1, String.length root_chatmd)
        ~f:(fun (count, bytes) bundle ->
          match bundle with
          | None -> Ok (count, bytes)
          | Some bundle ->
            let manifest = B.manifest bundle in
            let additional_count = List.length (B.Manifest.payloads manifest) in
            let additional_bytes = B.Manifest.payload_bytes manifest in
            if
              additional_count > B.max_payload_count - count
              || additional_bytes > B.max_payload_bytes - bytes
            then invalid ()
            else Ok (count + additional_count, bytes + additional_bytes))
    in
    if initial_count > B.max_payload_count || initial_bytes > B.max_payload_bytes
    then invalid ()
    else
      List.fold_result
        sources
        ~init:(initial_count, initial_bytes)
        ~f:(fun (count, bytes) source ->
          let length = String.length source.Source.contents in
          if count >= B.max_payload_count || length > B.max_payload_bytes - bytes
          then invalid ()
          else Ok (count + 1, bytes + length))
      |> Result.map ~f:(fun _ -> ())
  ;;

  let create
        ~revision_id
        ?prompt_definition_id
        ?source_receipt
        ?canonical_source
        ?(root_relative_path = "root.chatmd")
        ~root_chatmd
        ~sources
        ~parser_schema_version
        ~runtime_schema_version
        ?shell_manifest_sha256
        ?program_bundle
        ?content_bundle
        ~created_at
        ()
    =
    let open Result.Let_syntax in
    let%bind () =
      check_payload_budget ~root_chatmd ~sources ~program_bundle ~content_bundle
    in
    let known : Manifest.t =
      { version = 1
      ; revision_id
      ; prompt_definition_id
      ; source_receipt
      ; canonical_source
      ; root_relative_path
      ; root_sha256 = sha256 root_chatmd
      ; sources =
          List.map sources ~f:(fun (source : Source.t) ->
            Manifest.Source.
              { relative_path = source.relative_path; sha256 = source.sha256 })
      ; parser_schema_version
      ; runtime_schema_version
      ; shell_manifest_sha256
      ; program_bundle = Option.map program_bundle ~f:Program_bundle.manifest
      ; content_bundle = Option.map content_bundle ~f:Program_bundle.manifest
      ; created_at
      }
    in
    let open Result.Let_syntax in
    let%bind publication = Publication.create known |> Document_fields.store in
    of_publication publication ~root_chatmd ~sources ~program_bundle ~content_bundle
  ;;
end

type t =
  { env : Eio_unix.Stdenv.base
  ; root : string
  }

let eio_path t path = Eio.Path.(Eio.Stdenv.fs t.env / path)

let create ~env ~root =
  if not (Filename.is_absolute root)
  then
    Error
      (Store_error.Io
         { operation = "create prompt artifact store"
         ; path = root
         ; message = "path must be absolute"
         })
  else (
    try
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
      Ok { env; root }
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error (Store_error.of_exn ~operation:"create prompt artifact store" ~path:root exn))
;;

let open_existing ~env ~root =
  if not (Filename.is_absolute root)
  then Error (Store_error.Corrupt "prompt artifact root must be absolute")
  else (
    try
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / root) with
      | `Directory -> Ok { env; root }
      | `Not_found -> Error (Store_error.Missing root)
      | _ ->
        Error (Store_error.Corrupt "prompt artifact root is not a non-symlink directory")
    with
    | Eio.Io _ as exn ->
      Error (Store_error.of_exn ~operation:"open prompt artifact store" ~path:root exn))
;;

let discard_interrupted_installs t ~max_entries ~checkpoint =
  Prompt_artifact_staging.discard ~env:t.env ~root:t.root ~max_entries ~checkpoint
;;

let revision_directory t revision_id =
  Filename.concat t.root (Agent_protocol.Id.Prompt_revision.to_string revision_id)
;;

let materialized_tree t revision_id =
  eio_path t (Filename.concat (revision_directory t revision_id) "tree")
;;

let exists t revision_id =
  Eio.Path.is_directory (eio_path t (revision_directory t revision_id))
;;

let installed_revisions t =
  try
    Eio.Path.read_dir (eio_path t t.root)
    |> List.filter_map ~f:(fun name ->
      Agent_protocol.Id.Prompt_revision.of_string name |> Result.ok)
    |> Result.return
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"list prompt artifacts" ~path:t.root exn)
;;

let is_protected protected revision_id =
  List.mem protected revision_id ~equal:(fun left right ->
    Agent_protocol.Id.Prompt_revision.compare left right = 0)
;;

let remove_revision t revision_id =
  let path = revision_directory t revision_id in
  try
    Eio.Path.rmtree ~missing_ok:false (eio_path t path);
    Ok ()
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"prune prompt artifact" ~path exn)
;;

let prune_unreferenced t ~protected =
  let open Result.Let_syntax in
  let%bind revisions = installed_revisions t in
  let removable =
    List.filter revisions ~f:(fun revision_id -> not (is_protected protected revision_id))
  in
  let%bind () = Result.all_unit (List.map removable ~f:(remove_revision t)) in
  let%map () =
    if List.is_empty removable
    then Ok ()
    else Durable_file.sync_directory ~env:t.env ~path:t.root
  in
  List.length removable
;;

let save_file t path contents =
  Eio.Path.with_open_out ~create:(`Exclusive 0o400) (eio_path t path) (fun flow ->
    Eio.Flow.copy_string contents flow;
    Eio.File.sync flow)
;;

let ensure_parent t path =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path t (Filename.dirname path))
;;

let install_files t directory artifact =
  save_file t (Filename.concat directory "root.chatmd") artifact.Artifact.root_chatmd;
  let tree_root = Filename.concat directory "tree" in
  let tree_root_file = Filename.concat tree_root artifact.root_relative_path in
  ensure_parent t tree_root_file;
  save_file t tree_root_file artifact.root_chatmd;
  List.iter artifact.sources ~f:(fun source ->
    let path =
      Filename.concat (Filename.concat directory "sources") source.Source.relative_path
    in
    ensure_parent t path;
    save_file t path source.contents;
    let tree_path = Filename.concat tree_root source.relative_path in
    ensure_parent t tree_path;
    save_file t tree_path source.contents);
  List.iter
    [ "programs", artifact.program_bundle; "content", artifact.content_bundle ]
    ~f:(fun (pool, bundle) ->
      Option.iter bundle ~f:(fun bundle ->
        List.iter (Program_bundle.payloads bundle) ~f:(fun payload ->
          let path =
            Filename.concat
              (Filename.concat directory pool)
              (Program_bundle.Payload.sha256 payload)
          in
          ensure_parent t path;
          save_file t path (Program_bundle.Payload.contents payload))));
  let encoded_manifest = Publication.bytes artifact.Artifact.manifest_publication in
  save_file t (Filename.concat directory "manifest.sexp") encoded_manifest;
  save_file
    t
    (Filename.concat directory "manifest.sha256")
    (sha256 encoded_manifest ^ "\n")
;;

let validate_artifact artifact =
  let open Result.Let_syntax in
  let%bind () =
    if String.equal artifact.Artifact.root_sha256 (sha256 artifact.root_chatmd)
    then Ok ()
    else Error (Store_error.Corrupt "root ChatMD digest is invalid")
  in
  let%bind () =
    Result.all_unit
      (List.map artifact.sources ~f:(fun source ->
         if
           valid_relative_path source.Source.relative_path
           && String.equal source.sha256 (sha256 source.contents)
         then Ok ()
         else Error (Store_error.Corrupt "prompt source metadata is invalid")))
  in
  if
    String.equal
      artifact.manifest_sha256
      (Publication.sha256 artifact.manifest_publication)
    && Manifest.equal
         (Artifact.manifest artifact)
         (Prompt_manifest_document.value
            (Publication.document artifact.manifest_publication))
  then Ok ()
  else Error (Store_error.Corrupt "prompt artifact manifest digest is invalid")
;;

let rec sync_staging t path =
  let open Result.Let_syntax in
  match Eio.Path.kind ~follow:false (eio_path t path) with
  | `Directory ->
    let%bind () =
      Eio.Path.read_dir (eio_path t path)
      |> List.fold_result ~init:() ~f:(fun () name ->
        sync_staging t (Filename.concat path name))
    in
    Durable_file.sync_directory ~env:t.env ~path
  | `Regular_file -> Ok () (* save_file already fsyncs each exact publication. *)
  | _ -> Error (Store_error.Corrupt "invalid private prompt staging entry")
;;

let rec remove_staging t path =
  match Eio.Path.kind ~follow:false (eio_path t path) with
  | `Not_found -> ()
  | `Directory ->
    Eio.Path.read_dir (eio_path t path)
    |> List.iter ~f:(fun name -> remove_staging t (Filename.concat path name));
    Eio.Path.rmdir (eio_path t path)
  | `Regular_file | `Symbolic_link -> Eio.Path.unlink (eio_path t path)
  | _ -> raise_s [%sexp "invalid private prompt cleanup entry", (path : string)]
;;

let install t ~transaction_id artifact =
  let open Result.Let_syntax in
  let%bind () = validate_artifact artifact in
  let destination = revision_directory t artifact.Artifact.revision_id in
  match Eio.Path.kind ~follow:false (eio_path t destination) with
  | `Not_found ->
    let staging =
      Filename.concat
        t.root
        (".install-" ^ Agent_protocol.Id.Transaction.to_string transaction_id)
    in
    let created = ref false in
    let cleanup () =
      if !created
      then
        Eio.Cancel.protect (fun () ->
          try remove_staging t staging with
          | _ -> ())
    in
    let perform () =
      Eio.Path.mkdir ~perm:0o700 (eio_path t staging);
      created := true;
      install_files t staging artifact;
      let%bind () = sync_staging t staging in
      Eio.Path.rename (eio_path t staging) (eio_path t destination);
      Durable_file.sync_directory ~env:t.env ~path:t.root
    in
    (try
       match perform () with
       | Ok () -> Ok ()
       | Error failure ->
         cleanup ();
         Error failure
     with
     | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
       cleanup ();
       Error
         (Store_error.of_exn ~operation:"install prompt artifact" ~path:destination exn)
     | exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       cleanup ();
       Exn.raise_with_original_backtrace exn backtrace)
  | _ -> Error (Store_error.Corrupt "prompt revision artifact already exists")
;;

let load_manifest t directory revision_id =
  let open Result.Let_syntax in
  let%bind reader =
    Retention_reader.create
      ~env:t.env
      ~root:directory
      ~max_entries:16
      ~max_bytes:(Prompt_manifest_document.max_bytes + 128)
  in
  let%bind encoded =
    Retention_reader.read
      reader
      ~path:"manifest.sexp"
      ~max_bytes:Prompt_manifest_document.max_bytes
  in
  let%bind checksum =
    Retention_reader.read reader ~path:"manifest.sha256" ~max_bytes:128
  in
  Publication.of_bytes encoded ~revision_id ~expected_sha256:(String.strip checksum)
;;

let load_source reader source =
  let open Result.Let_syntax in
  let%bind contents =
    Retention_reader.read
      reader
      ~path:(Filename.concat "sources" source.Manifest.Source.relative_path)
      ~max_bytes:Program_bundle.max_payload_bytes
  in
  if String.equal (sha256 contents) source.sha256
  then Source.create ~relative_path:source.relative_path ~contents
  else Error (Store_error.Corrupt "prompt source digest mismatch")
;;

let load_private_bundle t directory reader ~pool manifest =
  let open Result.Let_syntax in
  let expected =
    Option.value_map manifest ~default:[] ~f:Program_bundle.Manifest.payloads
  in
  let%bind () =
    match
      Eio.Path.kind ~follow:false (eio_path t (Filename.concat directory pool)), expected
    with
    | `Not_found, [] -> Ok ()
    | `Directory, _ :: _ ->
      (* Bound enumeration by the admitted inventory before collecting names.
         This reader owns only the directory scan; payload reads continue to
         share the artifact-wide byte allowance below. *)
      let%bind names_reader =
        Retention_reader.create
          ~env:t.env
          ~root:directory
          ~max_entries:(1 + List.length expected)
          ~max_bytes:0
      in
      let%bind names = Retention_reader.list names_reader ~directory:pool in
      if List.equal String.equal names (List.map expected ~f:fst)
      then Ok ()
      else Error (Store_error.Corrupt ("private " ^ pool ^ " payload inventory differs"))
    | _ -> Error (Store_error.Corrupt ("invalid private " ^ pool ^ " payload directory"))
  in
  match manifest with
  | None -> Ok None
  | Some manifest ->
    let%bind payloads =
      List.fold_result expected ~init:[] ~f:(fun acc (digest, bytes) ->
        let%bind contents =
          Retention_reader.read
            reader
            ~path:(Filename.concat pool digest)
            ~max_bytes:bytes
        in
        let%bind payload = Program_bundle.Payload.create contents in
        if
          String.equal digest (Program_bundle.Payload.sha256 payload)
          && Int.equal bytes (Program_bundle.Payload.byte_length payload)
        then Ok (payload :: acc)
        else Error (Store_error.Corrupt ("private " ^ pool ^ " payload differs")))
    in
    Result.map (Program_bundle.restore manifest (List.rev payloads)) ~f:Option.some
;;

let verify_tree_with_checkpoint ~root (artifact : Artifact.t) ~checkpoint =
  let open Result.Let_syntax in
  let poll () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  poll ();
  let files =
    (artifact.root_relative_path, artifact.root_chatmd)
    :: List.map artifact.sources ~f:(fun source ->
      poll ();
      source.Source.relative_path, source.contents)
  in
  let files = String.Map.of_alist_exn files in
  let directories =
    Map.fold files ~init:(String.Set.singleton ".") ~f:(fun ~key ~data:_ directories ->
      let rec add directories path =
        poll ();
        let directory = Filename.dirname path in
        if String.equal directory "."
        then directories
        else add (Set.add directories directory) directory
      in
      add directories key)
  in
  let max_bytes =
    Map.fold files ~init:0 ~f:(fun ~key:_ ~data bytes ->
      poll ();
      bytes + String.length data)
  in
  (* Each directory consumes its kind, open and returned-child entries; each
     file consumes its returned name, kind and bounded read. *)
  let max_entries = (3 * (Map.length files + Set.length directories)) + 1 in
  let%bind reader = Retention_reader.create_at ~root ~max_entries ~max_bytes in
  let seen = ref String.Set.empty in
  let equal_contents expected actual =
    let rec loop offset =
      poll ();
      if offset = String.length expected
      then true
      else (
        let length = Int.min 4096 (String.length expected - offset) in
        let rec equal_chunk index =
          index = offset + length
          || (Char.equal expected.[index] actual.[index] && equal_chunk (index + 1))
        in
        if equal_chunk offset then loop (offset + length) else false)
    in
    Int.equal (String.length expected) (String.length actual) && loop 0
  in
  let rec verify relative =
    poll ();
    let%bind kind = Retention_reader.kind reader ~path:relative in
    match kind with
    | `Directory ->
      if not (Set.mem directories relative)
      then Error (Store_error.Corrupt "unexpected materialized prompt directory")
      else (
        let%bind names =
          Retention_reader.list_with_checkpoint
            reader
            ~directory:relative
            ~checkpoint:poll
        in
        List.fold_result names ~init:() ~f:(fun () name ->
          let child =
            if String.equal relative "." then name else Filename.concat relative name
          in
          verify child))
    | `File ->
      (match Map.find files relative with
       | None -> Error (Store_error.Corrupt "unexpected materialized prompt source")
       | Some expected ->
         let%bind actual =
           Retention_reader.read_with_checkpoint
             reader
             ~path:relative
             ~max_bytes:(String.length expected)
             ~checkpoint:poll
         in
         if equal_contents expected actual
         then (
           seen := Set.add !seen relative;
           Ok ())
         else Error (Store_error.Corrupt "materialized prompt source differs"))
  in
  let%bind () = verify "." in
  Map.fold files ~init:(Ok ()) ~f:(fun ~key ~data:_ result ->
    let%bind () = result in
    poll ();
    if Set.mem !seen key
    then Ok ()
    else Error (Store_error.Corrupt "missing materialized prompt source"))
;;

let verify_tree ~root artifact =
  verify_tree_with_checkpoint ~root artifact ~checkpoint:Eio.Fiber.check
;;

let verify_materialized_tree t artifact =
  verify_tree ~root:(materialized_tree t artifact.Artifact.revision_id) artifact
;;

let load t revision_id =
  let open Result.Let_syntax in
  let directory = revision_directory t revision_id in
  let%bind publication = load_manifest t directory revision_id in
  let manifest = Prompt_manifest_document.value (Publication.document publication) in
  if Agent_protocol.Id.Prompt_revision.compare manifest.revision_id revision_id <> 0
  then
    Error (Store_error.Corrupt "prompt artifact directory and manifest identity differ")
  else (
    let root_path = Filename.concat directory "root.chatmd" in
    try
      let%bind reader =
        Retention_reader.create
          ~env:t.env
          ~root:directory
          ~max_entries:(4 * Prompt_manifest_document.max_bytes)
          ~max_bytes:Program_bundle.max_payload_bytes
      in
      let%bind root_chatmd =
        Retention_reader.read
          reader
          ~path:"root.chatmd"
          ~max_bytes:Program_bundle.max_payload_bytes
      in
      if not (String.equal (sha256 root_chatmd) manifest.root_sha256)
      then Error (Store_error.Corrupt "root ChatMD digest mismatch")
      else (
        let%bind sources =
          Result.all (List.map manifest.sources ~f:(load_source reader))
        in
        let%bind program_bundle =
          load_private_bundle t directory reader ~pool:"programs" manifest.program_bundle
        in
        let%bind content_bundle =
          load_private_bundle t directory reader ~pool:"content" manifest.content_bundle
        in
        let%bind () =
          Artifact.check_payload_budget
            ~root_chatmd
            ~sources
            ~program_bundle
            ~content_bundle
        in
        let%bind artifact =
          Artifact.of_publication
            publication
            ~root_chatmd
            ~sources
            ~program_bundle
            ~content_bundle
        in
        let%map () = verify_materialized_tree t artifact in
        artifact)
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn -> Error (Store_error.of_exn ~operation:"load root ChatMD" ~path:root_path exn))
;;

let install_or_reuse t ~transaction_id candidate ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind () = validate_artifact candidate in
  let destination = revision_directory t candidate.Artifact.revision_id in
  let destination_kind () =
    try Ok (Eio.Path.kind ~follow:false (eio_path t destination)) with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error
        (Store_error.of_exn ~operation:"inspect prompt artifact" ~path:destination exn)
  in
  let verified_retained () =
    checkpoint ();
    let%bind retained = load t candidate.revision_id in
    checkpoint ();
    if not (Artifact.same_content candidate retained)
    then
      Error (Store_error.Corrupt "existing prompt revision differs from captured source")
    else (
      let%map () = Durable_file.sync_directory ~env:t.env ~path:t.root in
      checkpoint ();
      retained)
  in
  let%bind kind = destination_kind () in
  match kind with
  | `Directory -> verified_retained ()
  | `Not_found ->
    checkpoint ();
    (match install t ~transaction_id candidate with
     | Ok () -> verified_retained ()
     | Error original ->
       (* A competing publisher, or uncertainty after our rename, is resolved
          only by the complete installed artifact and parent sync. *)
       (match destination_kind () with
        | Ok `Directory -> verified_retained ()
        | Ok `Not_found -> Error original
        | Ok _ -> Error (Store_error.Corrupt "invalid prompt revision destination")
        | Error error -> Error error))
  | _ -> Error (Store_error.Corrupt "invalid prompt revision destination")
;;

let verify_retained t ~reader ~revision_id ~manifest_sha256 =
  let module R = Retention_reader in
  let open Result.Let_syntax in
  let%bind reader = R.at_root reader ~root:(revision_directory t revision_id) in
  let read path = R.read reader ~path ~max_bytes:Int.max_value in
  let%bind encoded =
    R.read reader ~path:"manifest.sexp" ~max_bytes:Prompt_manifest_document.max_bytes
  in
  let%bind checksum = R.read reader ~path:"manifest.sha256" ~max_bytes:128 in
  let%bind () =
    match
      String.equal (sha256 encoded) manifest_sha256
      && String.equal (String.strip checksum) manifest_sha256
    with
    | true -> Ok ()
    | false ->
      Error (Store_error.Corrupt "retained artifact does not match its admission")
  in
  let%bind publication =
    Publication.of_bytes encoded ~revision_id ~expected_sha256:manifest_sha256
  in
  let manifest = Prompt_manifest_document.value (Publication.document publication) in
  let expected =
    [ "manifest.sexp", manifest_sha256
    ; "manifest.sha256", sha256 checksum
    ; "root.chatmd", manifest.root_sha256
    ; Filename.concat "tree" manifest.root_relative_path, manifest.root_sha256
    ]
    @ Option.value_map manifest.program_bundle ~default:[] ~f:(fun bundle ->
      List.map (Program_bundle.Manifest.payloads bundle) ~f:(fun (digest, _) ->
        Filename.concat "programs" digest, digest))
    @ Option.value_map manifest.content_bundle ~default:[] ~f:(fun bundle ->
      List.map (Program_bundle.Manifest.payloads bundle) ~f:(fun (digest, _) ->
        Filename.concat "content" digest, digest))
    @ List.concat_map manifest.sources ~f:(fun source ->
      [ Filename.concat "sources" source.Manifest.Source.relative_path, source.sha256
      ; Filename.concat "tree" source.relative_path, source.sha256
      ])
  in
  let%bind expected =
    match String.Map.of_alist expected with
    | `Ok expected -> Ok expected
    | `Duplicate_key _ -> Error (Store_error.Corrupt "duplicate retained artifact path")
  in
  let rec visit relative seen =
    let%bind kind = R.kind reader ~path:relative in
    match kind with
    | `Directory ->
      let%bind names = R.list reader ~directory:relative in
      List.fold_result names ~init:seen ~f:(fun seen name ->
        visit
          (if String.equal relative "." then name else Filename.concat relative name)
          seen)
    | `File ->
      let%bind wanted =
        Map.find expected relative
        |> Result.of_option
             ~error:(Store_error.Corrupt "unexpected retained artifact file")
      in
      let%bind contents = read relative in
      (match String.equal (sha256 contents) wanted with
       | true -> Ok (Set.add seen relative)
       | false -> Error (Store_error.Corrupt "retained artifact source digest mismatch"))
  in
  let%bind seen = visit "." String.Set.empty in
  match Set.equal seen (Map.key_set expected) with
  | true -> Ok ()
  | false -> Error (Store_error.Corrupt "retained artifact file is missing")
;;
