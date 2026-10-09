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

module Artifact = struct
  type t =
    { revision_id : Agent_protocol.Id.Prompt_revision.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; canonical_source : string option
    ; root_relative_path : string
    ; root_chatmd : string
    ; root_sha256 : string
    ; sources : Source.t list
    ; parser_schema_version : int
    ; runtime_schema_version : int
    ; shell_manifest_sha256 : string option
    ; manifest_sha256 : string
    ; manifest_publication : Publication.t
    ; created_at : Agent_protocol.Timestamp.t
    }

  let manifest (artifact : t) : Manifest.t =
    { version = 1
    ; revision_id = artifact.revision_id
    ; prompt_definition_id = artifact.prompt_definition_id
    ; canonical_source = artifact.canonical_source
    ; root_relative_path = artifact.root_relative_path
    ; root_sha256 = artifact.root_sha256
    ; sources =
        List.map artifact.sources ~f:(fun (source : Source.t) ->
          Manifest.Source.{ relative_path = source.relative_path; sha256 = source.sha256 })
    ; parser_schema_version = artifact.parser_schema_version
    ; runtime_schema_version = artifact.runtime_schema_version
    ; shell_manifest_sha256 = artifact.shell_manifest_sha256
    ; created_at = artifact.created_at
    }
  ;;

  let of_publication publication ~root_chatmd ~sources =
    let known = Prompt_manifest_document.value (Publication.document publication) in
    let artifact =
      { revision_id = known.revision_id
      ; prompt_definition_id = known.prompt_definition_id
      ; canonical_source = known.canonical_source
      ; root_relative_path = known.root_relative_path
      ; root_chatmd
      ; root_sha256 = known.root_sha256
      ; sources
      ; parser_schema_version = known.parser_schema_version
      ; runtime_schema_version = known.runtime_schema_version
      ; shell_manifest_sha256 = known.shell_manifest_sha256
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

  let same_content (a : t) (b : t) =
    Manifest.equal (manifest a) { (manifest b) with created_at = a.created_at }
  ;;

  let create
        ~revision_id
        ?prompt_definition_id
        ?canonical_source
        ?(root_relative_path = "root.chatmd")
        ~root_chatmd
        ~sources
        ~parser_schema_version
        ~runtime_schema_version
        ?shell_manifest_sha256
        ~created_at
        ()
    =
    let known : Manifest.t =
      { version = 1
      ; revision_id
      ; prompt_definition_id
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
      ; created_at
      }
    in
    let open Result.Let_syntax in
    let%bind publication = Publication.create known |> Document_fields.store in
    of_publication publication ~root_chatmd ~sources
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

let load_source t directory source =
  let path =
    Filename.concat
      (Filename.concat directory "sources")
      source.Manifest.Source.relative_path
  in
  try
    let contents = Eio.Path.load (eio_path t path) in
    if String.equal (sha256 contents) source.sha256
    then Source.create ~relative_path:source.relative_path ~contents
    else
      Error
        (Store_error.Corrupt ("prompt source digest mismatch: " ^ source.relative_path))
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"load prompt source" ~path exn)
;;

let materialized_files (artifact : Artifact.t) =
  (artifact.root_relative_path, artifact.root_sha256)
  :: List.map artifact.sources ~f:(fun source ->
    source.Source.relative_path, source.sha256)
;;

let rec verify_tree_node root expected relative =
  let path = if String.is_empty relative then root else Eio.Path.(root / relative) in
  match Eio.Path.kind ~follow:false path with
  | `Directory ->
    Eio.Path.read_dir path
    |> List.iter ~f:(fun name ->
      verify_tree_node
        root
        expected
        (if String.is_empty relative then name else Filename.concat relative name))
  | `Regular_file ->
    (match List.Assoc.find expected relative ~equal:String.equal with
     | Some digest when String.equal digest (sha256 (Eio.Path.load path)) -> ()
     | _ -> failwith ("materialized prompt source differs: " ^ relative))
  | _ -> failwith ("invalid materialized prompt source kind: " ^ relative)
;;

let verify_tree ~root artifact =
  try
    let expected = materialized_files artifact in
    verify_tree_node root expected "";
    List.iter expected ~f:(fun (relative, _) ->
      match Eio.Path.kind ~follow:false Eio.Path.(root / relative) with
      | `Regular_file -> ()
      | _ -> failwith ("missing materialized prompt source: " ^ relative));
    Ok ()
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.Corrupt ("prompt tree verification failed: " ^ Exn.to_string exn))
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
      let root_chatmd = Eio.Path.load (eio_path t root_path) in
      if not (String.equal (sha256 root_chatmd) manifest.root_sha256)
      then Error (Store_error.Corrupt "root ChatMD digest mismatch")
      else (
        let%bind sources =
          Result.all (List.map manifest.sources ~f:(load_source t directory))
        in
        let%bind artifact = Artifact.of_publication publication ~root_chatmd ~sources in
        let%map () = verify_materialized_tree t artifact in
        artifact)
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn -> Error (Store_error.of_exn ~operation:"load root ChatMD" ~path:root_path exn))
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
