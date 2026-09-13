open Core

let sha256 contents = Digestif.SHA256.(digest_string contents |> to_hex)

let valid_relative_path path =
  (not (String.is_empty path))
  && (not (Filename.is_absolute path))
  && String.split path ~on:'/'
     |> List.for_all ~f:(fun component ->
       (not (String.is_empty component))
       && (not (String.equal component "."))
       && not (String.equal component ".."))
;;

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

module Manifest = struct
  type source =
    { relative_path : string
    ; sha256 : string
    }
  [@@deriving sexp]

  type t =
    { version : int
    ; revision_id : Agent_protocol.Id.Prompt_revision.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; canonical_source : string option
    ; root_relative_path : string
    ; root_sha256 : string
    ; sources : source list
    ; parser_schema_version : int
    ; runtime_schema_version : int
    ; shell_manifest_sha256 : string option
    ; created_at : Agent_protocol.Timestamp.t
    }
  [@@deriving sexp]
end

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
    ; created_at : Agent_protocol.Timestamp.t
    }

  let manifest (artifact : t) =
    Manifest.
      { version = 1
      ; revision_id = artifact.revision_id
      ; prompt_definition_id = artifact.prompt_definition_id
      ; canonical_source = artifact.canonical_source
      ; root_relative_path = artifact.root_relative_path
      ; root_sha256 = artifact.root_sha256
      ; sources =
          List.map artifact.sources ~f:(fun (source : Source.t) ->
            { relative_path = source.Source.relative_path; sha256 = source.sha256 })
      ; parser_schema_version = artifact.parser_schema_version
      ; runtime_schema_version = artifact.runtime_schema_version
      ; shell_manifest_sha256 = artifact.shell_manifest_sha256
      ; created_at = artifact.created_at
      }
  ;;

  let manifest_sha256 (artifact : t) =
    manifest artifact |> [%sexp_of: Manifest.t] |> Sexp.to_string_mach |> sha256
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
    if parser_schema_version < 0 || runtime_schema_version < 0
    then Error (Store_error.Corrupt "prompt artifact schema versions must be nonnegative")
    else if not (valid_relative_path root_relative_path)
    then Error (Store_error.Corrupt "prompt artifact root path must be relative")
    else if
      List.contains_dup sources ~compare:(fun left right ->
        String.compare left.Source.relative_path right.relative_path)
    then Error (Store_error.Corrupt "prompt artifact source paths must be unique")
    else (
      let provisional =
        { revision_id
        ; prompt_definition_id
        ; canonical_source
        ; root_relative_path
        ; root_chatmd
        ; root_sha256 = sha256 root_chatmd
        ; sources
        ; parser_schema_version
        ; runtime_schema_version
        ; shell_manifest_sha256
        ; manifest_sha256 = ""
        ; created_at
        }
      in
      Ok { provisional with manifest_sha256 = manifest_sha256 provisional })
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
  let manifest = Artifact.manifest artifact in
  let encoded_manifest = Sexp.to_string_mach ([%sexp_of: Manifest.t] manifest) in
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
  if String.equal artifact.manifest_sha256 (Artifact.manifest_sha256 artifact)
  then Ok ()
  else Error (Store_error.Corrupt "prompt artifact manifest digest is invalid")
;;

let install t ~transaction_id artifact =
  let open Result.Let_syntax in
  let%bind () = validate_artifact artifact in
  let destination = revision_directory t artifact.Artifact.revision_id in
  if Eio.Path.is_directory (eio_path t destination)
  then Error (Store_error.Corrupt "prompt revision artifact already exists")
  else (
    let staging =
      Filename.concat
        t.root
        (".install-" ^ Agent_protocol.Id.Transaction.to_string transaction_id)
    in
    try
      Eio.Path.mkdir ~perm:0o700 (eio_path t staging);
      install_files t staging artifact;
      Eio.Path.rename (eio_path t staging) (eio_path t destination);
      Ok ()
    with
    | exn ->
      (try Eio.Path.rmtree ~missing_ok:true (eio_path t staging) with
       | _ -> ());
      Error
        (Store_error.of_exn ~operation:"install prompt artifact" ~path:destination exn))
;;

let load_manifest t directory =
  let path = Filename.concat directory "manifest.sexp" in
  try
    let encoded = Eio.Path.load (eio_path t path) in
    let expected =
      Eio.Path.load (eio_path t (Filename.concat directory "manifest.sha256"))
      |> String.strip
    in
    if not (String.equal (sha256 encoded) expected)
    then Error (Store_error.Corrupt "prompt manifest digest mismatch")
    else encoded |> Sexp.of_string |> [%of_sexp: Manifest.t] |> Result.return
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.Corrupt ("prompt manifest decode failed: " ^ Exn.to_string exn))
;;

let load_source t directory source =
  let path =
    Filename.concat (Filename.concat directory "sources") source.Manifest.relative_path
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
      if
        not
          (Poly.equal
             (Eio.Path.kind ~follow:false Eio.Path.(root / relative))
             `Regular_file)
      then failwith ("missing materialized prompt source: " ^ relative));
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
  let%bind manifest = load_manifest t directory in
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
        let%bind artifact =
          Artifact.create
            ~revision_id
            ?prompt_definition_id:manifest.prompt_definition_id
            ?canonical_source:manifest.canonical_source
            ~root_relative_path:manifest.root_relative_path
            ~root_chatmd
            ~sources
            ~parser_schema_version:manifest.parser_schema_version
            ~runtime_schema_version:manifest.runtime_schema_version
            ?shell_manifest_sha256:manifest.shell_manifest_sha256
            ~created_at:manifest.created_at
            ()
        in
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
  let%bind encoded = read "manifest.sexp" in
  let%bind checksum = read "manifest.sha256" in
  let%bind () =
    match
      String.equal (sha256 encoded) manifest_sha256
      && String.equal (String.strip checksum) manifest_sha256
    with
    | true -> Ok ()
    | false ->
      Error (Store_error.Corrupt "retained artifact does not match its admission")
  in
  let%bind manifest =
    Result.try_with (fun () -> encoded |> Sexp.of_string |> [%of_sexp: Manifest.t])
    |> Result.map_error ~f:(fun _ ->
      Store_error.Corrupt "invalid retained artifact manifest")
  in
  let%bind () =
    match
      Int.equal manifest.version 1
      && Agent_protocol.Id.Prompt_revision.equal revision_id manifest.revision_id
      && valid_relative_path manifest.root_relative_path
      && List.for_all manifest.sources ~f:(fun source ->
        valid_relative_path source.Manifest.relative_path)
    with
    | true -> Ok ()
    | false -> Error (Store_error.Corrupt "invalid retained artifact identity or paths")
  in
  let expected =
    [ "manifest.sexp", manifest_sha256
    ; "manifest.sha256", sha256 checksum
    ; "root.chatmd", manifest.root_sha256
    ; Filename.concat "tree" manifest.root_relative_path, manifest.root_sha256
    ]
    @ List.concat_map manifest.sources ~f:(fun source ->
      [ Filename.concat "sources" source.Manifest.relative_path, source.sha256
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
