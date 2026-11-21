open! Core
module P = Agent_protocol
module Files = Result_intent_file
module Record = Recovery_asset_record
module Reference = Recovery_asset_reference
module Reader = Retention_reader

type entry =
  { filename : string
  ; record : Record.t
  ; committed : bool
  }

type t =
  { entries : entry list
  ; temporaries : (string * string) list
  }

let entries t = t.entries
let corrupt message = Error (Store_error.Corrupt message)

let list reader phase =
  let open Result.Let_syntax in
  let directory = Files.recovery_asset_directory phase in
  let%bind kind = Reader.kind_or_missing reader ~path:directory in
  match kind with
  | None -> Ok []
  | Some `Directory -> Reader.list reader ~directory
  | Some `File -> corrupt "recovery asset namespace is not a directory"
;;

let intent_count reader =
  let%map.Result names = list reader Intent in
  List.count names ~f:(fun name -> Option.is_none (Durable_file.temporary_target name))
;;

let has_entries reader =
  let open Result.Let_syntax in
  match%bind list reader Intent with
  | _ :: _ -> Ok true
  | [] -> Result.map (list reader Commit) ~f:(fun names -> not (List.is_empty names))
;;

let names reader phase =
  let open Result.Let_syntax in
  let%bind names = list reader phase in
  List.fold_result names ~init:([], []) ~f:(fun (exact, temporaries) name ->
    Eio.Fiber.check ();
    match Durable_file.temporary_target name with
    | Some target when Files.valid_recovery_asset_filename target ->
      Ok (exact, (name, target) :: temporaries)
    | None when Files.valid_recovery_asset_filename name -> Ok (name :: exact, temporaries)
    | Some _ | None -> corrupt "unknown recovery asset publication filename")
;;

let read reader ~owner =
  let open Result.Let_syntax in
  let path phase filename =
    Filename.concat (Files.recovery_asset_directory phase) filename
  in
  let frame phase filename =
    Eio.Fiber.check ();
    Reader.read reader ~path:(path phase filename) ~max_bytes:Record.max_frame_bytes
  in
  let%bind intents, intent_temporaries = names reader Intent in
  let%bind commits, commit_temporaries = names reader Commit in
  let committed = String.Hash_set.of_list commits in
  let selected = String.Table.create () in
  let%bind entries =
    List.map intents ~f:(fun filename ->
      let%bind contents = frame Intent filename in
      let%bind record = Record.decode contents in
      let reference = Record.reference record in
      let citation = Reference.citation reference in
      let source =
        P.Recovery_window.Citation.scope citation |> P.Recovery_window.Scope.session
      in
      let%bind expected =
        Record.filename citation ~part:(Reference.part reference)
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "invalid recovery asset part")
      in
      if not (P.Session_ref.equal source owner && String.equal filename expected)
      then corrupt "recovery asset intent does not match its owner or filename"
      else (
        Hashtbl.add_exn selected ~key:filename ~data:contents;
        let%map committed =
          if Hash_set.mem committed filename
          then (
            Hash_set.remove committed filename;
            let%bind actual = frame Commit filename in
            if String.equal actual contents
            then Ok true
            else corrupt "recovery asset commit differs from its intent")
          else Ok false
        in
        { filename; record; committed }))
    |> Result.all
  in
  let valid_temporary phase target contents =
    match Hashtbl.find selected target with
    | Some expected -> String.is_prefix expected ~prefix:contents
    | None ->
      (match phase with
       | Files.Commit -> false
       | Intent ->
         (* A writer interrupted before the first rename may leave only
            a partial atomic temporary. If complete, verify its real owner
            and exact filename before treating it as an abandoned attempt. *)
         (match
            Frame.decode ~max_payload_length:Record.max_frame_bytes ~contents ~offset:0
          with
          | Ok (Incomplete_tail _) -> true
          | Ok (Complete _) ->
            (match Record.decode contents with
             | Error _ -> false
             | Ok record ->
               let reference = Record.reference record in
               let citation = Reference.citation reference in
               let source =
                 P.Recovery_window.Citation.scope citation
                 |> P.Recovery_window.Scope.session
               in
               P.Session_ref.equal owner source
               &&
                 (match Record.filename citation ~part:(Reference.part reference) with
                 | Ok expected -> String.equal target expected
                 | Error _ -> false))
          | Error _ -> false))
  in
  if not (Hash_set.is_empty committed)
  then corrupt "recovery asset commit has no private intent"
  else (
    let%map temporaries =
      List.concat_map
        [ Files.Intent, intent_temporaries; Commit, commit_temporaries ]
        ~f:(fun (phase, files) ->
          List.map files ~f:(fun (name, target) ->
            let%bind contents = frame phase name in
            if valid_temporary phase target contents
            then Ok (path phase name, contents)
            else corrupt "recovery asset temporary differs from its private selection"))
      |> Result.all
    in
    { entries; temporaries })
;;

let remove_temporaries t ~env ~reader =
  let open Result.Let_syntax in
  List.fold_result t.temporaries ~init:() ~f:(fun () (path, expected) ->
    let%bind actual = Reader.read reader ~path ~max_bytes:Record.max_frame_bytes in
    if not (String.equal actual expected)
    then corrupt "recovery asset temporary changed before collection"
    else (
      let native_path = Filename.concat (Reader.root reader) path in
      try
        let file = Eio.Path.(Eio.Stdenv.fs env / native_path) in
        match Eio.Path.kind ~follow:false file with
        | `Regular_file ->
          Eio.Path.unlink file;
          Durable_file.sync_directory ~env ~path:(Filename.dirname native_path)
        | _ -> corrupt "invalid recovery asset temporary during collection"
      with
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        Error
          (Store_error.of_exn
             ~operation:"remove recovery publication temporary"
             ~path
             exn)))
;;
