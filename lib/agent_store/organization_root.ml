open Core
module D = Document_schema
module F = Document_fields

let directory ~env ~sw root =
  let path = Eio.Path.(Eio.Stdenv.fs env / Data_root.indexes_path root) in
  try
    match Eio.Path.kind ~follow:false path with
    | `Directory -> Ok (Eio.Path.open_dir ~sw path)
    | _ -> Error (Store_error.Corrupt "organization indexes directory is not owned")
  with
  | Eio.Io _ as exn ->
    Error
      (Store_error.of_exn
         ~operation:"open organization directory"
         ~path:(Data_root.indexes_path root)
         exn)
;;

let check_identity ~env root server_id =
  let open Result.Let_syntax in
  let%bind encoded =
    Durable_file.load_bounded ~env ~path:(Data_root.server_id_path root) ~max_bytes:256
  in
  let%bind stored =
    Agent_protocol.Id.Server.of_string (String.strip encoded) |> F.protocol |> F.store
  in
  if Agent_protocol.Id.Server.equal stored server_id
  then Ok ()
  else Error (Store_error.Corrupt "root server identity differs from organization owner")
;;

let inspect_authority ~env ~root ~server_id ~required =
  let open Result.Let_syntax in
  let%bind () = check_identity ~env root server_id in
  let path = Eio.Path.(Eio.Stdenv.fs env / Data_root.indexes_path root) in
  try
    if
      not
        (match Eio.Path.kind ~follow:false path with
         | `Directory -> true
         | _ -> false)
    then Error (Store_error.Corrupt "organization indexes directory is not owned")
    else
      Eio.Path.with_open_dir path (fun directory ->
        match
          Durable_file.load_bounded_in
            ~directory
            ~basename:"organization.json"
            ~max_bytes:(D.Limits.max_bytes Organization_document.limits)
        with
        | Error (Store_error.Missing _) when not required -> Ok ()
        | Error error -> Error error
        | Ok bytes ->
          let%bind document =
            D.Document.decode ~limits:Organization_document.limits bytes |> F.store
          in
          let%bind carrier = Organization_document.restore document in
          if
            Agent_protocol.Id.Server.equal
              server_id
              (Organization_state.server_id (D.Extension_carrier.value carrier))
          then Ok ()
          else Error (Store_error.Corrupt "organization document belongs to another host"))
  with
  | Eio.Io _ as exn ->
    Error
      (Store_error.of_exn
         ~operation:"inspect organization authority"
         ~path:(Data_root.indexes_path root)
         exn)
;;

let publish_schema ~env root carrier =
  let open Result.Let_syntax in
  let%bind document = Store_schema_document.to_document carrier |> F.store in
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(Data_root.schema_path root)
    (D.Document.to_string document)
;;

let publish_initialized_schema ~env root carrier =
  let open Result.Let_syntax in
  (* A prior attempt may have installed organization.json but failed before its
     directory sync. Reading valid bytes alone cannot certify that namespace
     publication. Flush the authority parent before making schema 2 durable. *)
  let%bind () = Durable_file.sync_directory ~env ~path:(Data_root.indexes_path root) in
  publish_schema ~env root carrier
;;

let finish_initialization organizations ~publish =
  let close () =
    try Eio.Cancel.protect (fun () -> Organization_store.close organizations) with
    | _ -> ()
  in
  (* Publication may have installed schema bytes before failing. Close only
     this attempt's receiver; never roll back the durable root authority. *)
  match publish () with
  | Ok () -> Ok organizations
  | Error error ->
    close ();
    Error error
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    close ();
    Exn.raise_with_original_backtrace exn backtrace
;;

let open_owned ~env ~sw ~root ~server_id =
  let open Result.Let_syntax in
  let%bind () = check_identity ~env root server_id in
  let schema_path = Data_root.schema_path root in
  let%bind () =
    try
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / schema_path) with
      | `Regular_file -> Ok ()
      | _ -> Error (Store_error.Corrupt "root schema is not a regular owned file")
    with
    | Eio.Io _ as exn ->
      Error
        (Store_error.of_exn ~operation:"admit organization root" ~path:schema_path exn)
  in
  let%bind bytes =
    Durable_file.load_bounded
      ~env
      ~path:schema_path
      ~max_bytes:(D.Limits.max_bytes Store_schema_document.limits)
  in
  let%bind document =
    D.Document.decode ~limits:Store_schema_document.limits bytes |> F.store
  in
  let%bind version = Store_schema_document.stored_version document |> F.store in
  let%bind carrier = Store_schema_document.of_document document |> F.store in
  let%bind directory = directory ~env ~sw root in
  let initialization =
    if version = 1 then Organization_store.Create_if_missing else Require_existing
  in
  let%bind organizations =
    Organization_store.open_owned ~directory ~server_id ~initialization
  in
  finish_initialization organizations ~publish:(fun () ->
    if version = 1 then publish_initialized_schema ~env root carrier else Ok ())
;;

let create_owned ~env ~sw ~root ~server_id ~created_at =
  let open Result.Let_syntax in
  let%bind directory = directory ~env ~sw root in
  let%bind organizations =
    Organization_store.open_owned ~directory ~server_id ~initialization:Create_if_missing
  in
  finish_initialization organizations ~publish:(fun () ->
    publish_initialized_schema
      ~env
      root
      (D.Extension_carrier.of_authored_value Store_schema_document.{ created_at }))
;;
