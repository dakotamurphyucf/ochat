open Core

type mode =
  | Validate_only
  | Dry_run
  | Apply
[@@deriving compare, equal, sexp]

type status =
  | Current
  | Migration_required
  | Schema_too_new
[@@deriving compare, equal, sexp]

type plan =
  { source_version : int
  ; target_version : int
  ; session_count : int
  ; status : status
  ; mode : mode
  }
[@@deriving sexp]

let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let schema_version contents =
  let open Result.Let_syntax in
  let%bind document =
    Document_schema.Document.decode ~limits:Store_schema_document.limits contents
    |> Document_fields.store
  in
  Store_schema_document.stored_version document |> Document_fields.store
;;

let status source_version =
  if source_version = Session_store.current_schema_version
  then Current
  else if source_version < Session_store.current_schema_version
  then Migration_required
  else Schema_too_new
;;

let session_count ~env root =
  try
    Eio.Path.read_dir (eio_path env (Filename.concat root "sessions"))
    |> List.count ~f:(fun name ->
      String.is_prefix name ~prefix:"ses_"
      && Eio.Path.is_directory
           (eio_path env (Filename.concat (Filename.concat root "sessions") name)))
    |> Result.return
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.of_exn ~operation:"inspect migration sessions" ~path:root exn)
;;

let stored_server_id ~env root =
  let open Result.Let_syntax in
  let%bind bytes =
    Durable_file.load_bounded ~env ~path:(Data_root.server_id_path root) ~max_bytes:256
  in
  Agent_protocol.Id.Server.of_string (String.strip bytes)
  |> Document_fields.protocol
  |> Document_fields.store
;;

let inspect ~env ~root ~mode =
  if not (Filename.is_absolute root)
  then
    Error
      (Store_error.Io
         { operation = "inspect migration"
         ; path = root
         ; message = "path must be absolute"
         })
  else
    let open Result.Let_syntax in
    let schema_path = Filename.concat root "schema.sexp" in
    let%bind () =
      try
        match Eio.Path.kind ~follow:false (eio_path env schema_path) with
        | `Regular_file -> Ok ()
        | _ -> Error (Store_error.Corrupt "migration schema is not a regular file")
      with
      | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
        Error
          (Store_error.of_exn ~operation:"inspect migration schema" ~path:schema_path exn)
    in
    let%bind contents =
      Durable_file.load_bounded
        ~env
        ~path:schema_path
        ~max_bytes:(Document_schema.Limits.max_bytes Store_schema_document.limits)
    in
    let%bind source_version = schema_version contents in
    let%bind data_root = Data_root.open_existing ~env ~path:root in
    let%bind server_id = stored_server_id ~env data_root in
    let%bind () =
      if source_version <= Session_store.current_schema_version
      then
        Organization_root.inspect_authority
          ~env
          ~root:data_root
          ~server_id
          ~required:(source_version >= 2)
      else Ok ()
    in
    let%map session_count = session_count ~env root in
    { source_version
    ; target_version = Session_store.current_schema_version
    ; session_count
    ; status = status source_version
    ; mode
    }
;;

let finish plan =
  match plan.mode, plan.status with
  | (Validate_only | Dry_run), _ | Apply, Current -> Ok plan
  | Apply, Migration_required ->
    Error (Store_error.Migration_required plan.source_version)
  | Apply, Schema_too_new -> Error (Store_error.Schema_too_new plan.source_version)
;;

let run ~env ~sw ~root ~server_id ~process_start_identity ~lock_nonce ~mode =
  let open Result.Let_syntax in
  let%bind data_root = Data_root.open_existing ~env ~path:root in
  let%bind lock =
    Lock.acquire
      ~env
      ~sw
      ~path:(Data_root.daemon_lock_path data_root)
      ~server_id
      ~process_start_identity
      ~nonce:lock_nonce
  in
  let migrate () =
    let%bind plan = inspect ~env ~root ~mode in
    match mode, plan.status, plan.source_version with
    | Apply, Migration_required, 1 ->
      let%bind stored_server_id = stored_server_id ~env data_root in
      let%bind organizations =
        Organization_root.open_owned ~env ~sw ~root:data_root ~server_id:stored_server_id
      in
      Organization_store.close organizations;
      Ok { plan with status = Current }
    | _ -> finish plan
  in
  let release_after_failure () =
    try
      Eio.Cancel.protect (fun () ->
        ignore (Lock.release ~env lock : (unit, Store_error.t) result))
    with
    | _ -> ()
  in
  match migrate () with
  | Ok plan ->
    (* With no earlier failure, release is part of this operation's outcome.
       Its typed filesystem error or original exception must remain visible. *)
    Result.map (Lock.release ~env lock) ~f:(fun () -> plan)
  | Error _ as failure ->
    release_after_failure ();
    failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    release_after_failure ();
    Exn.raise_with_original_backtrace exn backtrace
;;
