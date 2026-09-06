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
  match Or_error.try_with (fun () -> Sexp.of_string contents) with
  | Error _ -> Error (Store_error.Corrupt "store schema is not a valid S-expression")
  | Ok (Sexp.List fields) ->
    (match
       List.find_map fields ~f:(function
         | Sexp.List [ Sexp.Atom "version"; Sexp.Atom version ] ->
           Int.of_string_opt version
         | _ -> None)
     with
     | Some version -> Ok version
     | None -> Error (Store_error.Corrupt "store schema has no version"))
  | _ -> Error (Store_error.Corrupt "store schema must be a record")
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
  | exn ->
    Error (Store_error.of_exn ~operation:"inspect migration sessions" ~path:root exn)
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
    let%bind contents = Durable_file.load ~env ~path:schema_path in
    let%bind source_version = schema_version contents in
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
  Exn.protect
    ~f:(fun () ->
      let%bind plan = inspect ~env ~root ~mode in
      finish plan)
    ~finally:(fun () -> ignore (Lock.release ~env lock : (unit, Store_error.t) result))
;;
