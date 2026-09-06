open Core

module Metadata = struct
  type t =
    { blob : Agent_protocol.Blob.Metadata.t
    ; creating_principal : Agent_protocol.Id.Principal.t
    ; target_session : Agent_protocol.Id.Session.t option
    ; allowed_use : string
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t option
    ; durable : bool
    }
  [@@deriving sexp]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; temporary_directory : string
  ; durable_directory : string
  ; max_upload_bytes : int64
  }

type store = t

module Handle = struct
  type t =
    { mutable metadata : Metadata.t
    ; mutable data_path : string
    ; mutable metadata_path : string
    }

  let metadata t = t.metadata
end

module Upload = struct
  type t =
    { store : store
    ; id : Agent_protocol.Id.Blob.t
    ; creating_principal : Agent_protocol.Id.Principal.t
    ; target_session : Agent_protocol.Id.Session.t option
    ; kind : Agent_protocol.Blob.kind
    ; media_type : string
    ; display_name : string option
    ; allowed_use : string
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t option
    ; partial_path : string
    ; final_path : string
    ; metadata_path : string
    ; flow : Eio.File.rw_ty Eio.Resource.t
    ; mutable length : int64
    ; mutable digest : Digestif.SHA256.ctx
    ; mutable closed : bool
    }
end

let eio_path t path = Eio.Path.(Eio.Stdenv.fs t.env / path)

let create ~env ~temporary_directory ~durable_directory ~max_upload_bytes =
  if
    (not (Filename.is_absolute temporary_directory))
    || not (Filename.is_absolute durable_directory)
  then
    Error
      (Store_error.Io
         { operation = "create blob store"
         ; path = temporary_directory
         ; message = "blob directories must be absolute"
         })
  else if Int64.(max_upload_bytes <= zero)
  then Error (Store_error.Corrupt "maximum upload size must be positive")
  else (
    try
      let fs = Eio.Stdenv.fs env in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / temporary_directory);
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / durable_directory);
      Ok { env; temporary_directory; durable_directory; max_upload_bytes }
    with
    | exn ->
      Error
        (Store_error.of_exn ~operation:"create blob store" ~path:temporary_directory exn))
;;

let blob_name id suffix = Agent_protocol.Id.Blob.to_string id ^ suffix

let begin_upload
      store
      ~sw
      ~id
      ~creating_principal
      ~target_session
      ~kind
      ~media_type
      ~display_name
      ~allowed_use
      ~created_at
      ~expires_at
  =
  let partial_path = Filename.concat store.temporary_directory (blob_name id ".part") in
  let final_path = Filename.concat store.temporary_directory (blob_name id ".blob") in
  let metadata_path = Filename.concat store.temporary_directory (blob_name id ".sexp") in
  try
    let flow =
      Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) (eio_path store partial_path)
    in
    Ok
      Upload.
        { store
        ; id
        ; creating_principal
        ; target_session
        ; kind
        ; media_type
        ; display_name
        ; allowed_use
        ; created_at
        ; expires_at
        ; partial_path
        ; final_path
        ; metadata_path
        ; flow
        ; length = Int64.zero
        ; digest = Digestif.SHA256.empty
        ; closed = false
        }
  with
  | exn ->
    Error (Store_error.of_exn ~operation:"begin blob upload" ~path:partial_path exn)
;;

let abort upload =
  if not upload.Upload.closed
  then (
    upload.closed <- true;
    (try Eio.Resource.close upload.flow with
     | _ -> ());
    try Eio.Path.unlink (eio_path upload.store upload.partial_path) with
    | _ -> ())
;;

let write_string upload chunk =
  if upload.Upload.closed
  then Error (Store_error.Corrupt "blob upload is closed")
  else (
    let next_length = Int64.(upload.length + of_int (String.length chunk)) in
    if Int64.(next_length > upload.store.max_upload_bytes)
    then (
      abort upload;
      Error (Store_error.Corrupt "blob upload exceeds configured maximum"))
    else (
      try
        Eio.Flow.copy_string chunk upload.flow;
        upload.length <- next_length;
        upload.digest <- Digestif.SHA256.feed_string upload.digest chunk;
        Ok ()
      with
      | exn ->
        abort upload;
        Error
          (Store_error.of_exn
             ~operation:"write blob upload"
             ~path:upload.partial_path
             exn)))
;;

let save_metadata store path metadata =
  Durable_file.replace
    ~env:store.env
    ~durability:Flush_file_and_directory
    ~path
    (Sexp.to_string_mach ([%sexp_of: Metadata.t] metadata))
;;

let finish upload ~expected_digest =
  if upload.Upload.closed
  then Error (Store_error.Corrupt "blob upload is closed")
  else (
    let digest = Digestif.SHA256.(get upload.digest |> to_hex) in
    if not (Option.for_all expected_digest ~f:(String.Caseless.equal digest))
    then (
      abort upload;
      Error (Store_error.Corrupt "blob digest differs from the expected digest"))
    else
      let open Result.Let_syntax in
      let%bind blob =
        Agent_protocol.Blob.Metadata.create
          ~id:upload.id
          ~kind:upload.kind
          ~media_type:upload.media_type
          ~byte_length:upload.length
          ~digest
          ?display_name:upload.display_name
          ()
        |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
      in
      let metadata =
        Metadata.
          { blob
          ; creating_principal = upload.creating_principal
          ; target_session = upload.target_session
          ; allowed_use = upload.allowed_use
          ; created_at = upload.created_at
          ; expires_at = upload.expires_at
          ; durable = false
          }
      in
      try
        Eio.File.sync upload.flow;
        Eio.Resource.close upload.flow;
        upload.closed <- true;
        Eio.Path.rename
          (eio_path upload.store upload.partial_path)
          (eio_path upload.store upload.final_path);
        let%map () = save_metadata upload.store upload.metadata_path metadata in
        { Handle.metadata
        ; data_path = upload.final_path
        ; metadata_path = upload.metadata_path
        }
      with
      | exn ->
        (try Eio.Path.unlink (eio_path upload.store upload.partial_path) with
         | _ -> ());
        (try Eio.Path.unlink (eio_path upload.store upload.final_path) with
         | _ -> ());
        Error
          (Store_error.of_exn ~operation:"finish blob upload" ~path:upload.final_path exn))
;;

let load_metadata store path =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env:store.env ~path in
  try Ok ([%of_sexp: Metadata.t] (Sexp.of_string contents)) with
  | exn ->
    Error (Store_error.Corrupt ("blob metadata decode failed: " ^ Exn.to_string exn))
;;

let open_temporary store id =
  let metadata_path = Filename.concat store.temporary_directory (blob_name id ".sexp") in
  let data_path = Filename.concat store.temporary_directory (blob_name id ".blob") in
  let open Result.Let_syntax in
  let%bind metadata = load_metadata store metadata_path in
  if Agent_protocol.Id.Blob.compare metadata.blob.id id <> 0 || metadata.durable
  then Error (Store_error.Corrupt "temporary blob metadata identity is invalid")
  else if not (Eio.Path.is_file (eio_path store data_path))
  then Error (Store_error.Missing data_path)
  else Ok { Handle.metadata; data_path; metadata_path }
;;

let open_session store session id =
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let metadata_path = Filename.concat directory (blob_name id ".sexp") in
  let data_path = Filename.concat directory (blob_name id ".blob") in
  let open Result.Let_syntax in
  let%bind metadata = load_metadata store metadata_path in
  if Agent_protocol.Id.Blob.compare metadata.blob.id id <> 0 || not metadata.durable
  then Error (Store_error.Corrupt "session blob metadata identity is invalid")
  else if
    not
      (Option.exists metadata.target_session ~f:(fun target ->
         Agent_protocol.Id.Session.compare
           target
           (Session_store.Handle.session_id session)
         = 0))
  then Error (Store_error.Corrupt "session blob target identity is invalid")
  else if not (Eio.Path.is_file (eio_path store data_path))
  then Error (Store_error.Missing data_path)
  else Ok { Handle.metadata; data_path; metadata_path }
;;

let load store handle =
  try Ok (Eio.Path.load (eio_path store handle.Handle.data_path)) with
  | exn -> Error (Store_error.of_exn ~operation:"load blob" ~path:handle.data_path exn)
;;

let read_range store ~sw handle ~offset ~max_bytes =
  let length = handle.Handle.metadata.blob.byte_length in
  if Int64.(offset < zero || offset > length)
  then Error (Store_error.Corrupt "blob read offset is outside the blob")
  else if max_bytes <= 0
  then Error (Store_error.Corrupt "blob read size must be positive")
  else (
    try
      let source = Eio.Path.open_in ~sw (eio_path store handle.Handle.data_path) in
      let remaining = Int64.(length - offset) in
      let count = Int64.min remaining (Int64.of_int max_bytes) |> Int64.to_int_exn in
      let buffer = Cstruct.create count in
      let read =
        if count = 0
        then 0
        else Eio.File.pread source ~file_offset:(Optint.Int63.of_int64 offset) [ buffer ]
      in
      Ok (Cstruct.to_string (Cstruct.sub buffer 0 read))
    with
    | exn ->
      Error (Store_error.of_exn ~operation:"read blob range" ~path:handle.data_path exn))
;;

let iter_chunks store ~sw handle ~chunk_size ~f =
  if chunk_size <= 0
  then Error (Store_error.Corrupt "blob chunk size must be positive")
  else (
    try
      let source = Eio.Path.open_in ~sw (eio_path store handle.Handle.data_path) in
      let buffer = Cstruct.create chunk_size in
      let rec loop () =
        match Eio.Flow.single_read source buffer with
        | 0 -> Ok ()
        | count ->
          f (Cstruct.to_string (Cstruct.sub buffer 0 count));
          loop ()
        | exception End_of_file -> Ok ()
      in
      loop ()
    with
    | exn ->
      Error (Store_error.of_exn ~operation:"stream blob" ~path:handle.data_path exn))
;;

let adopt store session handle =
  if handle.Handle.metadata.durable
  then Ok handle
  else
    let open Result.Let_syntax in
    let session_id = Session_store.Handle.session_id session in
    let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
    let destination_data =
      Filename.concat directory (blob_name handle.metadata.blob.id ".blob")
    in
    let destination_metadata =
      Filename.concat directory (blob_name handle.metadata.blob.id ".sexp")
    in
    let%bind () =
      try
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path store directory);
        Eio.Path.rename
          (eio_path store handle.data_path)
          (eio_path store destination_data);
        Ok ()
      with
      | exn ->
        Error (Store_error.of_exn ~operation:"adopt blob data" ~path:destination_data exn)
    in
    let metadata =
      { handle.metadata with target_session = Some session_id; durable = true }
    in
    let%bind () = save_metadata store destination_metadata metadata in
    (try Eio.Path.unlink (eio_path store handle.metadata_path) with
     | _ -> ());
    handle.metadata <- metadata;
    handle.data_path <- destination_data;
    handle.metadata_path <- destination_metadata;
    Ok handle
;;

let expired ~now metadata =
  Option.value_map metadata.Metadata.expires_at ~default:false ~f:(fun expires_at ->
    Agent_protocol.Timestamp.compare expires_at now <= 0)
;;

let cleanup_one store ~now filename =
  let metadata_path = Filename.concat store.temporary_directory filename in
  match load_metadata store metadata_path with
  | Error _ -> Ok 0
  | Ok metadata when not (expired ~now metadata) -> Ok 0
  | Ok metadata ->
    let data_path =
      Filename.concat store.temporary_directory (blob_name metadata.blob.id ".blob")
    in
    (try Eio.Path.unlink (eio_path store data_path) with
     | _ -> ());
    (try Eio.Path.unlink (eio_path store metadata_path) with
     | _ -> ());
    Ok 1
;;

let cleanup_expired store ~now =
  try
    Eio.Path.read_dir (eio_path store store.temporary_directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".sexp")
    |> List.map ~f:(cleanup_one store ~now)
    |> Result.all
    |> Result.map ~f:(List.fold ~init:0 ~f:( + ))
  with
  | exn ->
    Error
      (Store_error.of_exn
         ~operation:"cleanup temporary blobs"
         ~path:store.temporary_directory
         exn)
;;
