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

type coordination =
  { mutex : Eio.Mutex.t
  ; mutable active_uploads : int
  ; mutable active_reads : int
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; temporary_directory : string
  ; durable_directory : string
  ; max_upload_bytes : int64
  ; coordination : coordination
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
    ; mutable release_hook : Eio.Switch.hook
    ; mutable counted : bool
    }
end

let eio_path t path = Eio.Path.(Eio.Stdenv.fs t.env / path)
let max_upload_bytes t = t.max_upload_bytes

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
      Ok
        { env
        ; temporary_directory
        ; durable_directory
        ; max_upload_bytes
        ; coordination =
            { mutex = Eio.Mutex.create (); active_uploads = 0; active_reads = 0 }
        }
    with
    | exn ->
      Error
        (Store_error.of_exn ~operation:"create blob store" ~path:temporary_directory exn))
;;

let with_max_upload_bytes store ~max_upload_bytes =
  match Int64.(max_upload_bytes > zero) with
  | false -> Error (Store_error.Corrupt "maximum upload size must be positive")
  | true -> Ok { store with max_upload_bytes }
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
        ; release_hook = Eio.Switch.null_hook
        ; counted = false
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
      Exn.protect ~finally:(fun () -> Eio.Resource.close source) ~f:loop
    with
    | exn ->
      Error (Store_error.of_exn ~operation:"stream blob" ~path:handle.data_path exn))
;;

let adopt store session handle =
  let session_id = Session_store.Handle.session_id session in
  match handle.Handle.metadata.target_session with
  | Some id when not (Agent_protocol.Id.Session.equal id session_id) ->
    Error (Store_error.Corrupt "blob is bound to another target session")
  | target ->
    (match handle.metadata.durable, target with
     | true, Some _ -> Ok handle
     | true, None -> Error (Store_error.Corrupt "durable blob has no target session")
     | false, _ ->
       Eio.Cancel.protect (fun () ->
         let open Result.Let_syntax in
         let directory =
           Filename.concat (Session_store.Handle.directory session) "blobs"
         in
         let destination_data =
           Filename.concat directory (blob_name handle.metadata.blob.id ".blob")
         in
         let destination_metadata =
           Filename.concat directory (blob_name handle.metadata.blob.id ".sexp")
         in
         let%bind () =
           try
             Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path store directory);
             match
               ( Eio.Path.kind ~follow:false (eio_path store destination_data)
               , Eio.Path.kind ~follow:false (eio_path store destination_metadata) )
             with
             | `Not_found, `Not_found ->
               Eio.Path.rename
                 (eio_path store handle.data_path)
                 (eio_path store destination_data);
               Ok ()
             | _ ->
               Error (Store_error.Corrupt "blob adoption would overwrite existing data")
           with
           | exn ->
             Error
               (Store_error.of_exn
                  ~operation:"adopt blob data"
                  ~path:destination_data
                  exn)
         in
         let metadata =
           { handle.metadata with target_session = Some session_id; durable = true }
         in
         match save_metadata store destination_metadata metadata with
         | Error failure ->
           (try
              Eio.Path.rename
                (eio_path store destination_data)
                (eio_path store handle.data_path);
              (match
                 Eio.Path.kind ~follow:false (eio_path store destination_metadata)
               with
               | `Not_found -> ()
               | _ -> Eio.Path.unlink (eio_path store destination_metadata));
              let%bind () = Durable_file.sync_directory ~env:store.env ~path:directory in
              let%bind () =
                Durable_file.sync_directory ~env:store.env ~path:store.temporary_directory
              in
              Error failure
            with
            | exn ->
              Error
                (Store_error.of_exn
                   ~operation:"restore failed blob adoption"
                   ~path:destination_data
                   exn))
         | Ok () ->
           (try Eio.Path.unlink (eio_path store handle.metadata_path) with
            | _ -> ());
           handle.metadata <- metadata;
           handle.data_path <- destination_data;
           handle.metadata_path <- destination_metadata;
           Ok handle))
;;

let load_verified store ~sw handle ~max_bytes =
  let metadata = handle.Handle.metadata.blob in
  match
    max_bytes > 0
    && Int64.(metadata.byte_length >= zero && metadata.byte_length <= of_int max_bytes)
  with
  | false -> Error (Store_error.Corrupt "blob exceeds the requested read limit")
  | true ->
    let buffer = Buffer.create (Int.min max_bytes 8192) in
    let digest = ref Digestif.SHA256.empty in
    let overflow = ref false in
    let open Result.Let_syntax in
    let read =
      iter_chunks store ~sw handle ~chunk_size:8192 ~f:(fun chunk ->
        match
          String.length chunk <= max_bytes - Buffer.length buffer && not !overflow
        with
        | false ->
          overflow := true;
          raise Exit
        | true ->
          Buffer.add_string buffer chunk;
          digest := Digestif.SHA256.feed_string !digest chunk)
    in
    let%bind () =
      match read, !overflow with
      | _, true -> Error (Store_error.Corrupt "blob grew beyond its read limit")
      | result, false -> result
    in
    (match
       (not !overflow)
       && Int64.equal metadata.byte_length (Int64.of_int (Buffer.length buffer))
       && String.equal metadata.digest Digestif.SHA256.(get !digest |> to_hex)
     with
     | true -> Ok (Buffer.contents buffer)
     | false ->
       Error
         (Store_error.Corrupt "blob content does not match its recorded length and digest"))
;;

let load_staged_content store ~sw session ~(metadata : Metadata.t) ~max_bytes =
  let invalid message = Error (Store_error.Corrupt message) in
  let open Result.Let_syntax in
  let%bind () =
    match
      (not metadata.durable)
      && Option.exists
           metadata.target_session
           ~f:(Agent_protocol.Id.Session.equal (Session_store.Handle.session_id session))
    with
    | true -> Ok ()
    | false -> invalid "staged result belongs to another session"
  in
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let locations =
    [ directory, ".blob", false
    ; store.temporary_directory, ".blob", false
    ; store.temporary_directory, ".part", true
    ]
  in
  let rec read = function
    | [] -> Ok None
    | (directory, suffix, partial) :: rest ->
      let%bind () =
        match Eio.Path.kind ~follow:false (eio_path store directory) with
        | `Directory | `Not_found -> Ok ()
        | _ -> invalid "staged result directory is not a regular directory"
      in
      let path = Filename.concat directory (blob_name metadata.blob.id suffix) in
      (match Eio.Path.kind ~follow:false (eio_path store path) with
       | `Not_found -> read rest
       | `Regular_file ->
         let size =
           (Eio.Path.stat ~follow:false (eio_path store path)).size
           |> Optint.Int63.to_int64
         in
         (match partial && Int64.(size < metadata.blob.byte_length) with
          | true -> Ok None
          | false ->
            let handle = { Handle.metadata; data_path = path; metadata_path = path } in
            let%map content = load_verified store ~sw handle ~max_bytes in
            Some content)
       | _ -> invalid "staged result data is not a regular file")
  in
  try read locations with
  | exn -> Error (Store_error.of_exn ~operation:"load staged result" ~path:directory exn)
;;

let ensure_staged_content store ~sw session ~(metadata : Metadata.t) content =
  let session_id = Session_store.Handle.session_id session in
  let invalid message = Error (Store_error.Corrupt message) in
  let open Result.Let_syntax in
  let%bind _ =
    Agent_protocol.Blob.Metadata.of_json
      (Agent_protocol.Blob.Metadata.to_json metadata.blob)
    |> Result.map_error ~f:(fun error ->
      Store_error.Corrupt error.Agent_protocol.Error.message)
  in
  let%bind () =
    match
      (not metadata.durable)
      && Option.exists
           metadata.target_session
           ~f:(Agent_protocol.Id.Session.equal session_id)
      && Int64.equal metadata.blob.byte_length (Int64.of_int (String.length content))
      && Int64.(metadata.blob.byte_length <= store.max_upload_bytes)
      && String.equal
           metadata.blob.digest
           Digestif.SHA256.(digest_string content |> to_hex)
    with
    | true -> Ok ()
    | false -> invalid "staged blob content differs from its owned metadata"
  in
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let name suffix = blob_name metadata.blob.id suffix in
  let temporary_data = Filename.concat store.temporary_directory (name ".blob") in
  let temporary_metadata = Filename.concat store.temporary_directory (name ".sexp") in
  let partial = Filename.concat store.temporary_directory (name ".part") in
  let final_data = Filename.concat directory (name ".blob") in
  let final_metadata = Filename.concat directory (name ".sexp") in
  let durable = { metadata with durable = true } in
  let verify path expected ~partial =
    let handle = { Handle.metadata; data_path = path; metadata_path = path } in
    let offset = ref 0 in
    let mismatch = ref false in
    let read =
      iter_chunks store ~sw handle ~chunk_size:8192 ~f:(fun chunk ->
        let count = String.length chunk in
        match
          count <= String.length expected - !offset
          && String.equal chunk (String.sub expected ~pos:!offset ~len:count)
        with
        | false ->
          mismatch := true;
          raise Exit
        | true -> offset := !offset + count)
    in
    match !mismatch, read with
    | true, _ -> invalid "staged blob file differs from the selected completion"
    | false, (Error _ as failure) -> failure
    | false, Ok () when partial || !offset = String.length expected -> Ok ()
    | false, Ok () -> invalid "staged blob file is incomplete"
  in
  let inspect path expected ~partial =
    match Eio.Path.kind ~follow:false (eio_path store path) with
    | `Not_found -> Ok false
    | `Regular_file -> Result.map (verify path expected ~partial) ~f:(fun () -> true)
    | _ -> invalid "staged blob path is not a regular file"
  in
  Eio.Cancel.protect (fun () ->
    try
      let%bind () =
        List.fold_result
          [ directory; store.temporary_directory ]
          ~init:()
          ~f:(fun () path ->
            match Eio.Path.kind ~follow:false (eio_path store path) with
            | `Directory | `Not_found -> Ok ()
            | _ -> invalid "staged blob directory is not a regular directory")
      in
      let%bind _ =
        inspect
          temporary_metadata
          (Metadata.sexp_of_t metadata |> Sexp.to_string_mach)
          ~partial:false
      in
      let%bind _ =
        inspect
          final_metadata
          (Metadata.sexp_of_t durable |> Sexp.to_string_mach)
          ~partial:false
      in
      let%bind has_final = inspect final_data content ~partial:false in
      let%bind has_temporary = inspect temporary_data content ~partial:false in
      let%bind has_partial = inspect partial content ~partial:true in
      let%bind () =
        match has_final, has_temporary with
        | true, _ | false, true -> Ok ()
        | false, false ->
          if has_partial then Eio.Path.unlink (eio_path store partial);
          let%bind upload =
            begin_upload
              store
              ~sw
              ~id:metadata.blob.id
              ~creating_principal:metadata.creating_principal
              ~target_session:metadata.target_session
              ~kind:metadata.blob.kind
              ~media_type:metadata.blob.media_type
              ~display_name:metadata.blob.display_name
              ~allowed_use:metadata.allowed_use
              ~created_at:metadata.created_at
              ~expires_at:metadata.expires_at
          in
          Exn.protect
            ~finally:(fun () -> abort upload)
            ~f:(fun () ->
              let%bind () = write_string upload content in
              let%map _ = finish upload ~expected_digest:(Some metadata.blob.digest) in
              ())
      in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path store directory);
      let%bind () =
        Durable_file.sync_directory
          ~env:store.env
          ~path:(Session_store.Handle.directory session)
      in
      (match has_final with
       | true -> ()
       | false ->
         Eio.Path.rename (eio_path store temporary_data) (eio_path store final_data));
      let%bind () = save_metadata store final_metadata durable in
      List.iter [ temporary_data; temporary_metadata; partial ] ~f:(fun path ->
        match Eio.Path.kind ~follow:false (eio_path store path) with
        | `Not_found -> ()
        | `Regular_file -> Eio.Path.unlink (eio_path store path)
        | _ -> failwith "staged blob cleanup encountered a non-file");
      let%map () =
        Durable_file.sync_directory ~env:store.env ~path:store.temporary_directory
      in
      { Handle.metadata = durable
      ; data_path = final_data
      ; metadata_path = final_metadata
      }
    with
    | exn ->
      Error (Store_error.of_exn ~operation:"resume staged blob" ~path:final_data exn))
;;

let discard_unreferenced store session handle =
  let id = handle.Handle.metadata.blob.id in
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let data_path = Filename.concat directory (blob_name id ".blob") in
  let metadata_path = Filename.concat directory (blob_name id ".sexp") in
  let open Result.Let_syntax in
  let%bind () =
    match handle.metadata.target_session with
    | Some target
      when Agent_protocol.Id.Session.equal
             target
             (Session_store.Handle.session_id session) -> Ok ()
    | _ -> Error (Store_error.Corrupt "unreferenced blob belongs to another session")
  in
  let%bind current =
    match load_metadata store metadata_path with
    | Ok metadata -> Ok (Some metadata)
    | Error (Store_error.Missing _ as error) ->
      (match Eio.Path.kind ~follow:false (eio_path store data_path) with
       | `Not_found -> Ok None
       | _ -> Error error)
    | Error error -> Error error
  in
  match
    Option.for_all current ~f:(fun current ->
      Sexp.equal ([%sexp_of: Metadata.t] current) ([%sexp_of: Metadata.t] handle.metadata))
  with
  | false -> Error (Store_error.Corrupt "blob changed before unreferenced cleanup")
  | true ->
    Eio.Cancel.protect (fun () ->
      try
        List.iter [ data_path; metadata_path ] ~f:(fun path ->
          match Eio.Path.kind ~follow:false (eio_path store path) with
          | `Not_found -> ()
          | _ -> Eio.Path.unlink (eio_path store path));
        Durable_file.sync_directory ~env:store.env ~path:directory
      with
      | exn ->
        Error
          (Store_error.of_exn ~operation:"discard unreferenced blob" ~path:data_path exn))
;;

let expired ~now metadata =
  Option.value_map metadata.Metadata.expires_at ~default:false ~f:(fun expires_at ->
    Agent_protocol.Timestamp.compare expires_at now <= 0)
;;

let cleanup_one store ~protect ~now filename =
  let metadata_path = Filename.concat store.temporary_directory filename in
  match load_metadata store metadata_path with
  | Error _ -> Ok 0
  | Ok metadata when not (expired ~now metadata) -> Ok 0
  | Ok metadata ->
    let open Result.Let_syntax in
    let%bind _ =
      Agent_protocol.Blob.Metadata.of_json
        (Agent_protocol.Blob.Metadata.to_json metadata.blob)
      |> Result.map_error ~f:(fun failure ->
        Store_error.Corrupt failure.Agent_protocol.Error.message)
    in
    let%bind () =
      match
        (not metadata.durable)
        && String.equal filename (blob_name metadata.blob.id ".sexp")
      with
      | true -> Ok ()
      | false -> Error (Store_error.Corrupt "temporary expiry metadata identity mismatch")
    in
    let%bind protected = protect metadata in
    (match protected with
     | true -> Ok 0
     | false ->
       let data_path =
         Filename.concat store.temporary_directory (blob_name metadata.blob.id ".blob")
       in
       (try Eio.Path.unlink (eio_path store data_path) with
        | _ -> ());
       (try Eio.Path.unlink (eio_path store metadata_path) with
        | _ -> ());
       Ok 1)
;;

let cleanup_expired ?(protect = fun _ -> Ok false) store ~now =
  try
    Eio.Path.read_dir (eio_path store store.temporary_directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".sexp")
    |> List.map ~f:(cleanup_one store ~protect ~now)
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

(* Composite storage operations above call their lexical, unguarded helpers.
   Public entrypoints below acquire the shared coordinator once. Never call a
   public entrypoint from a retention callback. *)
type retention =
  { store : t
  ; mutable active : bool
  }

let coordinated store f =
  (* Individual operations preserve recoverable files and upload accounting on
     failure. An IO error or cancelled reader must not poison the shared mutex. *)
  let outcome =
    Eio.Mutex.use_rw ~protect:false store.coordination.mutex (fun () ->
      try Ok (f ()) with
      | exn -> Error exn)
  in
  match outcome with
  | Ok value -> value
  | Error exn -> raise exn
;;

let discard_retained_unreferenced retention session handle =
  match retention.active with
  | false -> Error (Store_error.Corrupt "blob retention scope has ended")
  | true -> discard_unreferenced retention.store session handle
;;

let with_retention store ~f =
  coordinated store (fun () ->
    Eio.Cancel.protect (fun () ->
      match store.coordination.active_uploads, store.coordination.active_reads with
      | 0, 0 ->
        let retention = { store; active = true } in
        Exn.protect
          ~finally:(fun () -> retention.active <- false)
          ~f:(fun () -> Result.map (f retention) ~f:Option.some)
      | _ -> Ok None))
;;

let reading store f =
  let entered = ref false in
  Exn.protect
    ~finally:(fun () ->
      match !entered with
      | false -> ()
      | true ->
        Eio.Cancel.protect (fun () ->
          coordinated store (fun () ->
            store.coordination.active_reads <- store.coordination.active_reads - 1)))
    ~f:(fun () ->
      coordinated store (fun () ->
        store.coordination.active_reads <- store.coordination.active_reads + 1;
        entered := true);
      f ())
;;

let release_upload upload =
  Eio.Switch.remove_hook upload.Upload.release_hook;
  upload.release_hook <- Eio.Switch.null_hook;
  match upload.counted with
  | false -> ()
  | true ->
    upload.counted <- false;
    upload.store.coordination.active_uploads
    <- upload.store.coordination.active_uploads - 1
;;

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
  (* Register before acquiring the mutex: an already-finished switch invokes
     its release hook immediately. The holder and upload accounting are only
     accessed under the coordinator. *)
  let holder = ref None in
  let hook = ref Eio.Switch.null_hook in
  try
    hook
    := Eio.Switch.on_release_cancellable sw (fun () ->
         coordinated store (fun () ->
           Option.iter !holder ~f:(fun upload ->
             abort upload;
             release_upload upload)));
    coordinated store (fun () ->
      Eio.Switch.check sw;
      match
        begin_upload
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
      with
      | Error _ as failure ->
        Eio.Switch.remove_hook !hook;
        failure
      | Ok upload ->
        upload.release_hook <- !hook;
        upload.counted <- true;
        store.coordination.active_uploads <- store.coordination.active_uploads + 1;
        holder := Some upload;
        Ok upload)
  with
  | exn ->
    Eio.Switch.remove_hook !hook;
    Error
      (Store_error.of_exn
         ~operation:"begin coordinated blob upload"
         ~path:store.temporary_directory
         exn)
;;

let abort upload =
  Eio.Cancel.protect (fun () ->
    coordinated upload.Upload.store (fun () ->
      Exn.protect ~finally:(fun () -> release_upload upload) ~f:(fun () -> abort upload)))
;;

let write_string upload chunk =
  coordinated upload.Upload.store (fun () ->
    Exn.protect
      ~finally:(fun () -> if upload.closed then release_upload upload)
      ~f:(fun () -> write_string upload chunk))
;;

let finish upload ~expected_digest =
  coordinated upload.Upload.store (fun () ->
    Exn.protect
      ~finally:(fun () -> if upload.closed then release_upload upload)
      ~f:(fun () -> finish upload ~expected_digest))
;;

let open_temporary store id = reading store (fun () -> open_temporary store id)

let open_session store session id =
  reading store (fun () -> open_session store session id)
;;

let load store handle = reading store (fun () -> load store handle)

let read_range store ~sw handle ~offset ~max_bytes =
  reading store (fun () -> read_range store ~sw handle ~offset ~max_bytes)
;;

let iter_chunks store ~sw handle ~chunk_size ~f =
  reading store (fun () -> iter_chunks store ~sw handle ~chunk_size ~f)
;;

let adopt store session handle = coordinated store (fun () -> adopt store session handle)

let load_verified store ~sw handle ~max_bytes =
  reading store (fun () -> load_verified store ~sw handle ~max_bytes)
;;

let load_staged_content store ~sw session ~metadata ~max_bytes =
  reading store (fun () -> load_staged_content store ~sw session ~metadata ~max_bytes)
;;

let ensure_staged_content store ~sw session ~metadata content =
  coordinated store (fun () -> ensure_staged_content store ~sw session ~metadata content)
;;

let discard_unreferenced store session handle =
  coordinated store (fun () -> discard_unreferenced store session handle)
;;

let cleanup_expired ?protect store ~now =
  coordinated store (fun () -> cleanup_expired ?protect store ~now)
;;
