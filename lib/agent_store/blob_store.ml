open Core
module Metadata = Blob_metadata

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
  module Location = struct
    type t =
      { data_path : string
      ; metadata_path : string
      }
  end

  module Observation = struct
    type t =
      { document : Blob_metadata_document.t
      ; location : Location.t
      }

    let metadata t = Blob_metadata_document.value t.document
  end

  type availability =
    | Available of Observation.t
    | Unavailable of
        { last_observation : Observation.t
        ; failure : Store_error.t
        }

  type t = { mutable availability : availability }

  let create document ~data_path ~metadata_path =
    { availability = Available { document; location = { data_path; metadata_path } } }
  ;;

  let observation t =
    match t.availability with
    | Available observation -> observation
    | Unavailable { last_observation; _ } -> last_observation
  ;;

  let metadata t = Observation.metadata (observation t)
  let document t = (observation t).document
  let data_path t = (observation t).location.data_path
  let metadata_path t = (observation t).location.metadata_path

  let metadata_checked t =
    match t.availability with
    | Available observation -> Ok (Observation.metadata observation)
    | Unavailable { failure; _ } -> Error failure
  ;;

  let unavailable t failure =
    t.availability <- Unavailable { last_observation = observation t; failure }
  ;;

  let publish t document ~data_path ~metadata_path =
    t.availability <- Available { document; location = { data_path; metadata_path } }
  ;;
end

(* Secondary recovery runs only after a primary adoption failure. It must never
   replace that primary result or exception, and begins by removing authority. *)
module Adoption = struct
  let recover handle ~f =
    Handle.unavailable
      handle
      (Store_error.Corrupt
         "blob adoption requires verified reopen after uncertain publication");
    Eio.Cancel.protect (fun () ->
      try f () with
      | _ -> ())
  ;;

  let restore handle ~f =
    recover handle ~f:(fun () -> ignore (f () : (unit, Store_error.t) result))
  ;;
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
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
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
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.of_exn ~operation:"begin blob upload" ~path:partial_path exn)
;;

let abort upload =
  if not upload.Upload.closed
  then (
    upload.closed <- true;
    (try Eio.Resource.close upload.flow with
     | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
     | _ -> ());
    try Eio.Path.unlink (eio_path upload.store upload.partial_path) with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
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
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        abort upload;
        Error
          (Store_error.of_exn
             ~operation:"write blob upload"
             ~path:upload.partial_path
             exn)))
;;

let save_metadata store path document =
  let open Result.Let_syntax in
  let%bind contents = Blob_metadata_document.to_bytes document |> Document_fields.store in
  Durable_file.replace ~env:store.env ~durability:Flush_file_and_directory ~path contents
;;

let finish ?publication upload ~expected_digest =
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
      let%bind document, publication_bytes =
        match publication with
        | None ->
          let%bind document =
            Blob_metadata_document.create metadata |> Document_fields.store
          in
          let%map bytes =
            Blob_metadata_document.to_bytes document |> Document_fields.store
          in
          document, bytes
        | Some stage ->
          let document = Blob_stage_documents.temporary stage in
          if Metadata.equal metadata (Blob_metadata_document.value document)
          then Ok (document, Blob_stage_documents.temporary_bytes stage)
          else
            Error (Store_error.Corrupt "upload differs from selected stage publication")
      in
      try
        Eio.File.sync upload.flow;
        Eio.Resource.close upload.flow;
        upload.closed <- true;
        Eio.Path.rename
          (eio_path upload.store upload.partial_path)
          (eio_path upload.store upload.final_path);
        let%map () =
          Durable_file.replace
            ~env:upload.store.env
            ~durability:Flush_file_and_directory
            ~path:upload.metadata_path
            publication_bytes
        in
        Handle.create
          document
          ~data_path:upload.final_path
          ~metadata_path:upload.metadata_path
      with
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        (try Eio.Path.unlink (eio_path upload.store upload.partial_path) with
         | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
         | _ -> ());
        (try Eio.Path.unlink (eio_path upload.store upload.final_path) with
         | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
         | _ -> ());
        Error
          (Store_error.of_exn ~operation:"finish blob upload" ~path:upload.final_path exn))
;;

let load_metadata store path =
  let open Result.Let_syntax in
  let%bind () =
    try
      match Eio.Path.kind ~follow:false (eio_path store path) with
      | `Regular_file -> Ok ()
      | `Not_found -> Error (Store_error.Missing path)
      | _ -> Error (Store_error.Corrupt "blob metadata is not a regular file")
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"inspect blob metadata" ~path exn)
  in
  let%bind contents =
    Durable_file.load_bounded
      ~env:store.env
      ~path
      ~max_bytes:(Document_schema.Limits.max_bytes Blob_metadata_document.limits)
  in
  let%bind original =
    Document_schema.Document.decode ~limits:Blob_metadata_document.limits contents
    |> Document_fields.store
  in
  let%bind original_id =
    Blob_metadata_document.stored_blob_id original |> Document_fields.store
  in
  if not (String.equal (Filename.basename path) (blob_name original_id ".sexp"))
  then Error (Store_error.Corrupt "original blob metadata identity differs from filename")
  else Blob_metadata_document.of_document original |> Document_fields.store
;;

let open_temporary store id =
  let metadata_path = Filename.concat store.temporary_directory (blob_name id ".sexp") in
  let data_path = Filename.concat store.temporary_directory (blob_name id ".blob") in
  let open Result.Let_syntax in
  let%bind document = load_metadata store metadata_path in
  let metadata = Blob_metadata_document.value document in
  if Agent_protocol.Id.Blob.compare metadata.blob.id id <> 0 || metadata.durable
  then Error (Store_error.Corrupt "temporary blob metadata identity is invalid")
  else if not (Eio.Path.is_file (eio_path store data_path))
  then Error (Store_error.Missing data_path)
  else Ok (Handle.create document ~data_path ~metadata_path)
;;

let open_session store session id =
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let metadata_path = Filename.concat directory (blob_name id ".sexp") in
  let data_path = Filename.concat directory (blob_name id ".blob") in
  let open Result.Let_syntax in
  let%bind document = load_metadata store metadata_path in
  let metadata = Blob_metadata_document.value document in
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
  else Ok (Handle.create document ~data_path ~metadata_path)
;;

let load store handle =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  try Ok (Eio.Path.load (eio_path store (Handle.data_path handle))) with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.of_exn ~operation:"load blob" ~path:(Handle.data_path handle) exn)
;;

let read_range store ~sw handle ~offset ~max_bytes =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  let length = (Handle.metadata handle).blob.byte_length in
  if Int64.(offset < zero || offset > length)
  then Error (Store_error.Corrupt "blob read offset is outside the blob")
  else if max_bytes <= 0
  then Error (Store_error.Corrupt "blob read size must be positive")
  else (
    try
      let source = Eio.Path.open_in ~sw (eio_path store (Handle.data_path handle)) in
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
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error
        (Store_error.of_exn
           ~operation:"read blob range"
           ~path:(Handle.data_path handle)
           exn))
;;

let iter_chunks store ~sw handle ~chunk_size ~f =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  if chunk_size <= 0
  then Error (Store_error.Corrupt "blob chunk size must be positive")
  else (
    try
      let source = Eio.Path.open_in ~sw (eio_path store (Handle.data_path handle)) in
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
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error
        (Store_error.of_exn ~operation:"stream blob" ~path:(Handle.data_path handle) exn))
;;

let adopt store session handle =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  let session_id = Session_store.Handle.session_id session in
  match (Handle.metadata handle).target_session with
  | Some id when not (Agent_protocol.Id.Session.equal id session_id) ->
    Error (Store_error.Corrupt "blob is bound to another target session")
  | target ->
    (match (Handle.metadata handle).durable, target with
     | true, Some _ -> Ok handle
     | true, None -> Error (Store_error.Corrupt "durable blob has no target session")
     | false, _ ->
       Eio.Cancel.protect (fun () ->
         let open Result.Let_syntax in
         let directory =
           Filename.concat (Session_store.Handle.directory session) "blobs"
         in
         let destination_data =
           Filename.concat directory (blob_name (Handle.metadata handle).blob.id ".blob")
         in
         let destination_metadata =
           Filename.concat directory (blob_name (Handle.metadata handle).blob.id ".sexp")
         in
         let metadata =
           { (Handle.metadata handle) with
             target_session = Some session_id
           ; durable = true
           }
         in
         let%bind document =
           Blob_metadata_document.with_value (Handle.document handle) metadata
           |> Document_fields.store
         in
         let old_data = Handle.data_path handle
         and old_metadata = Handle.metadata_path handle
         and old_document = Handle.document handle in
         let refresh () =
           Handle.unavailable
             handle
             (Store_error.Corrupt
                "blob adoption requires verified reopen after uncertain publication");
           let candidate data_path metadata_path expected =
             let%bind () =
               match Eio.Path.kind ~follow:false (eio_path store data_path) with
               | `Regular_file -> Ok ()
               | _ -> Error (Store_error.Missing data_path)
             in
             let%bind actual = load_metadata store metadata_path in
             let%bind actual_document =
               Blob_metadata_document.to_document actual |> Document_fields.store
             in
             let%bind expected_document =
               Blob_metadata_document.to_document expected |> Document_fields.store
             in
             if
               Jsonaf.exactly_equal
                 (Document_schema.Document.json actual_document)
                 (Document_schema.Document.json expected_document)
             then (
               let metadata = Blob_metadata_document.value actual in
               let candidate = Handle.create actual ~data_path ~metadata_path in
               let length = ref 0L in
               let digest = ref Digestif.SHA256.empty in
               let%bind () =
                 Eio.Switch.run (fun sw ->
                   iter_chunks store ~sw candidate ~chunk_size:8192 ~f:(fun bytes ->
                     (length := Int64.(!length + of_int (String.length bytes)));
                     digest := Digestif.SHA256.feed_string !digest bytes))
               in
               if
                 Int64.equal !length metadata.blob.byte_length
                 && String.equal
                      Digestif.SHA256.(get !digest |> to_hex)
                      metadata.blob.digest
               then Ok (data_path, metadata_path, actual)
               else Error (Store_error.Corrupt "adoption blob integrity changed"))
             else Error (Store_error.Corrupt "adoption metadata changed")
           in
           let old = candidate old_data old_metadata old_document in
           let final = candidate destination_data destination_metadata document in
           match old, final with
           | Ok (data_path, metadata_path, document), Error _
           | Error _, Ok (data_path, metadata_path, document) ->
             Handle.publish handle document ~data_path ~metadata_path
           | _ -> ()
         in
         let perform () =
           let%bind () =
             try
               Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path store directory);
               let%bind () =
                 Durable_file.sync_directory
                   ~env:store.env
                   ~path:(Session_store.Handle.directory session)
               in
               match
                 ( Eio.Path.kind ~follow:false (eio_path store destination_data)
                 , Eio.Path.kind ~follow:false (eio_path store destination_metadata) )
               with
               | `Not_found, `Not_found ->
                 Eio.Path.rename
                   (eio_path store (Handle.data_path handle))
                   (eio_path store destination_data);
                 Ok ()
               | _ ->
                 Error (Store_error.Corrupt "blob adoption would overwrite existing data")
             with
             | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
             | exn ->
               Error
                 (Store_error.of_exn
                    ~operation:"adopt blob data"
                    ~path:destination_data
                    exn)
           in
           match save_metadata store destination_metadata document with
           | Error failure ->
             let restore () =
               Eio.Path.rename (eio_path store destination_data) (eio_path store old_data);
               (match
                  Eio.Path.kind ~follow:false (eio_path store destination_metadata)
                with
                | `Not_found -> ()
                | _ -> Eio.Path.unlink (eio_path store destination_metadata));
               let%bind () = Durable_file.sync_directory ~env:store.env ~path:directory in
               Durable_file.sync_directory ~env:store.env ~path:store.temporary_directory
             in
             Adoption.restore handle ~f:restore;
             Error failure
           | Ok () ->
             let%bind () =
               try
                 Eio.Path.unlink (eio_path store old_metadata);
                 Ok ()
               with
               | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
               | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
                 Error
                   (Store_error.of_exn
                      ~operation:"remove adopted temporary metadata"
                      ~path:old_metadata
                      exn)
             in
             let%map () =
               Durable_file.sync_directory ~env:store.env ~path:store.temporary_directory
             in
             Handle.publish
               handle
               document
               ~data_path:destination_data
               ~metadata_path:destination_metadata;
             handle
         in
         match perform () with
         | Ok _ as success -> success
         | Error _ as error ->
           Adoption.recover handle ~f:refresh;
           error
         | exception exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           Adoption.recover handle ~f:refresh;
           Exn.raise_with_original_backtrace exn backtrace))
;;

let load_verified store ~sw handle ~max_bytes =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  let metadata = (Handle.metadata handle).blob in
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
  let%bind document = Blob_metadata_document.create metadata |> Document_fields.store in
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
            let handle = Handle.create document ~data_path:path ~metadata_path:path in
            let%map content = load_verified store ~sw handle ~max_bytes in
            Some content)
       | _ -> invalid "staged result data is not a regular file")
  in
  try read locations with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"load staged result" ~path:directory exn)
;;

let ensure_staged_content store ~sw session ~stage content =
  let document = Blob_stage_documents.temporary stage in
  let metadata = Blob_metadata_document.value document in
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
  let verify path expected ~partial =
    let handle = Handle.create document ~data_path:path ~metadata_path:path in
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
          (Blob_stage_documents.temporary_bytes stage)
          ~partial:false
      in
      let%bind _ =
        inspect final_metadata (Blob_stage_documents.durable_bytes stage) ~partial:false
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
              let%map _ =
                finish
                  ~publication:stage
                  upload
                  ~expected_digest:(Some metadata.blob.digest)
              in
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
      let%bind () =
        Durable_file.replace
          ~env:store.env
          ~durability:Flush_file_and_directory
          ~path:final_metadata
          (Blob_stage_documents.durable_bytes stage)
      in
      List.iter [ temporary_data; temporary_metadata; partial ] ~f:(fun path ->
        match Eio.Path.kind ~follow:false (eio_path store path) with
        | `Not_found -> ()
        | `Regular_file -> Eio.Path.unlink (eio_path store path)
        | _ -> failwith "staged blob cleanup encountered a non-file");
      let%map () =
        Durable_file.sync_directory ~env:store.env ~path:store.temporary_directory
      in
      Handle.create
        (Blob_stage_documents.durable stage)
        ~data_path:final_data
        ~metadata_path:final_metadata
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error (Store_error.of_exn ~operation:"resume staged blob" ~path:final_data exn))
;;

let discard_unreferenced store session handle =
  let open Result.Let_syntax in
  let%bind _ = Handle.metadata_checked handle in
  let id = (Handle.metadata handle).blob.id in
  let directory = Filename.concat (Session_store.Handle.directory session) "blobs" in
  let data_path = Filename.concat directory (blob_name id ".blob") in
  let metadata_path = Filename.concat directory (blob_name id ".sexp") in
  let open Result.Let_syntax in
  let%bind () =
    match (Handle.metadata handle).target_session with
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
      match
        ( Blob_metadata_document.to_document current
        , Blob_metadata_document.to_document (Handle.document handle) )
      with
      | Ok current, Ok expected ->
        Jsonaf.exactly_equal
          (Document_schema.Document.json current)
          (Document_schema.Document.json expected)
      | _ -> false)
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
      | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
      | exn ->
        Error
          (Store_error.of_exn ~operation:"discard unreferenced blob" ~path:data_path exn))
;;

let expired ~now metadata =
  Option.value_map metadata.Metadata.expires_at ~default:false ~f:(fun expires_at ->
    Agent_protocol.Timestamp.compare expires_at now <= 0)
;;

let prepare_cleanup store ~protect ~now filename =
  let open Result.Let_syntax in
  let metadata_path = Filename.concat store.temporary_directory filename in
  let%bind document = load_metadata store metadata_path in
  let metadata = Blob_metadata_document.value document in
  let%bind () =
    if
      (not metadata.durable) && String.equal filename (blob_name metadata.blob.id ".sexp")
    then Ok ()
    else Error (Store_error.Corrupt "temporary expiry metadata identity mismatch")
  in
  let%map protected = if expired ~now metadata then protect metadata else Ok true in
  metadata, protected
;;

let unlink_expired_file store path =
  try
    Eio.Path.unlink (eio_path store path);
    Ok ()
  with
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok ()
  | Core_unix.Unix_error (ENOENT, _, _) -> Ok ()
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"unlink expired temporary blob" ~path exn)
;;

let cleanup_one store (metadata, protected) =
  if protected
  then Ok 0
  else
    let open Result.Let_syntax in
    let data_path =
      Filename.concat
        store.temporary_directory
        (blob_name metadata.Metadata.blob.id ".blob")
    in
    let metadata_path =
      Filename.concat store.temporary_directory (blob_name metadata.blob.id ".sexp")
    in
    let%bind () = unlink_expired_file store data_path in
    let%map () = unlink_expired_file store metadata_path in
    1
;;

let cleanup_expired ?(protect = fun _ -> Ok false) store ~now =
  try
    Eio.Path.read_dir (eio_path store store.temporary_directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".sexp")
    |> List.map ~f:(prepare_cleanup store ~protect ~now)
    |> Result.all
    |> Result.bind ~f:(fun prepared ->
      List.map prepared ~f:(cleanup_one store)
      |> Result.all
      |> Result.map ~f:(List.fold ~init:0 ~f:( + )))
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
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

let retention_directories retention session =
  match retention.active with
  | false -> Error (Store_error.Corrupt "blob retention scope has ended")
  | true ->
    Ok (Session_store.Handle.directory session, retention.store.temporary_directory)
;;

let retention_reserved_directory retention =
  match retention.active with
  | false -> Error (Store_error.Corrupt "blob retention scope has ended")
  | true -> Ok retention.store.durable_directory
;;

let discard_staged_unreferenced retention ~reader session ~stage =
  let metadata = Blob_metadata_document.value (Blob_stage_documents.temporary stage) in
  let open Result.Let_syntax in
  let invalid message = Error (Store_error.Corrupt message) in
  let%bind session_root, temporary_root = retention_directories retention session in
  let store = retention.store in
  let%bind _ =
    Agent_protocol.Blob.Metadata.of_json
      (Agent_protocol.Blob.Metadata.to_json metadata.blob)
    |> Result.map_error ~f:(fun error ->
      Store_error.Corrupt error.Agent_protocol.Error.message)
  in
  let%bind () =
    match
      String.equal (Retention_reader.root reader) session_root
      && (not metadata.durable)
      && Option.exists
           metadata.target_session
           ~f:(Agent_protocol.Id.Session.equal (Session_store.Handle.session_id session))
      && Int64.(metadata.blob.byte_length <= of_int Int.max_value)
    with
    | true -> Ok ()
    | false -> invalid "staged discard ownership or reader mismatch"
  in
  let%bind temporary_reader = Retention_reader.at_root reader ~root:temporary_root in
  let final_directory = Filename.concat session_root "blobs" in
  let data_limit = Int64.to_int_exn metadata.blob.byte_length in
  let inspect_directory reader relative native ~durable =
    let%bind names = Retention_reader.list reader ~directory:relative in
    let expected =
      if durable
      then Blob_stage_documents.durable_bytes stage
      else Blob_stage_documents.temporary_bytes stage
    in
    let metadata_name = blob_name metadata.blob.id ".sexp" in
    let temporaries =
      List.filter names ~f:(fun name ->
        Option.exists (Durable_file.temporary_target name) ~f:(String.equal metadata_name))
    in
    let files =
      [ blob_name metadata.blob.id ".blob", `Data; metadata_name, `Metadata ]
      @ (if durable then [] else [ blob_name metadata.blob.id ".part", `Partial ])
      @ List.map temporaries ~f:(fun name -> name, `Temporary_metadata)
    in
    List.fold_result files ~init:[] ~f:(fun paths (name, kind) ->
      let path = Filename.concat native name in
      match Eio.Path.kind ~follow:false (eio_path store path) with
      | `Not_found -> Ok paths
      | `Regular_file ->
        let relative =
          if String.equal relative "." then name else Filename.concat relative name
        in
        let limit =
          match kind with
          | `Data | `Partial -> data_limit
          | `Metadata | `Temporary_metadata -> String.length expected
        in
        let%bind contents =
          Retention_reader.read reader ~path:relative ~max_bytes:limit
        in
        let valid =
          match kind with
          | `Metadata -> String.equal contents expected
          | `Temporary_metadata -> String.is_prefix expected ~prefix:contents
          | `Partial when String.length contents < data_limit -> true
          | `Data | `Partial ->
            String.length contents = data_limit
            && String.equal
                 metadata.blob.digest
                 Digestif.SHA256.(digest_string contents |> to_hex)
        in
        (match valid with
         | true -> Ok ((path, kind) :: paths)
         | false -> invalid "staged discard file differs from its private preparation")
      | _ -> invalid "staged discard encountered a non-regular file")
  in
  try
    let%bind final = inspect_directory reader "blobs" final_directory ~durable:true in
    let%bind temporary =
      inspect_directory temporary_reader "." temporary_root ~durable:false
    in
    let files = final @ temporary in
    let data, metadata_files =
      List.partition_tf files ~f:(fun (_, kind) ->
        match kind with
        | `Data | `Partial -> true
        | `Metadata | `Temporary_metadata -> false)
    in
    let%bind () =
      List.fold_result (data @ metadata_files) ~init:() ~f:(fun () (path, _) ->
        match Eio.Path.kind ~follow:false (eio_path store path) with
        | `Regular_file ->
          Eio.Path.unlink (eio_path store path);
          Ok ()
        | `Not_found -> Ok ()
        | _ -> invalid "staged discard path changed before removal")
    in
    let%bind () = Durable_file.sync_directory ~env:store.env ~path:final_directory in
    Durable_file.sync_directory ~env:store.env ~path:temporary_root
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error
      (Store_error.of_exn ~operation:"discard staged result files" ~path:session_root exn)
;;

let coordinated store f =
  (* Individual operations preserve recoverable files and upload accounting on
     failure. An IO error or cancelled reader must not poison the shared mutex. *)
  let outcome =
    Eio.Mutex.use_rw ~protect:false store.coordination.mutex (fun () ->
      try Ok (f ()) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
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
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
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

let ensure_staged_content store ~sw session ~stage content =
  coordinated store (fun () -> ensure_staged_content store ~sw session ~stage content)
;;

let discard_unreferenced store session handle =
  coordinated store (fun () -> discard_unreferenced store session handle)
;;

let cleanup_expired ?protect store ~now =
  coordinated store (fun () -> cleanup_expired ?protect store ~now)
;;
