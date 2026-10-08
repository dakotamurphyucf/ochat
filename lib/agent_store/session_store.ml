open Core
module D = Document_schema
module F = Document_fields
module Metadata = Session_metadata

module Initial_projection = struct
  type t =
    { metadata : Metadata.t
    ; entry : Session_index.Entry.t
    }

  let metadata t = t.metadata
  let entry t = t.entry

  let create ~metadata ~entry =
    let open Result.Let_syntax in
    let%bind () =
      if
        Jsonaf.exactly_equal
          (Agent_protocol.Session.to_json metadata.Metadata.session)
          (Agent_protocol.Session.to_json entry.Session_index.Entry.session)
      then Ok ()
      else Error (Store_error.Corrupt "initial projection summary differs from metadata")
    in
    let%bind document =
      Session_metadata_document.to_document
        (D.Extension_carrier.of_authored_value metadata)
      |> F.store
    in
    let%bind _ = Session_metadata_document.of_document document |> F.store in
    let%bind document =
      Session_index_document.to_document (D.Extension_carrier.of_authored_value [ entry ])
      |> F.store
    in
    let%map _ = Session_index_document.of_document document |> F.store in
    { metadata; entry }
  ;;
end

module Handle = struct
  type t =
    { mutable metadata : Metadata.t
    ; mutable metadata_carrier : Metadata.t D.Extension_carrier.t
    ; mutable metadata_unavailable : Store_error.t option
    ; mutable canonical_projection : Session_projection_update.Pending.t option
    ; directory : string
    ; metadata_path : string
    ; actor_lock : Lock.t
    ; snapshot_directory : string
    ; journal_directory : string
    ; cache_directory : string
    ; workspace_directory : string
    ; responses_directory : string
    ; audit_directory : string
    ; exports_directory : string
    ; archive_directory : string
    ; idempotency_directory : string
    }

  let metadata t = t.metadata

  let metadata_checked t =
    match t.metadata_unavailable with
    | None -> Ok t.metadata
    | Some error -> Error error
  ;;

  let session_id t = t.metadata.session.id
  let directory t = t.directory
  let snapshot_directory t = t.snapshot_directory
  let journal_directory t = t.journal_directory
  let cache_directory t = t.cache_directory
  let workspace_directory t = t.workspace_directory
  let responses_directory t = t.responses_directory
  let audit_directory t = t.audit_directory
  let exports_directory t = t.exports_directory
  let archive_directory t = t.archive_directory
  let idempotency_directory t = t.idempotency_directory
end

module Schema = struct
  type t =
    { version : int
    ; created_at : Agent_protocol.Timestamp.t
    }
  [@@deriving sexp]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; root : Data_root.t
  ; server_id : Agent_protocol.Id.Server.t
  ; daemon_lock : Lock.t
  ; index : Session_index.t
  ; delegations : Delegation_store.t
  ; projection_updates : Session_projection_update.t
  ; mutable index_was_rebuilt : bool
  ; mutable closed : bool
  }

let current_schema_version = 1
let data_root t = t.root
let server_id t = t.server_id
let session_index t = t.index
let delegations t = t.delegations
let index_was_rebuilt t = t.index_was_rebuilt
let is_closed t = t.closed
let eio_path t path = Eio.Path.(Eio.Stdenv.fs t.env / path)

let check_writable t =
  if t.closed
  then Error (Store_error.Corrupt "session store is closed")
  else (
    let nonce =
      Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
    in
    let path = Filename.concat (Data_root.path t.root) (".health-" ^ nonce) in
    let file = eio_path t path in
    try
      Eio.Switch.run (fun sw ->
        let flow = Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) file in
        Eio.Flow.copy_string "" flow);
      Eio.Path.unlink file;
      Ok ()
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      (try Eio.Path.unlink file with
       | _ -> ());
      Error (Store_error.of_exn ~operation:"probe session store" ~path exn))
;;

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let save_schema ~env root =
  let open Result.Let_syntax in
  let value = Store_schema_document.{ created_at = timestamp env } in
  let%bind document =
    Store_schema_document.to_document (D.Extension_carrier.of_authored_value value)
    |> F.store
  in
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(Data_root.schema_path root)
    (D.Document.to_string document)
;;

let load_schema ~env root =
  let open Result.Let_syntax in
  let%bind contents =
    Durable_file.load_bounded
      ~env
      ~path:(Data_root.schema_path root)
      ~max_bytes:(D.Limits.max_bytes Store_schema_document.limits)
  in
  let%bind document =
    D.Document.decode ~limits:Store_schema_document.limits contents |> F.store
  in
  let%map carrier = Store_schema_document.of_document document |> F.store in
  Schema.{ version = 1; created_at = (D.Extension_carrier.value carrier).created_at }
;;

let save_server_id ~env root server_id =
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(Data_root.server_id_path root)
    (Agent_protocol.Id.Server.to_string server_id ^ "\n")
;;

let load_server_id ~env root =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path:(Data_root.server_id_path root) in
  Agent_protocol.Id.Server.of_string (String.strip contents)
  |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
;;

let metadata_path directory = Filename.concat directory "metadata.sexp"
let archive_marker_path directory = Filename.concat directory "ARCHIVED"

let recovery_marker_path root =
  Filename.concat (Data_root.indexes_path root) "sessions.recovery-required"
;;

let read_recovery_marker ~env root =
  Session_projection_update.pending ~env ~marker_path:(recovery_marker_path root)
;;

let complete_index_recovery t =
  Result.map
    (Session_projection_update.complete_recovery t.projection_updates)
    ~f:(fun () -> t.index_was_rebuilt <- false)
;;

let session_directories directory =
  List.map
    [ "snapshot"
    ; "journal"
    ; "cache"
    ; "workspace"
    ; "responses"
    ; "audit"
    ; "exports"
    ; "archive"
    ; "idempotency"
    ]
    ~f:(Filename.concat directory)
;;

let load_metadata_carrier_at ~env path =
  let open Result.Let_syntax in
  let%bind () =
    try
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Regular_file -> Ok ()
      | _ -> Error (Store_error.Corrupt "session metadata is not a regular file")
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"inspect session metadata" ~path exn)
  in
  let%bind contents =
    Durable_file.load_bounded
      ~env
      ~path
      ~max_bytes:(D.Limits.max_bytes Session_metadata_document.limits)
  in
  let%bind document =
    D.Document.decode ~limits:Session_metadata_document.limits contents |> F.store
  in
  let%bind stored_id = Session_metadata_document.stored_session_id document |> F.store in
  if
    not
      (String.equal
         (Filename.basename (Filename.dirname path))
         (Agent_protocol.Id.Session.to_string stored_id))
  then Error (Store_error.Corrupt "session directory and stored metadata identity differ")
  else Session_metadata_document.of_document document |> F.store
;;

let load_metadata_at ~env path =
  load_metadata_carrier_at ~env path |> Result.map ~f:D.Extension_carrier.value
;;

let index_entry metadata =
  Session_index.Entry.
    { session = metadata.Metadata.session
    ; runnable_job_count = 0
    ; deliverable_job_count = 0
    ; earliest_schedule_due = None
    ; owner_grace_deadline = None
    ; pending_initial_start = false
    ; archived = false
    }
;;

let require_kind ~env path expected =
  let actual = Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) in
  match actual, expected with
  | `Directory, `Directory | `Regular_file, `Regular_file -> Ok ()
  | _ -> Error (Store_error.Corrupt ("invalid session layout path: " ^ path))
;;

let validate_layout ~env directory =
  let open Result.Let_syntax in
  let%bind () =
    Result.all_unit
      (List.map (directory :: session_directories directory) ~f:(fun path ->
         require_kind ~env path `Directory))
  in
  require_kind ~env (metadata_path directory) `Regular_file
;;

let validate_metadata ?(require_durable = false) name metadata =
  let open Result.Let_syntax in
  let validate_version version =
    if version = current_schema_version
    then Ok ()
    else if version > current_schema_version
    then Error (Store_error.Schema_too_new version)
    else Error (Store_error.Migration_required version)
  in
  let%bind () = validate_version metadata.Metadata.schema_version in
  let%bind () =
    if metadata.data_schema_version > 0
    then Ok ()
    else Error (Store_error.Corrupt "session data schema version must be positive")
  in
  let%bind (_ : Agent_protocol.Session.t) =
    Agent_protocol.Session.of_json (Agent_protocol.Session.to_json metadata.session)
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
  in
  if not (String.equal name (Agent_protocol.Id.Session.to_string metadata.session.id))
  then Error (Store_error.Corrupt "session directory and metadata identity differ")
  else if
    require_durable
    && not
         (Agent_protocol.Session.equal_persistence
            metadata.session.spec.persistence
            Durable)
  then Error (Store_error.Corrupt "durable layout contains a transient session")
  else if
    String.is_empty metadata.prompt_artifact
    || String.is_empty metadata.workspace_identity
  then
    Error
      (Store_error.Corrupt "session metadata is missing artifact or workspace identity")
  else Ok ()
;;

let archive_carrier ~env directory session_id =
  let path = archive_marker_path directory in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Not_found -> Ok None
    | `Regular_file ->
      let open Result.Let_syntax in
      let%bind bytes =
        Durable_file.load_bounded
          ~env
          ~path
          ~max_bytes:(D.Limits.max_bytes Session_archive_document.limits)
      in
      let%bind document =
        D.Document.decode ~limits:Session_archive_document.limits bytes |> F.store
      in
      let%bind stored_id =
        Session_archive_document.stored_session_id document |> F.store
      in
      if not (Agent_protocol.Id.Session.equal stored_id session_id)
      then Error (Store_error.Corrupt "session archive marker identity differs")
      else
        Session_archive_document.of_document document
        |> F.store
        |> Result.map ~f:Option.some
    | _ -> Error (Store_error.Corrupt "session archive marker is not a regular file")
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"read session archive marker" ~path exn)
;;

let read_archive_marker ~env directory session_id =
  archive_carrier ~env directory session_id |> Result.map ~f:Option.is_some
;;

let write_archive_marker ~env directory session_id =
  let open Result.Let_syntax in
  let%bind carrier = archive_carrier ~env directory session_id in
  let carrier =
    Option.value carrier ~default:(D.Extension_carrier.of_authored_value session_id)
  in
  let%bind document = Session_archive_document.to_document carrier |> F.store in
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(archive_marker_path directory)
    (D.Document.to_string document)
;;

let recover_index_entry ~env sessions_directory name =
  let open Result.Let_syntax in
  let directory = Filename.concat sessions_directory name in
  let%bind () = validate_layout ~env directory in
  let%bind metadata = load_metadata_at ~env (metadata_path directory) in
  let%bind () = validate_metadata ~require_durable:true name metadata in
  let%map archived = read_archive_marker ~env directory metadata.session.id in
  { (index_entry metadata) with archived }
;;

let rebuild_index ~env root =
  let open Result.Let_syntax in
  let directory = Data_root.sessions_path root in
  let%bind () = require_kind ~env directory `Directory in
  Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
  |> List.filter ~f:(String.is_prefix ~prefix:"ses_")
  |> List.sort ~compare:String.compare
  |> List.map ~f:(recover_index_entry ~env directory)
  |> Result.all
;;

let reconcile_archive ~env root entry =
  let open Result.Let_syntax in
  let%bind session_id =
    Agent_protocol.Id.Session.of_string
      (Agent_protocol.Id.Session.to_string entry.Session_index.Entry.session.id)
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
  in
  let directory = Data_root.session_path root session_id in
  let%bind archived = read_archive_marker ~env directory session_id in
  let%map () =
    if entry.archived && not archived
    then write_archive_marker ~env directory session_id
    else Ok ()
  in
  { entry with archived = entry.archived || archived }
;;

let reconcile_archives ~env root index =
  let open Result.Let_syntax in
  let%bind previous = Session_index.list_checked index in
  let%bind entries = Result.all (List.map previous ~f:(reconcile_archive ~env root)) in
  if
    List.equal
      (fun left right ->
         Bool.equal left.Session_index.Entry.archived right.Session_index.Entry.archived)
      previous
      entries
  then Ok ()
  else Session_index.replace_all index entries
;;

let open_index ~env root =
  let open Result.Let_syntax in
  let path = Filename.concat (Data_root.indexes_path root) "sessions.snapshot" in
  let%bind index =
    Session_index.open_or_rebuild ~env ~path ~rebuild:(fun () ->
      let%bind entries = rebuild_index ~env root in
      let%map () =
        if List.is_empty entries
        then Ok ()
        else
          Session_projection_update.require ~env ~marker_path:(recovery_marker_path root)
      in
      entries)
  in
  let%bind pending = read_recovery_marker ~env root in
  let%bind () =
    if pending
    then (
      let%bind entries = rebuild_index ~env root in
      let entries =
        List.map entries ~f:(fun entry ->
          match Session_index.find index entry.Session_index.Entry.session.id with
          | Some old when old.archived -> { entry with archived = true }
          | None | Some _ -> entry)
      in
      let%bind entries =
        List.map entries ~f:(reconcile_archive ~env root) |> Result.all
      in
      Session_index.replace_all index entries)
    else Ok ()
  in
  let%map () = reconcile_archives ~env root index in
  index, pending
;;

let make ~env ~root ~server_id ~daemon_lock (index, index_was_rebuilt) =
  { env
  ; root
  ; server_id
  ; daemon_lock
  ; index
  ; projection_updates =
      Session_projection_update.create ~env ~marker_path:(recovery_marker_path root) ()
  ; index_was_rebuilt
  ; closed = false
  ; delegations = Delegation_store.create ~env ~data_root:root
  }
;;

let release_on_error ~env lock result =
  match result with
  | Ok _ -> result
  | Error _ as error ->
    ignore (Lock.release ~env lock : (unit, Store_error.t) result);
    error
;;

let create ~env ~sw ~root ~server_id ~process_start_identity ~lock_nonce =
  let open Result.Let_syntax in
  let%bind root = Data_root.create ~env ~path:root in
  let schema_file = Eio.Path.(Eio.Stdenv.fs env / Data_root.schema_path root) in
  if Eio.Path.is_file schema_file
  then Error (Store_error.Corrupt "refusing to create an already initialized store")
  else (
    let%bind daemon_lock =
      Lock.acquire
        ~env
        ~sw
        ~path:(Data_root.daemon_lock_path root)
        ~server_id
        ~process_start_identity
        ~nonce:lock_nonce
    in
    release_on_error ~env daemon_lock
    @@
    let%bind () = save_schema ~env root in
    let%bind () = save_server_id ~env root server_id in
    let%map index = open_index ~env root in
    make ~env ~root ~server_id ~daemon_lock index)
;;

let open_existing ~env ~sw ~root ~process_start_identity ~lock_nonce =
  let open Result.Let_syntax in
  let%bind root = Data_root.open_existing ~env ~path:root in
  let%bind server_id = load_server_id ~env root in
  let%bind daemon_lock =
    Lock.acquire
      ~env
      ~sw
      ~path:(Data_root.daemon_lock_path root)
      ~server_id
      ~process_start_identity
      ~nonce:lock_nonce
  in
  release_on_error ~env daemon_lock
  @@
  let%bind (_ : Schema.t) = load_schema ~env root in
  let%map index = open_index ~env root in
  make ~env ~root ~server_id ~daemon_lock index
;;

let close t =
  if t.closed
  then Ok ()
  else Result.map (Lock.release ~env:t.env t.daemon_lock) ~f:(fun () -> t.closed <- true)
;;

let metadata_document carrier =
  let open Result.Let_syntax in
  let%bind document = Session_metadata_document.to_document carrier |> F.store in
  let%map carrier = Session_metadata_document.of_document document |> F.store in
  D.Document.to_string document, carrier
;;

let save_metadata ~env path metadata =
  let open Result.Let_syntax in
  let%bind bytes, _ =
    metadata_document (D.Extension_carrier.of_authored_value metadata)
  in
  Durable_file.replace ~env ~durability:Flush_file_and_directory ~path bytes
;;

let load_metadata t path = load_metadata_at ~env:t.env path

let make_handle ~directory:session_directory ~metadata:session_metadata ~actor_lock =
  let child name = Filename.concat session_directory name in
  { Handle.metadata = session_metadata
  ; metadata_carrier = D.Extension_carrier.of_authored_value session_metadata
  ; metadata_unavailable = None
  ; canonical_projection = None
  ; directory = session_directory
  ; metadata_path = metadata_path session_directory
  ; actor_lock
  ; snapshot_directory = child "snapshot"
  ; journal_directory = child "journal"
  ; cache_directory = child "cache"
  ; workspace_directory = child "workspace"
  ; responses_directory = child "responses"
  ; audit_directory = child "audit"
  ; exports_directory = child "exports"
  ; archive_directory = child "archive"
  ; idempotency_directory = child "idempotency"
  }
;;

let acquire_handle t ~sw ~directory ~actor_lock_nonce _metadata =
  let open Result.Let_syntax in
  let%bind metadata_carrier =
    load_metadata_carrier_at ~env:t.env (metadata_path directory)
  in
  let%map actor_lock =
    Lock.acquire
      ~env:t.env
      ~sw
      ~path:(Filename.concat directory "actor.lock")
      ~server_id:t.server_id
      ~process_start_identity:None
      ~nonce:actor_lock_nonce
  in
  let metadata = D.Extension_carrier.value metadata_carrier in
  let handle = make_handle ~directory ~metadata ~actor_lock in
  handle.Handle.metadata_carrier <- metadata_carrier;
  handle
;;

let create_session_initialized t ~sw ~transaction_id ~actor_lock_nonce ~initialize =
  let open Result.Let_syntax in
  let temporary_directory =
    Filename.concat
      (Data_root.sessions_path t.root)
      (".creating-" ^ Agent_protocol.Id.Transaction.to_string transaction_id)
  in
  let temporary = eio_path t temporary_directory in
  let layout_result =
    try
      Eio.Path.mkdir ~perm:0o700 temporary;
      List.iter (session_directories temporary_directory) ~f:(fun path ->
        Eio.Path.mkdir ~perm:0o700 (eio_path t path));
      Ok ()
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error
        (Store_error.of_exn
           ~operation:"create session layout"
           ~path:temporary_directory
           exn)
  in
  let%bind () =
    match layout_result with
    | Ok () -> Ok ()
    | Error _ as error ->
      (try Eio.Path.rmtree ~missing_ok:true temporary with
       | _ -> ());
      error
  in
  let%bind initial =
    match initialize ~staging_directory:temporary_directory with
    | Ok metadata -> Ok metadata
    | Error _ as error ->
      (try Eio.Path.rmtree ~missing_ok:true temporary with
       | _ -> ());
      error
  in
  let metadata = Initial_projection.metadata initial in
  let entry = Initial_projection.entry initial in
  let session_id = metadata.Metadata.session.id in
  let final_directory = Data_root.session_path t.root session_id in
  let destination = eio_path t final_directory in
  let%bind () =
    match save_metadata ~env:t.env (metadata_path temporary_directory) metadata with
    | Ok () -> Ok ()
    | Error _ as error ->
      (try Eio.Path.rmtree ~missing_ok:true temporary with
       | _ -> ());
      error
  in
  Session_projection_update.publish t.projection_updates ~f:(fun ~require_intent ->
    Session_index.with_prepared_upsert t.index entry ~publish_authority:(fun () ->
      let%bind () = require_intent () in
      let%bind () =
        try
          Eio.Path.rename temporary destination;
          Durable_file.sync_directory ~env:t.env ~path:(Data_root.sessions_path t.root)
        with
        | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
        | exn ->
          Error
            (Store_error.of_exn
               ~operation:"install session layout"
               ~path:final_directory
               exn)
      in
      acquire_handle t ~sw ~directory:final_directory ~actor_lock_nonce metadata))
;;

let create_session t ~sw ~transaction_id ~actor_lock_nonce metadata =
  create_session_initialized
    t
    ~sw
    ~transaction_id
    ~actor_lock_nonce
    ~initialize:(fun ~staging_directory:_ ->
      Initial_projection.create ~metadata ~entry:(index_entry metadata))
;;

let open_session t ~sw ~actor_lock_nonce session_id =
  let open Result.Let_syntax in
  let directory = Data_root.session_path t.root session_id in
  let%bind () =
    try
      match Eio.Path.kind ~follow:false (eio_path t directory) with
      | `Not_found -> Error (Store_error.Missing directory)
      | _ -> Ok ()
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"inspect session root" ~path:directory exn)
  in
  let%bind () = validate_layout ~env:t.env directory in
  let%bind metadata = load_metadata t (metadata_path directory) in
  if Agent_protocol.Id.Session.compare metadata.session.id session_id <> 0
  then Error (Store_error.Corrupt "session directory and metadata identity differ")
  else acquire_handle t ~sw ~directory ~actor_lock_nonce metadata
;;

let refresh_metadata t handle =
  match load_metadata_carrier_at ~env:t.env handle.Handle.metadata_path with
  | Ok current ->
    handle.metadata <- D.Extension_carrier.value current;
    handle.metadata_carrier <- current;
    handle.metadata_unavailable <- None
  | Error error -> handle.metadata_unavailable <- Some error
  | exception exn ->
    handle.metadata_unavailable
    <- Some (Store_error.Corrupt "metadata refresh failed during uncertain publication");
    raise exn
;;

let prepare_metadata_projection t handle metadata requested_entry =
  let open Result.Let_syntax in
  let%bind () =
    match handle.Handle.metadata_unavailable with
    | None -> Ok ()
    | Some error -> Error error
  in
  let%bind () =
    if
      Agent_protocol.Id.Session.equal
        metadata.Metadata.session.id
        (Handle.session_id handle)
    then Ok ()
    else Error (Store_error.Corrupt "metadata update cannot change session identity")
  in
  let%bind () =
    validate_metadata
      (Agent_protocol.Id.Session.to_string (Handle.session_id handle))
      metadata
  in
  let%bind bytes, carrier =
    metadata_document
      (D.Extension_carrier.with_value handle.Handle.metadata_carrier metadata)
  in
  let%bind previous = Session_index.find_checked t.index (Handle.session_id handle) in
  let%map entry =
    match requested_entry with
    | Some entry ->
      if
        not
          (Jsonaf.exactly_equal
             (Agent_protocol.Session.to_json entry.Session_index.Entry.session)
             (Agent_protocol.Session.to_json metadata.session))
      then Error (Store_error.Corrupt "supplied index summary differs from metadata")
      else
        Ok
          { entry with
            archived =
              entry.archived
              || Option.exists previous ~f:(fun old -> old.Session_index.Entry.archived)
          }
    | None ->
      Ok
        (match previous with
         | None -> index_entry metadata
         | Some entry -> { entry with session = metadata.session })
  in
  bytes, carrier, entry
;;

let prepare_canonical_projection t handle ~metadata ~entry =
  let open Result.Let_syntax in
  let%map pending =
    Session_projection_update.prepare_canonical
      t.projection_updates
      ~previous:handle.Handle.canonical_projection
      ~prepare:(fun () ->
        let%bind _, _, entry =
          prepare_metadata_projection t handle metadata (Some entry)
        in
        let%map () = Session_index.validate_upsert t.index entry in
        entry)
  in
  handle.canonical_projection <- Some pending
;;

let write_metadata ?entry:requested_entry t handle metadata =
  let open Result.Let_syntax in
  let%bind pending, entry =
    Session_projection_update.publish t.projection_updates ~f:(fun ~require_intent ->
      let%bind bytes, carrier, entry =
        prepare_metadata_projection t handle metadata requested_entry
      in
      let pending = handle.Handle.canonical_projection in
      let%bind () =
        match pending with
        | None -> Ok ()
        | Some pending ->
          if Session_projection_update.Pending.matches pending entry
          then Ok ()
          else
            Error
              (Store_error.Corrupt
                 "metadata/full hints differ from latest canonical projection target")
      in
      Session_index.with_prepared_upsert t.index entry ~publish_authority:(fun () ->
        let%bind () = require_intent () in
        match
          Durable_file.replace
            ~env:t.env
            ~durability:Flush_file_and_directory
            ~path:handle.metadata_path
            bytes
        with
        | Ok () ->
          handle.metadata <- metadata;
          handle.metadata_carrier <- carrier;
          handle.metadata_unavailable <- None;
          Ok (pending, entry)
        | Error _ as error ->
          Eio.Cancel.protect (fun () ->
            try refresh_metadata t handle with
            | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
            | _ -> ());
          error
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          Eio.Cancel.protect (fun () ->
            try refresh_metadata t handle with
            | _ -> ());
          Exn.raise_with_original_backtrace exn backtrace))
  in
  match pending with
  | None -> Ok ()
  | Some pending ->
    let%map () =
      Session_projection_update.finish_canonical t.projection_updates pending ~entry
    in
    handle.canonical_projection <- None
;;

let close_session t handle = Lock.release ~env:t.env handle.Handle.actor_lock

let is_archived t handle =
  read_archive_marker ~env:t.env (Handle.directory handle) (Handle.session_id handle)
;;

let archive_session t session_id =
  Session_projection_update.publish t.projection_updates ~f:(fun ~require_intent ->
    let open Result.Let_syntax in
    let%bind found = Session_index.find_checked t.index session_id in
    let%bind entry =
      found
      |> Result.of_option
           ~error:(Store_error.Missing (Data_root.session_path t.root session_id))
    in
    (* Marker decoding/encoding happens before acquiring the recovery intent,
       so unsupported semantics never get replaced by an archive retry. *)
    let directory = Data_root.session_path t.root session_id in
    let%bind previous = archive_carrier ~env:t.env directory session_id in
    let carrier =
      Option.value previous ~default:(D.Extension_carrier.of_authored_value session_id)
    in
    let%bind document = Session_archive_document.to_document carrier |> F.store in
    Session_index.with_prepared_upsert
      t.index
      { entry with archived = true }
      ~publish_authority:(fun () ->
        let%bind () = require_intent () in
        Durable_file.replace
          ~env:t.env
          ~durability:Flush_file_and_directory
          ~path:(archive_marker_path directory)
          (D.Document.to_string document)))
;;

let restore_removed_directory source tombstone error =
  try
    Eio.Path.rename tombstone source;
    error
  with
  | _ -> error
;;

let remove_session t session_id =
  let source_path = Data_root.session_path t.root session_id in
  let tombstone_path =
    Filename.concat
      (Data_root.lost_and_found_path t.root)
      ("deleted-"
       ^ Agent_protocol.Id.Session.to_string session_id
       ^ "-"
       ^ (Agent_protocol.Id.Transaction.create ()
          |> Agent_protocol.Id.Transaction.to_string))
  in
  let source = eio_path t source_path in
  let tombstone = eio_path t tombstone_path in
  try
    Eio.Path.rename source tombstone;
    match Session_index.remove t.index session_id with
    | Error _ as failure -> restore_removed_directory source tombstone failure
    | Ok () ->
      (try
         Eio.Path.rmtree ~missing_ok:true tombstone;
         Ok ()
       with
       | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
       | exn ->
         Error
           (Store_error.of_exn
              ~operation:"remove session tombstone"
              ~path:tombstone_path
              exn))
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"remove session" ~path:source_path exn)
;;

let cutoff_seconds older_than =
  Agent_protocol.Timestamp.to_time_ns older_than
  |> Time_ns.to_span_since_epoch
  |> Time_ns.Span.to_sec
;;

let rec prune_response_path t ~cutoff path =
  let native = Eio.Path.native_exn path in
  try
    let stat = Eio.Path.stat ~follow:false path in
    match stat.kind with
    | `Regular_file when Float.(stat.mtime <= cutoff) ->
      Eio.Path.unlink path;
      Ok 1
    | `Directory ->
      Eio.Path.read_dir path
      |> List.fold_result ~init:0 ~f:(fun count name ->
        Result.map (prune_response_path t ~cutoff Eio.Path.(path / name)) ~f:(( + ) count))
    | `Regular_file
    | `Symbolic_link
    | `Socket
    | `Fifo
    | `Character_special
    | `Block_device
    | `Unknown -> Ok 0
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.of_exn ~operation:"prune response artifact" ~path:native exn)
;;

let prune_response_artifacts t ~protected ~older_than =
  let cutoff = cutoff_seconds older_than in
  Session_index.list_checked t.index
  |> Result.bind ~f:(fun entries ->
    List.fold_result entries ~init:0 ~f:(fun count entry ->
      let session_id = entry.Session_index.Entry.session.id in
      if
        List.mem protected session_id ~equal:(fun a b ->
          Agent_protocol.Id.Session.compare a b = 0)
      then Ok count
      else (
        let directory =
          Data_root.session_path t.root session_id
          |> Fn.flip Filename.concat "responses"
          |> eio_path t
        in
        if Eio.Path.is_directory directory
        then Result.map (prune_response_path t ~cutoff directory) ~f:(( + ) count)
        else Ok count)))
;;

let list_sessions t = Session_index.list t.index
let list_sessions_checked t = Session_index.list_checked t.index
