open Core

module Metadata = struct
  type t =
    { schema_version : int
    ; session : Agent_protocol.Session.t
    ; prompt_artifact : string
    ; workspace_identity : string
    ; data_schema_version : int
    }
  [@@deriving sexp]
end

module Handle = struct
  type t =
    { mutable metadata : Metadata.t
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
  let schema = Schema.{ version = current_schema_version; created_at = timestamp env } in
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(Data_root.schema_path root)
    (Sexp.to_string_mach ([%sexp_of: Schema.t] schema))
;;

let load_schema ~env root =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path:(Data_root.schema_path root) in
  try
    let schema = [%of_sexp: Schema.t] (Sexp.of_string contents) in
    if schema.version = current_schema_version
    then Ok schema
    else if schema.version > current_schema_version
    then Error (Store_error.Schema_too_new schema.version)
    else Error (Store_error.Migration_required schema.version)
  with
  | exn ->
    Error (Store_error.Corrupt ("store schema decode failed: " ^ Exn.to_string exn))
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
  let path = recovery_marker_path root in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Not_found -> Ok false
    | `Regular_file ->
      Result.bind (Durable_file.load ~env ~path) ~f:(fun contents ->
        if String.equal contents "1\n"
        then Ok true
        else Error (Store_error.Corrupt "session index recovery marker is invalid"))
    | _ ->
      Error (Store_error.Corrupt "session index recovery marker is not a regular file")
  with
  | exn -> Error (Store_error.of_exn ~operation:"read index recovery marker" ~path exn)
;;

let complete_index_recovery t =
  let open Result.Let_syntax in
  let%bind pending = read_recovery_marker ~env:t.env t.root in
  if not pending
  then Ok ()
  else (
    let path = recovery_marker_path t.root in
    try
      Eio.Path.unlink (eio_path t path);
      let%map () =
        Durable_file.sync_directory ~env:t.env ~path:(Data_root.indexes_path t.root)
      in
      t.index_was_rebuilt <- false
    with
    | exn -> Error (Store_error.of_exn ~operation:"complete index recovery" ~path exn))
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

let load_metadata_at ~env path =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path in
  try Ok ([%of_sexp: Metadata.t] (Sexp.of_string contents)) with
  | exn ->
    Error (Store_error.Corrupt ("session metadata decode failed: " ^ Exn.to_string exn))
;;

let index_entry metadata =
  Session_index.Entry.
    { session = metadata.Metadata.session
    ; runnable_job_count = 0
    ; deliverable_job_count = 0
    ; earliest_schedule_due = None
    ; owner_grace_deadline = None
    ; archived = false
    }
;;

let require_kind ~env path expected =
  let actual = Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) in
  if Poly.equal actual expected
  then Ok ()
  else Error (Store_error.Corrupt ("invalid session layout path: " ^ path))
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

let validate_metadata name metadata =
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
    not
      (Agent_protocol.Session.equal_persistence metadata.session.spec.persistence Durable)
  then Error (Store_error.Corrupt "durable layout contains a transient session")
  else if
    String.is_empty metadata.prompt_artifact
    || String.is_empty metadata.workspace_identity
  then
    Error
      (Store_error.Corrupt "session metadata is missing artifact or workspace identity")
  else Ok ()
;;

let read_archive_marker ~env directory session_id =
  let path = archive_marker_path directory in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Not_found -> Ok false
    | `Regular_file ->
      Result.bind (Durable_file.load ~env ~path) ~f:(fun contents ->
        if String.equal contents (Agent_protocol.Id.Session.to_string session_id ^ "\n")
        then Ok true
        else Error (Store_error.Corrupt "session archive marker identity differs"))
    | _ -> Error (Store_error.Corrupt "session archive marker is not a regular file")
  with
  | exn -> Error (Store_error.of_exn ~operation:"read session archive marker" ~path exn)
;;

let write_archive_marker ~env directory session_id =
  try
    Result.bind (require_kind ~env directory `Directory) ~f:(fun () ->
      Durable_file.replace
        ~env
        ~durability:Flush_file_and_directory
        ~path:(archive_marker_path directory)
        (Agent_protocol.Id.Session.to_string session_id ^ "\n"))
  with
  | exn ->
    Error
      (Store_error.of_exn ~operation:"write session archive marker" ~path:directory exn)
;;

let recover_index_entry ~env sessions_directory name =
  let open Result.Let_syntax in
  let directory = Filename.concat sessions_directory name in
  let%bind () = validate_layout ~env directory in
  let%bind metadata = load_metadata_at ~env (metadata_path directory) in
  let%bind () = validate_metadata name metadata in
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
  let previous = Session_index.list index in
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
          Durable_file.replace
            ~env
            ~durability:Flush_file_and_directory
            ~path:(recovery_marker_path root)
            "1\n"
      in
      entries)
  in
  let%bind () = reconcile_archives ~env root index in
  let%map pending = read_recovery_marker ~env root in
  index, pending
;;

let make ~env ~root ~server_id ~daemon_lock (index, index_was_rebuilt) =
  { env
  ; root
  ; server_id
  ; daemon_lock
  ; index
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

let save_metadata ~env path metadata =
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path
    (Sexp.to_string_mach ([%sexp_of: Metadata.t] metadata))
;;

let load_metadata t path = load_metadata_at ~env:t.env path

let make_handle ~directory:session_directory ~metadata:session_metadata ~actor_lock =
  let child name = Filename.concat session_directory name in
  { Handle.metadata = session_metadata
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

let acquire_handle t ~sw ~directory ~actor_lock_nonce metadata =
  let open Result.Let_syntax in
  let%map actor_lock =
    Lock.acquire
      ~env:t.env
      ~sw
      ~path:(Filename.concat directory "actor.lock")
      ~server_id:t.server_id
      ~process_start_identity:None
      ~nonce:actor_lock_nonce
  in
  make_handle ~directory ~metadata ~actor_lock
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
  let%bind metadata =
    match initialize ~staging_directory:temporary_directory with
    | Ok metadata -> Ok metadata
    | Error _ as error ->
      (try Eio.Path.rmtree ~missing_ok:true temporary with
       | _ -> ());
      error
  in
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
  let%bind () =
    match Eio.Path.rename temporary destination with
    | () -> Ok ()
    | exception exn ->
      (try Eio.Path.rmtree ~missing_ok:true temporary with
       | _ -> ());
      Error
        (Store_error.of_exn ~operation:"install session layout" ~path:final_directory exn)
  in
  let%bind handle =
    acquire_handle t ~sw ~directory:final_directory ~actor_lock_nonce metadata
  in
  match Session_index.upsert t.index (index_entry metadata) with
  | Ok () -> Ok handle
  | Error _ as error ->
    ignore
      (Lock.release ~env:t.env handle.Handle.actor_lock : (unit, Store_error.t) result);
    error
;;

let create_session t ~sw ~transaction_id ~actor_lock_nonce metadata =
  create_session_initialized
    t
    ~sw
    ~transaction_id
    ~actor_lock_nonce
    ~initialize:(fun ~staging_directory:_ -> Ok metadata)
;;

let open_session t ~sw ~actor_lock_nonce session_id =
  let open Result.Let_syntax in
  let directory = Data_root.session_path t.root session_id in
  let%bind metadata = load_metadata t (metadata_path directory) in
  if Agent_protocol.Id.Session.compare metadata.session.id session_id <> 0
  then Error (Store_error.Corrupt "session directory and metadata identity differ")
  else acquire_handle t ~sw ~directory ~actor_lock_nonce metadata
;;

let write_metadata t handle metadata =
  if
    Agent_protocol.Id.Session.compare
      metadata.Metadata.session.id
      (Handle.session_id handle)
    <> 0
  then Error (Store_error.Corrupt "metadata update cannot change session identity")
  else
    Result.bind (save_metadata ~env:t.env handle.metadata_path metadata) ~f:(fun () ->
      Result.map
        (Session_index.upsert t.index (index_entry metadata))
        ~f:(fun () -> handle.metadata <- metadata))
;;

let close_session t handle = Lock.release ~env:t.env handle.Handle.actor_lock

let archive_session t session_id =
  Session_index.find t.index session_id
  |> Result.of_option
       ~error:(Store_error.Missing (Data_root.session_path t.root session_id))
  |> Result.bind ~f:(fun entry ->
    Result.bind
      (write_archive_marker
         ~env:t.env
         (Data_root.session_path t.root session_id)
         session_id)
      ~f:(fun () -> Session_index.upsert t.index { entry with archived = true }))
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
       | exn ->
         Error
           (Store_error.of_exn
              ~operation:"remove session tombstone"
              ~path:tombstone_path
              exn))
  with
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
  | exn ->
    Error (Store_error.of_exn ~operation:"prune response artifact" ~path:native exn)
;;

let prune_response_artifacts t ~protected ~older_than =
  let cutoff = cutoff_seconds older_than in
  Session_index.list t.index
  |> List.fold_result ~init:0 ~f:(fun count entry ->
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
      else Ok count))
;;

let list_sessions t = Session_index.list t.index
