open Core
module D = Document_schema
module F = Document_fields
module Metadata = Session_metadata
module Lifecycle_documents = Session_lifecycle_documents

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
    ; mutable lifecycle_unavailable : Store_error.t option
    ; mutable lifecycle_epoch : unit ref
    ; mutable closed : bool
    ; owner_available : unit -> bool
    ; store_owner : unit ref
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
    if t.closed || not (t.owner_available ())
    then Error (Store_error.Corrupt "session handle is closed")
    else (
      match t.metadata_unavailable, t.lifecycle_unavailable with
      | None, None -> Ok t.metadata
      | Some error, _ | _, Some error -> Error error)
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

module Lifecycle = struct
  module R = Session_archive_record

  module Observation = struct
    type t =
      { owner : Handle.t
      ; epoch : unit ref
      ; documents : Lifecycle_documents.t
      }

    let value t = Lifecycle_documents.value t.documents
  end

  module Prepared = struct
    type t =
      { observation : Observation.t
      ; previous_entry : Session_index.Entry.t option
      ; documents : Lifecycle_documents.Prepared.t
      }

    let outcome t = Lifecycle_documents.Prepared.outcome t.documents
  end

  module Current = struct
    type t =
      { owner : Handle.t
      ; epoch : unit ref
      ; entry : Session_index.Entry.t
      }

    let entry t = t.entry

    (* Physical identity intentionally compares live ownership capabilities. *)
    let is_current t handle =
      phys_equal t.owner handle
      && phys_equal t.epoch handle.Handle.lifecycle_epoch
      && (not handle.closed)
      && handle.owner_available ()
      && Option.is_none handle.lifecycle_unavailable
      && Option.is_none handle.metadata_unavailable
    ;;
  end

  module Installed = struct
    type t =
      { current : Current.t
      ; outcome : R.Outcome.t
      }

    let current t = t.current
    let entry t = Current.entry t.current
    let outcome t = t.outcome
    let is_current t handle = Current.is_current t.current handle
  end

  module Removal = struct
    module Ownership = struct
      type t =
        | Source of Handle.t
        | Retired of Session_removal_directory.t
        | Finished
    end

    type t =
      { store_owner : unit ref
      ; session_id : Agent_protocol.Id.Session.t
      ; outcome : R.Outcome.t option
      ; mutable ownership : Ownership.t
      }

    let outcome t = t.outcome

    let handle t =
      match t.ownership with
      | Source handle -> Some handle
      | Retired _ | Finished -> None
    ;;
  end
end

type t =
  { owner_token : unit ref
  ; env : Eio_unix.Stdenv.base
  ; root : Data_root.t
  ; server_id : Agent_protocol.Id.Server.t
  ; daemon_lock : Lock.t
  ; index : Session_index.t
  ; delegations : Delegation_store.t
  ; organizations : Organization_store.t
  ; projection_updates : Session_projection_update.t
  ; mutable index_was_rebuilt : bool
  ; mutable closed : bool
  }

let owns_handle t handle = phys_equal t.owner_token handle.Handle.store_owner

let check_handle t handle =
  if (not (owns_handle t handle)) || t.closed
  then Error (Store_error.Corrupt "session handle belongs to a different or closed store")
  else Handle.metadata_checked handle |> Result.map ~f:(fun _ -> ())
;;

let current_schema_version = 2
let current_metadata_schema_version = 1
let data_root t = t.root
let server_id t = t.server_id
let session_index t = t.index
let delegations t = t.delegations
let organizations t = t.organizations
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
    ; lifecycle_revision = Session_archive_record.Revision.zero
    ; admission = Automatic
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
    if version = current_metadata_schema_version
    then Ok ()
    else if version > current_metadata_schema_version
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
  archive_carrier ~env directory session_id
  |> Result.map ~f:(fun document ->
    Option.exists document ~f:(fun document ->
      Session_archive_record.Status.equal
        (Session_archive_record.status (Session_archive_document.value document))
        Archived))
;;

(* Existing archive callers remain an explicit bootstrap-only adapter until
   lifecycle publication owns atomic outcome proof. Do not report an archive
   after preserving an Active/Removed document unchanged. *)
let archive_document existing session_id =
  match existing with
  | Some document ->
    if
      Session_archive_record.Status.equal
        (Session_archive_record.status (Session_archive_document.value document))
        Archived
    then Ok document
    else
      Error
        (Store_error.Corrupt "archive requires lifecycle publication of current authority")
  | None ->
    Session_archive_document.authored
      (Session_archive_record.of_original_archive ~session_id)
    |> F.store
;;

let project_lifecycle entry document =
  let module R = Session_archive_record in
  let lifecycle = Session_archive_document.value document in
  match R.status lifecycle with
  | Removed -> Ok None
  | Active | Archived ->
    Session_index.Entry.with_lifecycle entry lifecycle |> Result.map ~f:Option.some
;;

let recover_index_entry ~env sessions_directory name =
  let open Result.Let_syntax in
  let directory = Filename.concat sessions_directory name in
  let%bind () = validate_layout ~env directory in
  let%bind metadata = load_metadata_at ~env (metadata_path directory) in
  let%bind () = validate_metadata ~require_durable:true name metadata in
  let%bind document = archive_carrier ~env directory metadata.session.id in
  match document with
  | None -> Ok (Some (index_entry metadata))
  | Some document -> project_lifecycle (index_entry metadata) document
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
  |> Result.map ~f:List.filter_opt
;;

let reconcile_archive ~env root entry =
  let open Result.Let_syntax in
  let session_id = entry.Session_index.Entry.session.id in
  let directory = Data_root.session_path root session_id in
  let%bind document = archive_carrier ~env directory session_id in
  match document with
  | Some document -> project_lifecycle entry document
  | None ->
    if
      (not entry.archived)
      && Session_archive_record.Revision.equal
           entry.lifecycle_revision
           Session_archive_record.Revision.zero
      && Session_archive_record.Admission.equal entry.admission Automatic
    then Ok (Some entry)
    else Error (Store_error.Corrupt "current index lifecycle authority is missing")
;;

let reconcile_archives ~env root index =
  let open Result.Let_syntax in
  let%bind previous = Session_index.list_checked index in
  let%bind entries = Result.all (List.map previous ~f:(reconcile_archive ~env root)) in
  let entries = List.filter_opt entries in
  if List.equal Session_index.Entry.equal previous entries
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
          | Some old ->
            { entry with
              archived = old.archived
            ; lifecycle_revision = old.lifecycle_revision
            ; admission = old.admission
            }
          | None -> entry)
      in
      let%bind entries =
        List.map entries ~f:(reconcile_archive ~env root)
        |> Result.all
        |> Result.map ~f:List.filter_opt
      in
      Session_index.replace_all index entries)
    else Ok ()
  in
  let%map () = reconcile_archives ~env root index in
  index, pending
;;

let make ~env ~root ~server_id ~daemon_lock ~organizations (index, index_was_rebuilt) =
  { owner_token = ref ()
  ; env
  ; root
  ; server_id
  ; daemon_lock
  ; index
  ; projection_updates =
      Session_projection_update.create ~env ~marker_path:(recovery_marker_path root) ()
  ; index_was_rebuilt
  ; closed = false
  ; delegations = Delegation_store.create ~env ~data_root:root
  ; organizations
  }
;;

let release_on_error ~env lock ~cleanup ~f =
  let best_effort action =
    (* Only secondary failed-attempt cleanup is suppressed. The initialization
       result or original exception remains the caller's primary outcome. *)
    try Eio.Cancel.protect action with
    | _ -> ()
  in
  let release () =
    best_effort cleanup;
    best_effort (fun () -> ignore (Lock.release ~env lock : (unit, Store_error.t) result))
  in
  match f () with
  | Ok _ as result -> result
  | Error _ as result ->
    release ();
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    release ();
    Exn.raise_with_original_backtrace exn backtrace
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
    release_on_error
      ~env
      daemon_lock
      ~cleanup:(fun () -> ())
      ~f:(fun () ->
        let%bind () = save_server_id ~env root server_id in
        let%bind index = open_index ~env root in
        let%map organizations =
          Organization_root.create_owned
            ~env
            ~sw
            ~root
            ~server_id
            ~created_at:(timestamp env)
        in
        make ~env ~root ~server_id ~daemon_lock ~organizations index))
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
  let acquired_organizations = ref None in
  release_on_error
    ~env
    daemon_lock
    ~cleanup:(fun () -> Option.iter !acquired_organizations ~f:Organization_store.close)
    ~f:(fun () ->
      let%bind organizations = Organization_root.open_owned ~env ~sw ~root ~server_id in
      acquired_organizations := Some organizations;
      let%map index = open_index ~env root in
      make ~env ~root ~server_id ~daemon_lock ~organizations index)
;;

let close t =
  if t.closed
  then Ok ()
  else (
    Organization_store.close t.organizations;
    Result.map (Lock.release ~env:t.env t.daemon_lock) ~f:(fun () -> t.closed <- true))
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

let make_handle
      ~directory:session_directory
      ~metadata:session_metadata
      ~actor_lock
      ~owner_available
      ~store_owner
  =
  let child name = Filename.concat session_directory name in
  { Handle.metadata = session_metadata
  ; metadata_carrier = D.Extension_carrier.of_authored_value session_metadata
  ; metadata_unavailable = None
  ; canonical_projection = None
  ; lifecycle_unavailable = None
  ; lifecycle_epoch = ref ()
  ; closed = false
  ; owner_available
  ; store_owner
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
  let handle =
    make_handle
      ~directory
      ~metadata
      ~actor_lock
      ~owner_available:(fun () -> not t.closed)
      ~store_owner:t.owner_token
  in
  Eio.Switch.on_release sw (fun () ->
    handle.Handle.closed <- true;
    handle.lifecycle_epoch <- ref ());
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
  else (
    let%bind handle = acquire_handle t ~sw ~directory ~actor_lock_nonce metadata in
    match archive_carrier ~env:t.env directory session_id with
    | Ok (Some document)
      when Session_archive_record.Status.equal
             (Session_archive_record.status (Session_archive_document.value document))
             Removed ->
      handle.Handle.closed <- true;
      handle.lifecycle_epoch <- ref ();
      let%bind () = Lock.release ~env:t.env handle.actor_lock in
      Error (Store_error.Missing directory)
    | Ok None | Ok (Some _) -> Ok handle
    | Error error ->
      handle.Handle.closed <- true;
      handle.lifecycle_epoch <- ref ();
      Eio.Cancel.protect (fun () ->
        try
          ignore
            (Lock.release ~env:t.env handle.actor_lock : (unit, Store_error.t) result)
        with
        | _ -> ());
      Error error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      handle.Handle.closed <- true;
      handle.lifecycle_epoch <- ref ();
      Eio.Cancel.protect (fun () ->
        try
          ignore
            (Lock.release ~env:t.env handle.actor_lock : (unit, Store_error.t) result)
        with
        | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace)
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
  let%bind () = check_handle t handle in
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
  let%bind entry =
    match requested_entry with
    | Some entry ->
      if
        not
          (Jsonaf.exactly_equal
             (Agent_protocol.Session.to_json entry.Session_index.Entry.session)
             (Agent_protocol.Session.to_json metadata.session))
      then Error (Store_error.Corrupt "supplied index summary differs from metadata")
      else Ok entry
    | None ->
      Ok
        (match previous with
         | None -> index_entry metadata
         | Some entry -> { entry with session = metadata.session })
  in
  let%bind document =
    archive_carrier ~env:t.env (Handle.directory handle) (Handle.session_id handle)
  in
  let%map entry =
    match document with
    | Some document ->
      Session_index.Entry.with_lifecycle entry (Session_archive_document.value document)
    | None ->
      (match previous with
       | Some previous ->
         if
           previous.archived
           || not
                (Session_archive_record.Revision.equal
                   previous.lifecycle_revision
                   Session_archive_record.Revision.zero)
         then Error (Store_error.Corrupt "current lifecycle authority is missing")
         else
           Ok
             { entry with
               archived = false
             ; lifecycle_revision = Session_archive_record.Revision.zero
             ; admission = Automatic
             }
       | None -> Session_index.Entry.validate entry |> Result.map ~f:(fun () -> entry))
  in
  bytes, carrier, entry
;;

let prepare_canonical_projection t handle ~metadata ~entry =
  let open Result.Let_syntax in
  let%bind () = check_handle t handle in
  handle.Handle.lifecycle_epoch <- ref ();
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
  let%bind () = check_handle t handle in
  handle.Handle.lifecycle_epoch <- ref ();
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

let close_session t handle =
  if not (phys_equal t.owner_token handle.Handle.store_owner)
  then Error (Store_error.Corrupt "session handle belongs to a different store")
  else (
    handle.closed <- true;
    handle.lifecycle_epoch <- ref ();
    Lock.release ~env:t.env handle.actor_lock)
;;

let is_archived t handle =
  let open Result.Let_syntax in
  let%bind () = check_handle t handle in
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
    let%bind carrier = archive_document previous session_id in
    let%bind document = Session_archive_document.to_document carrier |> F.store in
    let%bind entry =
      Session_index.Entry.with_lifecycle entry (Session_archive_document.value carrier)
    in
    Session_index.with_prepared_upsert t.index entry ~publish_authority:(fun () ->
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

let read_lifecycle t handle =
  let open Result.Let_syntax in
  let%bind () = check_handle t handle in
  let%bind document =
    archive_carrier ~env:t.env (Handle.directory handle) (Handle.session_id handle)
  in
  let%map documents =
    Lifecycle_documents.create ~session_id:(Handle.session_id handle) document
  in
  { Lifecycle.Observation.owner = handle; epoch = handle.lifecycle_epoch; documents }
;;

let check_lifecycle_observation t handle observation =
  let open Result.Let_syntax in
  let%bind () = check_handle t handle in
  if
    (not (phys_equal observation.Lifecycle.Observation.owner handle))
    || not (phys_equal observation.epoch handle.Handle.lifecycle_epoch)
  then Error (Store_error.Corrupt "lifecycle observation ownership changed")
  else (
    let%bind current = read_lifecycle t handle in
    if Lifecycle_documents.equal observation.documents current.documents
    then Ok ()
    else Error (Store_error.Corrupt "lifecycle authority observation changed"))
;;

let current_lifecycle t handle ~current_entry =
  let open Result.Let_syntax in
  let%bind observation = read_lifecycle t handle in
  let%bind metadata = Handle.metadata_checked handle in
  let%bind () =
    if
      Jsonaf.exactly_equal
        (Agent_protocol.Session.to_json current_entry.Session_index.Entry.session)
        (Agent_protocol.Session.to_json metadata.session)
      && Option.is_none handle.Handle.canonical_projection
    then Ok ()
    else Error (Store_error.Corrupt "selection canonical projection is not current")
  in
  let%bind entry =
    Session_index.Entry.with_lifecycle
      current_entry
      (Lifecycle.Observation.value observation)
  in
  let%bind indexed = Session_index.find_checked t.index (Handle.session_id handle) in
  let%bind () = check_handle t handle in
  let%bind () =
    if
      phys_equal observation.Lifecycle.Observation.epoch handle.Handle.lifecycle_epoch
      && Option.equal Session_index.Entry.equal indexed (Some entry)
    then Ok ()
    else Error (Store_error.Corrupt "selection lifecycle projection is not current")
  in
  Ok ({ owner = handle; epoch = handle.lifecycle_epoch; entry } : Lifecycle.Current.t)
;;

let prepare_lifecycle t handle observation ~current_entry ~transition ~now =
  let open Result.Let_syntax in
  let%bind () = check_lifecycle_observation t handle observation in
  let%bind () =
    if
      Jsonaf.exactly_equal
        (Agent_protocol.Session.to_json current_entry.Session_index.Entry.session)
        (Agent_protocol.Session.to_json (Handle.metadata handle).session)
      && Option.is_none handle.Handle.canonical_projection
    then Ok ()
    else Error (Store_error.Corrupt "lifecycle canonical projection is not current")
  in
  let%bind previous_entry =
    Session_index.find_checked t.index (Handle.session_id handle)
  in
  let%bind documents =
    Lifecycle_documents.prepare
      observation.Lifecycle.Observation.documents
      ~current_entry
      ~transition
      ~now
  in
  let%map () =
    match Lifecycle_documents.Prepared.target documents with
    | Upsert entry -> Session_index.validate_upsert t.index entry
    | Remove session_id -> Session_index.validate_remove t.index session_id
  in
  { Lifecycle.Prepared.observation; previous_entry; documents }
;;

let lifecycle_publication_uncertain handle =
  handle.Handle.lifecycle_unavailable
  <- Some
       (Store_error.Corrupt
          "lifecycle publication durability requires restart reconciliation")
;;

let publish_prepared_lifecycle t handle prepared =
  let open Result.Let_syntax in
  let%bind () =
    check_lifecycle_observation t handle prepared.Lifecycle.Prepared.observation
  in
  let%bind () =
    if Lifecycle_documents.Prepared.has_current_outcome prepared.documents
    then Ok ()
    else
      Error
        (Store_error.Corrupt "older lifecycle replay cannot install a current witness")
  in
  let prior_epoch = handle.Handle.lifecycle_epoch in
  let publication () =
    Session_projection_update.publish t.projection_updates ~f:(fun ~require_intent ->
      let%bind () = check_lifecycle_observation t handle prepared.observation in
      let publish_projection =
        match Lifecycle_documents.Prepared.target prepared.documents with
        | Lifecycle_documents.Target.Upsert entry ->
          Session_index.with_prepared_upsert
            ~expected_entry:prepared.previous_entry
            t.index
            entry
        | Remove session_id ->
          Session_index.with_prepared_remove
            ~expected_entry:prepared.previous_entry
            t.index
            session_id
      in
      publish_projection ~publish_authority:(fun () ->
        let%bind () = require_intent () in
        handle.lifecycle_epoch <- ref ();
        handle.lifecycle_unavailable
        <- Some (Store_error.Corrupt "lifecycle publication is in progress");
        match
          Durable_file.replace
            ~env:t.env
            ~durability:Flush_file_and_directory
            ~path:(archive_marker_path (Handle.directory handle))
            (Lifecycle_documents.Prepared.bytes prepared.documents)
        with
        | Ok () ->
          handle.lifecycle_unavailable <- None;
          Ok ()
        | Error _ as error ->
          lifecycle_publication_uncertain handle;
          error
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          lifecycle_publication_uncertain handle;
          Exn.raise_with_original_backtrace exn backtrace))
  in
  let reconcile_failed_projection () =
    if not (phys_equal prior_epoch handle.Handle.lifecycle_epoch)
    then lifecycle_publication_uncertain handle
  in
  let%bind () =
    match publication () with
    | Ok () -> Ok ()
    | Error _ as error ->
      reconcile_failed_projection ();
      error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      reconcile_failed_projection ();
      Exn.raise_with_original_backtrace exn backtrace
  in
  Session_index.availability t.index
;;

let publish_lifecycle t handle prepared =
  let open Result.Let_syntax in
  match Lifecycle_documents.Prepared.target prepared.Lifecycle.Prepared.documents with
  | Remove _ -> Error (Store_error.Corrupt "removed authority needs a removal capability")
  | Upsert entry ->
    let%map () = publish_prepared_lifecycle t handle prepared in
    { Lifecycle.Installed.current =
        { Lifecycle.Current.owner = handle; epoch = handle.lifecycle_epoch; entry }
    ; outcome = Lifecycle_documents.Prepared.outcome prepared.documents
    }
;;

let begin_removal t handle prepared =
  let open Result.Let_syntax in
  match Lifecycle_documents.Prepared.target prepared.Lifecycle.Prepared.documents with
  | Upsert _ ->
    Error (Store_error.Corrupt "removal requires terminal lifecycle authority")
  | Remove _ ->
    let%map () = publish_prepared_lifecycle t handle prepared in
    { Lifecycle.Removal.store_owner = t.owner_token
    ; session_id = Handle.session_id handle
    ; outcome = Some (Lifecycle_documents.Prepared.outcome prepared.documents)
    ; ownership = Source handle
    }
;;

let complete_lifecycle_outcome t handle ~key ~request_digest ~complete =
  let open Result.Let_syntax in
  let module R = Session_archive_record in
  let%bind observation = read_lifecycle t handle in
  let%bind receipt =
    List.find
      (R.receipts (Lifecycle_documents.value observation.documents))
      ~f:(fun receipt ->
        Int.equal (Idempotency_store.Key.compare receipt.R.Receipt.key key) 0)
    |> Result.of_option
         ~error:(Store_error.Corrupt "lifecycle completion proof is absent")
  in
  let%bind () =
    if String.equal receipt.request_digest request_digest
    then Ok ()
    else Error (Store_error.Corrupt "lifecycle completion request digest conflicts")
  in
  (* Generic result completion may reenter independent store owners. The caller
     still owns the target fence, but neither projection nor index mutex is held. *)
  let%bind () = complete receipt.outcome in
  let%bind () = check_lifecycle_observation t handle observation in
  if receipt.completion_acknowledged
  then Ok ()
  else (
    let%bind documents =
      Lifecycle_documents.acknowledge observation.documents ~key ~request_digest
    in
    let%bind document =
      Lifecycle_documents.document documents
      |> Result.of_option
           ~error:(Store_error.Corrupt "acknowledged lifecycle carrier is absent")
    in
    let%bind bytes = Lifecycle_documents.encoded_document document in
    let prior_epoch = handle.Handle.lifecycle_epoch in
    let publication () =
      Session_projection_update.publish t.projection_updates ~f:(fun ~require_intent ->
        let%bind () = check_lifecycle_observation t handle observation in
        let%bind () = require_intent () in
        handle.Handle.lifecycle_epoch <- ref ();
        handle.lifecycle_unavailable
        <- Some (Store_error.Corrupt "lifecycle acknowledgement is in progress");
        match
          Durable_file.replace
            ~env:t.env
            ~durability:Flush_file_and_directory
            ~path:(archive_marker_path (Handle.directory handle))
            bytes
        with
        | Ok () ->
          handle.lifecycle_unavailable <- None;
          Ok ()
        | Error _ as error ->
          lifecycle_publication_uncertain handle;
          error
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          lifecycle_publication_uncertain handle;
          Exn.raise_with_original_backtrace exn backtrace)
    in
    match publication () with
    | Ok () -> Ok ()
    | Error _ as error ->
      if not (phys_equal prior_epoch handle.lifecycle_epoch)
      then lifecycle_publication_uncertain handle;
      error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      if not (phys_equal prior_epoch handle.lifecycle_epoch)
      then lifecycle_publication_uncertain handle;
      Exn.raise_with_original_backtrace exn backtrace)
;;

let finish_removal t removal ~complete ~retire =
  let open Result.Let_syntax in
  if (not (phys_equal t.owner_token removal.Lifecycle.Removal.store_owner)) || t.closed
  then
    Error
      (Store_error.Corrupt "removal capability belongs to a different or closed store")
  else (
    let check_callback_owner expected =
      let same_ownership =
        match expected, removal.ownership with
        | Lifecycle.Removal.Ownership.Source left, Source right -> phys_equal left right
        | Retired left, Retired right -> phys_equal left right
        | Finished, Finished -> true
        | Source _, (Retired _ | Finished)
        | Retired _, (Source _ | Finished)
        | Finished, (Source _ | Retired _) -> false
      in
      if t.closed || not same_ownership
      then Error (Store_error.Corrupt "removal owner changed during receipt completion")
      else Ok ()
    in
    let complete_owned receipt =
      let expected = removal.ownership in
      let%bind () = check_callback_owner expected in
      let%bind () = complete receipt in
      check_callback_owner expected
    in
    let finish directory =
      let%bind () =
        Session_removal_directory.complete_receipts directory ~complete:complete_owned
      in
      let%map () = Session_removal_directory.cleanup directory in
      removal.ownership <- Finished
    in
    match removal.ownership with
    | Finished -> Ok ()
    | Retired directory -> finish directory
    | Source handle ->
      let%bind () =
        if phys_equal t.owner_token handle.Handle.store_owner
        then Ok ()
        else
          Error (Store_error.Corrupt "removed source Handle has a different store owner")
      in
      let%bind directory =
        Session_removal_directory.create ~env:t.env ~data_root:t.root removal.session_id
      in
      handle.lifecycle_epoch <- ref ();
      let%bind () =
        match
          Session_removal_directory.complete_receipts directory ~complete:complete_owned
        with
        | Ok () -> Ok ()
        | Error _ as error ->
          lifecycle_publication_uncertain handle;
          error
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          lifecycle_publication_uncertain handle;
          Exn.raise_with_original_backtrace exn backtrace
      in
      let expected = removal.ownership in
      let%bind () = if handle.closed then Ok () else retire handle in
      let%bind () = check_callback_owner expected in
      let%bind () =
        if handle.closed
        then Ok ()
        else Error (Store_error.Corrupt "runtime owner did not close the removed Handle")
      in
      removal.ownership <- Retired directory;
      let%map () = Session_removal_directory.cleanup directory in
      removal.ownership <- Finished)
;;

let pending_removal_ids t =
  let open Result.Let_syntax in
  if t.closed
  then Error (Store_error.Corrupt "session store is closed")
  else (
    let%bind retired = Session_removal_directory.discover ~env:t.env ~data_root:t.root in
    let directory = Data_root.sessions_path t.root in
    let%bind names =
      try Ok (Eio.Path.read_dir (eio_path t directory)) with
      | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
        Error
          (Store_error.of_exn
             ~operation:"discover terminal session sources"
             ~path:directory
             exn)
    in
    let%bind source_ids =
      List.filter names ~f:(String.is_prefix ~prefix:"ses_")
      |> List.map ~f:(fun name ->
        let%bind session_id =
          Agent_protocol.Id.Session.of_string name
          |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
        in
        let%bind () =
          require_kind ~env:t.env (Data_root.session_path t.root session_id) `Directory
        in
        let%map document =
          archive_carrier ~env:t.env (Data_root.session_path t.root session_id) session_id
        in
        match document with
        | Some document
          when Session_archive_record.Status.equal
                 (Session_archive_record.status (Session_archive_document.value document))
                 Removed -> Some session_id
        | Some _ | None -> None)
      |> Result.all
      |> Result.map ~f:List.filter_opt
    in
    Ok
      (List.dedup_and_sort
         (source_ids @ List.map retired ~f:Session_removal_directory.session_id)
         ~compare:Agent_protocol.Id.Session.compare))
;;

let open_removal t ~sw ~actor_lock_nonce session_id =
  let open Result.Let_syntax in
  if t.closed
  then Error (Store_error.Corrupt "session store is closed")
  else (
    let%bind retired = Session_removal_directory.discover ~env:t.env ~data_root:t.root in
    match
      List.find retired ~f:(fun directory ->
        Agent_protocol.Id.Session.equal
          (Session_removal_directory.session_id directory)
          session_id)
    with
    | Some directory ->
      let outcome = Session_removal_directory.terminal_outcome directory in
      let%bind () =
        match outcome with
        | None -> Ok ()
        | Some _ ->
          let source = Data_root.session_path t.root session_id in
          let%bind source_kind =
            try Ok (Eio.Path.kind ~follow:false (eio_path t source)) with
            | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
              Error
                (Store_error.of_exn ~operation:"inspect retired source" ~path:source exn)
          in
          (match source_kind with
           | `Not_found -> Ok ()
           | _ -> Error (Store_error.Corrupt "removed payload and source both remain"))
      in
      Ok
        { Lifecycle.Removal.store_owner = t.owner_token
        ; session_id
        ; outcome
        ; ownership = Retired directory
        }
    | None ->
      let directory = Data_root.session_path t.root session_id in
      let%bind () = validate_layout ~env:t.env directory in
      let%bind metadata = load_metadata_at ~env:t.env (metadata_path directory) in
      let%bind () =
        validate_metadata (Agent_protocol.Id.Session.to_string session_id) metadata
      in
      let%bind handle = acquire_handle t ~sw ~directory ~actor_lock_nonce metadata in
      let admitted () =
        let%bind _ = Handle.metadata_checked handle in
        let%bind directory =
          Session_removal_directory.create ~env:t.env ~data_root:t.root session_id
        in
        let%bind entry = Session_index.find_checked t.index session_id in
        if Option.is_some entry
        then Error (Store_error.Corrupt "terminal source is still in the active catalog")
        else
          Ok
            { Lifecycle.Removal.store_owner = t.owner_token
            ; session_id
            ; outcome = Session_removal_directory.terminal_outcome directory
            ; ownership = Source handle
            }
      in
      (match admitted () with
       | Ok removal -> Ok removal
       | Error _ as error ->
         Eio.Cancel.protect (fun () ->
           try ignore (close_session t handle : (unit, Store_error.t) result) with
           | _ -> ());
         error
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         Eio.Cancel.protect (fun () ->
           try ignore (close_session t handle : (unit, Store_error.t) result) with
           | _ -> ());
         Exn.raise_with_original_backtrace exn backtrace))
;;
