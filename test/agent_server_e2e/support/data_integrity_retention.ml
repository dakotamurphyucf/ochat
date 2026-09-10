open Core
module Store = Agent_store.Session_store
module Blobs = Agent_store.Blob_store
module Idempotency = Agent_store.Idempotency_store

let require condition message = if not condition then failwith message

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let timestamp seconds =
  Time_ns.Span.of_sec seconds
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let seconds timestamp =
  Agent_protocol.Timestamp.to_time_ns timestamp
  |> Time_ns.to_span_since_epoch
  |> Time_ns.Span.to_sec
;;

let shifted time delta = timestamp (seconds time +. delta)
let path environment native = Temporary_environment.path environment native

let save environment native body =
  Eio.Path.save ~create:(`Exclusive 0o600) (path environment native) body
;;

let exists environment native = Eio.Path.is_file (path environment native)

type t =
  { env : Eio_unix.Stdenv.base
  ; environment : Temporary_environment.t
  ; sessions : Store.t
  ; blobs : Blobs.t
  ; idempotency : Idempotency.t
  ; idempotency_path : string
  ; principal : Agent_protocol.Id.Principal.t
  }

let session_store ~sw env root =
  Store.create
    ~env
    ~sw
    ~root:(Filename.concat root "store")
    ~server_id:(Agent_protocol.Id.Server.create ())
    ~process_start_identity:None
    ~lock_nonce:"data-retention-store"
  |> store_ok
;;

let blob_store env root =
  Blobs.create
    ~env
    ~temporary_directory:(Filename.concat root "temporary-blobs")
    ~durable_directory:(Filename.concat root "durable-blobs")
    ~max_upload_bytes:1024L
  |> store_ok
;;

let create ~sw env environment =
  let root =
    Filename.concat (Temporary_environment.roots environment).temporary "data-maintenance"
  in
  Eio.Path.mkdir ~perm:0o700 (path environment root);
  let sessions = session_store ~sw env root in
  let blobs = blob_store env root in
  let idempotency_path = Filename.concat root "idempotency.sexp" in
  let idempotency = Idempotency.open_or_create ~env ~path:idempotency_path |> store_ok in
  { env
  ; environment
  ; sessions
  ; blobs
  ; idempotency
  ; idempotency_path
  ; principal = Agent_protocol.Id.Principal.create ()
  }
;;

let session_spec () =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Agent_protocol.Id.Prompt_definition.create ()))
    ~workspace:(Configured (Agent_protocol.Id.Workspace_definition.create ()))
    ~liveness:Detached
    ~persistence:Durable
    ~start_immediately:false
    ~labels:[]
    ()
  |> protocol_ok
;;

let session_summary t now =
  Agent_protocol.Session.
    { id = Agent_protocol.Id.Session.create ()
    ; creator = Some t.principal
    ; created_at = now
    ; updated_at = now
    ; generation = 0
    ; spec = session_spec ()
    ; desired_state = Stopped
    ; observed_state = Stopped
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 0L
    ; latest_event_sequence = 0L
    }
;;

let session t ~sw now name =
  let metadata =
    Store.Metadata.
      { schema_version = Store.current_schema_version
      ; session = session_summary t now
      ; prompt_artifact = "data-retention-fixture"
      ; workspace_identity = "fixture"
      ; data_schema_version = 1
      }
  in
  Store.create_session
    t.sessions
    ~sw
    ~transaction_id:(Agent_protocol.Id.Transaction.create ())
    ~actor_lock_nonce:name
    metadata
  |> store_ok
;;

let artifact t handle =
  let directory = Filename.concat (Store.Handle.responses_directory handle) "nested" in
  Eio.Path.mkdir ~perm:0o700 (path t.environment directory);
  let artifact = Filename.concat directory "response.json" in
  save t.environment artifact "retained-response";
  artifact
;;

let upload t ~sw now expires_at =
  let upload =
    Blobs.begin_upload
      t.blobs
      ~sw
      ~id:(Agent_protocol.Id.Blob.create ())
      ~creating_principal:t.principal
      ~target_session:None
      ~kind:File
      ~media_type:"text/plain"
      ~display_name:None
      ~allowed_use:"fixture"
      ~created_at:(shifted now (-100.))
      ~expires_at
    |> store_ok
  in
  Blobs.write_string upload "temporary-content" |> store_ok;
  Blobs.finish upload ~expected_digest:None |> store_ok
;;

let receipt t now name retention expires_at =
  let key =
    Idempotency.Key.
      { principal_id = t.principal
      ; session_id = None
      ; method_name = "session.start"
      ; idempotency_key = Agent_protocol.Idempotency_key.of_string name |> protocol_ok
      }
  in
  let record =
    Idempotency.
      { key
      ; request_digest = name
      ; accepted_transaction_sequence = None
      ; outcome = Success (`String name)
      ; created_at = shifted now (-100.)
      ; expires_at
      ; retention
      }
  in
  ignore (Idempotency.record t.idempotency record |> store_ok : Idempotency.record);
  record
;;

let maintenance t ~now ~protected =
  Agent_server.Maintenance.run_once
    ~registry:None
    ~env:t.env
    ~idempotency_store:t.idempotency
    ~blob_store:t.blobs
    ~session_store:t.sessions
    ~protected_response_sessions:protected
    ~response_retention:(Time_ns.Span.of_hr 1.)
    ~now
  |> store_ok
;;

let assert_stats (stats : Agent_server.Maintenance.stats) idempotency blobs responses =
  require
    (stats.expired_idempotency_records = idempotency)
    "maintenance idempotency count differs";
  require
    (stats.expired_temporary_blobs = blobs)
    "maintenance temporary blob count differs";
  require
    (stats.expired_response_artifacts = responses)
    "maintenance response artifact count differs"
;;

let assert_receipt store (record : Idempotency.record) retained =
  match
    Idempotency.lookup store ~key:record.key ~request_digest:record.request_digest
  with
  | Replay replay when retained ->
    require
      (String.equal replay.request_digest record.request_digest)
      "retained receipt digest changed";
    (match replay.outcome with
     | Success (`String value) ->
       require
         (String.equal value record.request_digest)
         "retained receipt result changed"
     | _ -> failwith "retained receipt lost its successful outcome")
  | Missing when not retained -> ()
  | _ -> failwith "maintenance receipt retention differs"
;;

let assert_blob t handle retained =
  let id = (Blobs.Handle.metadata handle).blob.id in
  match Blobs.open_temporary t.blobs id with
  | Ok handle when retained ->
    require
      (String.equal (Blobs.load t.blobs handle |> store_ok) "temporary-content")
      "retained blob content changed"
  | Error (Agent_store.Store_error.Missing _) when not retained -> ()
  | _ -> failwith "maintenance blob retention differs"
;;

let seed_receipts t now =
  [ receipt t now "expired" Standard (Some (shifted now (-1.))), false
  ; receipt t now "boundary" Standard (Some now), false
  ; receipt t now "protected" Protected (Some now), true
  ; receipt t now "future" Standard (Some (shifted now 1.)), true
  ; receipt t now "permanent" Standard None, true
  ]
;;

let seed_blobs t ~sw now =
  [ upload t ~sw now (Some (shifted now (-1.))), false
  ; upload t ~sw now (Some now), false
  ; upload t ~sw now (Some (shifted now 1.)), true
  ; upload t ~sw now None, true
  ]
;;

let assert_temporary_pairs t blobs =
  let expected =
    List.concat_map blobs ~f:(fun (blob, retained) ->
      if not retained
      then []
      else (
        let id = Agent_protocol.Id.Blob.to_string (Blobs.Handle.metadata blob).blob.id in
        [ id ^ ".blob"; id ^ ".sexp" ]))
    |> List.sort ~compare:String.compare
  in
  let directory =
    Filename.concat (Filename.dirname t.idempotency_path) "temporary-blobs"
  in
  let actual =
    Eio.Path.read_dir (path t.environment directory) |> List.sort ~compare:String.compare
  in
  require
    (List.equal String.equal actual expected)
    "maintenance left orphaned temporary blob files or removed live pairs"
;;

let verify_storage t ~env receipts blobs adopted handle =
  List.iter receipts ~f:(fun (receipt, retained) ->
    assert_receipt t.idempotency receipt retained);
  let reopened = Idempotency.open_or_create ~env ~path:t.idempotency_path |> store_ok in
  List.iter receipts ~f:(fun (receipt, retained) ->
    assert_receipt reopened receipt retained);
  List.iter blobs ~f:(fun (blob, retained) -> assert_blob t blob retained);
  assert_temporary_pairs t blobs;
  let id = (Blobs.Handle.metadata adopted).blob.id in
  let reopened = Blobs.open_session t.blobs handle id |> store_ok in
  require
    (String.equal (Blobs.load t.blobs reopened |> store_ok) "temporary-content")
    "maintenance removed adopted durable blob"
;;

let verify_response_cycles t now active active_file idle_file =
  let protected = [ Store.Handle.session_id active ] in
  assert_stats (maintenance t ~now ~protected) 2 2 1;
  require
    (exists t.environment active_file)
    "maintenance removed active-protected response artifact";
  require
    (String.equal (Eio.Path.load (path t.environment active_file)) "retained-response")
    "maintenance changed protected response content";
  require
    (not (exists t.environment idle_file))
    "maintenance retained expired inactive response artifact";
  assert_stats (maintenance t ~now ~protected) 0 0 0;
  assert_stats (maintenance t ~now ~protected:[]) 0 0 1;
  require
    (not (exists t.environment active_file))
    "released response artifact remained protected";
  assert_stats (maintenance t ~now ~protected:[]) 0 0 0
;;

let exercise t ~sw env =
  let wall = timestamp (Eio.Time.now (Eio.Stdenv.clock env)) in
  let active = session t ~sw wall "active-protected" in
  let idle = session t ~sw wall "idle" in
  let active_file = artifact t active in
  let idle_file = artifact t idle in
  let mtime = (Eio.Path.stat ~follow:false (path t.environment idle_file)).mtime in
  let now = timestamp (Float.round_up mtime +. 3601.) in
  let receipts = seed_receipts t now in
  let blobs = seed_blobs t ~sw now in
  let adopted = upload t ~sw now (Some now) |> Blobs.adopt t.blobs active |> store_ok in
  let cache = Filename.concat (Store.Handle.cache_directory idle) "unrelated" in
  save t.environment cache "keep-cache";
  assert_stats (maintenance t ~now:(shifted now (-120.)) ~protected:[]) 0 0 0;
  require
    (exists t.environment active_file && exists t.environment idle_file)
    "retention removed newer-than-cutoff artifacts";
  verify_response_cycles t now active active_file idle_file;
  require (exists t.environment cache) "response retention touched non-response artifacts";
  verify_storage t ~env receipts blobs adopted active;
  Store.close_session t.sessions active |> store_ok;
  Store.close_session t.sessions idle |> store_ok
;;

let run env environment =
  Eio.Switch.run (fun sw ->
    let t = create ~sw env environment in
    Exn.protect
      ~f:(fun () -> exercise t ~sw env)
      ~finally:(fun () -> Store.close t.sessions |> store_ok))
;;
