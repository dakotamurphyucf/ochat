open Core

let () = Mirage_crypto_rng_unix.use_default ()

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected store error", (error : Agent_store.Store_error.t)]
;;

let frame_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected frame error", (error : Agent_store.Frame.error)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected protocol error", (error : Agent_protocol.Error.t)]
;;

let with_temp_directory name f =
  Eio_main.run (fun env ->
    let path =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        (name
         ^ "."
         ^ (Agent_protocol.Id.Transaction.create ()
            |> Agent_protocol.Id.Transaction.to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env path)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let directory_exists env path = Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / path)

let path_exists env path =
  match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
  | `Not_found -> false
  | _ -> true
;;

let server_id =
  match Agent_protocol.Id.Server.of_string "srv_agent_store_test" with
  | Ok id -> id
  | Error error ->
    raise_s [%sexp "invalid test server ID", (error : Agent_protocol.Error.t)]
;;

let timestamp =
  Agent_protocol.Timestamp.of_string "2026-08-15T12:00:00Z"
  |> function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "invalid test timestamp", (error : Agent_protocol.Error.t)]
;;

let session_id = Agent_protocol.Id.Session.of_string "ses_agent_store_test" |> protocol_ok

let transaction_id =
  Agent_protocol.Id.Transaction.of_string "txn_agent_store_test" |> protocol_ok
;;

let session_summary revision =
  let prompt_id =
    Agent_protocol.Id.Prompt_definition.of_string "prd_agent_store_test" |> protocol_ok
  in
  let workspace_id =
    Agent_protocol.Id.Workspace_definition.of_string "wsd_agent_store_test" |> protocol_ok
  in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt_id)
      ~workspace:(Configured workspace_id)
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~display_name:"store test"
      ~labels:[ "suite", "agent-store" ]
      ()
    |> protocol_ok
  in
  Agent_protocol.Session.
    { id = session_id
    ; creator = None
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec
    ; desired_state = Stopped
    ; observed_state = Stopped
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision
    ; latest_event_sequence = revision
    }
;;

let metadata revision =
  Agent_store.Session_store.Metadata.
    { schema_version = 1
    ; session = session_summary revision
    ; prompt_artifact = "prompt-artifact"
    ; workspace_identity = "workspace-instance"
    ; data_schema_version = 1
    }
;;
