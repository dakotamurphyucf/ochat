open Core
open Fixtures
module P = Agent_protocol
module State = Agent_session.Session_state
module Results = Agent_store.Job_result_store

type t =
  { publisher : Results.Publisher.t
  ; session : Agent_store.Session_store.Handle.t
  ; sessions : Agent_store.Session_store.t
  ; blobs : Agent_store.Blob_store.t
  ; temporary_directory : string
  }

let create env sw (initial : State.t) =
  let root =
    Filename.concat
      initial.spec.workspace_instance.canonical_root.native_path
      "result-store"
  in
  let sessions =
    Agent_store.Session_store.create
      ~env
      ~sw
      ~root
      ~server_id:(P.Id.Server.create ())
      ~process_start_identity:None
      ~lock_nonce:"artifact-retry"
    |> store_ok
  in
  let session =
    Agent_store.Session_store.create_session
      sessions
      ~sw
      ~transaction_id:(P.Id.Transaction.create ())
      ~actor_lock_nonce:"artifact-retry"
      { schema_version = 1
      ; session = State.summary initial
      ; prompt_artifact = P.Id.Prompt_revision.to_string initial.spec.prompt_revision_id
      ; workspace_identity = initial.spec.workspace_instance.conflict_domain
      ; data_schema_version = State.current_schema_version
      }
    |> store_ok
  in
  Eio.Switch.on_release sw (fun () ->
    Agent_store.Session_store.close_session sessions session |> store_ok;
    Agent_store.Session_store.close sessions |> store_ok);
  let blobs =
    Agent_store.Blob_store.create
      ~env
      ~temporary_directory:(Filename.concat root "temporary")
      ~durable_directory:(Filename.concat root "durable")
      ~max_upload_bytes:100000L
    |> store_ok
  in
  let publisher =
    Results.Publisher.create
      ~env
      ~blobs
      ~sw
      ~session
      ~principal:(P.Id.Principal.create ())
      ~inline_bytes:64
      ~max_bytes:4096
    |> protocol_ok
  in
  { publisher
  ; session
  ; sessions
  ; blobs
  ; temporary_directory = Filename.concat root "temporary"
  }
;;
