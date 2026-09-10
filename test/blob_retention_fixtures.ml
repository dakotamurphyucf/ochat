open Core
open Agent_store_test_fixtures
open Job_store_fixtures
module Intent = Agent_store.Job_result_intent
module Retention = Agent_store.Blob_retention
module Reader = Agent_store.Retention_reader
module Handle = Agent_store.Session_store.Handle

let upload blobs sw session id ~media_type ~allowed_use contents =
  let upload =
    Blob.begin_upload
      blobs
      ~sw
      ~id
      ~creating_principal:principal
      ~target_session:(Some (Handle.session_id session))
      ~kind:File
      ~media_type
      ~display_name:None
      ~allowed_use
      ~created_at:timestamp
      ~expires_at:None
    |> store_ok
  in
  Blob.write_string upload contents |> store_ok;
  Blob.finish upload ~expected_digest:None |> store_ok
;;

let stage env sw blobs session id value =
  let contents = P.Completion.to_json (Succeeded value) |> Jsonaf.to_string in
  let job = job session in
  let blob =
    P.Blob.Metadata.create
      ~id
      ~kind:File
      ~media_type:P.Job_artifact.media_type
      ~byte_length:(Int64.of_int (String.length contents))
      ~digest:Digestif.SHA256.(digest_string contents |> to_hex)
      ()
    |> protocol_ok
  in
  let reference =
    P.Job_artifact.create
      ~session_id:job.session_id
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~blob
    |> protocol_ok
  in
  let metadata : Blob.Metadata.t =
    { blob
    ; creating_principal = principal
    ; target_session = Some job.session_id
    ; allowed_use = P.Job_artifact.allowed_use reference
    ; created_at = timestamp
    ; expires_at = None
    ; durable = false
    }
  in
  let intent = Intent.create ~env ~session ~reference ~metadata |> store_ok in
  upload
    blobs
    sw
    session
    id
    ~media_type:blob.media_type
    ~allowed_use:metadata.allowed_use
    contents
  |> Blob.adopt blobs session
  |> store_ok
  |> ignore;
  intent
;;

let scan ?(max_bytes = 65536) ?(max_entries = 256) env blobs session =
  Blob.with_retention blobs ~f:(fun scope ->
    let open Result.Let_syntax in
    let%bind reader =
      Reader.create ~env ~root:(Handle.directory session) ~max_entries ~max_bytes
    in
    let%bind intents = Intent.list_with_reader ~reader ~session ~max_count:16 in
    Retention.scan ~scope ~session ~reader ~intents ~max_file_bytes:16384)
  |> Result.map ~f:(fun result -> Option.value_exn result)
;;
