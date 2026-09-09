open Core
open Agent_store_test_fixtures
module P = Agent_protocol
module Blob = Agent_store.Blob_store

let with_store ?(wrap_env = Fn.id) f =
  with_temp_directory "ochat-job-result" (fun env directory ->
    let env = wrap_env env in
    Eio.Switch.run (fun sw ->
      let root = Filename.concat directory "store" in
      let sessions =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root
          ~server_id
          ~process_start_identity:None
          ~lock_nonce:"job-artifact-test"
        |> store_ok
      in
      let session name =
        let metadata = metadata 0L in
        let id = P.Id.Session.of_string name |> protocol_ok in
        Agent_store.Session_store.create_session
          sessions
          ~sw
          ~transaction_id:(P.Id.Transaction.create ())
          ~actor_lock_nonce:name
          { metadata with session = { metadata.session with id } }
        |> store_ok
      in
      let first = session "ses_job_artifact_first"
      and second = session "ses_job_artifact_second" in
      let open_blobs () =
        Blob.create
          ~env
          ~temporary_directory:(Filename.concat directory "temporary")
          ~durable_directory:(Filename.concat directory "durable")
          ~max_upload_bytes:100_000L
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Agent_store.Session_store.close_session sessions first |> store_ok;
          Agent_store.Session_store.close_session sessions second |> store_ok;
          Agent_store.Session_store.close sessions |> store_ok)
        ~f:(fun () -> f env sw (open_blobs ()) open_blobs first second)))
;;

let job session =
  P.Job.
    { id = P.Id.Job.create ()
    ; session_id = Agent_store.Session_store.Handle.session_id session
    ; generation = 0
    ; kind = Async_tool
    ; payload = `Null
    ; status = Running
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = timestamp
    ; started_at = Some timestamp
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    ; launch = None
    ; progress = None
    }
;;

let principal = P.Id.Principal.of_string "pri_job_artifact" |> protocol_ok

let fail_metadata_rename
      ?(on_failure = fun _ -> ())
      ?(before_unlink = fun _ -> ())
      (Eio.Resource.T (directory, handler) as native_directory)
      armed
  =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let rename directory source _destination target =
      match !armed, String.is_suffix target ~suffix:".sexp" with
      | Some after_rename, true ->
        armed := None;
        on_failure target;
        if after_rename then Original.rename directory source native_directory target;
        failwith "injected metadata rename failure"
      | _ -> Original.rename directory source native_directory target
    ;;

    let unlink directory path =
      before_unlink path;
      Original.unlink directory path
    ;;
  end
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
;;

let fault_env ?on_failure ?before_unlink env armed =
  let directory, path = Eio.Stdenv.fs env in
  let fs = fail_metadata_rename ?on_failure ?before_unlink directory armed, path in
  object
    method fs = fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
