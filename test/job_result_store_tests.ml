open Core
open Agent_store_test_fixtures
module Store = Agent_store.Job_result_store
module Blob = Agent_store.Blob_store
module P = Agent_protocol

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

let prepare blobs sw session completion =
  Store.prepare
    blobs
    ~sw
    ~session
    ~job:(job session)
    ~creating_principal:principal
    ~now:timestamp
    ~max_bytes:4096
    completion
  |> store_ok
;;

let%expect_test
    "result artifact retries retain one reference and verify ownership after reopening"
  =
  with_store (fun env sw blobs reopen first second ->
    let completion =
      P.Completion.Succeeded
        (`Object [ "text", `String "saved résumé"; "count", `Number "3" ])
    in
    let prepared = prepare blobs sw first completion in
    let reference = Store.reference prepared in
    let commits = ref 0 in
    let saved =
      Filename.concat
        (Agent_store.Session_store.Handle.directory first)
        "job-result-reference.json"
    in
    let persist actual =
      incr commits;
      assert (P.Id.Blob.equal actual.P.Job_artifact.blob.id reference.blob.id);
      let open Result.Let_syntax in
      let%bind () =
        Agent_store.Durable_file.replace
          ~env
          ~durability:Flush_file_and_directory
          ~path:saved
          (P.Job_artifact.to_json actual |> Jsonaf.to_string)
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      match !commits with
      | 1 ->
        Error
          (P.Error.create
             Internal_error
             ~message:"injected reference save acknowledgement failure"
             ~retryable:false
             ())
      | _ -> Ok ()
    in
    let read_reference () =
      Agent_store.Durable_file.load ~env ~path:saved
      |> store_ok
      |> Jsonaf.of_string
      |> P.Job_artifact.of_json
      |> protocol_ok
    in
    assert (Result.is_error (Store.commit prepared ~persist));
    assert (Result.is_error (Store.discard prepared));
    assert (
      P.Completion.equal
        completion
        (Store.load (reopen ()) ~sw ~session:first ~max_bytes:4096 (read_reference ())
         |> store_ok));
    Store.commit prepared ~persist |> protocol_ok;
    assert (Result.is_error (Store.discard prepared));
    let restored = read_reference () in
    let blobs = reopen () in
    let result =
      Store.load blobs ~sw ~session:first ~max_bytes:4096 restored |> store_ok
    in
    assert (P.Completion.equal completion result);
    assert (
      Result.is_error (Store.load blobs ~sw ~session:second ~max_bytes:4096 restored));
    let forged =
      P.Job_artifact.create
        ~session_id:restored.session_id
        ~job_id:(P.Id.Job.create ())
        ~generation:restored.generation
        ~attempt:restored.attempt
        ~blob:restored.blob
      |> protocol_ok
    in
    assert (Result.is_error (Store.load blobs ~sw ~session:first ~max_bytes:4096 forged));
    let handle = Blob.open_session blobs first reference.blob.id |> store_ok in
    assert (Result.is_error (Blob.adopt blobs second handle));
    print_s [%sexp (result : P.Completion.t), (Int.to_string !commits : string)];
    print_endline
      "same artifact retried; retained data reopened; foreign session/job rejected");
  [%expect
    {|
    ((Succeeded
      (Object ((text (String "saved r\195\169sum\195\169")) (count (Number 3)))))
     2)
    same artifact retried; retained data reopened; foreign session/job rejected
    |}]
;;

let%expect_test
    "result artifact corruption, read bounds and discarded preparations fail closed"
  =
  with_store (fun env sw blobs _reopen first _second ->
    let completion = P.Completion.Succeeded (`String "original completion") in
    let prepared = prepare blobs sw first completion in
    let reference = Store.reference prepared in
    assert (Result.is_error (Store.load blobs ~sw ~session:first ~max_bytes:4 reference));
    let path =
      Filename.concat
        (Filename.concat (Agent_store.Session_store.Handle.directory first) "blobs")
        (P.Id.Blob.to_string reference.blob.id ^ ".blob")
    in
    let original = P.Completion.to_json completion |> Jsonaf.to_string in
    List.iter
      [ String.make (String.length original) 'x'; ""; String.make 5000 'x' ]
      ~f:(fun content ->
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / path)
          content;
        match Store.load blobs ~sw ~session:first ~max_bytes:4096 reference with
        | Error (Agent_store.Store_error.Corrupt _) ->
          print_endline "corrupt bytes rejected before decoding"
        | _ -> failwith "modified result artifact was accepted");
    Store.discard prepared |> store_ok;
    Store.discard prepared |> store_ok;
    assert (
      Result.is_error (Store.load blobs ~sw ~session:first ~max_bytes:4096 reference));
    let invoked = ref false in
    assert (
      Result.is_error
        (Store.commit prepared ~persist:(fun _ ->
           invoked := true;
           Ok ())));
    assert (not !invoked);
    assert (
      Result.is_error
        (Store.prepare
           blobs
           ~sw
           ~session:first
           ~job:(job first)
           ~creating_principal:principal
           ~now:timestamp
           ~max_bytes:8
           completion));
    print_endline
      "discard is idempotent; ended preparation cannot commit; output bound enforced");
  [%expect
    {|
    corrupt bytes rejected before decoding
    corrupt bytes rejected before decoding
    corrupt bytes rejected before decoding
    discard is idempotent; ended preparation cannot commit; output bound enforced
    |}]
;;

let%expect_test
    "blob adoption preserves target ownership and refuses destination collisions"
  =
  with_store (fun env sw blobs _reopen first second ->
    let id = P.Id.Blob.create () in
    let upload =
      Blob.begin_upload
        blobs
        ~sw
        ~id
        ~creating_principal:principal
        ~target_session:(Some (Agent_store.Session_store.Handle.session_id first))
        ~kind:File
        ~media_type:"text/plain"
        ~display_name:None
        ~allowed_use:"fixture"
        ~created_at:timestamp
        ~expires_at:None
      |> store_ok
    in
    Blob.write_string upload "new data" |> store_ok;
    let handle = Blob.finish upload ~expected_digest:None |> store_ok in
    assert (Result.is_error (Blob.adopt blobs second handle));
    [%test_eq: string]
      "new data"
      (Blob.load_verified blobs ~sw handle ~max_bytes:32 |> store_ok);
    let directory =
      Filename.concat (Agent_store.Session_store.Handle.directory first) "blobs"
    in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / directory);
    let path = Filename.concat directory (P.Id.Blob.to_string id ^ ".blob") in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / path)
      "existing data";
    assert (Result.is_error (Blob.adopt blobs first handle));
    [%test_eq: string] "existing data" (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path));
    [%test_eq: string]
      "new data"
      (Blob.load_verified blobs ~sw handle ~max_bytes:32 |> store_ok);
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / path);
    let adopted = Blob.adopt blobs first handle |> store_ok in
    [%test_eq: string]
      "new data"
      (Blob.load_verified blobs ~sw adopted ~max_bytes:32 |> store_ok);
    print_endline
      "foreign adoption rejected; collision preserved both blobs; original target \
       succeeded");
  [%expect
    {| foreign adoption rejected; collision preserved both blobs; original target succeeded |}]
;;

let fail_metadata_rename (Eio.Resource.T (directory, handler) as native_directory) armed =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let rename directory source _destination target =
      match !armed, String.is_suffix target ~suffix:".sexp" with
      | Some after_rename, true ->
        armed := None;
        if after_rename then Original.rename directory source native_directory target;
        failwith "injected metadata rename failure"
      | _ -> Original.rename directory source native_directory target
    ;;
  end
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
;;

let fault_env env armed =
  let directory, path = Eio.Stdenv.fs env in
  let fs = fail_metadata_rename directory armed, path in
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

let%expect_test "failed adoption restores temporary data before a safe retry" =
  List.iter [ false; true ] ~f:(fun after_rename ->
    let armed = ref None in
    with_store
      ~wrap_env:(fun env -> fault_env env armed)
      (fun env sw blobs _ first _ ->
         let id = P.Id.Blob.create () in
         let upload =
           Blob.begin_upload
             blobs
             ~sw
             ~id
             ~creating_principal:principal
             ~target_session:(Some (Agent_store.Session_store.Handle.session_id first))
             ~kind:File
             ~media_type:"text/plain"
             ~display_name:None
             ~allowed_use:"fixture"
             ~created_at:timestamp
             ~expires_at:None
           |> store_ok
         in
         Blob.write_string upload "retained through save failure" |> store_ok;
         let handle = Blob.finish upload ~expected_digest:None |> store_ok in
         armed := Some after_rename;
         assert (Result.is_error (Blob.adopt blobs first handle));
         assert (Option.is_none !armed);
         let temporary = Blob.open_temporary blobs id |> store_ok in
         [%test_eq: string]
           "retained through save failure"
           (Blob.load_verified blobs ~sw temporary ~max_bytes:128 |> store_ok);
         let destination =
           Filename.concat (Agent_store.Session_store.Handle.directory first) "blobs"
         in
         assert (
           List.is_empty (Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / destination)));
         Blob.adopt blobs first handle |> store_ok |> ignore;
         let adopted = Blob.open_session blobs first id |> store_ok in
         [%test_eq: string]
           "retained through save failure"
           (Blob.load_verified blobs ~sw adopted ~max_bytes:128 |> store_ok);
         assert (Result.is_error (Blob.open_temporary blobs id));
         print_s [%sexp { after_rename : bool; restored_and_retried = (true : bool) }]));
  [%expect
    {|
    ((after_rename false) (restored_and_retried true))
    ((after_rename true) (restored_and_retried true))
    |}]
;;
