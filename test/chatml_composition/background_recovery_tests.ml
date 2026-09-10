open Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol
module Store = Agent_store.Job_result_store
module Blob = Agent_store.Blob_store

let store_ok result =
  Result.map_error result ~f:Agent_store.Store_error.to_protocol_error |> protocol_ok
;;

let%expect_test
    "daemon startup publishes finished stages before interrupting unfinished jobs"
  =
  let expected = ref [] in
  let completion = P.Completion.Succeeded (`String (String.make 1024 'r')) in
  with_background_daemon
    ~check_restored:(fun before restored ->
      let stage, reference =
        List.find_map !expected ~f:(fun (id, stage, reference) ->
          Option.some_if (P.Id.Job.equal before.J.id id) (stage, reference))
        |> Option.value_exn
      in
      match stage, restored.J.status with
      | ("durable" | "temporary"), Succeeded ->
        (match J.terminal_result restored |> protocol_ok with
         | Some (Artifact { reference = actual; _ }) ->
           assert (P.Id.Blob.equal reference.P.Job_artifact.blob.id actual.blob.id)
         | _ -> failwith "expected recovered reference")
      | "partial", Interrupted _ -> ()
      | "cancelled", Cancelled -> ()
      | _ -> raise_s [%sexp (stage : string), (restored.status : J.status)])
    ~after_recovery:(fun env client _ before ->
      List.iter !expected ~f:(fun (id, stage, reference) ->
        match stage with
        | "durable" | "temporary" ->
          let actual =
            Background_artifact_tests.read_artifact client reference |> protocol_ok
          in
          assert (P.Completion.equal actual completion);
          let prior =
            List.find_exn before.jobs ~f:(fun job -> P.Id.Job.equal job.id id)
          in
          let current, _ = Background_artifact_tests.await_result env client prior in
          assert (
            P.Timestamp.equal
              (Option.value_exn current.completed_at)
              (Option.value_exn prior.started_at));
          print_endline (stage ^ ": original artifact and selection time survived restart")
        | _ -> ());
      print_endline
        "incomplete stage interrupted; explicit cancellation preserved; no tool or model \
         replay")
    (fun env _client entry capabilities ->
       Eio.Switch.run (fun sw ->
         let session =
           Option.value_exn entry.Agent_server.Session_registry.store_handle
         in
         let directory = Agent_store.Session_store.Handle.directory session in
         let root =
           Agent_store.Data_root.open_existing
             ~env
             ~path:(Filename.dirname (Filename.dirname directory))
           |> store_ok
         in
         let blobs =
           Blob.create
             ~env
             ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path root)
             ~durable_directory:(Agent_store.Data_root.durable_blobs_path root)
             ~max_upload_bytes:100000L
           |> store_ok
         in
         List.iter [ "durable"; "temporary"; "partial"; "cancelled" ] ~f:(fun stage ->
           let state = A.state entry.actor |> protocol_ok in
           let at = P.Timestamp.now () in
           let job : J.t =
             { id = P.Id.Job.create ()
             ; session_id = state.identity.session_id
             ; generation = state.identity.generation
             ; kind = Async_tool
             ; payload = B.to_json (native capabilities "report.txt")
             ; status = Running
             ; retry_policy = Never
             ; attempt = 1
             ; created_at = at
             ; started_at = Some at
             ; next_run_at = None
             ; completed_at = None
             ; result = None
             ; delivery = Not_required
             ; launch = None
             ; progress = None
             }
           in
           A.add_job entry.actor job |> protocol_ok |> ignore;
           let prepared =
             Store.prepare
               blobs
               ~env
               ~sw
               ~session
               ~job
               ~creating_principal:(principal ()).id
               ~now:at
               ~max_bytes:4096
               completion
             |> store_ok
           in
           let reference = Store.reference prepared in
           expected := (job.id, stage, reference) :: !expected;
           let id = P.Id.Blob.to_string reference.blob.id in
           let path directory suffix =
             Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory (id ^ suffix))
           in
           let final = Filename.concat directory "blobs" in
           match stage with
           | "durable" -> ()
           | "temporary" ->
             Eio.Path.unlink (path final ".sexp");
             Eio.Path.rename
               (path final ".blob")
               (path (Agent_store.Data_root.temporary_blobs_path root) ".blob")
           | "partial" ->
             Eio.Path.unlink (path final ".sexp");
             Eio.Path.unlink (path final ".blob");
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               (path (Agent_store.Data_root.temporary_blobs_path root) ".part")
               (String.prefix (P.Completion.to_json completion |> Jsonaf.to_string) 20)
           | "cancelled" ->
             A.cancel_job_internal entry.actor ~job_id:job.id |> protocol_ok |> ignore
           | _ -> assert false)));
  [%expect
    {|
    temporary: original artifact and selection time survived restart
    durable: original artifact and selection time survived restart
    incomplete stage interrupted; explicit cancellation preserved; no tool or model replay
    |}]
;;
