open! Core

let durable_storage path =
  let root =
    Agent_server.Local_storage.Root.create ~path ()
    |> Result.map_error ~f:(fun (error : Agent_protocol.Error.t) -> error.message)
    |> Result.ok_or_failwith
  in
  Agent_server.Local_storage.Durable root
;;

let ok = function
  | Ok value -> value
  | Error error -> failwith error.Agent_protocol.Error.message
;;

let key value = Agent_protocol.Idempotency_key.of_string value |> ok
let request connection command = Agent_client.Connection.request connection command

let get connection session_id =
  match request connection (Session_get { session_id; history = None }) |> ok with
  | Session_get snapshot -> Agent_protocol.Public.Snapshot.fields snapshot
  | _ -> failwith "expected snapshot"
;;

let attach connection session_id =
  ignore
    (Agent_client.Session_handle.initialize
       connection
       ~implementation_name:"spec-audit"
       ~implementation_version:"dev"
     |> ok
     : Agent_protocol.Initialize.Response.t);
  match
    request
      connection
      (Session_attach
         { session_id
         ; requested_mode = Read_only
         ; subscribe = false
         ; after_sequence = None
         ; reclaim_token = None
         ; idempotency_key = key "attach"
         })
    |> ok
  with
  | Session_attach attached -> attached.attachment
  | _ -> failwith "expected attachment"
;;

let describe = function
  | Ok _ -> "accepted"
  | Error error -> Agent_protocol.Error.code_to_string error.Agent_protocol.Error.code
;;

let probe env host reader =
  let writer = Agent_server.Embedded.connection host in
  let session_id = Agent_server.Embedded.session_id host in
  let writer_attachment = Agent_server.Embedded.attachment host in
  ignore
    (request
       writer
       (Session_stop
          { session_id
          ; attachment_id = writer_attachment.id
          ; mode = Cancel
          ; idempotency_key = key "stop"
          })
     |> ok
     : Agent_protocol.Public.Result.t);
  let attachment = attach reader session_id in
  let before = get writer session_id in
  let started =
    request
      reader
      (Session_start
         { session_id
         ; attachment_id = attachment.id
         ; queue_if_limited = false
         ; idempotency_key = key "readonly-start"
         })
  in
  let after = get writer session_id in
  let deleted =
    request
      reader
      (Session_delete
         { session_id
         ; attachment_id = attachment.id
         ; expected_revision = after.revision
         ; policy = Remove
         ; confirmation = Agent_protocol.Id.Session.to_string session_id
         ; idempotency_key = key "readonly-delete"
         })
  in
  assert (String.equal (describe started) "permission_denied");
  assert (Int64.equal before.revision after.revision);
  assert (Int64.equal before.latest_event_sequence after.latest_event_sequence);
  assert (String.equal (describe deleted) "permission_denied");
  Eio.Flow.copy_string
    (sprintf
       "mode=read_only start=%s revision_before=%Ld revision_after=%Ld delete=%s\n"
       (describe started)
       before.revision
       after.revision
       (describe deleted))
    (Eio.Stdenv.stdout env)
;;

let probe_nested_source env root host =
  let session_id = Agent_server.Embedded.session_id host in
  let snapshot = get (Agent_server.Embedded.connection host) session_id in
  let revision = Option.value_exn snapshot.session.prompt_revision in
  let tree_path =
    Filename.concat
      (Filename.concat root "store/prompt-artifacts")
      (Agent_protocol.Id.Prompt_revision.to_string revision ^ "/tree")
  in
  let tree = Eio.Path.(Eio.Stdenv.fs env / tree_path) in
  let source = Eio.Path.(Eio.Stdenv.fs env / root) in
  let name = "spec-audit-nested-child.chatmd" in
  let cache =
    Chat_response.Cache.load ~file:Eio.Path.(source / "cache.bin") ~max_size:10 ()
  in
  let fixture =
    Inference_fixture.create
      ~namespace:Agent_protocol.Id.Transaction.(to_string (create ()))
      ~default_model:"o3"
      ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected specification inference")
  in
  let preparation_ok result =
    Result.map_error result ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default
    |> preparation_ok
  in
  let context = Inference_fixture.resolve fixture target |> preparation_ok in
  let ctx =
    Chat_response.Ctx.create
      ~inference_context:context
      ~inference_identity:(Inference_fixture.identity fixture)
      ~on_inference_attempt:ignore
      ~on_inference_observation:ignore
      ~on_inference_completion:ignore
      ~env
      ~dir:tree
      ~tool_dir:source
      ~cache
      ()
  in
  let fetched =
    Result.try_with (fun () -> Chat_response.Fetch.get ~ctx name ~is_local:true)
  in
  assert (Result.is_ok fetched);
  assert (Eio.Path.is_file Eio.Path.(tree / name));
  Eio.Flow.copy_string
    (sprintf
       "nested_source_exists=%b nested_artifact_exists=%b nested_fetch=%s\n"
       (Eio.Path.is_file Eio.Path.(source / name))
       (Eio.Path.is_file Eio.Path.(tree / name))
       (if Result.is_ok fetched then "loaded" else "failed"))
    (Eio.Stdenv.stdout env)
;;

let run_host env sw root =
  let prompt_file = Filename.concat root "prompt.chatmd" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / prompt_file)
    "<developer>Offline specification audit fixture.</developer>\n\
     <tool name=\"child\" agent=\"spec-audit-nested-child.chatmd\" local/>";
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    Eio.Path.(Eio.Stdenv.fs env / root / "spec-audit-nested-child.chatmd")
    "<developer>Offline nested fixture.</developer>";
  let options =
    Agent_server.Embedded.
      { prompt_file
      ; workspace = root
      ; tool_dir = root
      ; home = Some root
      ; storage = durable_storage (Filename.concat root "store")
      ; start_immediately = true
      ; permission_profile = default_permission_profile
      ; attachment_mode = Read_write
      ; event_capacity = 128
      }
  in
  let daemon_options =
    { Agent_server.Daemon.default_options with
      inference_policy =
        Agent_server_test_support.inference_policy
          ~default_model:"o3"
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            failwith "unexpected specification inference")
    }
  in
  let host = Agent_server.Embedded.start ~sw ~env ~daemon_options options |> ok in
  Exn.protect
    ~finally:(fun () -> Agent_server.Embedded.close host)
    ~f:(fun () ->
      probe_nested_source env root host;
      let reader = Agent_server.Embedded.connect host in
      Exn.protect
        ~finally:(fun () -> Agent_client.Connection.close reader)
        ~f:(fun () -> probe env host reader))
;;

let () =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      Filename.concat
        "/tmp"
        ("ochat-readonly-audit-" ^ Agent_protocol.Id.Transaction.(to_string (create ())))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:false path)
      ~f:(fun () ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          Eio.Switch.run (fun sw -> run_host env sw root))))
;;
