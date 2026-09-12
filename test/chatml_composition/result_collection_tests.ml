open Core
open Agent_server_test_support
module P = Agent_protocol
module S = Agent_store
module A = Agent_session.Session_actor
module Results = S.Job_result_store

let store_ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : S.Store_error.t)]
;;

let with_daemon f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let prompt = Filename.concat root "agent.chatmd" in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / prompt)
            "<system>Retained result fixture.</system>";
          let daemon =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:(config root root prompt)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Agent_server.Daemon.default_options with
                  model_post_stream =
                    Some (fun ~sw:_ ~inputs:_ -> failwith "unexpected model request")
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let client = connection daemon (principal ()) in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close client)
                ~f:(fun () ->
                  initialize client;
                  let session, attachment = create_session client in
                  let entry =
                    Agent_server.Session_registry.find
                      (Agent_server.Daemon.registry daemon)
                      session.id
                    |> Option.value_exn
                  in
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                    f env sw root daemon entry attachment))))))
;;

let prepare env sw daemon entry text =
  let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
  let job : P.Job.t =
    { id = P.Id.Job.create ()
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; kind = Async_tool
    ; payload = `Null
    ; status = Running
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = P.Timestamp.now ()
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    ; launch = None
    ; progress = None
    }
  in
  Results.prepare
    (Agent_server.Daemon.blob_store daemon)
    ~env
    ~sw
    ~session:(Option.value_exn entry.store_handle)
    ~job
    ~creating_principal:(principal ()).id
    ~now:(P.Timestamp.now ())
    ~max_bytes:4096
    (P.Completion.Succeeded (`String text))
  |> store_ok
;;

let show result =
  print_s [%sexp (result |> protocol_ok : Results.Publisher.collection_stats option)]
;;

let rec collect_ready env entry =
  match entry.Agent_server.Session_registry.collect_results () with
  | Ok None ->
    (* Stopped-runtime retirement can briefly own the scope after explicit unload.
       None means defer, not a completed sweep. The fixture's outer timeout bounds
       this wait; retain all exact collection counts and file assertions below. *)
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    collect_ready env entry
  | result -> result
;;

let%expect_test
    "factory collector coordinates runtime ownership, auxiliary roots and malformed \
     storage"
  =
  with_daemon (fun env sw _root daemon entry _attachment ->
    let first = prepare env sw daemon entry "response retained" in
    let second = prepare env sw daemon entry "orphan" in
    let first_ref = Results.reference first
    and second_ref = Results.reference second in
    let handle = Option.value_exn entry.store_handle in
    let response =
      Eio.Path.(
        Eio.Stdenv.fs env
        / S.Session_store.Handle.responses_directory handle
        / "raw-openai-streaming-response.txt")
    in
    let id = P.Id.Blob.to_string first_ref.blob.id in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      response
      ("event: response.output_text.delta\ndata: {\"result\":\"\\u0062"
       ^ String.drop_prefix id 1
       ^ "\"}\n\n");
    Agent_server.Runtime_owner.ensure_loaded entry.runtime |> protocol_ok;
    show (entry.collect_results ());
    Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
    show (collect_ready env entry);
    assert (
      Result.is_ok
        (Results.load
           (Agent_server.Daemon.blob_store daemon)
           ~sw
           ~session:handle
           ~max_bytes:4096
           first_ref));
    assert (
      Result.is_error
        (Results.load
           (Agent_server.Daemon.blob_store daemon)
           ~sw
           ~session:handle
           ~max_bytes:4096
           second_ref));
    Eio.Path.save ~create:(`Or_truncate 0o600) response "data: {\"result\":\"unfinished";
    assert (Result.is_error (collect_ready env entry));
    assert (
      Result.is_ok
        (Results.load
           (Agent_server.Daemon.blob_store daemon)
           ~sw
           ~session:handle
           ~max_bytes:4096
           first_ref));
    Eio.Path.unlink response;
    show (collect_ready env entry);
    print_endline
      "loaded runtime deferred; escaped response retained its result; unrelated orphan \
       removed";
    print_endline
      "malformed response retained evidence; repaired roots allowed remaining cleanup");
  [%expect
    {|
    ()
    (((discarded 1) (retired 0) (retained 1)))
    (((discarded 1) (retired 0) (retained 0)))
    loaded runtime deferred; escaped response retained its result; unrelated orphan removed
    malformed response retained evidence; repaired roots allowed remaining cleanup
    |}]
;;

let%expect_test "maintenance reloads a stopped indexed session to collect preparations" =
  with_daemon (fun env sw root daemon entry attachment ->
    let prepared = prepare env sw daemon entry "abandoned preparation" in
    let reference = Results.reference prepared in
    A.detach entry.actor attachment.P.Session.Attachment.id |> protocol_ok;
    let sessions = Agent_server.Daemon.store daemon in
    let registry = Agent_server.Daemon.registry daemon in
    [%test_eq: int]
      1
      (Agent_server.Session_registry.unload_inactive
         registry
         ~index_entries:(S.Session_store.list_sessions sessions));
    assert (
      Option.is_none (Agent_server.Session_registry.find registry reference.session_id));
    (* Expiry uses an empty ledger here; the reconstructed entry's collector still
       holds the daemon's actual idempotency cache while proving references. *)
    let expiry =
      S.Idempotency_store.open_or_create
        ~env
        ~path:(Filename.concat root "expiry-only.sexp")
      |> store_ok
    in
    let stats =
      Agent_server.Maintenance.run_once
        ~env
        ~idempotency_store:expiry
        ~blob_store:(Agent_server.Daemon.blob_store daemon)
        ~session_store:sessions
        ~registry:(Some registry)
        ~protected_response_sessions:[]
        ~response_retention:(Time_ns.Span.of_day 1.)
        ~now:(P.Timestamp.now ())
      |> store_ok
    in
    [%test_eq: int] 1 stats.discarded_job_results;
    let reloaded =
      Agent_server.Session_registry.find registry reference.session_id |> Option.value_exn
    in
    assert (
      List.is_empty
        (S.Job_result_intent.list
           ~env
           ~session:(Option.value_exn reloaded.store_handle)
           ~max_count:8
         |> store_ok));
    print_endline
      "maintenance lazily reloaded the stopped actor and removed its unreferenced \
       preparation");
  [%expect
    {| maintenance lazily reloaded the stopped actor and removed its unreferenced preparation |}]
;;
