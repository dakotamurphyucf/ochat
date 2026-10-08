open! Core
module P = Agent_protocol
module S = Agent_server
module Q = P.Inference_query

let ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let%expect_test
    "actual initialized dispatcher queries current authority without requiring feature \
     selection"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      Filename.concat
        "/tmp"
        ("inference-query-" ^ P.Id.Transaction.(to_string (create ())))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(path / "prompt.chatmd")
          "<developer>Read-only query fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let dispatches = ref 0 in
          let options =
            { S.Daemon.default_options with
              inference_policy =
                Agent_server_test_support.inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream:(fun ~sw:_ ~inputs:_ ->
                    Int.incr dispatches;
                    failwith "query dispatched inference")
            }
          in
          let host =
            S.Embedded.start
              ~sw
              ~env
              ~daemon_options:options
              { prompt_file
              ; workspace = root
              ; tool_dir = root
              ; home = root
              ; data_root = Some (Filename.concat root "store")
              ; start_immediately = false
              ; permission_profile = S.Embedded.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 128
              }
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> S.Embedded.close host)
            ~f:(fun () ->
              let dispatcher = S.Embedded.dispatcher host in
              let session_id = S.Embedded.session_id host in
              let owner = S.Embedded.principal host in
              let context scopes =
                let principal = { owner with scopes = P.Scope.Set.of_list scopes } in
                let context =
                  S.Connection_context.create
                    ~connection_id:"query-only"
                    ~principal
                    ~transport:Http
                    ~publish_notification:(fun _ ->
                      failwith "query published notification")
                    ~max_attachments:1
                in
                let initialize =
                  P.Initialize.Request.create
                    ~implementation:
                      (P.Initialize.Implementation.create ~name:"query-test" ~version:"1"
                       |> ok)
                    ~protocol_min:P.Version.current
                    ~protocol_max:P.Version.current
                    ~features:[]
                    ~event_encodings:[ Json ]
                    ~max_inbound_event_bytes:(16 * 1024 * 1024)
                    ()
                  |> ok
                in
                ignore
                  (S.Dispatcher.dispatch_command
                     dispatcher
                     ~context
                     (Protocol_initialize initialize)
                   |> ok
                   : P.Public.Result.t);
                context
              in
              let no_transcript = context [] in
              let summary =
                S.Dispatcher.dispatch_command
                  dispatcher
                  ~context:no_transcript
                  (Session_inference_summary { session_id })
                |> ok
              in
              (match summary with
               | P.Public.Result.Non_history value ->
                 (match P.Public.Result.Non_history.value value with
                  | Session_inference_summary summary ->
                    [%test_eq: int64] 0L (Q.Summary.retained_attempts summary)
                  | _ -> failwith "summary method correlation lost")
               | _ -> failwith "unexpected inline history result");
              let row_request include_configuration =
                Q.Request.create
                  ~session_id
                  ~page:(P.Page.Request.create ~limit:1 () |> ok)
                  ~include_configuration
                  ~include_diagnostics:false
                |> ok
              in
              let rejected result =
                match result with
                | Error (error : P.Error.t) ->
                  [%test_eq: P.Error.code] Permission_denied error.code
                | Ok _ -> failwith "query disclosure was granted"
              in
              rejected
                (S.Dispatcher.dispatch_command
                   dispatcher
                   ~context:no_transcript
                   (Session_inference_observations (row_request false)));
              let transcript = context [ View_session_transcript ] in
              rejected
                (S.Dispatcher.dispatch_command
                   dispatcher
                   ~context:transcript
                   (Session_inference_observations (row_request true)));
              let detailed = context [ View_session_transcript; Diagnostics ] in
              ignore
                (S.Dispatcher.dispatch_command
                   dispatcher
                   ~context:detailed
                   (Session_inference_observations (row_request true))
                 |> ok
                 : P.Public.Result.t);
              let foreign =
                let principal =
                  { owner with
                    id = P.Id.Principal.of_string "pri_foreign_query" |> ok
                  ; scopes = P.Scope.Set.of_list [ View_session_transcript; Diagnostics ]
                  }
                in
                let context =
                  S.Connection_context.create
                    ~connection_id:"foreign"
                    ~principal
                    ~transport:Http
                    ~publish_notification:ignore
                    ~max_attachments:1
                in
                S.Connection_context.mark_initialized ~version:P.Version.current context;
                context
              in
              rejected
                (S.Dispatcher.dispatch_command
                   dispatcher
                   ~context:foreign
                   (Session_inference_summary { session_id }));
              let id =
                P.Envelope.Request_id.of_json (`String "actual\"correlation\n😀") |> ok
              in
              let response =
                S.Dispatcher.dispatch_envelope
                  dispatcher
                  ~context:transcript
                  (P.Envelope.request
                     ~id
                     ~method_:"session.inference_observations"
                     ~params:(Q.Request.to_json (row_request false))
                     ())
                |> ok
                |> Option.value_exn
              in
              (match response with
               | P.Envelope.Response { id = actual; outcome = Ok body } ->
                 assert (P.Envelope.Request_id.compare id actual = 0);
                 ignore
                   (Q.Response.of_json body ~max_bytes:(16 * 1024 * 1024) |> ok
                    : Q.Response.t)
               | _ -> failwith "query envelope correlation lost");
              assert (
                String.length (Jsonaf.to_string (P.Envelope.to_json response))
                <= 16 * 1024 * 1024);
              let huge_id =
                P.Envelope.Request_id.of_json
                  (`String (String.make (16 * 1024 * 1024) 'x'))
                |> ok
              in
              assert (
                Result.is_error
                  (S.Dispatcher.dispatch_envelope
                     dispatcher
                     ~context:transcript
                     (P.Envelope.request
                        ~id:huge_id
                        ~method_:"session.inference_observations"
                        ~params:(Q.Request.to_json (row_request false))
                        ())));
              [%test_eq: int] 0 !dispatches))));
  print_endline
    "summary/rows/details use distinct authority; no feature gate or model dispatch; \
     correlation bounded";
  [%expect
    {| summary/rows/details use distinct authority; no feature gate or model dispatch; correlation bounded |}]
;;

let%expect_test
    "dispatcher pages retained ordinal gaps and rejects changed accounting before \
     continuation"
  =
  let module A = Agent_session in
  let module L = A.Inference_ledger in
  let module F = Inference_ledger_tests.Ledger_fixture in
  let module Fixtures = Agent_session_test.Fixtures in
  Fixtures.with_actor_workspace (fun env workspace_instance ->
    Mirage_crypto_rng_unix.use_default ();
    let root = Agent_server_test_support.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Read-only pagination fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon =
            S.Daemon.start
              ~sw
              ~env
              ~config:(Agent_server_test_support.config root root prompt_file)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ()
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> ignore (S.Daemon.shutdown daemon |> ok : unit))
            ~f:(fun () ->
              let p =
                P.Principal.create
                  ~id:(P.Id.Principal.of_string "pri_query_pages" |> ok)
                  ~authentication_kind:"local"
                  ~attributes:[]
                  ~scopes:(P.Scope.Set.of_list [ View_session_transcript; Diagnostics ])
                |> ok
              in
              let before =
                Fixtures.actor_state
                  ~workspace_instance
                  ~liveness:Detached
                  ~start_immediately:false
              in
              let ledger =
                L.create
                  ~session_id:before.identity.session_id
                  ~generation:0
                  ~before_tracking_unknown:false
                  ~limits:(F.limits ~attempts:2 ())
                |> F.ledger_ok
              in
              let closed ledger =
                let ledger, handle, _ = F.admit ledger in
                L.set_state ledger handle F.interrupted |> F.ledger_ok
              in
              let ledger = closed (closed (closed ledger)) in
              [%test_eq: int64 list]
                [ 2L; 3L ]
                (List.map (L.rows ledger) ~f:(fun row ->
                   L.Handle.ordinal (L.Row.handle row)));
              let current =
                ref
                  { before with
                    identity = { before.identity with creating_principal = Some p.id }
                  ; inference_ledger = ledger
                  }
              in
              let registry = S.Daemon.registry daemon in
              S.Session_registry.index
                registry
                { session = A.Session_state.summary !current
                ; runnable_job_count = 0
                ; deliverable_job_count = 0
                ; earliest_schedule_due = None
                ; owner_grace_deadline = None
                ; pending_initial_start = false
                ; archived = false
                };
              let reads = ref 0 in
              S.Session_registry.install_reader registry (fun _ ->
                Int.incr reads;
                Ok !current);
              let context =
                S.Connection_context.create
                  ~connection_id:"retained-pages"
                  ~principal:p
                  ~transport:Http
                  ~publish_notification:ignore
                  ~max_attachments:1
              in
              S.Connection_context.mark_initialized ~version:P.Version.current context;
              let query ?cursor () =
                Q.Request.create
                  ~session_id:before.identity.session_id
                  ~page:(P.Page.Request.create ~limit:1 ?cursor () |> ok)
                  ~include_configuration:false
                  ~include_diagnostics:false
                |> ok
              in
              let dispatch request =
                S.Dispatcher.dispatch_command
                  (S.Daemon.dispatcher daemon)
                  ~context
                  (Session_inference_observations request)
              in
              let page result =
                match result |> ok with
                | P.Public.Result.Non_history value ->
                  (match P.Public.Result.Non_history.value value with
                   | Session_inference_observations response ->
                     Q.Response.attempts response
                   | _ -> failwith "page method correlation lost")
                | _ -> failwith "unexpected history result"
              in
              let first = dispatch (query ()) |> page in
              [%test_eq: int64 list] [ 2L ] (List.map first.items ~f:Q.Attempt.ordinal);
              let cursor = Option.value_exn first.next_cursor in
              let second = dispatch (query ~cursor ()) |> page in
              [%test_eq: int64 list] [ 3L ] (List.map second.items ~f:Q.Attempt.ordinal);
              assert (Option.is_none second.next_cursor);
              let row = List.hd_exn (L.rows ledger) in
              let handle = L.Row.handle row in
              let changed, _ =
                L.observe ledger handle (F.usage handle ~revision:0L 7L) |> F.ledger_ok
              in
              current := { !current with inference_ledger = changed };
              (match dispatch (query ~cursor ()) with
               | Error error ->
                 [%test_eq: P.Error.code] Conflict error.code;
                 assert (
                   Document_schema.Json.equal
                     error.data
                     (`Object [ "restart_required", `True ]))
               | Ok _ -> failwith "changed ledger continuation was accepted");
              [%test_eq: int] 3 !reads;
              [%test_eq: int] 0 (S.Session_registry.stats registry).loaded))));
  print_endline
    "ordinal gap pages 2 then 3; changed accounting rejects continuation; no load";
  [%expect
    {| ordinal gap pages 2 then 3; changed accounting rejects continuation; no load |}]
;;
