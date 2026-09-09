open Core
open Agent_server_test_support
module A = Agent_session.Session_actor
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module J = Agent_protocol.Job
module Completion = Agent_protocol.Completion

let native_agent =
  {|<tool name="read_file"><read id="reports" path="${workspace}/reports"/></tool>|}
;;

let with_background_daemon
      ?(profile = permission_profile)
      ?(agent = native_agent)
      ?(sources = [])
      ?model_post_stream
      ?(expected_model_calls = 0)
      ?(after_recovery = fun _env _client _entry _before -> ())
      ?(check_restored =
        fun job restored ->
          assert (Jsonaf.exactly_equal (J.to_json job) (J.to_json restored)))
      f
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdirs
          ~exists_ok:true
          ~perm:0o700
          Eio.Path.(Eio.Stdenv.fs env / workspace / "reports");
        let save path source =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / path)
            source
        in
        let prompt = Filename.concat root "agent.chatmd" in
        save prompt agent;
        List.iter sources ~f:(fun (name, source) ->
          save (Filename.concat root name) source);
        save (Filename.concat workspace "reports/report.txt") "background report";
        save (Filename.concat workspace "reports/second.txt") "second report";
        save (Filename.concat workspace "secret.txt") "PRIVATE-BACKGROUND-SENTINEL";
        let configuration = config ~profile root workspace prompt in
        let requests = ref 0 in
        let stage = ref "starting first daemon" in
        let live_switch = ref None in
        let with_fixture_switch f =
          Eio.Fiber.first
            (fun () ->
               Eio.Switch.run (fun sw ->
                 live_switch := Some sw;
                 f sw))
            (fun () ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 30.;
               let dump =
                 Option.value_map
                   !live_switch
                   ~default:"no switch"
                   ~f:(Format.asprintf "%a" Eio.Switch.dump)
               in
               failwith ("background fixture timed out: " ^ !stage ^ "\n" ^ dump))
        in
        with_fixture_switch (fun sw ->
          let start () =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:configuration
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Agent_server.Daemon.default_options with
                  qualify_chatml_extensions = true
                ; model_post_stream =
                    Some
                      (fun ~sw ~inputs ->
                        incr requests;
                        match model_post_stream with
                        | Some post -> post ~sw ~inputs
                        | None -> failwith "background work must not call the model")
                }
              ()
            |> protocol_ok
          in
          let daemon = start () in
          let completed_state =
            Exn.protect
              ~finally:(fun () ->
                stage := "stopping first daemon";
                Agent_server.Daemon.shutdown daemon |> protocol_ok;
                stage := "first daemon stopped")
              ~f:(fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    let session, _ = create_session ~start_immediately:true client in
                    let entry =
                      Agent_server.Session_registry.find
                        (Agent_server.Daemon.registry daemon)
                        session.id
                      |> Option.value_exn
                    in
                    let initial = A.state entry.actor |> protocol_ok in
                    let capabilities =
                      Agent_server.Runtime_owner.with_background_runtime
                        entry.runtime
                        (fun runtime ->
                           Ok
                             (Agent_session.Script_tool_calls.current_capabilities
                                (Option.value_exn runtime.moderator_script_tools)))
                      |> protocol_ok
                    in
                    stage := "running test callback";
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                      f env client entry capabilities);
                    stage := "checking final actor state";
                    [%test_eq: int] expected_model_calls !requests;
                    let state = A.state entry.actor |> protocol_ok in
                    [%test_eq: int]
                      (List.length initial.conversation.canonical_history)
                      (List.length state.conversation.canonical_history);
                    state))
          in
          stage := "starting recovered daemon";
          let recovered = start () in
          stage := "reading recovered jobs";
          Exn.protect
            ~finally:(fun () ->
              stage := "stopping recovered daemon";
              Agent_server.Daemon.shutdown recovered |> protocol_ok;
              stage := "recovered daemon stopped")
            ~f:(fun () ->
              let client = connection recovered (principal ()) in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close client)
                ~f:(fun () ->
                  initialize client;
                  List.iter completed_state.jobs ~f:(fun job ->
                    match
                      Agent_client.Connection.request
                        client
                        (Job_get { session_id = job.session_id; job_id = job.id })
                      |> protocol_ok
                    with
                    | Job_get restored -> check_restored job restored
                    | _ -> failwith "unexpected recovered job response");
                  let entry =
                    Agent_server.Session_registry.find
                      (Agent_server.Daemon.registry recovered)
                      completed_state.identity.session_id
                    |> Option.value_exn
                  in
                  after_recovery env client entry completed_state;
                  [%test_eq: int] expected_model_calls !requests));
          stage := "joining fixture switch")))
;;

let submit ?created_at entry payload =
  let state = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
  let job =
    J.
      { id = Agent_protocol.Id.Job.create ()
      ; session_id = state.identity.session_id
      ; generation = state.identity.generation
      ; kind = Async_tool
      ; payload
      ; status = Queued
      ; retry_policy = Never
      ; attempt = 0
      ; created_at =
          Option.value_or_thunk created_at ~default:Agent_protocol.Timestamp.now
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Pending
      ; launch = None
      ; progress = None
      }
  in
  A.add_job entry.actor job |> protocol_ok |> ignore;
  job
;;

let rec await env client (job : J.t) =
  let current =
    match
      Agent_client.Connection.request
        client
        (Job_get { session_id = job.session_id; job_id = job.id })
      |> protocol_ok
    with
    | Job_get job -> job
    | _ -> failwith "unexpected job.get response"
  in
  match current.status with
  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await env client job
  | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    (match current.delivery with
     | Pending -> ()
     | _ -> failwith "generic job delivered as model event");
    current, Completion.of_json (Option.value_exn current.result) |> protocol_ok
;;

let tool capabilities name input =
  let reference =
    C.find capabilities ~name
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
    |> C.reference
  in
  B.tool
    ~capabilities
    ~reference
    ~input
    ~policy:Chat_response.One_off_request.default_policy
  |> protocol_ok
;;

let native capabilities file =
  tool
    capabilities
    "read_file"
    (`Object [ "root", `String "reports"; "file", `String file ])
;;

let script env capabilities source input =
  let prepared =
    Chat_response.One_off_script.prepare_in_domain
      ~env
      ~capabilities
      ~tools:[ "read_file" ]
      ~source
      ()
    |> Result.map_error ~f:(fun _ -> "fixture compilation failed")
    |> Result.ok_or_failwith
  in
  B.script ~prepared ~input ~policy:Chat_response.One_off_request.default_policy
  |> protocol_ok
;;
