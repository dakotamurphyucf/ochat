open! Core
open Support
module T = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Factory = Agent_server.Session_factory
module Embedded = Agent_server.Embedded
module Client = Agent_client
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference
module Res = Openai.Responses

let completed_response () =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = "485-dispatched-result"
    ; status = "completed"
    ; content =
        [ { annotations = []; text = "485 actual timer result"; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = message.id
      ; output_index = 0
      ; content_index = 0
      ; delta = "485 actual timer result"
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let protect_cleanup ~finally f =
  match f () with
  | result ->
    Eio.Cancel.protect finally;
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let start env sw root config mode ~policy =
  Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:
      { Daemon.default_options with
        startup_mode = mode
      ; qualify_chatml_extensions = true
      ; inference_policy = policy
      }
    ()
  |> protocol_ok
;;

let with_daemon env root config mode ~policy f =
  Eio.Switch.run (fun sw ->
    let daemon = start env sw root config mode ~policy in
    protect_cleanup
      ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
      (fun () -> f sw daemon))
;;

let loaded daemon session_id =
  Registry.find (Daemon.registry daemon) session_id |> Option.value_exn
;;

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
    let rec loop () =
      match f () with
      | Some result -> result
      | None ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let healthy (state : A.Session_state.t) =
  Option.iter state.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
  List.iter state.jobs ~f:(fun job ->
    match job.P.Job.status with
    | Failed error -> raise_s [%sexp (error : P.Error.t)]
    | Cancelled | Interrupted _ -> raise_s [%sexp (job : P.Job.t)]
    | Queued | Running | Waiting_permission _ | Waiting_completion _ | Succeeded -> ());
  List.iter state.moderator_executions ~f:(fun execution ->
    match execution.P.Moderator_execution.status with
    | Failed _ | Interrupted _ -> raise_s [%sexp (execution : P.Moderator_execution.t)]
    | Running | Completed _ -> ())
;;

let row state name =
  C.find (Option.value_exn state.A.Session_state.recurrence_catalog) (id name)
  |> Option.value_exn
;;

let execution state name =
  E.frontier (row state name) |> protocol_ok |> H.executions |> List.hd_exn
;;

let admission state name =
  let execution = execution state name in
  let current = Execution.current_attempt execution in
  let origin = Execution.Attempt.origin current |> Option.value_exn in
  let admission = Origin.admission origin in
  let index = Option.value_exn state.A.Session_state.run_state in
  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
  Origin.validate_run origin ~run ~admission |> protocol_ok;
  let reciprocal = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
  assert (Origin.equal origin reciprocal);
  execution, origin, admission, run
;;

let terminal state name =
  let execution, origin, admission, run = admission state name in
  let retained = Execution.current_attempt execution |> Execution.Attempt.terminal in
  match run.lifecycle, retained with
  | Terminal (Completed _), Some terminal ->
    Terminal.validate terminal ~origin ~run ~receipt:(Terminal.receipt terminal)
    |> protocol_ok;
    assert (not (Execution.is_pending execution));
    execution, admission, terminal
  | (Admitted | Active | Waiting _), _ | Terminal (Completed _), None ->
    failwith "automatic dispatch has not imported its actual terminal reference"
  | Terminal _, _ -> raise_s [%sexp (run : P.Run.t)]
;;

let configured (config : Agent_server.Config.t) session_id =
  let planning =
    Agent_server.Recurrence_automation_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_timer_planning" |> protocol_ok)
      ~controllers:[ session_id ]
      ~expires_at:None
      ~zone_directory:None
    |> protocol_ok
  in
  let grant =
    Agent_server.Recurrence_execution_configuration.Controller.create
      ~session_id
      ~existing_targets:[ session_id ]
      ~captured_children:false
    |> protocol_ok
  in
  let execution =
    Agent_server.Recurrence_execution_configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_485_timer_execution" |> protocol_ok)
      ~controllers:[ grant ]
      ~expires_at:None
    |> protocol_ok
  in
  { config with
    Agent_server.Config.server =
      { config.server with
        recurrence_automation = Some planning
      ; recurrence_execution = Some execution
      }
  }
;;

let bootstrap env root config ~sentinel =
  let policy =
    T.inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      failwith "bootstrap invoked inference")
  in
  with_daemon env root config Execute ~policy (fun _sw daemon ->
    let connection = T.connection daemon (T.principal ()) in
    protect_cleanup
      ~finally:(fun () -> Client.Connection.close connection)
      (fun () ->
         T.initialize connection;
         let created, _ = T.create_session ~start_immediately:true connection in
         let entry = loaded daemon created.id in
         await env (fun () ->
           let current =
             A.Session_actor.with_moderator_checkpoint entry.actor (fun () ->
               A.Session_actor.state entry.actor)
             |> protocol_ok
           in
           healthy current;
           match current.runtime_initialization, current.lifecycle.observed with
           | Ready, Idle when not current.pending_initial_start -> Some ()
           | _ -> None)
         |> ignore;
         let controller =
           P.Session_ref.create
             ~server_id:(S.server_id (Daemon.store daemon))
             ~session_id:created.id
         in
         let period =
           Recurrence_rules.Rule.Period.create_ms 600_000L
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
           |> Result.ok_or_failwith
         in
         let schedule =
           A.Recurrence_definition.Schedule.Interval
             { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
             ; first_due = P.Timestamp.to_time_ns (P.Timestamp.now ())
             }
         in
         create entry.actor ~controller "485_first" schedule |> ignore;
         if sentinel then create entry.actor ~controller "485_sentinel" schedule |> ignore;
         created.id))
;;

let run_page connection session =
  let initialized = Client.Connection.initialization connection |> Option.value_exn in
  let views = Client.Run_views.create connection ~server_id:initialized.server_id in
  Client.Run_views.page
    views
    (P.Run_query.Request.create
       ~session
       ~page:(P.Page.Request.create ~limit:10 () |> protocol_ok)
     |> protocol_ok)
  |> protocol_ok
  |> fun page -> page.P.Page.items
;;

let frontier_diagnostic (current : A.Session_state.t) =
  Option.value_map current.recurrence_catalog ~default:[] ~f:C.entries
  |> List.map ~f:(fun recurrence ->
    let frontier = E.frontier recurrence |> protocol_ok in
    [%sexp
      (E.id recurrence : P.Id.Recurrence.t)
    , (E.control recurrence : E.control)
    , (List.length (H.ready frontier) : int)
    , (List.length (H.executions frontier) : int)
    , (List.length (H.protected_children frontier) : int)
    , (H.issue frontier : H.Issue.t option)
    , (Option.is_some (H.alarm frontier) : bool)])
;;

let daemon_diagnostic daemon session_id =
  let health = Daemon.health daemon ~include_details:true in
  print_s
    [%sexp
      "scheduler health"
    , (List.filter health.components ~f:(fun component ->
         String.is_suffix component.P.Health.Component.name ~suffix:"scheduler")
       : P.Health.Component.t list)];
  match Registry.find (Daemon.registry daemon) session_id with
  | None -> print_s [%sexp "controller not loaded"]
  | Some entry ->
    let current = state entry.actor in
    print_s
      [%sexp
        "controller"
      , (current.lifecycle.desired : P.Session.desired_state)
      , (current.lifecycle.observed : P.Session.observed_state)
      , (current.runtime_initialization : A.Session_state.Runtime_initialization.t)
      , (current.pending_initial_start : bool)
      , (current.halted : bool)
      , (current.failure : P.Error.t option)
      , (Registry.execution_selected (Daemon.registry daemon) entry : bool)
      , (List.length current.jobs : int)
      , (Option.is_some current.active_operation : bool)
      , (frontier_diagnostic current : Sexp.t list)];
    print_s
      [%sexp
        "installed source"
      , (A.Recurrence_source.installed current : (P.Run_source.t, P.Error.t) result)];
    let original = Operator_authorization.trusted_local (T.principal ()) in
    let planning =
      A.Session_actor.read_recurrence_planning
        entry.actor
        ~actor:original
        ~authorize:(fun _ -> Ok ())
      |> Result.map ~f:(fun read -> read.planning)
    in
    print_s
      [%sexp "planning", (planning : (A.Recurrence_planning_status.t, P.Error.t) result)];
    Option.iter (Daemon.recurrence_execution_host daemon) ~f:(fun host ->
      match Agent_server.Recurrence_execution_host.pending_occupancy host with
      | Error error -> print_s [%sexp "occupancy error", (error : P.Error.t)]
      | Ok occupancy ->
        print_s
          [%sexp
            "occupancy"
          , (List.length occupancy.known : int)
          , (List.map occupancy.unknown ~f:(fun row -> row.error) : P.Error.t list)])
;;

let embedded_diagnostic host session_id =
  let connection = Embedded.host_connection host in
  let health =
    Client.Connection.request_without_history
      connection
      (P.Command.Server_health { include_details = true })
  in
  (match health with
   | Ok (Server_health health) ->
     print_s [%sexp "embedded health", (health.components : P.Health.Component.t list)]
   | Ok _ -> failwith "health diagnostic returned another method"
   | Error error -> print_s [%sexp "embedded health error", (error : P.Error.t)]);
  match
    Client.Connection.request
      connection
      (P.Command.Session_get { session_id; history = None })
  with
  | Ok (Session_get snapshot) ->
    let snapshot = P.Public_snapshot.fields snapshot in
    print_s
      [%sexp
        "embedded controller"
      , (snapshot.session.desired_state : P.Session.desired_state)
      , (snapshot.session.observed_state : P.Session.observed_state)
      , (snapshot.halted : bool)
      , (snapshot.failure : P.Error.t option)
      , (List.length snapshot.jobs : int)
      , (Option.is_some snapshot.session.active_operation : bool)]
  | Ok _ -> failwith "session diagnostic returned another method"
  | Error error -> print_s [%sexp "embedded session error", (error : P.Error.t)]
;;

let with_diagnostics env ~stage ~count ~diagnose f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. f with
  | Eio.Time.Timeout as exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    print_s [%sexp "automatic delivery timed out", (!stage : string), (!count : int)];
    (try
       Eio.Cancel.protect (fun () ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. diagnose)
     with
     | secondary ->
       print_s [%sexp "diagnostic unavailable", (Exn.to_string secondary : string)]);
    Exn.raise_with_original_backtrace exn backtrace
;;

let with_fixture ~sentinel f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
      (fun () ->
         let prompt = Filename.concat root "controller.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(env#fs / prompt)
           {|<developer>Real sole-scheduler input delivery.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
         let config = T.config root root prompt in
         let session_id = bootstrap env root config ~sentinel in
         let entered, signal_entered = Eio.Promise.create () in
         let release, signal_release = Eio.Promise.create () in
         let second, signal_second = Eio.Promise.create () in
         let release_second, signal_release_second = Eio.Promise.create () in
         let count = ref 0 in
         let resolve_once promise resolver =
           if Option.is_none (Eio.Promise.peek promise)
           then Eio.Promise.resolve resolver ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs:_ ->
               Int.incr count;
               (match !count with
                | 1 ->
                  Eio.Promise.resolve signal_entered ();
                  Eio.Promise.await release
                | 2 when sentinel ->
                  Eio.Promise.resolve signal_second ();
                  Eio.Promise.await release_second
                | _ -> failwith "unexpected automatic inference replay");
               completed_response ())
         in
         protect_cleanup
           ~finally:(fun () ->
             resolve_once release signal_release;
             resolve_once release_second signal_release_second)
           (fun () ->
              f
                env
                root
                config
                (configured config session_id)
                session_id
                policy
                count
                entered
                (fun () -> resolve_once release signal_release)
                second
                (fun () -> resolve_once release_second signal_release_second))))
;;

let%expect_test "sole daemon timer activates and imports the actual terminal Run" =
  with_fixture
    ~sentinel:false
    (fun env root _base config session_id policy count entered release _ _ ->
       with_daemon env root config Execute ~policy (fun _sw daemon ->
         protect_cleanup ~finally:release (fun () ->
           let stage = ref "first provider" in
           with_diagnostics
             env
             ~stage
             ~count
             ~diagnose:(fun () -> daemon_diagnostic daemon session_id)
             (fun () ->
                Eio.Promise.await entered;
                stage := "terminal controller import";
                let target = loaded daemon session_id in
                let before = state target.actor in
                healthy before;
                let _, origin, receipt, run = admission before "485_first" in
                assert (P.Run.Mode.equal run.mode Single_turn);
                assert (
                  P.Id.Principal.equal
                    receipt.principal_id
                    (P.Id.Principal.of_string "pri_485_timer_execution" |> protocol_ok));
                assert (not (P.Id.Principal.equal receipt.principal_id principal.id));
                assert (
                  Origin.equal
                    origin
                    (Option.value_exn
                       (A.Run_state.recurrence_origin
                          (Option.value_exn before.run_state)
                          run.id)));
                [%test_eq: int] 1 !count;
                release ();
                let final =
                  await env (fun () ->
                    let current = state target.actor in
                    healthy current;
                    let execution = execution current "485_first" in
                    if
                      Option.is_some
                        (Execution.current_attempt execution |> Execution.Attempt.terminal)
                    then Some current
                    else None)
                in
                let execution, exact, _ = terminal final "485_first" in
                assert (P.Run_receipt.equal receipt exact);
                [%test_eq: int] 1 (List.length (Execution.attempts execution));
                [%test_eq: int] 1 !count;
                assert (Option.is_none final.active_operation)))));
  print_endline
    "sole timer: real controller ACK and reciprocal Admission precede inference; exact \
     terminal import settles once";
  [%expect
    {| sole timer: real controller ACK and reciprocal Admission precede inference; exact terminal import settles once |}]
;;

let%expect_test "embedded sole timer imports before a same-target progress sentinel" =
  with_fixture
    ~sentinel:true
    (fun
        env
         root
         base
         config
         session_id
         policy
         count
         entered
         release
         second
         release_second
       ->
       let first_receipt =
         Eio.Switch.run (fun sw ->
           let host =
             Embedded.open_host
               ~sw
               ~env
               ~startup_mode:Execute
               ~config
               ~tool_dir:root
               ~home:root
               ~event_capacity:64
               ~daemon_options:
                 { Daemon.default_options with
                   qualify_chatml_extensions = true
                 ; inference_policy = policy
                 }
               ()
             |> protocol_ok
           in
           protect_cleanup
             ~finally:(fun () ->
               release ();
               release_second ();
               Embedded.close_host host)
             (fun () ->
                let stage = ref "first provider" in
                with_diagnostics
                  env
                  ~stage
                  ~count
                  ~diagnose:(fun () -> embedded_diagnostic host session_id)
                  (fun () ->
                     Eio.Promise.await entered;
                     let connection = Embedded.host_connection host in
                     let initialized =
                       Client.Connection.initialization connection |> Option.value_exn
                     in
                     let session =
                       P.Session_ref.create ~server_id:initialized.server_id ~session_id
                     in
                     let first = run_page connection session |> List.hd_exn in
                     let receipt = Option.value_exn first.admission_receipt in
                     assert (P.Run.Mode.equal first.run.mode Single_turn);
                     [%test_eq: int] 1 !count;
                     release ();
                     (* This second admission is a same-target progress sentinel.
                     The joined immutable read below independently validates
                     the exact first controller terminal import. *)
                     stage := "same-target second provider sentinel";
                     Eio.Promise.await second;
                     let first =
                       run_page connection session
                       |> List.find_exn ~f:(fun view ->
                         P.Id.Run.equal view.P.Run_query.View.run.id receipt.run_id)
                     in
                     (match first.run.lifecycle with
                      | Terminal (Completed _) -> ()
                      | _ -> raise_s [%sexp (first : P.Run_query.View.t)]);
                     assert (
                       P.Run_receipt.equal
                         receipt
                         (Option.value_exn first.admission_receipt));
                     assert (Option.is_some first.terminal_receipt);
                     [%test_eq: int] 2 !count;
                     release_second ();
                     receipt)))
       in
       (* Joined host close releases the same root. An operator-only immutable read
       checks the actual persisted first import without selecting or replaying. *)
       let no_inference =
         T.inference_policy
           ~default_model:"fixture-model"
           ~post_stream:(fun ~sw:_ ~inputs:_ ->
             failwith "factual check replayed inference")
       in
       with_daemon env root base Operator_only ~policy:no_inference (fun _sw daemon ->
         [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded;
         let session =
           P.Session_ref.create ~server_id:(S.server_id (Daemon.store daemon)) ~session_id
         in
         let original = Operator_authorization.trusted_local (T.principal ()) in
         let saved =
           Factory.read_recurrence_state
             (Daemon.factory daemon)
             ~session
             ~actor:original
             ~authorize:(fun summary ->
               if Agent_server.Authorization.session_visible_to (T.principal ()) summary
               then Ok ()
               else
                 Error
                   (P.Error.create
                      Permission_denied
                      ~message:"hidden final import"
                      ~retryable:false
                      ()))
           |> protocol_ok
         in
         let _, receipt, reference = terminal saved "485_first" in
         assert (P.Run_receipt.equal first_receipt receipt);
         assert (P.Id.Run.equal first_receipt.run_id (Terminal.run_id reference));
         [%test_eq: int] 0 (Registry.stats (Daemon.registry daemon)).loaded;
         [%test_eq: int] 2 !count));
  print_endline
    "embedded: same-target admission is a progress sentinel; joined close preserves \
     exact first terminal import";
  [%expect
    {| embedded: same-target admission is a progress sentinel; joined close preserves exact first terminal import |}]
;;
