open! Core
open Support
module T = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Client = Agent_client
module Handle = Client.Session_handle

let start env sw root configuration mode =
  Daemon.start
    ~sw
    ~env
    ~config:configuration
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:
      { Daemon.default_options with
        startup_mode = mode
      ; qualify_chatml_extensions = true
      ; inference_policy =
          T.inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "planner activated provider")
      }
    ()
  |> protocol_ok
;;

let loaded daemon session_id =
  Agent_server.Session_registry.find (Daemon.registry daemon) session_id
  |> Option.value_exn
;;

let await env actor predicate =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec loop () =
      let current = state actor in
      if predicate current
      then current
      else (
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        loop ())
    in
    loop ())
;;

(* At most 16 bounded phase records survive in one fixture-owned file. This is
   diagnostic custody only: failed writes must not replace the test exception. *)
let phase_diagnostic env root =
  let recent = ref [] in
  fun phase ->
    recent := List.take (String.prefix phase 512 :: !recent) 16;
    try
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / root / "recurrence-runtime-phase.txt")
        (String.concat ~sep:"\n" (List.rev !recent) ^ "\n")
    with
    | _ -> ()
;;

(* Teardown runs while the switch and its actors are still alive. If an oracle
   raises, preserve that exception and backtrace even when cleanup also fails. *)
let with_daemon env root configuration mode ~mark f =
  Eio.Switch.run (fun sw ->
    mark "start.begin";
    let daemon = start env sw root configuration mode in
    mark "start.returned";
    let shutdown () =
      Eio.Cancel.protect (fun () ->
        mark "shutdown.begin";
        Daemon.shutdown daemon |> protocol_ok;
        mark "shutdown.returned")
    in
    match f sw daemon with
    | value ->
      shutdown ();
      value
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      mark ("primary: " ^ Exn.to_string exn);
      (try shutdown () with
       | cleanup -> mark ("cleanup: " ^ Exn.to_string cleanup));
      Exn.raise_with_original_backtrace exn backtrace)
;;

let await_completed env actor phase ~previous ~mark =
  mark "moderator.await_terminal";
  let current =
    await env actor (fun state ->
      List.exists state.moderator_executions ~f:(fun receipt ->
        P.Moderator_execution.equal_phase receipt.context.phase phase
        && (not
              (List.mem previous receipt.context.id ~equal:P.Id.Moderator_execution.equal))
        &&
        match receipt.status with
        | Completed _ | Failed _ | Interrupted _ -> true
        | Running -> false))
  in
  let receipt =
    List.find_exn current.moderator_executions ~f:(fun receipt ->
      P.Moderator_execution.equal_phase receipt.context.phase phase
      && (not
            (List.mem previous receipt.context.id ~equal:P.Id.Moderator_execution.equal))
      &&
      match receipt.status with
      | Completed _ | Failed _ | Interrupted _ -> true
      | Running -> false)
  in
  (match receipt.status with
   | Completed _ -> ()
   | Failed _ | Interrupted _ | Running ->
     raise_s [%sexp (receipt : P.Moderator_execution.t)]);
  mark "moderator.completed";
  List.filter_map current.moderator_executions ~f:(fun receipt ->
    if P.Moderator_execution.equal_phase receipt.context.phase phase
    then Some receipt.context.id
    else None)
;;

type policy =
  | Absent
  | Revoked
  | Expired
  | Granted
[@@deriving sexp_of]

let%expect_test
    "configured daemon plans after real recovery; stored creator and selection never \
     restore a grant"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt = Filename.concat root "controller.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          {|<developer>Offline recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
        let configuration = T.config root workspace prompt in
        let diagnostic = phase_diagnostic env root in
        let session_id, zone_directory =
          with_daemon
            env
            root
            configuration
            Execute
            ~mark:(fun phase -> diagnostic ("initial." ^ phase))
            (fun sw daemon ->
               let mark phase = diagnostic ("initial." ^ phase) in
               mark "client.initialize";
               let client = T.connection daemon (T.principal ()) in
               T.initialize client;
               let created, _ = T.create_session client in
               let handle =
                 Handle.attach
                   ~sw
                   ~clock:(Eio.Stdenv.clock env)
                   ~connection:client
                   ~session_id:created.id
                   ~mode:Read_write
                   ()
                 |> protocol_ok
               in
               mark "session.start";
               Handle.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
               let actor = (loaded daemon created.id).actor in
               await_completed env actor Session_start ~previous:[] ~mark |> ignore;
               await env actor (fun state ->
                 Option.is_some state.moderator
                 && Option.is_none state.active_operation
                 && not state.pending_initial_start)
               |> ignore;
               let controller =
                 P.Session_ref.create
                   ~server_id:(S.server_id (Daemon.store daemon))
                   ~session_id:created.id
               in
               let first_due =
                 P.Timestamp.add_ms (P.Timestamp.now ()) 86_400_000
                 |> protocol_ok
                 |> P.Timestamp.to_time_ns
               in
               mark "recurrence.create";
               create actor ~controller "daemon" (interval ~first_due ()) |> ignore;
               mark "recurrence.created";
               let directory =
                 Filename.concat
                   (state actor).spec.workspace_instance.canonical_root.native_path
                   "zones"
               in
               Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / directory);
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / directory / "UTC")
                 utc_bytes;
               let now = P.Timestamp.now () |> P.Timestamp.to_time_ns in
               let zone_directory =
                 Configuration.Zone_directory.create
                   ~directory
                   ~source:"pinned-2025b"
                   ~from:(Time_ns.sub now (Time_ns.Span.of_int_sec 86_400))
                   ~until:(Time_ns.add now (Time_ns.Span.of_int_sec 259_200))
                 |> protocol_ok
               in
               Handle.close handle;
               Client.Connection.close client;
               assert (E.equal_control (E.control (entry actor "daemon")) Enabled);
               created.id, zone_directory)
        in
        let configured policy =
          let recurrence_automation =
            match policy with
            | Absent -> None
            | Revoked | Expired | Granted ->
              Some
                (Configuration.create
                 (* Same as the stored creator: it still needs an actual fresh grant. *)
                   ~principal_id:(T.principal ()).id
                   ~controllers:
                     [ (match policy with
                        | Revoked ->
                          P.Id.Session.of_string "ses_not_controller" |> protocol_ok
                        | Absent | Expired | Granted -> session_id)
                     ]
                   ~expires_at:
                     (match policy with
                      | Expired ->
                        Some (P.Timestamp.of_string "2000-01-01T00:00:00Z" |> protocol_ok)
                      | Absent | Revoked | Granted -> None)
                   ~zone_directory:(Some zone_directory)
                 |> protocol_ok)
          in
          { configuration with
            server = { configuration.server with recurrence_automation }
          }
        in
        let previous_resume_ids = ref [] in
        List.iter [ Absent; Revoked; Expired; Granted ] ~f:(fun policy ->
          let mark phase =
            diagnostic (Sexp.to_string (sexp_of_policy policy) ^ "." ^ phase)
          in
          with_daemon env root (configured policy) Execute ~mark (fun _sw daemon ->
            mark "registry.loaded";
            let actor = (loaded daemon session_id).actor in
            previous_resume_ids
            := await_completed
                 env
                 actor
                 Session_resume
                 ~previous:!previous_resume_ids
                 ~mark;
            let read () =
              mark "planning.read";
              let result =
                A.Session_actor.read_recurrence_planning actor ~actor:proof ~authorize
                |> protocol_ok
              in
              mark "planning.read_returned";
              result
            in
            (match policy with
             | Granted ->
               mark "interval.await_alarm";
               await env actor (fun _ ->
                 Option.is_some (H.alarm (frontier actor "daemon")))
               |> ignore;
               assert (A.Recurrence_planning_status.equal (read ()).planning Available);
               assert (
                 Host.Zone_status.equal (Daemon.recurrence_zone_status daemon) Available);
               let today =
                 P.Timestamp.now ()
                 |> P.Timestamp.to_string
                 |> fun text -> Date.of_string (String.prefix text 10)
               in
               let rule =
                 Recurrence_rules.Rule.Calendar.create
                   ~pattern:Daily
                   ~ofday:(Time_ns.Ofday.of_string "23:59:59")
                   ~zone:"UTC"
                   ~start_date:(Date.add_days today 1)
                   ()
                 |> Result.map_error ~f:(fun e ->
                   Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t e))
                 |> Result.ok_or_failwith
               in
               let controller =
                 P.Session_ref.create
                   ~server_id:(S.server_id (Daemon.store daemon))
                   ~session_id
               in
               mark "calendar.create";
               create actor ~controller "calendar" (Calendar rule) |> ignore;
               mark "calendar.await_alarm";
               await env actor (fun _ ->
                 Option.is_some (H.alarm (frontier actor "calendar")))
               |> ignore;
               let nominal =
                 H.lookahead (frontier actor "calendar") |> Option.value_exn
               in
               (match A.Recurrence_nominal.label nominal with
                | Calendar label ->
                  assert (
                    String.equal
                      (Recurrence_rules.Zone.Provenance.source label.provenance)
                      "pinned-2025b");
                  assert (
                    String.equal
                      (Recurrence_rules.Zone.Provenance.sha256 label.provenance)
                      "fddce1e648a1732ac29afd9a16151b2973cdf082e7ec0c690f7e42be6b598b93")
                | Interval -> failwith "calendar provenance was lost")
             | Absent | Revoked | Expired ->
               let expected =
                 match policy with
                 | Absent -> A.Recurrence_planning_status.Reason.Not_configured
                 | Revoked -> Not_granted
                 | Expired -> Expired
                 | Granted -> assert false
               in
               assert (
                 A.Recurrence_planning_status.equal
                   (read ()).planning
                   (Unavailable expected));
               let before = (state actor).counters.transaction_sequence in
               mark "unavailable.sweep";
               sweep actor;
               mark "unavailable.sweep_returned";
               assert (Int64.equal before (state actor).counters.transaction_sequence);
               assert (Option.is_none (H.alarm (frontier actor "daemon"))));
            assert (
              Int.equal
                (List.length (read ()).entries)
                (match policy with
                 | Granted -> 2
                 | Absent | Revoked | Expired -> 1));
            assert (E.equal_control (E.control (entry actor "daemon")) Enabled);
            print_s
              [%sexp
                (policy : policy)
              , (Option.is_some (H.alarm (frontier actor "daemon")) : bool)];
            mark "oracle.completed"));
        List.iter [ Daemon.On_demand; Operator_only ] ~f:(fun mode ->
          let mark phase =
            diagnostic
              ((match mode with
                | Daemon.On_demand -> "On_demand"
                | Operator_only -> "Operator_only"
                | Execute -> assert false)
               ^ "."
               ^ phase)
          in
          with_daemon env root (configured Granted) mode ~mark (fun sw daemon ->
            mark "registry.empty";
            assert (
              Int.equal
                (Agent_server.Session_registry.stats (Daemon.registry daemon)).loaded
                0);
            let client = T.connection daemon (T.principal ()) in
            T.initialize client;
            (* Catalog reads are nonactivating even on an operator-only host;
               session.get is deliberately excluded by that host's command gate. *)
            let query =
              P.Session.List_request.
                { page = P.Page.Request.create ~limit:10 () |> protocol_ok
                ; desired_state = None
                ; prompt_id = None
                ; workspace_id = None
                ; owner_principal_id = None
                ; organization = P.Session_organization.Query.default
                ; labels = []
                ; sort = P.Session_catalog_query.Sort.default
                ; archive = Active
                ; creator_principal_id = None
                ; active_owner_principal_id = None
                }
            in
            let catalog = Client.Admin.list_sessions_page client query |> protocol_ok in
            let retained =
              List.find_exn catalog.items ~f:(fun row ->
                P.Id.Session.equal row.P.Session_catalog.session.id session_id)
            in
            assert (
              Int.equal
                (Agent_server.Session_registry.stats (Daemon.registry daemon)).loaded
                0);
            let expected =
              P.Session_lifecycle.Expected.create
                ~reference:
                  (P.Session_ref.create
                     ~server_id:(S.server_id (Daemon.store daemon))
                     ~session_id)
                ~generation:retained.session.generation
                ~session_revision:retained.session.revision
                ~lifecycle_revision:retained.lifecycle_revision
              |> protocol_ok
            in
            (match mode with
             | On_demand ->
               mark "selection.begin";
               let selected =
                 Daemon.select_session daemon ~principal:(T.principal ()) ~expected
                 |> protocol_ok
               in
               mark "selection.returned";
               previous_resume_ids
               := await_completed
                    env
                    selected.actor
                    Session_resume
                    ~previous:!previous_resume_ids
                    ~mark;
               let read =
                 A.Session_actor.read_recurrence_planning
                   selected.actor
                   ~actor:proof
                   ~authorize
                 |> protocol_ok
               in
               assert (A.Recurrence_planning_status.equal read.planning Available);
               (* Stop remains a real controller control, independent of planner config. *)
               let handle =
                 Handle.attach
                   ~sw
                   ~clock:(Eio.Stdenv.clock env)
                   ~connection:client
                   ~session_id
                   ~mode:Read_write
                   ()
                 |> protocol_ok
               in
               mark "controller.stop";
               Handle.stop handle ~mode:Cancel |> protocol_ok |> ignore;
               mark "controller.stopped";
               assert (
                 E.equal_control
                   (E.control (entry selected.actor "daemon"))
                   (Paused Controller_stop));
               Handle.close handle
             | Operator_only ->
               (match Client.Admin.get_session client session_id with
                | Error error -> assert (P.Error.equal_code error.code Invalid_state)
                | Ok _ -> failwith "operator-only host admitted session.get");
               (match
                  Daemon.select_session daemon ~principal:(T.principal ()) ~expected
                with
                | Error error -> assert (P.Error.equal_code error.code Invalid_state)
                | Ok _ -> failwith "operator-only host admitted execution selection");
               assert (
                 Int.equal
                   (Agent_server.Session_registry.stats (Daemon.registry daemon)).loaded
                   0)
             | Execute -> assert false);
            Client.Connection.close client;
            mark "oracle.completed"))));
  [%expect
    {|
    (Absent false)
    (Revoked false)
    (Expired false)
    (Granted true)
    |}]
;;
