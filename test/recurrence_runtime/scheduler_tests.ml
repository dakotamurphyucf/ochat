open! Core
open Support
module T = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Client = Agent_client
module Handle = Client.Session_handle

let with_wall_clock (env : Eio_unix.Stdenv.base) clock : Eio_unix.Stdenv.base =
  object
    method fs = env#fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

(* Cache bounded metadata while polling; a failure report must not make another
   actor request or serialize captured tool input/history. *)
let await_callback env actor predicate ~mark =
  let observed = ref (Sexp.Atom "not observed") in
  let take values = List.take (List.rev values) 12 in
  let error (error : P.Invocation.tool_error) =
    String.prefix error.code 128, String.prefix error.message 256
  in
  let summary (state : A.Session_state.t) =
    let schedules =
      take state.schedules
      |> List.map ~f:(fun schedule ->
        let status =
          match schedule.P.Schedule.status with
          | Scheduled -> "scheduled", None
          | Delivering -> "delivering", None
          | Delivered -> "delivered", None
          | Cancelled -> "cancelled", None
          | Failed failure -> "failed", Some (String.prefix failure.message 256)
        in
        schedule.id, status, schedule.delivery_count)
    in
    let executions =
      take state.moderator_executions
      |> List.map ~f:(fun receipt ->
        let status =
          match receipt.P.Moderator_execution.status with
          | Running -> "running", None
          | Completed _ -> "completed", None
          | Failed failure -> "failed", Some (error failure)
          | Interrupted reason ->
            "interrupted", Some ("interrupted", String.prefix reason 256)
        in
        receipt.context.phase, status)
    in
    let permissions =
      take state.permissions
      |> List.map ~f:(fun permission ->
        ( permission.P.Permission.owner
        , String.prefix permission.tool_name 128
        , permission.state ))
    in
    let invocations =
      take state.invocations
      |> List.map ~f:(fun invocation ->
        let status =
          match invocation.P.Invocation.status with
          | Admitted -> "admitted", None
          | Dispatching -> "dispatching", None
          | Resolved outcome | Published outcome ->
            (match outcome with
             | Complete _ -> "complete", None
             | Pending _ -> "pending", None
             | Fail failure -> "failed", Some (error failure)
             | Cancelled reason ->
               "cancelled", Some ("cancelled", String.prefix reason 256))
        in
        invocation.context.id, String.prefix invocation.context.tool_name 128, status)
    in
    [%message
      "legacy callback wait"
        ~generation:(state.identity.generation : int)
        ~desired:(state.lifecycle.desired : P.Session.desired_state)
        ~observed:(T.Failure_diagnostic.observed_status state.lifecycle.observed : string)
        (schedules : (P.Id.Schedule.t * (string * string option) * int) list)
        (executions
         : (P.Moderator_execution.phase * (string * (string * string) option)) list)
        (permissions : (P.Permission.owner * string * P.Permission.state) list)
        (invocations
         : (P.Id.Invocation.t * string * (string * (string * string) option)) list)]
  in
  mark "callback.await_permission";
  match
    Daemon_tests.await env actor (fun state ->
      observed := summary state;
      predicate state)
  with
  | state ->
    mark "callback.permission_observed";
    state
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.traceln "%s" (Sexp.to_string_hum !observed) with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let%expect_test
    "sole daemon scheduler advances recurrence while a real legacy callback awaits \
     permission"
  =
  Eio_main.run (fun real_env ->
    Mirage_crypto_rng_unix.use_default ();
    let initial_wall = Eio.Time.now (Eio.Stdenv.clock real_env) in
    let clock, pause, resume, advance =
      T.controlled_wall_clock (Eio.Stdenv.clock real_env) ~initial:initial_wall
    in
    pause ();
    let env = with_wall_clock real_env clock in
    let root = T.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        resume ();
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
          "permission required";
        let prompt = Filename.concat root "controller.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt)
          {|<developer>Offline timer concurrency.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* timer = Schedule.after_ms_with_policy(0, `Null, `Deliver_once_immediately) in
  Task.pure(state)
| `Internal_event(payload) ->
  let* result = Tool.call("read_file", `Object([{key = "root"; value = `String("data")}; {key = "file"; value = `String("value.txt")}])) in
  Task.pure(state)
| _ -> Task.pure(state)
</script>|};
        let configuration =
          T.config
            ~profile:{ T.permission_profile with tool_default = Ask }
            root
            workspace
            prompt
        in
        let diagnostic = Daemon_tests.phase_diagnostic env root in
        let with_daemon configuration prefix f =
          Daemon_tests.with_daemon
            env
            root
            configuration
            Execute
            ~mark:(fun phase -> diagnostic (prefix ^ "." ^ phase))
            (fun sw daemon ->
               (* Release logical waits before joining the live daemon on every
                 exit, including a failed oracle. The real timeout is unchanged. *)
               Exn.protect ~finally:resume ~f:(fun () -> f sw daemon))
        in
        let session_id =
          with_daemon configuration "legacy.initial" (fun _sw daemon ->
            let client = T.connection daemon (T.principal ()) in
            T.initialize client;
            let created, _ = T.create_session client in
            let actor = (Daemon_tests.loaded daemon created.id).actor in
            let current = state actor in
            assert (P.Session.equal_desired_state current.lifecycle.desired Stopped);
            assert (List.is_empty current.schedules);
            assert (List.is_empty current.moderator_executions);
            Client.Connection.close client;
            created.id)
        in
        pause ();
        let recurrence_automation =
          Configuration.create
            ~principal_id:(P.Id.Principal.of_string "pri_live_planner" |> protocol_ok)
            ~controllers:[ session_id ]
            ~expires_at:None
            ~zone_directory:None
          |> protocol_ok
        in
        let configuration =
          { configuration with
            server =
              { configuration.server with
                recurrence_automation = Some recurrence_automation
              }
          }
        in
        with_daemon configuration "legacy.recovered" (fun sw daemon ->
          let mark phase = diagnostic ("legacy.recovered." ^ phase) in
          let client = T.connection daemon (T.principal ()) in
          T.initialize client;
          (* Ask requires an actual current responder. Create the timer only
             after this configured host has authenticated its live attachment. *)
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
          let actor = (Daemon_tests.loaded daemon session_id).actor in
          mark "controller.start";
          Handle.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          let pending state =
            List.exists state.A.Session_state.permissions ~f:(fun permission ->
              P.Permission.equal_state permission.state Pending)
          in
          let busy = await_callback real_env actor pending ~mark in
          let callback_busy state =
            pending state
            && List.exists state.A.Session_state.schedules ~f:(fun schedule ->
              match schedule.P.Schedule.status with
              | Delivered -> Int.equal schedule.delivery_count 1
              | Scheduled | Delivering | Cancelled | Failed _ -> false)
            && List.exists state.moderator_executions ~f:(fun receipt ->
              P.Moderator_execution.equal_phase receipt.context.phase Internal_event
              &&
              match receipt.status with
              | Running -> true
              | Completed _ | Failed _ | Interrupted _ -> false)
          in
          (* One-shot Delivered acknowledges enqueue, not callback completion. *)
          assert (callback_busy busy);
          let controller =
            P.Session_ref.create
              ~server_id:(S.server_id (Daemon.store daemon))
              ~session_id
          in
          let due =
            Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Eio.Time.now clock))
          in
          create actor ~controller "while-busy" (interval ~first_due:due ()) |> ignore;
          let progressed =
            Daemon_tests.await real_env actor (fun _ ->
              Int64.equal (consumed actor "while-busy") 1L)
          in
          assert (callback_busy progressed);
          resume ();
          Handle.stop handle ~mode:Cancel |> protocol_ok |> ignore;
          assert (
            E.equal_control
              (E.control (entry actor "while-busy"))
              (Paused Controller_stop));
          Handle.close handle;
          Client.Connection.close client)));
  print_endline
    "real legacy permission stayed pending while the same daemon timer committed a due \
     occurrence; Stop joined callback and sweep owners";
  [%expect
    {| real legacy permission stayed pending while the same daemon timer committed a due occurrence; Stop joined callback and sweep owners |}]
;;
