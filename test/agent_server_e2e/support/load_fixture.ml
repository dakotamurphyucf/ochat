open Core
module F = Background_fixture
module C = Config_fixture
module D = Daemon_process

let integer name default =
  match Sys.getenv name with
  | None -> default
  | Some value ->
    let value = Int.of_string value in
    F.require (value > 0) (name ^ " must be positive");
    value
;;

let prompt =
  {|<developer>Isolated load fixture; never call a provider.</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Wake ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun ctx state event ->
  match event with | `Wake -> Task.pure(state + 1) | _ -> Task.pure(state)
</script>|}
;;

let configure env temporary =
  let fixture = C.create temporary ~name:"load" ~http_port:(F.reserve_port env) in
  F.save fixture (C.prompt_path fixture) prompt;
  let config = C.configuration fixture () in
  let config =
    List.fold
      [ "(max_root_agents 2)", "(max_root_agents 128)"
      ; "(max_connections 32)", "(max_connections 2048)"
      ; "(max_attachments_per_session 16)", "(max_attachments_per_session 64)"
      ; "(idle_connection_timeout_ms 5000)", "(idle_connection_timeout_ms 120000)"
      ; "(max_events_per_session 1000)", "(max_events_per_session 64)"
      ]
      ~init:config
      ~f:(fun text (pattern, with_) -> String.substr_replace_all text ~pattern ~with_)
  in
  F.save fixture (C.config_path fixture) config;
  fixture
;;

let request client command = F.request client command
let now env = Eio.Time.now (Eio.Stdenv.clock env)

let wait env description f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
    let rec loop () =
      if f ()
      then ()
      else (
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
        loop ())
    in
    try loop () with
    | exn -> Exn.reraise exn description)
;;

let attach client summary key = fst (F.attach client summary key)

let stop client session key =
  ignore
    (request
       client
       (Session_stop
          { session_id = session.F.summary.id
          ; attachment_id = session.attachment_id
          ; mode = Graceful
          ; idempotency_key = F.key key
          })
     : Agent_protocol.Method_result.t)
;;

let detach client session key =
  ignore
    (request
       client
       (Session_detach
          { session_id = session.F.summary.id
          ; attachment_id = session.attachment_id
          ; idempotency_key = F.key key
          })
     : Agent_protocol.Method_result.t)
;;

let health client =
  match request client (Server_health { include_details = true }) with
  | Server_health value ->
    F.require value.ready "daemon is not ready";
    value
  | _ -> failwith "unexpected health response"
;;

let loaded client =
  let value = health client in
  let component =
    List.find_exn value.components ~f:(fun c -> String.equal c.name "session_registry")
  in
  String.split (Option.value_exn component.message) ~on:' '
  |> List.hd_exn
  |> Int.of_string
;;

let await_schedules env client session count =
  wait env "schedule delivery" (fun () ->
    let s = F.snapshot client session in
    F.require (Option.is_none s.failure) "moderator failed";
    List.length s.schedules = count
    && List.for_all s.schedules ~f:(fun x ->
      Poly.equal x.status Delivered && x.delivery_count = 1))
;;

let with_daemon env f =
  Temporary_environment.with_ ~scenario:"load" ~env (fun temporary ->
    let fixture = configure env temporary in
    Eio.Switch.run (fun sw ->
      let daemon = F.start ~sw env fixture 1 in
      Exn.protect
        ~f:(fun () ->
          F.with_client ~sw env fixture (fun client -> f sw fixture daemon client))
        ~finally:(fun () -> F.stop env daemon)))
;;
