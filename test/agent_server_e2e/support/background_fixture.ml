open Core

type session =
  { summary : Agent_protocol.Session.t
  ; attachment_id : Agent_protocol.Id.Attachment.t
  }

let require condition message =
  if not condition then raise_s [%sexp "background E2E assertion", (message : string)]
;;

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "background RPC failed", (error : Agent_protocol.Error.t)]
;;

let result_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "background HTTP failed", (error : string)]
;;

let key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok
let request client command = (Http_driver.request client command |> protocol_ok).result
let page () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let save fixture path contents =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path (Config_fixture.environment fixture) path)
    contents
;;

let with_client ~sw env fixture f =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some (Config_fixture.admin_token fixture))
    |> result_ok
  in
  Exn.protect
    ~f:(fun () ->
      ignore (Http_driver.initialize client |> protocol_ok : _);
      f client)
    ~finally:(fun () -> Http_driver.shutdown client)
;;

let close_client client =
  ignore (Http_driver.close_connection client |> result_ok : Http_driver.response)
;;

let catalog client =
  let prompt =
    match
      request
        client
        (Prompt_list { page = page (); enabled = Some true; available = Some true })
    with
    | Prompt_list page -> List.hd_exn page.items
    | _ -> failwith "unexpected prompt result"
  in
  let workspace =
    match
      request
        client
        (Workspace_list
           { page = page (); kind = None; access = None; available = Some true })
    with
    | Workspace_list page ->
      List.find_exn page.items ~f:(fun workspace ->
        String.equal workspace.name "physical")
    | _ -> failwith "unexpected workspace result"
  in
  prompt, workspace
;;

let create client name =
  let prompt, workspace = catalog client in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately:true
      ~labels:[ "suite", "background" ]
      ()
    |> protocol_ok
  in
  match
    request
      client
      (Session_create
         { spec
         ; requested_mode = Some Read_write
         ; subscribe = false
         ; idempotency_key = key name
         })
  with
  | Session_create { session; attachment = Some attachment; _ } ->
    { summary = session; attachment_id = attachment.attachment.id }
  | _ -> failwith "unexpected create result"
;;

let attach_after client summary name after_sequence =
  match
    request
      client
      (Session_attach
         { session_id = summary.Agent_protocol.Session.id
         ; requested_mode = Read_write
         ; subscribe = false
         ; after_sequence
         ; reclaim_token = None
         ; idempotency_key = key name
         })
  with
  | Session_attach attached ->
    { summary; attachment_id = attached.attachment.id }, attached.replay
  | _ -> failwith "unexpected attach result"
;;

let attach client summary name = attach_after client summary name (Some 0L)

let snapshot client session =
  match
    request client (Session_get { session_id = session.summary.id; history = None })
  with
  | Session_get snapshot -> snapshot
  | _ -> failwith "unexpected snapshot result"
;;

let await env description predicate =
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Time.with_timeout_exn clock 10. (fun () ->
      let rec loop () =
        match predicate () with
        | Some value -> value
        | None ->
          Eio.Time.sleep clock 0.02;
          loop ()
      in
      loop ())
  with
  | Eio.Time.Timeout ->
    raise_s [%sexp "background wait timed out", (description : string)]
;;

let await_snapshot env client session description predicate =
  await env description (fun () ->
    let state = snapshot client session in
    Option.iter state.failure ~f:(fun error ->
      raise_s [%sexp "background session failed", (error : Agent_protocol.Error.t)]);
    if predicate state then Some state else None)
;;

let start ~sw env fixture port =
  let daemon =
    Daemon_process.start_with_environment_overrides
      ~sw
      ~env
      ~fixture
      ~environment_overrides:
        [ "API_URL", sprintf "http://127.0.0.1:%d" port
        ; "OPENAI_API_KEY", "background-local-only"
        ]
      ~config_path:(Config_fixture.config_path fixture)
  in
  (match Daemon_process.wait_ready daemon ~env ~timeout_seconds:10. with
   | Ok _ -> ()
   | Error error ->
     raise_s
       [%sexp
         "background daemon readiness"
       , (error : Daemon_process.readiness_error)
       , ((Daemon_process.stderr daemon).contents : string)]);
  daemon
;;

let stop env daemon =
  if Option.is_none (Daemon_process.result daemon)
  then
    ignore
      (Daemon_process.stop daemon ~env ~grace_seconds:3. : Process_manager.termination)
;;

let schedule_with_policy client session name event delay misfire =
  let payload = Chatml.Chatml_value_codec.Snapshot.(to_jsonaf (Variant (event, []))) in
  match
    request
      client
      (Schedule_create
         { session_id = session.summary.id
         ; attachment_id = session.attachment_id
         ; payload
         ; due = After_ms delay
         ; misfire
         ; idempotency_key = key name
         })
  with
  | Schedule_create result -> result.schedule
  | _ -> failwith "unexpected schedule result"
;;

let schedule client session name event delay =
  schedule_with_policy client session name event delay Deliver_once_immediately
;;

let events_since client session name sequence =
  let _, replay = attach_after client session.summary name (Some sequence) in
  match replay with
  | Events events -> events
  | Current | Snapshot _ -> failwith "background replay did not return retained events"
;;

let events client session name = events_since client session name 0L

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "background checkpoint read", (error : Agent_store.Store_error.t)]
;;

let journal_tail env directory state =
  let open Result.Let_syntax in
  let%bind journal =
    Agent_store.Journal.open_existing
      ~env
      ~directory
      ~max_payload_length:(16 * 1024 * 1024)
      ~max_segment_bytes:67_108_864L
      ~max_segment_frames:10_000
  in
  let%bind scan = Agent_store.Journal.scan journal in
  List.fold scan.entries ~init:(Ok state) ~f:(fun state entry ->
    let%bind state = state in
    if Agent_store.Frame.flags entry.frame <> 0
    then Ok state
    else (
      let%bind transaction =
        Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
      in
      if
        Int64.(
          transaction.transaction_sequence
          <= state.Agent_session.Session_state.counters.transaction_sequence)
      then Ok state
      else Agent_session.Session_persistence.apply_transaction state transaction))
;;

let checkpoint env fixture session =
  let directory =
    Filename.concat
      (Config_fixture.data_dir fixture)
      ("sessions/" ^ Agent_protocol.Id.Session.to_string session.summary.id ^ "/snapshot")
  in
  match
    Agent_store.Snapshot.load_current
      ~env
      ~directory
      ~max_payload_length:(64 * 1024 * 1024)
  with
  | Error (Agent_store.Store_error.Missing _) -> None
  | Error error ->
    raise_s [%sexp "background checkpoint read", (error : Agent_store.Store_error.t)]
  | Ok None -> None
  | Ok (Some installed) ->
    (match
       Agent_session.Session_persistence.restore_snapshot installed.snapshot.payload
     with
     | Ok state ->
       (match
          journal_tail env (Filename.concat (Filename.dirname directory) "journal") state
        with
        | Error (Agent_store.Store_error.Missing _) -> None
        | result -> Some (store_ok result))
     | Error error ->
       raise_s [%sexp "background checkpoint decode", (error : Agent_store.Store_error.t)])
;;

let moderator_state state =
  let json = Option.value_exn state.Agent_session.Session_state.moderator in
  let encoded =
    match Jsonaf.member "identity_snapshot_sexp" json with
    | Some (`String encoded) -> encoded
    | _ -> failwith "moderator checkpoint has no identity snapshot"
  in
  let snapshot =
    Session.Moderator_state.Identity_snapshot.t_of_sexp (Sexp.of_string encoded)
  in
  match snapshot.current_state with
  | String state -> state
  | Int state -> Int.to_string state
  | _ -> failwith "moderator checkpoint has nonscalar state"
;;
