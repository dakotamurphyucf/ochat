open! Core

let invalid message = Agent_protocol.Error.invalid_request message

exception End_of_input

type dispatch_slots =
  { mutex : Eio.Mutex.t
  ; mutable ordinary_busy : bool
  ; mutable control_busy : bool
  }

let create_slots () =
  { mutex = Eio.Mutex.create (); ordinary_busy = false; control_busy = false }
;;

let reserve slots ~control =
  Eio.Mutex.use_rw ~protect:true slots.mutex (fun () ->
    if control
    then
      if slots.control_busy
      then false
      else (
        slots.control_busy <- true;
        true)
    else if slots.ordinary_busy
    then false
    else (
      slots.ordinary_busy <- true;
      true))
;;

let release slots ~control =
  Eio.Mutex.use_rw ~protect:true slots.mutex (fun () ->
    if control then slots.control_busy <- false else slots.ordinary_busy <- false)
;;

let parse_line line =
  match Result.try_with (fun () -> Jsonaf.of_string line) with
  | Error exn -> Error (invalid ("invalid JSON envelope: " ^ Exn.to_string exn))
  | Ok json -> Agent_protocol.Envelope.of_json json
;;

let request_id_of_line line =
  match Result.try_with (fun () -> Jsonaf.of_string line) with
  | Error _ -> None
  | Ok (`Object fields) ->
    List.Assoc.find fields "id" ~equal:String.equal
    |> Option.bind ~f:(fun value ->
      Agent_protocol.Envelope.Request_id.of_json value |> Result.ok)
  | Ok _ -> None
;;

let write_envelope output envelope =
  envelope
  |> Agent_protocol.Envelope.to_json
  |> Jsonaf.to_string
  |> fun line -> Eio.Flow.copy_string (line ^ "\n") output
;;

let writer output outgoing =
  let rec loop () =
    match Agent_session.Mailbox.pop outgoing with
    | None -> ()
    | Some envelope ->
      write_envelope output envelope;
      loop ()
  in
  loop ()
;;

let publish outgoing envelope =
  Agent_session.Mailbox.try_push outgoing ~priority:Normal envelope
;;

let report_parse_failure outgoing on_error line failure =
  on_error failure;
  Option.iter (request_id_of_line line) ~f:(fun id ->
    ignore (publish outgoing (Agent_protocol.Envelope.failure ~id failure) : bool))
;;

let dispatch ~worker_sw dispatcher context outgoing on_error envelope =
  match Agent_server.Dispatcher.dispatch_envelope dispatcher ~context envelope with
  | Ok None -> ()
  | Ok (Some response) ->
    if not (publish outgoing response)
    then (
      on_error (invalid "connection outgoing queue is full");
      Eio.Switch.fail worker_sw (Failure "connection outgoing queue is full"))
  | Error failure -> on_error failure
;;

let busy_failure ~worker_sw outgoing on_error envelope =
  let failure =
    Agent_protocol.Error.create
      Resource_limit
      ~message:"connection dispatch lane is busy"
      ~retryable:true
      ()
  in
  match envelope with
  | Agent_protocol.Envelope.Request request ->
    if not (publish outgoing (Agent_protocol.Envelope.failure ~id:request.id failure))
    then (
      on_error (invalid "connection outgoing queue is full");
      Eio.Switch.fail worker_sw (Failure "connection outgoing queue is full"))
  | Notification _ | Response _ -> on_error failure
;;

let dispatch_admitted ~worker_sw ~slots ~dispatcher ~context ~outgoing ~on_error envelope =
  let open Agent_server.Dispatcher in
  let schedule = Agent_server.Dispatcher.dispatch_schedule dispatcher envelope in
  let control =
    match schedule with
    | Repl_cancel -> true
    | Immediate | Repl_wait -> false
  in
  if not (reserve slots ~control)
  then busy_failure ~worker_sw outgoing on_error envelope
  else if
    match schedule with
    | Immediate -> true
    | Repl_wait | Repl_cancel -> false
  then
    Exn.protect
      ~f:(fun () -> dispatch ~worker_sw dispatcher context outgoing on_error envelope)
      ~finally:(fun () -> release slots ~control)
  else (
    match
      Eio.Fiber.fork ~sw:worker_sw (fun () ->
        Exn.protect
          ~f:(fun () -> dispatch ~worker_sw dispatcher context outgoing on_error envelope)
          ~finally:(fun () -> release slots ~control))
    with
    | () -> ()
    | exception exn ->
      release slots ~control;
      raise exn)
;;

let reader
      ~worker_sw
      ~slots
      ~dispatcher
      ~context
      ~input
      ~max_line_length
      ~outgoing
      ~on_error
  =
  let buffer = Eio.Buf_read.of_flow input ~max_size:max_line_length in
  let rec loop () =
    match Eio.Buf_read.line buffer with
    | line ->
      (match parse_line line with
       | Ok envelope ->
         dispatch_admitted
           ~worker_sw
           ~slots
           ~dispatcher
           ~context
           ~outgoing
           ~on_error
           envelope
       | Error failure -> report_parse_failure outgoing on_error line failure);
      loop ()
    | exception End_of_file -> Eio.Switch.fail worker_sw End_of_input
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Exn.raise_with_original_backtrace exn backtrace
    | exception exn ->
      on_error (invalid ("stdio input failed: " ^ Exn.to_string exn));
      Eio.Switch.fail worker_sw End_of_input
  in
  loop ()
;;

let run_authenticated
      ~sw
      ~dispatcher
      ~close_connection
      ~actor
      ~connection_id
      ~input
      ~output
      ~max_line_length
      ~outgoing_capacity
      ~max_attachments
      ~on_error
  =
  let outgoing = Agent_session.Mailbox.create ~capacity:outgoing_capacity in
  let publish_notification envelope =
    if not (publish outgoing envelope)
    then Eio.Switch.fail sw (Failure "stdio outgoing queue overflow")
  in
  let context =
    Agent_server.Connection_context.create_authenticated
      ~connection_id
      ~actor
      ~transport:Stdio
      ~publish_notification
      ~max_attachments
  in
  Exn.protect
    ~f:(fun () ->
      Eio.Fiber.both
        (fun () -> writer output outgoing)
        (fun () ->
           (match
              Eio.Switch.run (fun worker_sw ->
                let slots = create_slots () in
                reader
                  ~worker_sw
                  ~slots
                  ~dispatcher
                  ~context
                  ~input
                  ~max_line_length
                  ~outgoing
                  ~on_error)
            with
            | () | (exception End_of_input) -> ());
           Agent_session.Mailbox.close outgoing))
    ~finally:(fun () ->
      Agent_session.Mailbox.close outgoing;
      close_connection context)
;;

let run
      ~sw
      ~dispatcher
      ~close_connection
      ~principal
      ~connection_id
      ~input
      ~output
      ~max_line_length
      ~outgoing_capacity
      ~max_attachments
      ~on_error
  =
  run_authenticated
    ~sw
    ~dispatcher
    ~close_connection
    ~actor:(Operator_authorization.trusted_local principal)
    ~connection_id
    ~input
    ~output
    ~max_line_length
    ~outgoing_capacity
    ~max_attachments
    ~on_error
;;
