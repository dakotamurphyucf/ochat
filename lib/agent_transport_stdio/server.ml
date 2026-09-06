open! Core

let invalid message = Agent_protocol.Error.invalid_request message

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

let dispatch dispatcher context outgoing on_error envelope =
  match Agent_server.Dispatcher.dispatch_envelope dispatcher ~context envelope with
  | Ok None -> ()
  | Ok (Some response) ->
    if not (publish outgoing response)
    then on_error (invalid "connection outgoing queue is full")
  | Error failure -> on_error failure
;;

let reader ~dispatcher ~context ~input ~max_line_length ~outgoing ~on_error =
  let buffer = Eio.Buf_read.of_flow input ~max_size:max_line_length in
  let rec loop () =
    match Eio.Buf_read.line buffer with
    | line ->
      (match parse_line line with
       | Ok envelope -> dispatch dispatcher context outgoing on_error envelope
       | Error failure -> report_parse_failure outgoing on_error line failure);
      loop ()
    | exception End_of_file -> ()
    | exception exn -> on_error (invalid ("stdio input failed: " ^ Exn.to_string exn))
  in
  loop ()
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
  let outgoing = Agent_session.Mailbox.create ~capacity:outgoing_capacity in
  let publish_notification envelope =
    if not (publish outgoing envelope)
    then Eio.Switch.fail sw (Failure "stdio outgoing queue overflow")
  in
  let context =
    Agent_server.Connection_context.create
      ~connection_id
      ~principal
      ~transport:Stdio
      ~publish_notification
      ~max_attachments
  in
  Exn.protect
    ~f:(fun () ->
      Eio.Fiber.both
        (fun () -> writer output outgoing)
        (fun () ->
           reader ~dispatcher ~context ~input ~max_line_length ~outgoing ~on_error;
           Agent_session.Mailbox.close outgoing))
    ~finally:(fun () ->
      Agent_session.Mailbox.close outgoing;
      close_connection context)
;;
