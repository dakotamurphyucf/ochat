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

let rec writer output outgoing =
  match Agent_session.Mailbox.pop outgoing with
  | None -> ()
  | Some envelope ->
    write_envelope output envelope;
    writer output outgoing
;;

let publish connection outgoing envelope =
  if Agent_session.Mailbox.try_push outgoing ~priority:Normal envelope
  then true
  else (
    Agent_client.Connection.close connection;
    false)
;;

let response connection (request : Agent_protocol.Envelope.request) =
  let open Result.Let_syntax in
  let%bind command =
    Agent_protocol.Command.of_method_and_params
      ~method_:request.Agent_protocol.Envelope.method_
      ~params:request.params
  in
  let%map result = Agent_client.Connection.request connection command in
  Agent_protocol.Method_result.to_json result
;;

let dispatch connection outgoing on_error = function
  | Agent_protocol.Envelope.Request request ->
    let envelope =
      match response connection request with
      | Ok result -> Agent_protocol.Envelope.success ~id:request.id result
      | Error failure -> Agent_protocol.Envelope.failure ~id:request.id failure
    in
    if not (publish connection outgoing envelope)
    then on_error (invalid "stdio gateway outgoing queue is full")
  | Notification _ | Response _ ->
    on_error (invalid "stdio gateway input must contain request envelopes")
;;

let report_parse_failure connection outgoing on_error line failure =
  on_error failure;
  Option.iter (request_id_of_line line) ~f:(fun id ->
    ignore
      (publish connection outgoing (Agent_protocol.Envelope.failure ~id failure) : bool))
;;

let reader connection input outgoing ~max_line_length ~on_error =
  let buffer = Eio.Buf_read.of_flow input ~max_size:max_line_length in
  let rec loop () =
    match Eio.Buf_read.line buffer with
    | line ->
      (match parse_line line with
       | Ok envelope -> dispatch connection outgoing on_error envelope
       | Error failure -> report_parse_failure connection outgoing on_error line failure);
      loop ()
    | exception End_of_file -> ()
    | exception exn ->
      on_error (invalid ("stdio gateway input failed: " ^ Exn.to_string exn))
  in
  loop ()
;;

let rec notifications connection outgoing ~on_error =
  match Agent_client.Connection.next_notification connection with
  | None -> ()
  | Some envelope ->
    if publish connection outgoing envelope
    then notifications connection outgoing ~on_error
    else on_error (invalid "stdio gateway outgoing queue is full")
;;

let run ~sw:_ ~connection ~input ~output ~max_line_length ~outgoing_capacity ~on_error =
  let outgoing = Agent_session.Mailbox.create ~capacity:outgoing_capacity in
  Exn.protect
    ~f:(fun () ->
      Eio.Fiber.both
        (fun () -> writer output outgoing)
        (fun () ->
           Eio.Fiber.both
             (fun () ->
                reader connection input outgoing ~max_line_length ~on_error;
                Agent_client.Connection.close connection)
             (fun () -> notifications connection outgoing ~on_error);
           Agent_session.Mailbox.close outgoing))
    ~finally:(fun () ->
      Agent_session.Mailbox.close outgoing;
      Agent_client.Connection.close connection)
;;
