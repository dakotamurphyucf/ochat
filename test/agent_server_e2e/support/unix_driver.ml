open Core

type raw =
  { flow : [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t
  ; reader : Eio.Buf_read.t
  }

type raw_read =
  | Envelope of Agent_protocol.Envelope.t
  | End_of_file
  | Timeout
  | Invalid_response of Agent_protocol.Error.t
[@@deriving sexp]

let connect ~sw ~env ~socket_path =
  Agent_transport_socket.Client.connect
    ~sw
    ~net:(Eio.Stdenv.net env)
    ~socket_path
    ~max_line_length:(16 * 1024 * 1024)
    ~notification_capacity:1_024
;;

let initialize connection =
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"agent-server-e2e"
    ~implementation_version:"dev"
;;

let ping connection ~payload =
  match
    Agent_client.Connection.request
      connection
      (Protocol_ping Agent_protocol.Ping.Request.{ payload })
  with
  | Ok (Protocol_ping response) -> Ok response
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected ping result")
  | Error _ as failure -> failure
;;

let next_notification connection ~clock ~timeout_seconds =
  match
    Eio.Time.with_timeout clock timeout_seconds (fun () ->
      Ok (Agent_client.Connection.next_notification connection))
  with
  | Error `Timeout -> `Timeout
  | Ok None -> `Closed
  | Ok (Some envelope) -> `Notification envelope
;;

let connect_raw ~sw ~env ~socket_path ~max_response_bytes =
  let flow =
    (Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path)
      :> [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t)
  in
  { flow; reader = Eio.Buf_read.of_flow flow ~max_size:max_response_bytes }
;;

let send_line t line = Eio.Flow.copy_string (line ^ "\n") t.flow

let decode_response line =
  Result.try_with (fun () -> Jsonaf.of_string line)
  |> Result.map_error ~f:(fun exn ->
    Agent_protocol.Error.invalid_request ("invalid JSON response: " ^ Exn.to_string exn))
  |> Result.bind ~f:Agent_protocol.Envelope.of_json
;;

let read_envelope t ~clock ~timeout_seconds =
  match
    Eio.Time.with_timeout clock timeout_seconds (fun () ->
      Ok (Eio.Buf_read.line t.reader))
  with
  | Error `Timeout -> Timeout
  | Ok line ->
    (match decode_response line with
     | Ok envelope -> Envelope envelope
     | Error error -> Invalid_response error)
  | exception End_of_file -> End_of_file
;;

let close_raw t = Eio.Flow.close t.flow
