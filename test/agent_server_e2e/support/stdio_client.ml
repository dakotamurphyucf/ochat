open Core

type t =
  { process : Stdio_process.t
  ; read_stdout : float -> [ `Item of Stdio_process.stdout_item | `Timeout ]
  ; mutable next_request_id : int64
  }

type response =
  { result : Agent_protocol.Method_result.t
  ; notifications : Agent_protocol.Envelope.t list
  }
[@@deriving sexp]

let interrupted message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:true ()
;;

let create ~process ~clock =
  { process
  ; read_stdout =
      (fun timeout_seconds -> Stdio_process.next_stdout process ~clock ~timeout_seconds)
  ; next_request_id = 1L
  }
;;

let process t = t.process

let next_id t =
  if Int64.equal t.next_request_id Int64.max_value
  then Error (interrupted "stdio request identifier space is exhausted")
  else (
    let value = t.next_request_id in
    t.next_request_id <- Int64.(value + 1L);
    Agent_protocol.Envelope.Request_id.of_json (`Number (Int64.to_string value)))
;;

let send_raw_line t line =
  Result.try_with (fun () -> Stdio_process.send_line t.process line)
  |> Result.map_error ~f:(fun exn ->
    interrupted ("stdio input write failed: " ^ Exn.to_string exn))
;;

let next_envelope t ~timeout_seconds =
  let stderr () = (Stdio_process.stderr t.process).contents in
  match t.read_stdout timeout_seconds with
  | `Timeout ->
    Error (interrupted ("timed out waiting for stdio output; stderr: " ^ stderr ()))
  | `Item (Envelope envelope) -> Ok envelope
  | `Item (Invalid_line { error; _ }) -> Error error
  | `Item (Read_error message) -> Error (interrupted ("stdio output failed: " ^ message))
  | `Item End_of_file -> Error (interrupted ("stdio output closed; stderr: " ^ stderr ()))
;;

let rec await_response t command id notifications =
  let open Result.Let_syntax in
  let%bind envelope = next_envelope t ~timeout_seconds:5. in
  match envelope with
  | Agent_protocol.Envelope.Notification _ ->
    await_response t command id (envelope :: notifications)
  | Response response when Agent_protocol.Envelope.Request_id.compare response.id id = 0
    ->
    let%map result =
      Result.bind response.outcome ~f:(fun json ->
        Agent_protocol.Method_result.of_json
          ~method_:(Agent_protocol.Command.method_name command)
          json)
    in
    { result; notifications = List.rev notifications }
  | Response _ -> Error (Agent_protocol.Error.invalid_request "stdio response ID differs")
  | Request _ -> Error (Agent_protocol.Error.invalid_request "stdio emitted a request")
;;

let request t command =
  let open Result.Let_syntax in
  let%bind id = next_id t in
  let envelope =
    Agent_protocol.Envelope.request
      ~id
      ~method_:(Agent_protocol.Command.method_name command)
      ~params:(Agent_protocol.Command.params command)
      ()
  in
  let%bind () =
    Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string |> send_raw_line t
  in
  await_response t command id []
;;

let initialize t =
  let open Result.Let_syntax in
  let%bind implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:"agent-server-e2e-stdio"
      ~version:"dev"
  in
  let%bind initialize_request =
    Agent_protocol.Initialize.Request.create
      ~implementation
      ~protocol_min:Agent_protocol.Version.initial
      ~protocol_max:Agent_protocol.Version.initial
      ~features:[]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
  in
  let%bind response = request t (Protocol_initialize initialize_request) in
  match response.result with
  | Protocol_initialize initialized -> Ok (initialized, response.notifications)
  | _ -> Error (Agent_protocol.Error.invalid_request "unexpected initialize result")
;;
