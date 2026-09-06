open! Core

type pending =
  { method_ : string
  ; resolver :
      (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result Eio.Promise.u
  }

type t =
  { flow : [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t
  ; writer_mutex : Eio.Mutex.t
  ; state_mutex : Eio.Mutex.t
  ; notifications : Agent_protocol.Envelope.t Agent_session.Mailbox.t
  ; mutable pending : (Agent_protocol.Envelope.Request_id.t, pending) Map.Poly.t
  ; mutable next_request_id : int64
  ; mutable closed : bool
  }

let interrupted message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:true ()
;;

let next_id t =
  Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
    let value = t.next_request_id in
    if Int64.equal value Int64.max_value
    then Error (interrupted "socket request identifier space is exhausted")
    else (
      t.next_request_id <- Int64.(value + 1L);
      Agent_protocol.Envelope.Request_id.of_json (`Number (Int64.to_string value))))
;;

let resolve_all t failure =
  let pending =
    Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
      if t.closed
      then []
      else (
        t.closed <- true;
        let pending = Map.data t.pending in
        t.pending <- Map.Poly.empty;
        pending))
  in
  List.iter pending ~f:(fun pending ->
    Eio.Promise.resolve pending.resolver (Error failure));
  Agent_session.Mailbox.close t.notifications
;;

let close_internal t =
  resolve_all t (interrupted "socket connection closed");
  ignore (Result.try_with (fun () -> Eio.Flow.shutdown t.flow `All) : (unit, exn) result);
  Eio.Flow.close t.flow
;;

let take_pending t id =
  Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
    let pending = Map.find t.pending id in
    t.pending <- Map.remove t.pending id;
    pending)
;;

let handle_response t response =
  Option.iter (take_pending t response.Agent_protocol.Envelope.id) ~f:(fun pending ->
    let result =
      Result.bind response.outcome ~f:(fun json ->
        Agent_protocol.Method_result.of_json ~method_:pending.method_ json)
    in
    Eio.Promise.resolve pending.resolver result)
;;

let handle_envelope t = function
  | Agent_protocol.Envelope.Response response -> handle_response t response
  | Notification _ as envelope ->
    if not (Agent_session.Mailbox.try_push t.notifications ~priority:Normal envelope)
    then close_internal t
  | Request _ -> close_internal t
;;

let parse_line line =
  Result.try_with (fun () -> Jsonaf.of_string line)
  |> Result.map_error ~f:(fun exn ->
    interrupted ("invalid server JSON: " ^ Exn.to_string exn))
  |> Result.bind ~f:Agent_protocol.Envelope.of_json
;;

let reader t ~max_line_length =
  let buffer = Eio.Buf_read.of_flow t.flow ~max_size:max_line_length in
  let rec loop () =
    match Eio.Buf_read.line buffer with
    | line ->
      (match parse_line line with
       | Ok envelope -> handle_envelope t envelope
       | Error failure -> resolve_all t failure);
      if not t.closed then loop ()
    | exception End_of_file -> close_internal t
    | exception exn ->
      resolve_all t (interrupted ("socket read failed: " ^ Exn.to_string exn))
  in
  loop ()
;;

let register_pending t id pending =
  Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
    if t.closed
    then Error (interrupted "socket connection is closed")
    else (
      t.pending <- Map.set t.pending ~key:id ~data:pending;
      Ok ()))
;;

let remove_pending t id =
  Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
    t.pending <- Map.remove t.pending id)
;;

let write_request t envelope =
  Eio.Mutex.use_rw ~protect:true t.writer_mutex (fun () ->
    envelope
    |> Agent_protocol.Envelope.to_json
    |> Jsonaf.to_string
    |> fun line -> Eio.Flow.copy_string (line ^ "\n") t.flow)
;;

let request t command =
  let open Result.Let_syntax in
  let%bind id = next_id t in
  let response, resolver = Eio.Promise.create () in
  let method_ = Agent_protocol.Command.method_name command in
  let%bind () = register_pending t id { method_; resolver } in
  let envelope =
    Agent_protocol.Envelope.request
      ~id
      ~method_
      ~params:(Agent_protocol.Command.params command)
      ()
  in
  match Result.try_with (fun () -> write_request t envelope) with
  | Ok () -> Eio.Promise.await response
  | Error exn ->
    remove_pending t id;
    Error (interrupted ("socket write failed: " ^ Exn.to_string exn))
;;

let next_notification t = Agent_session.Mailbox.pop t.notifications

let transport t =
  Agent_client.Transport.create
    ~request:(request t)
    ~next_notification:(fun () -> next_notification t)
    ~close:(fun () -> close_internal t)
;;

let connect ~sw ~net ~socket_path ~max_line_length ~notification_capacity =
  let flow =
    (Eio.Net.connect ~sw net (`Unix socket_path)
      :> [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t)
  in
  let t =
    { flow
    ; writer_mutex = Eio.Mutex.create ()
    ; state_mutex = Eio.Mutex.create ()
    ; notifications = Agent_session.Mailbox.create ~capacity:notification_capacity
    ; pending = Map.Poly.empty
    ; next_request_id = 1L
    ; closed = false
    }
  in
  Eio.Fiber.fork ~sw (fun () -> reader t ~max_line_length);
  Agent_client.Connection.create (transport t)
;;
