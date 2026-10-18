open! Core

module Request_id = struct
  include Agent_protocol.Envelope.Request_id
  include Comparator.Make (Agent_protocol.Envelope.Request_id)
end

type pending =
  | Command of
      { method_ : string
      ; resolver :
          (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result Eio.Promise.u
      }
  | Developer of { resolver : (Jsonaf.t, Agent_protocol.Error.t) result Eio.Promise.u }

type t =
  { flow : [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t
  ; writer_mutex : Eio.Mutex.t
  ; state_mutex : Eio.Mutex.t
  ; notifications : Agent_protocol.Envelope.t Agent_session.Mailbox.t
  ; mutable pending : (Request_id.t, pending, Request_id.comparator_witness) Map.t
  ; mutable next_request_id : int64
  ; max_line_length : int
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
        t.pending <- Map.empty (module Request_id);
        pending))
  in
  List.iter pending ~f:(function
    | Command pending -> Eio.Promise.resolve pending.resolver (Error failure)
    | Developer pending -> Eio.Promise.resolve pending.resolver (Error failure));
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

let handle_response t response ~raw_error_bytes =
  Option.iter (take_pending t response.Agent_protocol.Envelope.id) ~f:(fun pending ->
    match pending with
    | Command pending ->
      let result =
        Result.bind response.outcome ~f:(fun json ->
          Agent_protocol.Public.Result.of_json ~method_:pending.method_ json
          |> Result.map_error ~f:(fun failure ->
            interrupted
              ("socket success response could not be validated: "
               ^ failure.Agent_protocol.Error.message)))
      in
      Eio.Promise.resolve pending.resolver result
    | Developer pending ->
      let outcome =
        match response.outcome, raw_error_bytes with
        | Error _, Some bytes
          when bytes > Agent_client.Transport.Developer_response_bound.max_error_bytes ->
          Error (interrupted "Developer socket error response exceeds its bound")
        | Error failure, _
          when not
                 (Agent_client.Transport.Developer_response_bound.error_within_bound
                    failure) ->
          Error (interrupted "Developer socket error response exceeds its bound")
        | outcome, _ -> outcome
      in
      Eio.Promise.resolve pending.resolver outcome)
;;

let handle_envelope t envelope ~raw_error_bytes =
  match envelope with
  | Agent_protocol.Envelope.Response response ->
    handle_response t response ~raw_error_bytes
  | Notification _ as envelope ->
    if not (Agent_session.Mailbox.try_push t.notifications ~priority:Normal envelope)
    then close_internal t
  | Request _ -> close_internal t
;;

let parse_line line =
  Result.try_with (fun () -> Jsonaf.of_string line)
  |> Result.map_error ~f:(fun exn ->
    interrupted ("invalid server JSON: " ^ Exn.to_string exn))
  |> Result.bind ~f:(fun json ->
    let raw_error_bytes =
      match json with
      | `Object fields ->
        List.Assoc.find fields "error" ~equal:String.equal
        |> Option.map ~f:(fun error -> String.length (Jsonaf.to_string error))
      | _ -> None
    in
    Agent_protocol.Envelope.of_json json
    |> Result.map ~f:(fun envelope -> envelope, raw_error_bytes)
    |> Result.map_error ~f:(fun failure ->
      interrupted failure.Agent_protocol.Error.message))
;;

let reader t ~max_line_length =
  let buffer = Eio.Buf_read.of_flow t.flow ~max_size:max_line_length in
  let rec loop () =
    match Eio.Buf_read.line buffer with
    | line ->
      (match parse_line line with
       | Ok (envelope, raw_error_bytes) -> handle_envelope t envelope ~raw_error_bytes
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
  Eio.Mutex.use_rw ~protect:false t.writer_mutex (fun () ->
    envelope
    |> Agent_protocol.Envelope.to_json
    |> Jsonaf.to_string
    |> fun line -> Eio.Flow.copy_string (line ^ "\n") t.flow)
;;

let request_envelope t ~method_ ~params ~pending ~response ~bound =
  let open Result.Let_syntax in
  let%bind id = next_id t in
  let%bind () =
    match bound with
    | None -> Ok ()
    | Some Agent_client.Transport.Developer_response_bound.Bootstrap -> Ok ()
    | Some (Agent_client.Transport.Developer_response_bound.Installed limits) ->
      let placeholder =
        Agent_protocol.Envelope.success ~id `Null
        |> Agent_protocol.Envelope.to_json
        |> Jsonaf.to_string
      in
      let empty_error = Agent_protocol.Error.invalid_request "" in
      let error_json_bytes =
        Agent_protocol.Error.to_json empty_error |> Jsonaf.to_string |> String.length
      in
      let error_framing =
        Agent_protocol.Envelope.failure ~id empty_error
        |> Agent_protocol.Envelope.to_json
        |> Jsonaf.to_string
        |> String.length
        |> fun length -> length - error_json_bytes
      in
      let max_result_bytes =
        Agent_client.Transport.Developer_response_bound.max_result_bytes
          (Agent_client.Transport.Developer_response_bound.Installed limits)
      in
      if
        max_result_bytes > t.max_line_length - (String.length placeholder - 4)
        || Agent_client.Transport.Developer_response_bound.max_error_bytes
           > t.max_line_length - error_framing
      then Error (interrupted "Developer response exceeds socket line limit")
      else Ok ()
  in
  let envelope = Agent_protocol.Envelope.request ~id ~method_ ~params () in
  let%bind () =
    match bound with
    | None -> Ok ()
    | Some _ ->
      if
        String.length (Agent_protocol.Envelope.to_json envelope |> Jsonaf.to_string)
        > t.max_line_length
      then Error (interrupted "Developer request exceeds socket line limit")
      else Ok ()
  in
  let%bind () = register_pending t id pending in
  Exn.protect
    ~finally:(fun () -> remove_pending t id)
    ~f:(fun () ->
      match write_request t envelope with
      | () -> Eio.Promise.await response
      | exception exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        (* A failed or cancelled write may have published only part of a JSON
           line. Retire the channel before another command can append to it.
           The higher-level connection retains the uncertain command intent. *)
        Eio.Cancel.protect (fun () ->
          ignore (Result.try_with (fun () -> close_internal t) : (unit, exn) result));
        (match exn with
         | Eio.Cancel.Cancelled _ -> Exn.raise_with_original_backtrace exn backtrace
         | _ -> Error (interrupted ("socket write failed: " ^ Exn.to_string exn))))
;;

let request t command =
  let response, resolver = Eio.Promise.create () in
  request_envelope
    t
    ~method_:(Agent_protocol.Command.method_name command)
    ~params:(Agent_protocol.Command.params command)
    ~pending:(Command { method_ = Agent_protocol.Command.method_name command; resolver })
    ~response
    ~bound:None
;;

let request_developer t ~bound request =
  let response, resolver = Eio.Promise.create () in
  request_envelope
    t
    ~method_:(Agent_protocol.Developer_wire.Request.method_name request)
    ~params:(Agent_protocol.Developer_wire.Request.params request)
    ~pending:(Developer { resolver })
    ~response
    ~bound:(Some bound)
;;

let next_notification t = Agent_session.Mailbox.pop t.notifications

let transport t =
  Agent_client.Transport.create_with_developer
    ~request:(request t)
    ~request_developer:(request_developer t)
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
    ; pending = Map.empty (module Request_id)
    ; next_request_id = 1L
    ; max_line_length
    ; closed = false
    }
  in
  Eio.Fiber.fork ~sw (fun () -> reader t ~max_line_length);
  Agent_client.Connection.create (transport t)
;;
