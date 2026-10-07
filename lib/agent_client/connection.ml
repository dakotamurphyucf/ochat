open! Core
module P = Agent_protocol

type t =
  { transport : Transport.t
  ; mutex : Eio.Mutex.t
  ; mutable closed : bool
  ; mutable initialization : P.Initialize.Response.t option
  }

let create transport =
  { transport; mutex = Eio.Mutex.create (); closed = false; initialization = None }
;;

let closed_error () =
  Agent_protocol.Error.create
    Interrupted
    ~message:"client connection is closed"
    ~retryable:true
    ()
;;

let admit_initialization (request : P.Initialize.Request.t) response =
  let open Result.Let_syntax in
  let%bind response =
    P.Initialize.Response.of_json (P.Initialize.Response.to_json response)
  in
  if
    P.Version.compare response.selected_version request.protocol_min < 0
    || P.Version.compare response.selected_version request.protocol_max > 0
    || not
         (List.for_all response.enabled_features ~f:(fun feature ->
            List.mem request.features feature ~equal:String.equal))
  then
    Error (P.Error.invalid_request "initialize response exceeds requested capabilities")
  else Ok response
;;

let request t command =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.closed
    then Error (closed_error ())
    else (
      let result = Transport.request t.transport command in
      match command, result with
      | P.Command.Protocol_initialize request, Ok (P.Public.Result.Non_history value) ->
        (match P.Public.Result.Non_history.value value with
         | Protocol_initialize response ->
           let open Result.Let_syntax in
           let%bind () = P.Public.Result.validate (P.Public.Result.Non_history value) in
           let%bind response = admit_initialization request response in
           t.initialization <- Some response;
           result
         | _ -> result)
      | _, _ -> result))
;;

let initialization t =
  Eio.Mutex.use_ro t.mutex (fun () -> if t.closed then None else t.initialization)
;;

let next_notification t =
  if Eio.Mutex.use_ro t.mutex (fun () -> t.closed)
  then None
  else Transport.next_notification t.transport
;;

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not t.closed
    then (
      t.closed <- true;
      Transport.close t.transport))
;;

let request_without_history t command =
  match request t command with
  | Ok (Agent_protocol.Public.Result.Non_history value) ->
    Ok (Agent_protocol.Public.Result.Non_history.value value)
  | Ok (Session_get _ | Session_attach _ | Session_create _) ->
    Error (Agent_protocol.Error.invalid_request "unexpected history-bearing result")
  | Error _ as failure -> failure
;;
