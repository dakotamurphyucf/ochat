open! Core
module B = Agent_session.Runtime_builder
module P = Agent_protocol
module CO = Agent_session.Close_observation
module O = Native_cleanup_observation

let conflict message = P.Error.create Conflict ~message ~retryable:true ()

let as_unit observed =
  CO.create
    (match CO.exit observed with
     | Returned (Ok _) -> Returned (Ok ())
     | Returned (Error error) -> Returned (Error error)
     | Raised (exn, trace) -> Raised (exn, trace))
    ~diagnostics:(CO.diagnostics observed)
    ?primary:(CO.primary observed)
    ()
;;

let negative observed =
  CO.create
    (match CO.exit observed with
     | Returned (Error error) -> Returned (Error error)
     | Raised (exn, trace) -> Raised (exn, trace)
     | Returned (Ok _) -> invalid_arg "native driver expected a negative observation")
    ~diagnostics:(CO.diagnostics observed)
    ?primary:(CO.primary observed)
    ()
;;

module Close_request = struct
  type t =
    { result : unit O.t Eio.Promise.t
    ; resolve : unit O.t Eio.Promise.u
    }

  let create () =
    let result, resolve = Eio.Promise.create () in
    { result; resolve }
  ;;
end

module Driver = struct
  type local =
    | Not_entered
    | Preparing
    | Retaining
    | Drained of unit O.t
    | Aborted of unit O.t

  type t =
    { ready : B.Prepared_native.t O.t Eio.Promise.t
    ; mutable publish : B.Prepared_native.t O.t Eio.Promise.u option
    ; exited : unit O.t Eio.Promise.t
    ; exit : unit O.t Eio.Promise.u
    ; requests : Close_request.t Eio.Stream.t
    ; mutable closing : Close_request.t option
    ; mutable local : local
    }

  let create () =
    let ready, publish = Eio.Promise.create () in
    let exited, exit = Eio.Promise.create () in
    { ready
    ; publish = Some publish
    ; exited
    ; exit
    ; requests = Eio.Stream.create 1
    ; closing = None
    ; local = Not_entered
    }
  ;;

  let publish t result =
    Option.iter t.publish ~f:(fun resolver ->
      t.publish <- None;
      Eio.Promise.resolve resolver result)
  ;;

  let finish_request t request result =
    t.closing <- None;
    Eio.Promise.resolve request.Close_request.resolve result
  ;;

  let retain t ~prepare ~close_attempt =
    match t.local with
    | Preparing | Retaining | Drained _ | Aborted _ ->
      O.capture (fun () ->
        Error (conflict "native lifetime preparation was already entered"))
    | Not_entered ->
      t.local <- Preparing;
      (* The Cell already owns this driver before preparation can allocate. *)
      let prepared = O.capture_observed prepare in
      t.local <- Retaining;
      publish t prepared;
      let rec serve () =
        let request = Eio.Stream.take t.requests in
        let result = Eio.Cancel.protect (fun () -> O.capture_observed close_attempt) in
        if O.is_negative result
        then (
          finish_request t request result;
          serve ())
        else (
          t.local <- Drained result;
          finish_request t request result;
          O.combine (as_unit prepared) result)
      in
      serve ()
  ;;

  let run t f =
    let outcome = O.capture_observed (fun () -> f ~retain:(retain t)) in
    let incomplete () =
      if O.is_negative outcome
      then outcome
      else
        O.capture (fun () ->
          Error (conflict "native lifetime driver returned without completing its scope"))
    in
    let local_cleanup =
      match t.local with
      | Not_entered | Drained _ -> O.return ()
      | Preparing | Retaining ->
        let failure = incomplete () in
        t.local <- Aborted failure;
        failure
      | Aborted failure -> failure
    in
    (match t.publish with
     | None -> ()
     | Some _ -> publish t (negative (incomplete ())));
    Eio.Promise.resolve t.exit outcome;
    Option.iter t.closing ~f:(fun request -> finish_request t request local_cleanup)
  ;;

  let join t =
    (* Preserve original f diagnostics without treating its primary as a parent
       cleanup failure. Actual parent reports independently certify those joins. *)
    let outcome = Eio.Promise.await t.exited in
    let exited = CO.create (Returned (Ok ())) ~diagnostics:(CO.diagnostics outcome) () in
    match t.local with
    | Not_entered -> exited
    | Drained local -> O.combine local exited
    | Aborted failure -> O.combine failure exited
    | Preparing | Retaining ->
      O.combine
        (O.capture (fun () ->
           Error (conflict "native lifetime driver has not completed dependent cleanup")))
        exited
  ;;

  let close t =
    match t.local with
    | Drained _ | Aborted _ -> join t
    | Not_entered when Option.is_some (Eio.Promise.peek t.exited) -> join t
    | Not_entered | Preparing | Retaining ->
      let request =
        match t.closing with
        | Some request -> request
        | None ->
          let request = Close_request.create () in
          t.closing <- Some request;
          Eio.Stream.add t.requests request;
          request
      in
      let result = Eio.Promise.await request.result in
      if O.is_negative result then result else O.combine result (join t)
  ;;
end

type phase =
  | Fresh
  | Running of Driver.t
  | Closed

type t =
  { domain : int
  ; sw : Eio.Switch.t
  ; mutable phase : phase
  }

let create ~sw = { domain = (Stdlib.Domain.self () :> int); sw; phase = Fresh }

let check_domain t =
  if Int.equal t.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else Error (P.Error.invalid_request "native lifetime belongs to another Eio domain")
;;

let check_open t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Running { local = Retaining; _ } -> Ok ()
  | Fresh | Closed
  | Running { local = Not_entered | Preparing | Drained _ | Aborted _; _ } ->
    Error (conflict "native lifetime is not available for borrowing")
;;

let check_prepared t ~prepared =
  let open Result.Let_syntax in
  let%bind () = check_open t in
  match t.phase with
  | Running driver ->
    (match Eio.Promise.peek driver.ready with
     | Some observed ->
       (match CO.exit observed with
        | Returned (Ok actual)
          when B.Native_identity.equal
                 (B.Prepared_native.identity actual)
                 (B.Prepared_native.identity prepared) -> Ok ()
        | Returned (Ok _ | Error _) | Raised _ ->
          Error (conflict "native lifetime belongs to another prepared unit"))
     | None -> Error (conflict "native lifetime belongs to another prepared unit"))
  | Fresh | Closed -> Error (conflict "native lifetime has no prepared unit")
;;

let prepare_observed t ~f =
  O.capture_observed (fun () ->
    match check_domain t with
    | Error error -> O.capture (fun () -> Error error)
    | Ok () ->
      (match t.phase with
       | Running _ | Closed ->
         O.capture (fun () ->
           Error (conflict "native lifetime was already started or closed"))
       | Fresh ->
         Eio.Switch.check t.sw;
         let driver = Driver.create () in
         t.phase <- Running driver;
         Eio.Fiber.fork ~sw:t.sw (fun () -> Driver.run driver f);
         Eio.Promise.await driver.ready))
;;

let prepare t ~f =
  prepare_observed t ~f:(fun ~retain ->
    O.capture (fun () ->
      f ~retain:(fun ~prepare ~close_attempt ->
        retain
          ~prepare:(fun () -> O.capture prepare)
          ~close_attempt:(fun () -> O.capture close_attempt)
        |> CO.resume)))
  |> CO.resume
;;

let close_observed t =
  O.capture_observed (fun () ->
    match check_domain t with
    | Error error -> O.capture (fun () -> Error error)
    | Ok () ->
      Eio.Cancel.protect (fun () ->
        match t.phase with
        | Closed -> O.return ()
        | Fresh ->
          t.phase <- Closed;
          O.return ()
        | Running driver -> Driver.close driver))
;;

let close t = close_observed t |> CO.resume

let driver_has_exited t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Fresh | Closed -> Error (conflict "native lifetime has no started driver")
  | Running driver -> Ok (Option.is_some (Eio.Promise.peek driver.exited))
;;

let driver_observation t =
  O.capture_observed (fun () ->
    match check_domain t with
    | Error error -> O.capture (fun () -> Error error)
    | Ok () ->
      (match t.phase with
       | Fresh | Closed ->
         O.capture (fun () -> Error (conflict "native lifetime has no started driver"))
       | Running driver -> Eio.Promise.await driver.exited))
;;

let driver_result t = driver_observation t |> CO.resume
