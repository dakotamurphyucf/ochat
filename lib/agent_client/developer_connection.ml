open! Core
module P = Agent_protocol
module D = P.Developer_wire
module C = P.Developer_common

module Pending_identity = struct
  type t =
    { run_id : P.Id.Developer_run.t
    ; key : P.Idempotency_key.t
    }
end

type pending_submit =
  { request : C.Submit.t
  ; identity : Pending_identity.t
  ; initialization : P.Initialize.Response.t
  ; byte_count : int
  }

type t =
  { connection : Connection.t
  ; initialization : P.Initialize.Response.t
  ; capabilities : C.Capabilities.t
  ; mutex : Eio.Mutex.t
  ; mutable pending : pending_submit list
  }

let incompatible message =
  P.Error.create Incompatible_protocol ~message ~retryable:false ()
;;

let interrupted message = P.Error.create Interrupted ~message ~retryable:false ()

let current t =
  match Connection.initialization t.connection with
  | Some initialization when phys_equal initialization t.initialization -> Ok ()
  | Some _ | None -> Error (incompatible "Developer initialization changed")
;;

let require_feature t request =
  let feature =
    match request with
    | D.Request.Capabilities | Submit _ | Observe _ | Await_terminal _ | Cancel_and_wait _
      -> C.Features.run
    | Approval_list _ | Approval_respond _ | Approval_cancel _ -> C.Features.approvals
    | Watch _ | Unwatch _ -> C.Features.events
  in
  if List.mem t.initialization.enabled_features feature ~equal:String.equal
  then Ok ()
  else Error (incompatible "Developer feature was not selected")
;;

let require_events t =
  if
    List.mem t.initialization.enabled_features C.Features.events ~equal:String.equal
    && C.Capabilities.supports t.capabilities C.Method.Watch
  then Ok ()
  else Error (incompatible "Developer events were not selected")
;;

let require_method t request =
  let open Result.Let_syntax in
  let%bind method_ = C.Method.of_string (D.Request.method_name request) in
  if C.Capabilities.supports t.capabilities method_
  then Ok ()
  else Error (incompatible "Developer method is not advertised")
;;

let with_lock t f =
  match
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match f () with
      | value -> Ok value
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let maximum_pending = 64
let maximum_pending_bytes = 16 * 1024 * 1024

let same_key a b =
  P.Id.Developer_run.equal a.Pending_identity.run_id b.Pending_identity.run_id
  && P.Idempotency_key.equal a.key b.key
;;

let pending_identity pending = pending.identity
let pending_submits t = with_lock t (fun () -> t.pending)

let retain_capacity t byte_count =
  List.length t.pending < maximum_pending
  && List.sum (module Int) t.pending ~f:(fun pending -> pending.byte_count)
     <= maximum_pending_bytes - byte_count
;;

let retire t pending =
  t.pending <- List.filter t.pending ~f:(fun item -> not (phys_equal item pending))
;;

let send_locked t request =
  let open Result.Let_syntax in
  let%bind () = current t in
  let%bind () = require_feature t request in
  let%bind () = require_method t request in
  match request with
  | D.Request.Capabilities ->
    let%map capabilities = Connection.developer_capabilities t.connection in
    D.Response.Capabilities capabilities
  | _ ->
    Connection.request_developer
      t.connection
      ~limits:(C.Capabilities.limits t.capabilities)
      request
;;

let send t request =
  with_lock t (fun () ->
    match request with
    | D.Request.Submit submit ->
      let open Result.Let_syntax in
      let%bind () = current t in
      let%bind () = require_feature t request in
      let%bind () = require_method t request in
      let%bind request =
        D.Request.of_method_and_params
          ~limits:(C.Capabilities.limits t.capabilities)
          ~method_:(D.Request.method_name request)
          ~params:(D.Request.params request)
      in
      let identity =
        Pending_identity.{ run_id = C.Submit.run_id submit; key = C.Submit.key submit }
      in
      if List.exists t.pending ~f:(fun pending -> same_key pending.identity identity)
      then Error (interrupted "Developer Submit key remains unresolved")
      else (
        let%bind encoded = P.Json_codec.canonical_string (D.Request.params request) in
        let byte_count = String.length encoded in
        if not (retain_capacity t byte_count)
        then
          Error
            (P.Error.create
               Resource_limit
               ~message:"Developer pending capacity exhausted"
               ~retryable:false
               ())
        else (
          let pending =
            { request = submit; identity; initialization = t.initialization; byte_count }
          in
          t.pending <- pending :: t.pending;
          (* Once the transport is called, even a local response-decoding error
             cannot prove that the server did not execute the Submit. *)
          let result =
            Connection.request_developer
              t.connection
              ~limits:(C.Capabilities.limits t.capabilities)
              request
          in
          (match result with
           | Ok _ -> retire t pending
           | Error _ -> ());
          result))
    | _ -> send_locked t request)
;;

let create connection =
  let open Result.Let_syntax in
  let%bind initialization =
    Connection.initialization connection
    |> Result.of_option ~error:(incompatible "Developer requires initialization")
  in
  let%bind capabilities = Connection.developer_capabilities connection in
  let t =
    { connection
    ; initialization
    ; capabilities
    ; mutex = Eio.Mutex.create ()
    ; pending = []
    }
  in
  let%bind () = current t in
  let%map () = require_method t D.Request.Capabilities in
  t
;;

let capabilities t = t.capabilities

let watch_and_observe t lookup =
  let open Result.Let_syntax in
  let%bind watched = send t (D.Request.Watch lookup) in
  match watched with
  | D.Response.Watch ->
    let%bind observed = send t (D.Request.Observe lookup) in
    (match observed with
     | D.Response.Observe view -> Ok view
     | _ -> Error (incompatible "Developer Observe result changed"))
  | _ -> Error (incompatible "Developer Watch result changed")
;;

let change_of_notification t envelope =
  let open Result.Let_syntax in
  let%bind () = current t in
  match envelope with
  | P.Envelope.Notification notification
    when String.equal notification.method_ D.Change.method_name ->
    let%bind () = require_events t in
    D.Change.of_json ~limits:(C.Capabilities.limits t.capabilities) notification.params
    |> Result.map ~f:Option.some
  | Notification _ | Request _ | Response _ -> Ok None
;;

let adopt_pending t (incoming : pending_submit list) =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind () = current t in
    let same_source (pending : pending_submit) =
      let original = pending.initialization
      and current = t.initialization in
      P.Id.Server.equal original.server_id current.server_id
      && P.Id.Principal.equal original.principal.id current.principal.id
      && String.equal
           original.principal.authentication_kind
           current.principal.authentication_kind
    in
    if not (List.for_all incoming ~f:same_source)
    then Error (incompatible "Developer pending Submit belongs to another identity")
    else (
      let%bind combined =
        List.fold
          incoming
          ~init:(Ok t.pending)
          ~f:(fun accumulated (pending : pending_submit) ->
            Result.bind accumulated ~f:(fun (retained : pending_submit list) ->
              match
                List.find retained ~f:(fun (item : pending_submit) ->
                  same_key pending.identity item.identity)
              with
              | None -> Ok (pending :: retained)
              | Some item when phys_equal item pending -> Ok retained
              | Some _ ->
                Error
                  (P.Error.create
                     Idempotency_conflict
                     ~message:"Developer pending key has conflicting custody"
                     ~retryable:false
                     ())))
      in
      if
        List.length combined > maximum_pending
        || List.sum (module Int) combined ~f:(fun item -> item.byte_count)
           > maximum_pending_bytes
      then
        Error
          (P.Error.create
             Resource_limit
             ~message:"Developer pending adoption exceeds capacity"
             ~retryable:false
             ())
      else (
        t.pending <- combined;
        Ok ())))
;;

let owns_pending t pending = List.exists t.pending ~f:(phys_equal pending)

let observe_pending t pending =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    if not (owns_pending t pending)
    then Error (incompatible "Developer pending Submit is foreign")
    else (
      let lookup =
        C.Lookup.create ~run_id:pending.identity.run_id ~key:pending.identity.key
      in
      let%bind response = send_locked t (D.Request.Observe lookup) in
      match response with
      | D.Response.Observe view -> Ok view
      | _ -> Error (incompatible "Developer Observe result changed")))
;;

let retry_pending t pending =
  with_lock t (fun () ->
    let open Result.Let_syntax in
    if not (owns_pending t pending)
    then Error (incompatible "Developer pending Submit is foreign")
    else (
      let%bind response = send_locked t (D.Request.Submit pending.request) in
      match response with
      | D.Response.Submit view ->
        retire t pending;
        Ok view
      | _ -> Error (incompatible "Developer Submit result changed")))
;;

let abandon_pending t pending = with_lock t (fun () -> retire t pending)
