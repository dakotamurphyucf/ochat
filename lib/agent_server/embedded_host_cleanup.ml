open! Core
module Diagnostic = Session_registry.Cleanup_failure

module Connection_owner = struct
  type t =
    { connection : Agent_client.Connection.t
    ; close_actual : unit -> unit
    }

  let create ~connection ~close_actual = { connection; close_actual }
  let connection t = t.connection

  let close t =
    Agent_client.Connection.close t.connection;
    t.close_actual ()
  ;;
end

module Failure = struct
  type t =
    { primary : Diagnostic.t
    ; cleanup : Diagnostic.t
    }

  let primary t = t.primary
  let cleanup t = t.cleanup
end

exception Cleanup_failed of Failure.t

type admission =
  | Open
  | Closing
  | Closed

type t =
  { sw : Eio.Switch.t
  ; daemon : Daemon.t
  ; env : Eio_unix.Stdenv.base
  ; temporary_root : string option
  ; admission : admission Atomic.t
  ; close_mutex : Eio.Mutex.t
  ; mutable connections : Connection_owner.t list
  ; mutable daemon_released : bool
  ; mutable failure : Failure.t option
  }

let is_closing t =
  match Atomic.get t.admission with
  | Open -> false
  | Closing | Closed -> true
;;

let capture_step f =
  match f () with
  | Ok () -> None
  | Error error -> Some (Diagnostic.rejected error)
  | exception exn -> Some (Diagnostic.raised exn (Stdlib.Printexc.get_raw_backtrace ()))
;;

let close_connections t =
  let failed, first_failure =
    List.fold t.connections ~init:([], None) ~f:(fun (failed, first_failure) connection ->
      match
        capture_step (fun () ->
          Connection_owner.close connection;
          Ok ())
      with
      | None -> failed, first_failure
      | Some diagnostic ->
        let first_failure =
          match first_failure with
          | None -> Some diagnostic
          | Some _ -> first_failure
        in
        connection :: failed, first_failure)
  in
  t.connections <- List.rev failed;
  first_failure
;;

let close_owned t =
  (* Admission closes before the yielding coordinator/connection detach. *)
  ignore (Atomic.compare_and_set t.admission Open Closing : bool);
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.close_mutex (fun () ->
      match Atomic.get t.admission with
      | Closed -> Ok ()
      | Open | Closing ->
        let finish cleanup = Error ({ primary = cleanup; cleanup } : Failure.t) in
        (match close_connections t with
         | Some failure -> finish failure
         | None ->
           let daemon_failure =
             capture_step (fun () ->
               if t.daemon_released
               then Ok ()
               else
                 Result.map (Daemon.shutdown t.daemon) ~f:(fun () ->
                   t.daemon_released <- true))
           in
           (match daemon_failure with
            | Some failure -> finish failure
            | None ->
              let namespace_failure =
                capture_step (fun () ->
                  Option.iter t.temporary_root ~f:(fun path ->
                    Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs t.env / path));
                  Ok ())
              in
              (match namespace_failure with
               | Some failure -> finish failure
               | None ->
                 Atomic.set t.admission Closed;
                 Ok ())))))
;;

let record_failure t (failure : Failure.t) =
  let failure =
    match t.failure with
    | None -> failure
    | Some previous -> { previous with cleanup = failure.cleanup }
  in
  t.failure <- Some failure;
  failure
;;

let raise_scope_failure failure =
  match Diagnostic.exception_and_backtrace (Failure.cleanup failure) with
  | None -> raise (Cleanup_failed failure)
  | Some (_, backtrace) ->
    Exn.raise_with_original_backtrace (Cleanup_failed failure) backtrace
;;

let create ~sw ~env ~daemon ~temporary_root =
  let t =
    { sw
    ; daemon
    ; env
    ; temporary_root
    ; admission = Atomic.make Open
    ; close_mutex = Eio.Mutex.create ()
    ; connections = []
    ; daemon_released = false
    ; failure = None
    }
  in
  Eio.Switch.on_release sw (fun () ->
    match close_owned t with
    | Ok () -> ()
    | Error failure -> record_failure t failure |> raise_scope_failure);
  t
;;

let adopt_connection_exn t connection =
  match Atomic.get t.admission with
  | Open when List.is_empty t.connections -> t.connections <- [ connection ]
  | Open | Closing | Closed ->
    failwith "embedded connection ownership has already transferred"
;;

let adopt_additional_connection t ~create =
  match Atomic.get t.admission with
  | Closing | Closed ->
    Error
      (Agent_protocol.Error.create
         Interrupted
         ~message:"embedded host is closed"
         ~retryable:false
         ())
  | Open ->
    (* The trusted constructor is non-yielding; creation and adoption cannot race
       admission closure. No newly created owner exists on rejection. *)
    let connection = create () in
    t.connections <- connection :: t.connections;
    Ok (Connection_owner.connection connection)
;;

let close t =
  match close_owned t with
  | Ok () -> ()
  | Error failure ->
    let failure = record_failure t failure in
    (match Diagnostic.exception_and_backtrace (Failure.primary failure) with
     | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
     | None -> raise_scope_failure failure)
;;

let cleanup_after_failure t primary =
  let retain cleanup =
    let failure : Failure.t = { primary; cleanup } in
    t.failure <- Some failure;
    let backtrace =
      match Diagnostic.exception_and_backtrace primary with
      | Some (_, backtrace) -> backtrace
      | None ->
        (match Diagnostic.exception_and_backtrace cleanup with
         | Some (_, backtrace) -> backtrace
         | None -> Stdlib.Printexc.get_raw_backtrace ())
    in
    Eio.Switch.fail ~bt:backtrace t.sw (Cleanup_failed failure)
  in
  match close_owned t with
  | Ok () -> ()
  | Error failure -> retain (Failure.cleanup failure)
  | exception exn -> retain (Diagnostic.raised exn (Stdlib.Printexc.get_raw_backtrace ()))
;;

let protect t f =
  match f () with
  | Ok _ as success -> success
  | Error error as failure ->
    cleanup_after_failure t (Diagnostic.rejected error);
    failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    cleanup_after_failure t (Diagnostic.raised exn backtrace);
    Exn.raise_with_original_backtrace exn backtrace
;;
