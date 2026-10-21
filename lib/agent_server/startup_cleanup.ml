open! Core

module Failure = struct
  type t =
    { primary : Session_registry.Cleanup_failure.t
    ; cleanup : Session_registry.Cleanup_failure.t
    }

  let primary t = t.primary
  let cleanup t = t.cleanup
end

exception Cleanup_failed of Failure.t

type t =
  { store : Agent_store.Session_store.t
  ; mutable registry : Session_registry.t option
  ; mutable operator : Provider_operator_port.t option
  ; mutable authoring : Authoring_installation.t option
  ; mutable developer_events_close :
      (unit -> (unit, Agent_protocol.Error.t) result) option
  ; mutable developer_close : (unit -> (unit, Agent_protocol.Error.t) result) option
  ; mutable job_scheduler : Job_scheduler.t option
  ; mutable start_scheduler : Start_scheduler.t option
  ; mutable registry_closed : bool
  ; mutable operator_closed : bool
  ; mutable authoring_closed : bool
  ; mutable developer_events_closed : bool
  ; mutable developer_closed : bool
  ; mutable job_scheduler_closed : bool
  ; mutable start_scheduler_closed : bool
  ; mutable store_closed : bool
  ; mutable active : bool
  ; mutable failure : Failure.t option
  }

let close_remaining t =
  let open Result.Let_syntax in
  let%bind () =
    if t.developer_closed
    then Ok ()
    else (
      match t.developer_close with
      | None ->
        t.developer_closed <- true;
        Ok ()
      | Some close_and_wait ->
        close_and_wait () |> Result.map ~f:(fun () -> t.developer_closed <- true))
  in
  let%bind () =
    if t.job_scheduler_closed
    then Ok ()
    else (
      match t.job_scheduler with
      | None ->
        t.job_scheduler_closed <- true;
        Ok ()
      | Some scheduler ->
        Job_scheduler.close_and_wait scheduler
        |> Result.map ~f:(fun () -> t.job_scheduler_closed <- true))
  in
  if not t.start_scheduler_closed
  then (
    Option.iter t.start_scheduler ~f:Start_scheduler.close_and_wait;
    t.start_scheduler_closed <- true);
  if not t.registry_closed
  then (
    Option.iter t.registry ~f:Session_registry.shutdown;
    t.registry_closed <- true);
  if not t.operator_closed
  then (
    Option.iter t.operator ~f:Provider_operator_port.close;
    t.operator_closed <- true);
  let%bind () =
    if t.authoring_closed
    then Ok ()
    else (
      match t.authoring with
      | None ->
        t.authoring_closed <- true;
        Ok ()
      | Some authoring ->
        Authoring_installation.close_and_wait authoring
        |> Result.map ~f:(fun () -> t.authoring_closed <- true))
  in
  if t.store_closed
  then Ok ()
  else
    Agent_store.Session_store.close t.store
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> Result.map ~f:(fun () -> t.store_closed <- true)
;;

let close_events t =
  if t.developer_events_closed
  then Ok ()
  else (
    match t.developer_events_close with
    | None ->
      t.developer_events_closed <- true;
      Ok ()
    | Some close_and_wait ->
      close_and_wait () |> Result.map ~f:(fun () -> t.developer_events_closed <- true))
;;

let close t =
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let%bind () = close_events t in
    let%map () = close_remaining t in
    t.active <- false)
;;

let raise_failure failure =
  match
    Session_registry.Cleanup_failure.exception_and_backtrace (Failure.cleanup failure)
  with
  | None -> raise (Cleanup_failed failure)
  | Some (_, backtrace) ->
    Exn.raise_with_original_backtrace (Cleanup_failed failure) backtrace
;;

let create ~sw ~store =
  let t =
    { store
    ; registry = None
    ; operator = None
    ; authoring = None
    ; developer_events_close = None
    ; developer_close = None
    ; job_scheduler = None
    ; start_scheduler = None
    ; registry_closed = false
    ; operator_closed = false
    ; authoring_closed = false
    ; developer_events_closed = false
    ; developer_closed = false
    ; job_scheduler_closed = false
    ; start_scheduler_closed = false
    ; store_closed = false
    ; active = true
    ; failure = None
    }
  in
  Eio.Switch.on_release sw (fun () ->
    if t.active
    then (
      let report cleanup =
        let failure =
          match t.failure with
          | Some failure -> { failure with cleanup }
          | None -> { Failure.primary = cleanup; cleanup }
        in
        t.failure <- Some failure;
        raise_failure failure
      in
      match close t with
      | Ok () -> ()
      | Error error -> report (Session_registry.Cleanup_failure.rejected error)
      | exception exn ->
        report
          (Session_registry.Cleanup_failure.raised
             exn
             (Stdlib.Printexc.get_raw_backtrace ()))));
  t
;;

let adopt_registry_exn t registry =
  if (not t.active) || t.registry_closed || Option.is_some t.registry
  then failwith "startup registry ownership has already transferred";
  t.registry <- Some registry
;;

let adopt_operator_exn t operator =
  if (not t.active) || t.operator_closed || Option.is_some t.operator
  then failwith "startup operator ownership has already transferred";
  t.operator <- Some operator
;;

let adopt_authoring_exn t authoring =
  if (not t.active) || t.authoring_closed || Option.is_some t.authoring
  then failwith "startup authoring ownership has already transferred";
  t.authoring <- Some authoring
;;

let adopt_developer_events_exn t ~close_and_wait =
  if
    (not t.active) || t.developer_events_closed || Option.is_some t.developer_events_close
  then failwith "startup developer event ownership has already transferred";
  t.developer_events_close <- Some close_and_wait
;;

let adopt_developer_exn t ~close_and_wait =
  if (not t.active) || t.developer_closed || Option.is_some t.developer_close
  then failwith "startup developer ownership has already transferred";
  t.developer_close <- Some close_and_wait
;;

let adopt_job_scheduler_exn t scheduler =
  if (not t.active) || t.job_scheduler_closed || Option.is_some t.job_scheduler
  then failwith "startup job scheduler ownership has already transferred";
  t.job_scheduler <- Some scheduler
;;

let adopt_start_scheduler_exn t scheduler =
  if (not t.active) || t.start_scheduler_closed || Option.is_some t.start_scheduler
  then failwith "startup start scheduler ownership has already transferred";
  t.start_scheduler <- Some scheduler
;;

let cleanup_after_failure t primary =
  let retain cleanup = t.failure <- Some { Failure.primary; cleanup } in
  match close t with
  | Ok () -> ()
  | Error error -> retain (Session_registry.Cleanup_failure.rejected error)
  | exception exn ->
    retain
      (Session_registry.Cleanup_failure.raised exn (Stdlib.Printexc.get_raw_backtrace ()))
;;

let protect t f =
  match f () with
  | Ok _ as success ->
    t.active <- false;
    success
  | Error error as failure ->
    cleanup_after_failure t (Session_registry.Cleanup_failure.rejected error);
    failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    cleanup_after_failure t (Session_registry.Cleanup_failure.raised exn backtrace);
    Exn.raise_with_original_backtrace exn backtrace
;;

let release_operator_on_scope_exit t =
  if (not t.active) && not t.operator_closed
  then (
    Option.iter t.operator ~f:Provider_operator_port.close;
    t.operator_closed <- true)
;;
