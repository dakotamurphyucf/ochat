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
  ; mutable registry_closed : bool
  ; mutable operator_closed : bool
  ; mutable store_closed : bool
  ; mutable active : bool
  ; mutable failure : Failure.t option
  }

let close t =
  Eio.Cancel.protect (fun () ->
    if not t.registry_closed
    then (
      Option.iter t.registry ~f:Session_registry.shutdown;
      t.registry_closed <- true);
    if not t.operator_closed
    then (
      Option.iter t.operator ~f:Provider_operator_port.close;
      t.operator_closed <- true);
    if t.store_closed
    then Ok ()
    else
      Agent_store.Session_store.close t.store
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      |> Result.map ~f:(fun () ->
        t.store_closed <- true;
        t.active <- false))
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
    ; registry_closed = false
    ; operator_closed = false
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
