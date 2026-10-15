open! Core
module P = Agent_protocol
module C = Chat_response.Tool_capability
module Ops = Chat_response.Background_job_operations

type phase =
  | Open
  | Prepared
  | Committed

type t =
  { mutable selected : C.t
  ; mutable current_capabilities : unit -> C.t
  ; abort : P.Id.Job.t -> unit
  ; active : bool Atomic.t
  ; mutable issued : P.Id.Job.t list
  ; mutable ever_issued : bool
  ; mutable selection_rebound : bool
  ; mutable control : Chatml.Chatml_lang.execution_control option
  ; mutable phase : phase
  }

let live t = Atomic.get t.active

let selectable t =
  if not (live t)
  then Error "background script scope has ended"
  else (
    match t.phase with
    | Open | Prepared -> Ok ()
    | Committed -> Error "background script scope has ended")
;;

let check_selection t =
  let open Result.Let_syntax in
  let%bind () = selectable t in
  let names = List.map (C.references t.selected) ~f:(fun reference -> reference.C.name) in
  let%bind current =
    C.select (t.current_capabilities ()) ~names
    |> Result.map_error ~f:(fun error -> error.C.message)
  in
  let%bind () = selectable t in
  if String.equal (C.fingerprint current) (C.fingerprint t.selected)
  then Ok ()
  else Error "background tool selection changed"
;;

let check_open t =
  let open Result.Let_syntax in
  let%bind () = check_selection t in
  match t.phase with
  | Open -> Ok ()
  | Prepared | Committed -> Error "background script scope has ended"
;;

let rebind_selected t ~selected ~current_capabilities =
  let open Result.Let_syntax in
  let%bind () = check_open t in
  if t.selection_rebound || t.ever_issued
  then Error "background selection is already bound to started work"
  else (
    let names = List.map (C.references selected) ~f:(fun reference -> reference.C.name) in
    let%bind current =
      C.select (current_capabilities ()) ~names
      |> Result.map_error ~f:(fun error -> error.C.message)
    in
    if not (String.equal (C.fingerprint current) (C.fingerprint selected))
    then Error "managed dependency selection changed"
    else if (not (live t)) || t.selection_rebound || t.ever_issued
    then Error "background selection is already bound to started work"
    else (
      match t.phase with
      | Prepared | Committed -> Error "background script scope has ended"
      | Open ->
        t.selected <- selected;
        t.current_capabilities <- current_capabilities;
        t.selection_rebound <- true;
        Ok ()))
;;

let checkpoint t =
  Eio.Fiber.check ();
  Option.iter t.control ~f:(fun control -> control.checkpoint ());
  Eio.Fiber.check ()
;;

let cleanup t =
  let issued = t.issued in
  t.issued <- [];
  let first = ref None in
  Eio.Cancel.protect (fun () ->
    List.iter issued ~f:(fun id ->
      try t.abort id with
      | exn ->
        if Option.is_none !first
        then first := Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
  !first
;;

let raise_cleanup = function
  | None -> ()
  | Some (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_scope ~selected ~current_capabilities ~abort ~error f =
  let t =
    { selected
    ; current_capabilities
    ; abort
    ; active = Atomic.make true
    ; issued = []
    ; ever_issued = false
    ; selection_rebound = false
    ; control = None
    ; phase = Open
    }
  in
  let outcome =
    try
      let result =
        match check_open t with
        | Error message -> Error (error message)
        | Ok () -> f t
      in
      let result =
        match result, t.phase with
        | Error _, (Open | Prepared | Committed) -> result
        | Ok value, Open ->
          Eio.Fiber.yield ();
          checkpoint t;
          Result.map (check_open t) ~f:(fun () -> value) |> Result.map_error ~f:error
        | Ok _, Prepared -> Error (error "background commit was not acknowledged")
        | Ok _, Committed -> result
      in
      `Returned result
    with
    | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  Atomic.set t.active false;
  t.control <- None;
  match outcome with
  | `Raised (exn, backtrace) ->
    (try ignore (cleanup t : (exn * Stdlib.Printexc.raw_backtrace) option) with
     | _ -> ());
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | `Returned (Error error) ->
    raise_cleanup (cleanup t);
    Error error
  | `Returned (Ok _ as result) -> result
;;

let install ?control t ~handlers config =
  if not (live t)
  then invalid_arg "background script scope has ended"
  else (
    t.control <- control;
    Ops.install ?control ~handlers config)
;;

let record_start t id =
  let open Result.Let_syntax in
  let%bind () = check_open t in
  if List.mem t.issued id ~equal:P.Id.Job.equal
  then Error "background stage returned a duplicate job ID"
  else (
    t.ever_issued <- true;
    t.issued <- id :: t.issued;
    Ok ())
;;

let rollback_start t id =
  if List.mem t.issued id ~equal:P.Id.Job.equal
  then (
    t.issued <- List.filter t.issued ~f:(fun other -> not (P.Id.Job.equal id other));
    Eio.Cancel.protect (fun () -> t.abort id))
;;

let validate_work t = function
  | P.Invocation.Subscription _ -> Error "subscription service is not installed"
  | Job id ->
    let open Result.Let_syntax in
    let%bind () = check_open t in
    if List.mem t.issued id ~equal:P.Id.Job.equal
    then Ok ()
    else Error "pending job was not started by this invocation"
;;

let split_starts t effects =
  let open Result.Let_syntax in
  let%bind () = check_open t in
  let%bind ids, ordinary = Ops.split_starts effects in
  if List.for_all ids ~f:(fun id -> List.mem t.issued id ~equal:P.Id.Job.equal)
  then Ok (ids, ordinary)
  else Error "returned start belongs to another background script scope"
;;

let mark_prepared t =
  let open Result.Let_syntax in
  let%map () = check_open t in
  t.phase <- Prepared;
  fun () ->
    if
      (not (live t))
      ||
      match t.phase with
      | Open | Committed -> true
      | Prepared -> false
    then invalid_arg "background commit acknowledgment is late or repeated"
    else (
      t.phase <- Committed;
      t.issued <- [])
;;
