open! Core
module P = Agent_protocol

type t =
  { session_id : P.Id.Session.t
  ; domain_id : int
  ; changed : Eio.Condition.t
  ; foreign_observation : P.Error.t
  ; mutable failure : P.Error.t option
  ; mutable waiting : bool
  }

let error code message = P.Error.create code ~message ~retryable:true ()

let create session_id =
  { session_id
  ; domain_id = (Stdlib.Domain.self () :> int)
  ; changed = Eio.Condition.create ()
  ; foreign_observation = error Conflict "completion view observed another session"
  ; failure = None
  ; waiting = false
  }
;;

let check_domain t =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Completion_view must remain in its creating domain"
;;

let observe t projection =
  check_domain t;
  let fields = P.Public.Snapshot.fields (Projection.snapshot projection) in
  if not (P.Id.Session.equal t.session_id fields.session.id)
  then t.failure <- Some t.foreign_observation;
  Eio.Condition.broadcast t.changed
;;

let check_handle t handle =
  Eio.Fiber.check ();
  match t.failure, Session_handle.last_error handle with
  | Some failure, _ | None, Some failure -> Error failure
  | None, None ->
    if Session_handle.is_closed handle
    then Error (error Interrupted "session reader closed before completion delivery")
    else (
      let projection = Session_handle.projection handle in
      Eio.Fiber.check ();
      match Projection.synchronization projection with
      | Snapshot_required failure -> Error failure
      | Current -> Ok projection)
;;

let terminal projection ~operation ~after =
  let fields = P.Public.Snapshot.fields (Projection.snapshot projection) in
  if
    Int64.(fields.revision < after.P.Mutation_result.revision)
    || Int64.(fields.latest_event_sequence < after.latest_event_sequence)
  then None
  else (
    match fields.session.active_operation with
    | Some active ->
      if P.Id.Operation.equal active.id operation
      then None
      else Some (Error (error Conflict "another operation superseded the requested turn"))
    | None ->
      (match Projection.terminal_operation projection with
       | Some actual ->
         if not (P.Id.Operation.equal actual.id operation)
         then
           Some (Error (error Conflict "another terminal superseded the requested turn"))
         else (
           match actual.state with
           | Completed | Failed _ | Cancelled | Interrupted _ -> Some (Ok projection)
           | Starting | Running | Cancelling ->
             Some
               (Error (error Invalid_state "recorded terminal operation is not terminal")))
       | None ->
         Some
           (Error
              (error
                 Snapshot_required
                 "current session view has no observed terminal for the requested turn"))))
;;

let await_terminal t ~handle ~operation ~after ~on_view =
  check_domain t;
  Eio.Fiber.check ();
  if not (P.Id.Session.equal t.session_id (Session_handle.session_id handle))
  then Error (error Conflict "completion view and handle belong to different sessions")
  else if
    Int64.(after.P.Mutation_result.revision < 0L || after.latest_event_sequence < 0L)
  then Error (P.Error.invalid_request "completion command position must be nonnegative")
  else if t.waiting
  then Error (error Conflict "completion view already has a waiter")
  else (
    t.waiting <- true;
    Exn.protect
      ~finally:(fun () -> t.waiting <- false)
      ~f:(fun () ->
        let last_view = ref None in
        let await_observation () =
          Eio.Condition.loop_no_mutex t.changed (fun () ->
            match check_handle t handle with
            | Error failure -> Some (Error failure)
            | Ok projection ->
              (* Identity suppresses duplicate presentation of the same immutable
                 handle-owned view; it is never an admission/terminal proof. *)
              if not (Option.exists !last_view ~f:(phys_equal projection))
              then (
                last_view := Some projection;
                on_view projection);
              (* A yielding callback may have allowed another durable event,
                 reader failure or cancellation. Never deliver the older view. *)
              (match check_handle t handle with
               | Error failure -> Some (Error failure)
               | Ok current ->
                 if phys_equal projection current
                 then terminal current ~operation ~after
                 else None))
        in
        let await_reader () =
          Session_handle.await_closed handle;
          match check_handle t handle with
          | Error failure -> Error failure
          | Ok _ -> Error (error Interrupted "session reader closed")
        in
        match Eio.Fiber.first await_observation await_reader with
        | Error failure ->
          Eio.Fiber.check ();
          Error failure
        | Ok _ ->
          (match check_handle t handle with
           | Error failure -> Error failure
           | Ok current ->
             (match terminal current ~operation ~after with
              | Some result -> result
              | None ->
                Error (error Conflict "requested terminal changed before delivery")))))
;;
