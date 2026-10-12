open! Core
module P = Agent_protocol
module C = Agent_client

type t =
  { request : P.Run_query.Lookup_request.t
  ; admission : P.Run_receipt.t
  ; expected_mode : P.Run.Mode.t option
  ; views : C.Run_views.t
  ; domain : int
  ; changed : Eio.Condition.t
  ; mutable outcome : (P.Run_query.View.t option, P.Error.t) Result.t
  ; mutable observer : C.Projection.t -> unit
  ; mutable waiting : bool
  }

let error code message = P.Error.create code ~message ~retryable:false ()

let check_domain t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Managed_run_waiter must remain in its creating domain"
;;

let validate t (view : P.Run_query.View.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Session_ref.equal view.run.session t.request.session
      && P.Id.Run.equal view.run.id t.admission.run_id
      && P.Run_source.equal view.run.source t.admission.source
      && P.Id.Principal.equal view.run.principal_id t.admission.principal_id
      && Option.value_map
           t.expected_mode
           ~default:true
           ~f:(P.Run.Mode.equal view.run.mode)
    then Ok ()
    else Error (error Conflict "run evidence does not match the original admission")
  in
  let%map () =
    match view.admission_receipt with
    | Some original when P.Run_receipt.equal original t.admission -> Ok ()
    | Some _ -> Error (error Conflict "run admission receipt changed")
    | None -> Error (error Invalid_state "original run admission evidence is unavailable")
  in
  view
;;

let retain t outcome =
  check_domain t;
  (match t.outcome with
   | Error _ -> ()
   | Ok _ -> t.outcome <- outcome);
  Eio.Condition.broadcast t.changed
;;

let retain_query t = function
  | P.Run_query.Outcome.Unavailable _ ->
    retain t (Error (error Invalid_state "requested run evidence is unavailable"))
  | Available view -> retain t (validate t view |> Result.map ~f:Option.some)
;;

let create views ~sw ~request ~admission ~expected_mode =
  Eio.Fiber.check ();
  Eio.Switch.check sw;
  let t =
    { request
    ; admission
    ; expected_mode
    ; views
    ; domain = (Stdlib.Domain.self () :> int)
    ; changed = Eio.Condition.create ()
    ; outcome = Ok None
    ; observer = ignore
    ; waiting = false
    }
  in
  if not (P.Run_receipt.Kind.equal admission.kind Admission)
  then
    t.outcome
    <- Error (error Invalid_request "run observation requires admission receipt");
  t.observer
  <- C.Run_views.watch
       views
       ~sw
       ~request
       ~on_result:(retain_query t)
       ~on_error:(fun failure -> retain t (Error failure));
  t
;;

let observe t projection =
  check_domain t;
  let fields = P.Public.Snapshot.fields (C.Projection.snapshot projection) in
  if
    not
      (P.Id.Session.equal fields.session.id (P.Session_ref.session_id t.request.session))
  then retain t (Error (error Conflict "run observer received another session"))
  else t.observer projection;
  Eio.Condition.broadcast t.changed
;;

let current t handle =
  Eio.Fiber.check ();
  match t.outcome, C.Session_handle.last_error handle with
  | Error failure, _ | Ok _, Some failure -> Error failure
  | Ok _, None ->
    if C.Session_handle.is_closed handle
    then Error (error Interrupted "session reader closed before run delivery")
    else (
      let projection = C.Session_handle.projection handle in
      match C.Projection.synchronization projection with
      | Snapshot_required failure -> Error failure
      | Current -> Ok projection)
;;

let terminal t =
  match t.outcome with
  | Error failure -> Some (Error failure)
  | Ok None -> None
  | Ok (Some view) ->
    (match view.run.lifecycle with
     | Admitted | Active | Waiting _ -> None
     | Terminal _ ->
       (match view.terminal_receipt with
        | Some _ -> Some (Ok view)
        | None -> Some (Error (error Invalid_state "terminal run receipt is unavailable"))))
;;

let await_terminal t ~handle ~on_view =
  check_domain t;
  Eio.Fiber.check ();
  if
    not
      (P.Id.Session.equal
         (C.Session_handle.session_id handle)
         (P.Session_ref.session_id t.request.session))
  then Error (error Conflict "run waiter and handle belong to different sessions")
  else if t.waiting
  then Error (error Conflict "run waiter already has an active waiter")
  else (
    t.waiting <- true;
    Exn.protect
      ~finally:(fun () -> t.waiting <- false)
      ~f:(fun () ->
        observe t (C.Session_handle.projection handle);
        (match C.Run_views.lookup t.views t.request with
         | Ok outcome -> retain_query t outcome
         | Error failure -> retain t (Error failure));
        let last_view = ref None in
        let wait () =
          Eio.Condition.loop_no_mutex t.changed (fun () ->
            match current t handle with
            | Error failure -> Some (Error failure)
            | Ok projection ->
              if not (Option.exists !last_view ~f:(phys_equal projection))
              then (
                last_view := Some projection;
                on_view projection);
              (match current t handle with
               | Error failure -> Some (Error failure)
               | Ok fresh -> if phys_equal fresh projection then terminal t else None))
        in
        let closed () =
          C.Session_handle.await_closed handle;
          match current t handle with
          | Error failure -> Error failure
          | Ok _ -> Error (error Interrupted "session reader closed")
        in
        let%bind.Result _ = Eio.Fiber.first wait closed in
        (* The callback and observer may have yielded; only an actual fresh
           authorized lookup certifies final terminal delivery. *)
        let%bind.Result _ = current t handle in
        let%bind.Result outcome = C.Run_views.lookup t.views t.request in
        retain_query t outcome;
        let%bind.Result _ = current t handle in
        match terminal t with
        | Some result -> result
        | None -> Error (error Conflict "run terminal changed before delivery")))
;;
