open! Core
module P = Agent_protocol
module B = Agent_session.Runtime_builder

let conflict message = P.Error.create Conflict ~message ~retryable:true ()

module Failure = struct
  type t =
    | Protocol of P.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  let capture f =
    try Result.map_error (f ()) ~f:(fun error -> Protocol error) with
    | exn -> Error (Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  ;;

  let resume = function
    | Ok value -> Ok value
    | Error (Protocol error) -> Error error
    | Error (Raised (exn, backtrace)) -> Exn.raise_with_original_backtrace exn backtrace
  ;;
end

type slot =
  { domain : int
  ; mutable owner : t option
  }

and t =
  { slot : slot
  ; mutable phase : phase
  }

and phase =
  | Reserved
  | Allocated of Graph_tracking.t
  | Ready_to_finish of Graph_tracking.t
  | Closing of
      { graph : Graph_tracking.t
      ; result : (unit, Failure.t) result Eio.Promise.t
      }
  | Retained of Graph_tracking.t * Failure.t
  | Joined
  | Abandoned

let check_domain slot =
  if Int.equal slot.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else Error (P.Error.invalid_request "constructor graph belongs to another Eio domain")
;;

let require result =
  match result with
  | Ok value -> value
  | Error error -> invalid_arg error.P.Error.message
;;

let check t =
  let open Result.Let_syntax in
  let%bind () = check_domain t.slot in
  match t.phase with
  | Joined | Abandoned -> Ok ()
  | Reserved | Allocated _ | Ready_to_finish _ | Closing _ | Retained _ ->
    if Option.exists t.slot.owner ~f:(phys_equal t)
    then Ok ()
    else Error (conflict "constructor graph cleanup owner changed")
;;

let create () = { domain = (Stdlib.Domain.self () :> int); owner = None }

let pending slot =
  require (check_domain slot);
  Option.is_some slot.owner
;;

let reserve slot =
  let open Result.Let_syntax in
  let%bind () = check_domain slot in
  match slot.owner with
  | Some _ -> Error (conflict "previous constructor graph has not joined")
  | None ->
    let owner = { slot; phase = Reserved } in
    slot.owner <- Some owner;
    Ok owner
;;

let allocated t graph =
  require (check t);
  match t.phase with
  | Reserved -> t.phase <- Allocated graph
  | Allocated _ | Ready_to_finish _ | Closing _ | Retained _ | Joined | Abandoned ->
    invalid_arg "constructor graph was already transferred or retired"
;;

let inference t =
  require (check t);
  match t.phase with
  | Allocated graph ->
    { B.Pending_legacy_resume.Inference.identity = Graph_tracking.identity graph
    ; bind_identity = (fun allocate -> Graph_tracking.bind_identity graph ~allocate)
    ; on_attempt = Graph_tracking.on_attempt graph
    ; on_observation = Graph_tracking.on_observation graph
    ; on_completion = Graph_tracking.on_completion graph
    }
  | Reserved | Ready_to_finish _ | Closing _ | Retained _ | Joined | Abandoned ->
    invalid_arg "constructor inference requires its actual active graph"
;;

let abandon_unallocated t =
  require (check t);
  match t.phase with
  | Reserved ->
    t.phase <- Abandoned;
    t.slot.owner <- None
  | Abandoned -> ()
  | Allocated _ | Ready_to_finish _ | Closing _ | Retained _ | Joined ->
    invalid_arg "allocated constructor graph cannot be abandoned"
;;

let callback_exited t =
  require (check t);
  match t.phase with
  | Allocated graph -> t.phase <- Ready_to_finish graph
  | Ready_to_finish _ | Closing _ | Retained _ | Joined | Abandoned -> ()
  | Reserved -> invalid_arg "constructor creation has not returned its graph"
;;

let finish_graph graph =
  (* Attempt both factual legs even if seal fails; positive finish remains
     memoized in Graph_tracking and can be acknowledged on explicit retry. *)
  let sealed = Failure.capture (fun () -> Graph_tracking.seal graph) in
  let finished = Failure.capture (fun () -> Graph_tracking.finish graph) in
  match sealed with
  | Error _ -> sealed
  | Ok () -> finished
;;

let finish t =
  let open Result.Let_syntax in
  let%bind () = check t in
  Eio.Cancel.protect (fun () ->
    match t.phase with
    | Joined | Abandoned -> Ok ()
    | Reserved | Allocated _ -> Error (conflict "constructor callback has not joined")
    | Closing { result; _ } -> Eio.Promise.await result |> Failure.resume
    | Ready_to_finish graph | Retained (graph, _) ->
      let result, resolver = Eio.Promise.create () in
      t.phase <- Closing { graph; result };
      let outcome = finish_graph graph in
      (match outcome with
       | Ok () ->
         t.phase <- Joined;
         t.slot.owner <- None
       | Error failure -> t.phase <- Retained (graph, failure));
      Eio.Promise.resolve resolver outcome;
      Failure.resume outcome)
;;

let close_pending slot =
  let open Result.Let_syntax in
  let%bind () = check_domain slot in
  match slot.owner with
  | None -> Ok ()
  | Some owner -> finish owner
;;
