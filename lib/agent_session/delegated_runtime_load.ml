open! Core
module P = Agent_protocol

module Custody = struct
  type t =
    | Host of Operator_authorization.t option
    | Prepared_run of Run_preparation.t
end

type t =
  { custody : Custody.t
  ; tracking : Preparation_tracking.t
  ; mutable closed : bool
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let tracking t = t.tracking

let initial_preparation t =
  match t.custody with
  | Host _ -> None
  | Prepared_run preparation -> Some preparation
;;

let original_actor t =
  if t.closed
  then None
  else (
    match t.custody with
    | Host actor -> actor
    | Prepared_run preparation -> Run_preparation.original_actor preparation)
;;

let check_basis t ~owner ~state =
  if t.closed
  then conflict "delegated runtime load is closed"
  else Preparation_tracking.check_basis t.tracking ~owner ~state
;;

let check_current t ~owner ~state =
  let open Result.Let_syntax in
  let%bind () = check_basis t ~owner ~state in
  match t.custody with
  | Prepared_run preparation -> Run_preparation.check_current preparation ~owner ~state
  | Host None -> Ok ()
  | Host (Some actor) ->
    if Operator_authorization.is_current actor
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"runtime initialization authentication is no longer current"
           ~retryable:false
           ())
;;

let check_authority t ~owner ~state =
  let open Result.Let_syntax in
  let%bind () = check_basis t ~owner ~state in
  match t.custody with
  | Prepared_run preparation -> Run_preparation.check preparation ~owner ~state
  | Host (Some _ | None) -> check_current t ~owner ~state
;;

let capture ~owner ~(state : Session_state.t) ~(custody : Custody.t) =
  let open Result.Let_syntax in
  let%bind () =
    match state.spec.delegation with
    | Some _ -> Ok ()
    | None -> conflict "ordinary delegated runtime load requires its actual child"
  in
  let tracking =
    match custody with
    | Host (Some _ | None) -> Preparation_tracking.capture ~owner state
    | Prepared_run preparation ->
      Run_preparation.Custody.tracking (Run_preparation.custody preparation)
  in
  let result = { custody; tracking; closed = false } in
  let%map () = check_authority result ~owner ~state in
  result
;;

let close t =
  t.closed <- true;
  match t.custody with
  | Host (Some _ | None) -> Preparation_tracking.close t.tracking
  | Prepared_run _ -> ()
;;

module Source_binding = struct
  type preparation = t

  type phase =
    | Prepared
    | Consumed
    | Closed

  type t =
    { preparation : preparation
    ; source : Runtime_source_preparation.t
    ; mutable phase : phase
    }

  let preparation t = t.preparation
  let source t = t.source

  let check_basis t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () =
      match t.phase with
      | Prepared | Consumed -> Ok ()
      | Closed -> conflict "delegated runtime source is closed"
    in
    let%bind () = check_basis t.preparation ~owner ~state in
    Runtime_source_preparation.validate_target t.source state
  ;;

  let bind preparation ~owner ~state ~source =
    let open Result.Let_syntax in
    let%bind () = check_current preparation ~owner ~state in
    let%map () = Runtime_source_preparation.validate_target source state in
    { preparation; source; phase = Prepared }
  ;;

  let consume t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = check_basis t ~owner ~state in
    match t.phase with
    | Prepared ->
      t.phase <- Consumed;
      Ok ()
    | Consumed | Closed -> conflict "delegated runtime source was already consumed"
  ;;

  let close t = t.phase <- Closed
end
