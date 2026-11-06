open! Core
module P = Agent_protocol

type phase =
  | Prepared
  | Acknowledged
  | Finished
  | Closed_before_writer

type 'plan t =
  { owner : Preparation_tracking.Owner.t
  ; before : Session_state.t
  ; candidate : Session_transition.t
  ; operation : P.Operation.t
  ; remaining_audit : Document_schema.Document.t option
  ; commit_attempted : bool ref
  ; plan : 'plan
  ; mutable phase : phase
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Acknowledgement = struct
  type 'plan preparation = 'plan t
  type 'plan t = { preparation : 'plan preparation }
end

module Closed_before_writer = struct
  (* Retains the SAME physical live preparation. There is deliberately no
     serializer, decoder, public projection or fresh preparation constructor. *)
  type 'plan preparation = 'plan t
  type t = Closed : 'plan preparation -> t
end

let operation_matches (operation : P.Operation.t) (state : Session_state.t) =
  Option.exists state.active_operation ~f:(fun actual ->
    Jsonaf.exactly_equal (P.Operation.to_json operation) (P.Operation.to_json actual))
;;

let capture
      ~owner
      ~(before : Session_state.t)
      ~(candidate : Session_transition.t)
      ~(operation : P.Operation.t)
      ~remaining_audit
      ~commit_attempted
      ~plan
  =
  let open Result.Let_syntax in
  let%bind () =
    match operation.kind with
    | Turn _ -> Ok ()
    | Compaction -> conflict "Turn staging does not own a Compaction"
  in
  let%map () =
    if
      P.Id.Session.equal before.identity.session_id candidate.state.identity.session_id
      && Int.equal before.identity.generation candidate.state.identity.generation
      && Int.equal operation.generation before.identity.generation
      && (not (Int64.equal before.counters.revision Int64.max_value))
      && Int64.equal
           candidate.state.counters.revision
           (Int64.succ before.counters.revision)
      && operation_matches operation candidate.state
    then Ok ()
    else conflict "Turn staging requires its exact next candidate and Operation"
  in
  { owner
  ; before
  ; candidate
  ; operation
  ; remaining_audit
  ; commit_attempted
  ; plan
  ; phase = Prepared
  }
;;

let candidate t = t.candidate
let remaining_audit t = t.remaining_audit
let commit_attempted t = t.commit_attempted

let check_before_writer t ~owner ~state =
  if not (phys_equal owner t.owner)
  then conflict "Turn staging belongs to another Actor"
  else (
    match t.phase with
    | Acknowledged | Finished | Closed_before_writer ->
      conflict "Turn staging is no longer prepared"
    | Prepared ->
      if phys_equal state t.before
      then Ok ()
      else conflict "Turn staging original candidate was displaced")
;;

let acknowledge t ~owner ~state =
  if not (phys_equal owner t.owner)
  then conflict "Turn acknowledgement belongs to another Actor"
  else (
    match t.phase with
    | Finished | Closed_before_writer ->
      conflict "Turn acknowledgement was already consumed or closed"
    | Prepared | Acknowledged ->
      if phys_equal state t.candidate.state && operation_matches t.operation state
      then (
        t.phase <- Acknowledged;
        Ok ({ preparation = t } : _ Acknowledgement.t))
      else conflict "Turn acknowledgement differs from the actual installed candidate")
;;

let finish (acknowledgement : _ Acknowledgement.t) ~owner ~state =
  let t = acknowledgement.preparation in
  if not (phys_equal owner t.owner)
  then conflict "Turn suffix belongs to another Actor"
  else (
    match t.phase with
    | Prepared | Finished | Closed_before_writer ->
      conflict "Turn suffix has no unconsumed actual acknowledgement"
    | Acknowledged ->
      if phys_equal state t.candidate.state && operation_matches t.operation state
      then (
        t.phase <- Finished;
        Ok t.plan)
      else conflict "Turn suffix differs from the actual acknowledged candidate")
;;

let seal_before_writer t ~owner =
  if not (phys_equal owner t.owner)
  then conflict "Turn preparation belongs to another Actor"
  else (
    match t.phase with
    | Acknowledged | Finished | Closed_before_writer ->
      conflict "Turn preparation cannot be sealed after acknowledgement or close"
    | Prepared ->
      if !(t.commit_attempted)
      then conflict "Turn preparation has an actual writer attempt"
      else (
        t.phase <- Closed_before_writer;
        Ok (Closed_before_writer.Closed t)))
;;
