open! Core
module P = Agent_protocol

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

type phase =
  | Open of int64
  | Closed

type t =
  { owner : Owner.t
  ; principal_id : P.Id.Principal.t
  ; request : P.Run_start.t
  ; request_sha256 : string
  ; authorize : Session_state.t -> (unit, P.Error.t) result
  ; mutable phase : phase
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let belongs_to t ~owner = phys_equal t.owner owner
let close t = t.phase <- Closed
let request t = t.request
let principal_id t = t.principal_id
let request_sha256 t = t.request_sha256

let digest_valid digest =
  String.length digest = 64
  && String.for_all digest ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let create
      ~owner
      ~(state : Session_state.t)
      ~principal_id
      ~(request : P.Run_start.t)
      ~request_sha256
      ~authorize
  =
  let open Result.Let_syntax in
  let%bind () = authorize state in
  if not (digest_valid request_sha256)
  then Error (P.Error.invalid_request "run preparation requires a SHA-256 digest")
  else if
    (not (P.Id.Session.equal request.session_id state.identity.session_id))
    || (not (Int.equal request.generation state.identity.generation))
    || not (Int64.equal request.expected_revision state.counters.revision)
  then conflict "run preparation original generation or revision changed"
  else
    Ok
      { owner
      ; principal_id
      ; request
      ; request_sha256
      ; authorize
      ; phase = Open state.counters.revision
      }
;;

let check_basis t ~owner ~(state : Session_state.t) =
  (* Resource identity is intentional: another actor for the same durable
     session cannot issue, advance or consume this process-local capability. *)
  if not (phys_equal owner t.owner)
  then conflict "run preparation belongs to another actor"
  else (
    match t.phase with
    | Closed -> conflict "run preparation is closed or invalidated"
    | Open revision ->
      if
        P.Id.Session.equal t.request.session_id state.identity.session_id
        && Int.equal t.request.generation state.identity.generation
        && Int64.equal revision state.counters.revision
      then Ok ()
      else conflict "run preparation was displaced by another state change")
;;

let check t ~owner ~state =
  let open Result.Let_syntax in
  let%bind () = check_basis t ~owner ~state in
  t.authorize state
;;

let advance t ~owner ~(previous : Session_state.t) ~(current : Session_state.t) =
  let result =
    let open Result.Let_syntax in
    let%bind () = check_basis t ~owner ~state:previous in
    if
      Int64.equal previous.counters.revision Int64.max_value
      || (not
            (Int64.equal
               current.counters.revision
               (Int64.succ previous.counters.revision)))
      || (not
            (P.Id.Session.equal previous.identity.session_id current.identity.session_id))
      || not (Int.equal previous.identity.generation current.identity.generation)
    then conflict "run preparation commit is not its next owned revision"
    else (
      t.phase <- Open current.counters.revision;
      Ok ())
  in
  Result.iter_error result ~f:(fun _ -> close t);
  result
;;

module Decision = struct
  type preparation = t

  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Prepare of preparation
end
