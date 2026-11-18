open! Core
module P = Agent_protocol

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

type phase =
  | Open
  | Closed

type t =
  { owner : Owner.t
  ; session_id : P.Id.Session.t
  ; generation : int
  ; mutable phase : phase
  ; mutable revision : int64
  ; mutable constructor_jobs : P.Id.Job.t list
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let capture ~owner (state : Session_state.t) =
  { owner
  ; session_id = state.identity.session_id
  ; generation = state.identity.generation
  ; phase = Open
  ; revision = state.counters.revision
  ; constructor_jobs = []
  }
;;

let belongs_to t ~owner = phys_equal t.owner owner
let acknowledged_revision t = t.revision

let is_closed t =
  match t.phase with
  | Open -> false
  | Closed -> true
;;

let close t =
  t.phase <- Closed;
  t.constructor_jobs <- []
;;

let check_basis t ~owner ~(state : Session_state.t) =
  (* Resource identity is intentional: another actor for the same durable
       session cannot issue, advance or consume this process-local capability. *)
  if not (phys_equal owner t.owner)
  then conflict "run preparation belongs to another actor"
  else (
    match t.phase with
    | Closed -> conflict "run preparation is closed or invalidated"
    | Open ->
      if
        P.Id.Session.equal t.session_id state.identity.session_id
        && Int.equal t.generation state.identity.generation
        && Int64.equal t.revision state.counters.revision
      then Ok ()
      else conflict "run preparation was displaced by another state change")
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
      t.revision <- current.counters.revision;
      Ok ())
  in
  Result.iter_error result ~f:(fun _ -> close t);
  result
;;

module Constructor_jobs = struct
  type tracking = t

  type t =
    { preparation : tracking
    ; revision : int64
    ; basis : P.Id.Job.t list
    ; candidate : P.Id.Job.t list
    }

  let retained preparation (state : Session_state.t) =
    List.filter preparation.constructor_jobs ~f:(fun id ->
      List.exists state.jobs ~f:(fun job ->
        P.Id.Job.equal id job.P.Job.id
        && Int.equal job.generation state.identity.generation))
  ;;

  let prepare_add preparation ~owner ~(state : Session_state.t) ~(job : P.Job.t) =
    let open Result.Let_syntax in
    let%bind () = check_basis preparation ~owner ~state in
    let%bind _ = P.Id.Job.of_string (P.Id.Job.to_string job.id) in
    let%bind () =
      if
        P.Id.Session.equal job.session_id state.identity.session_id
        && Int.equal job.generation state.identity.generation
        && (not
              (List.exists state.jobs ~f:(fun previous ->
                 P.Id.Job.equal previous.id job.id)))
        && not (List.mem preparation.constructor_jobs job.id ~equal:P.Id.Job.equal)
      then Ok ()
      else conflict "constructor Job bookkeeping requires a fresh owned identity"
    in
    let%map () =
      match job.status with
      | Queued when Int.equal job.attempt 0 -> Ok ()
      | Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ ->
        conflict "constructor Job bookkeeping requires queued attempt zero"
    in
    { preparation
    ; revision = state.counters.revision
    ; basis = preparation.constructor_jobs
    ; candidate = job.id :: retained preparation state
    }
  ;;

  let prepare_handoff preparation ~owner ~(state : Session_state.t) =
    let open Result.Let_syntax in
    let%map () = check_basis preparation ~owner ~state in
    ( retained preparation state
    , { preparation
      ; revision = state.counters.revision
      ; basis = preparation.constructor_jobs
      ; candidate = []
      } )
  ;;

  let check_before_commit t ~owner ~(state : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = check_basis t.preparation ~owner ~state in
    if
      Int64.equal state.counters.revision t.revision
      && phys_equal t.basis t.preparation.constructor_jobs
    then Ok ()
    else conflict "constructor Job selection changed before its owner commit"
  ;;

  let acknowledge t =
    match t.preparation.phase with
    | Closed -> ()
    | Open -> t.preparation.constructor_jobs <- t.candidate
  ;;
end
