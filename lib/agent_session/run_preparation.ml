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
  ; original_actor : Operator_authorization.t option
  ; principal_id : P.Id.Principal.t
  ; request : Run_admission_request.t
  ; request_sha256 : string
  ; authorize : Session_state.t -> (unit, P.Error.t) result
  ; mutable phase : phase
  ; mutable constructor_jobs : P.Id.Job.t list
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let belongs_to t ~owner = phys_equal t.owner owner

let close t =
  t.phase <- Closed;
  t.constructor_jobs <- []
;;

let request t = t.request
let principal_id t = t.principal_id

let original_actor t =
  match t.phase with
  | Open _ -> t.original_actor
  | Closed -> None
;;

let request_sha256 t = t.request_sha256

let digest_valid digest =
  String.length digest = 64
  && String.for_all digest ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let create_request
      ~owner
      ~(state : Session_state.t)
      ~principal_id
      ~(request : Run_admission_request.t)
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
      ; original_actor = None
      ; principal_id
      ; request
      ; request_sha256
      ; authorize
      ; phase = Open state.counters.revision
      ; constructor_jobs = []
      }
;;

let create_authenticated_request ~owner ~state ~actor ~request ~request_sha256 ~authorize =
  let open Result.Let_syntax in
  let%map preparation =
    create_request
      ~owner
      ~state
      ~principal_id:(Operator_authorization.principal actor).id
      ~request
      ~request_sha256
      ~authorize:(Run_command_authorization.check actor ~authorize)
  in
  { preparation with original_actor = Some actor }
;;

let create ~owner ~state ~principal_id ~request ~request_sha256 ~authorize =
  create_request
    ~owner
    ~state
    ~principal_id
    ~request:(Run_admission_request.of_writer request)
    ~request_sha256
    ~authorize
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

module Constructor_jobs = struct
  type preparation = t

  type t =
    { preparation : preparation
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
    let%bind () = check preparation ~owner ~state in
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
    let%map () = check preparation ~owner ~state in
    ( retained preparation state
    , { preparation
      ; revision = state.counters.revision
      ; basis = preparation.constructor_jobs
      ; candidate = []
      } )
  ;;

  let check_before_commit t ~owner ~(state : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = check t.preparation ~owner ~state in
    if
      Int64.equal state.counters.revision t.revision
      && phys_equal t.basis t.preparation.constructor_jobs
    then Ok ()
    else conflict "constructor Job selection changed before its owner commit"
  ;;

  let acknowledge t =
    match t.preparation.phase with
    | Closed -> ()
    | Open _ -> t.preparation.constructor_jobs <- t.candidate
  ;;
end

module Decision = struct
  type preparation = t

  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Prepare of preparation
end
