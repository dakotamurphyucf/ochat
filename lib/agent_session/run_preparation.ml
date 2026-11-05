open! Core
module P = Agent_protocol

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

type phase =
  | Open
  | Closed

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Authorization = struct
  type t =
    { actor : Operator_authorization.t
    ; authorize : Session_state.t -> (unit, P.Error.t) result
    }

  let actor t = t.actor
  let check t state = t.authorize state
end

module Custody = struct
  type t =
    { owner : Owner.t
    ; original_actor : Operator_authorization.t option
    ; principal_id : P.Id.Principal.t
    ; session_id : P.Id.Session.t
    ; generation : int
    ; authorize : Session_state.t -> (unit, P.Error.t) result
    ; mutable phase : phase
      (* The one acknowledged revision survives closure. Initial deferred evidence
       reads this SAME field, so every exact common or initial ACK advances its
       factual basis without attaching negative permission to general custody. *)
    ; mutable revision : int64
    ; mutable constructor_jobs : P.Id.Job.t list
    }

  (* Initial request constructors validate policy, digest and original CAS before
     entering this private constructor. No unchecked creation is exported. *)
  let create
        ~owner
        ~original_actor
        ~principal_id
        ~session_id
        ~generation
        ~revision
        ~authorize
    =
    { owner
    ; original_actor
    ; principal_id
    ; session_id
    ; generation
    ; authorize
    ; phase = Open
    ; revision
    ; constructor_jobs = []
    }
  ;;

  let belongs_to t ~owner = phys_equal t.owner owner
  let principal_id t = t.principal_id
  let acknowledged_revision t = t.revision

  let original_actor t =
    match t.phase with
    | Open -> t.original_actor
    | Closed -> None
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

  let check_current t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = check_basis t ~owner ~state in
    match t.original_actor with
    | None -> Ok ()
    | Some actor ->
      if
        Operator_authorization.is_current actor
        && P.Principal.has_scope (Operator_authorization.principal actor) Send_messages
      then Ok ()
      else
        Error
          (P.Error.create
             Permission_denied
             ~message:"Run preparation authority expired"
             ~retryable:false
             ())
  ;;

  let check t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = check_basis t ~owner ~state in
    t.authorize state
  ;;

  let capture_authorization t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = check t ~owner ~state in
    let%map actor =
      Result.of_option
        t.original_actor
        ~error:
          (P.Error.create
             Permission_denied
             ~message:"held activation requires its original authentication"
             ~retryable:false
             ())
    in
    ({ actor; authorize = t.authorize } : Authorization.t)
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
              (P.Id.Session.equal
                 previous.identity.session_id
                 current.identity.session_id))
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
    type custody = t

    type t =
      { preparation : custody
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
      | Open -> t.preparation.constructor_jobs <- t.candidate
    ;;
  end
end

module Goal_continuation = struct
  module Cause = struct
    type t =
      | Resume of Goal_resume_request.t
      | Wake of
          { run_id : P.Id.Run.t
          ; wake : P.Run_wake.t
          }
      | Claimed_job of
          { run_id : P.Id.Run.t
          ; job : P.Job.t
          }
  end

  type t =
    { cause : Cause.t
    ; custody : Custody.t
    }

  let cause t = t.cause
  let custody t = t.custody

  let create_authenticated ~owner ~(state : Session_state.t) ~actor ~cause ~authorize =
    let open Result.Let_syntax in
    let authorize = Run_command_authorization.check actor ~authorize in
    let%bind () = authorize state in
    let%bind () =
      match (cause : Cause.t) with
      | Resume request ->
        if
          P.Id.Session.equal
            (Goal_resume_request.session_id request)
            state.identity.session_id
          && Int.equal (Goal_resume_request.generation request) state.identity.generation
        then Ok ()
        else conflict "Goal continuation original session or generation changed"
      | Wake { run_id; wake } ->
        let%bind _ = P.Id.Run.of_json (P.Id.Run.to_json run_id) in
        let%bind () = P.Run_wake.validate wake in
        if
          P.Id.Run.equal run_id wake.run_id
          && Int.equal wake.source.generation state.identity.generation
        then Ok ()
        else conflict "Goal continuation wake identity or generation changed"
      | Claimed_job { run_id; job } ->
        let%bind _ = P.Id.Run.of_json (P.Id.Run.to_json run_id) in
        if
          P.Id.Session.equal job.session_id state.identity.session_id
          && Int.equal job.generation state.identity.generation
          &&
          match job.status with
          | Running -> true
          | Queued
          | Waiting_permission _
          | Waiting_completion _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ -> false
        then Ok ()
        else conflict "Goal continuation Job session, generation or status changed"
    in
    Ok
      { cause
      ; custody =
          Custody.create
            ~owner
            ~original_actor:(Some actor)
            ~principal_id:(Operator_authorization.principal actor).id
            ~session_id:state.identity.session_id
            ~generation:state.identity.generation
            ~revision:state.counters.revision
            ~authorize
      }
  ;;
end

type exclusion =
  | Ordinary_execution
  | Admission_attempted
  | Activation_consumed
  | Uncertain_write
  | Displaced
[@@deriving equal, sexp_of]

type t =
  { custody : Custody.t
  ; request : Run_admission_request.t
  ; request_sha256 : string
  ; mutable deferred : deferred option
  }

and deferred =
  { preparation : t
  ; source_owner : Runtime_source_owner.Identity.t
  ; domain : int
  ; mutable exclusion : exclusion option
  ; mutable sealed : bool
  }

type closed_attempt =
  { deferred : deferred
  ; revision : int64
  }

let custody t = t.custody
let belongs_to t ~owner = Custody.belongs_to t.custody ~owner
let request t = t.request
let principal_id t = Custody.principal_id t.custody
let original_actor t = Custody.original_actor t.custody
let request_sha256 t = t.request_sha256
let close t = Custody.close t.custody
let check_basis t ~owner ~state = Custody.check_basis t.custody ~owner ~state
let check_current t ~owner ~state = Custody.check_current t.custody ~owner ~state
let check t ~owner ~state = Custody.check t.custody ~owner ~state

let capture_authorization t ~owner ~state =
  Custody.capture_authorization t.custody ~owner ~state
;;

let advance t ~owner ~previous ~current =
  Custody.advance t.custody ~owner ~previous ~current
;;

let digest_valid digest =
  String.length digest = 64
  && String.for_all digest ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let create_request_internal
      ~owner
      ~(state : Session_state.t)
      ~principal_id
      ~original_actor
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
      { custody =
          Custody.create
            ~owner
            ~original_actor
            ~principal_id
            ~session_id:request.session_id
            ~generation:request.generation
            ~revision:state.counters.revision
            ~authorize
      ; request
      ; request_sha256
      ; deferred = None
      }
;;

let create_request ~owner ~state ~principal_id ~request ~request_sha256 ~authorize =
  create_request_internal
    ~owner
    ~state
    ~principal_id
    ~original_actor:None
    ~request
    ~request_sha256
    ~authorize
;;

let create_authenticated_request ~owner ~state ~actor ~request ~request_sha256 ~authorize =
  create_request_internal
    ~owner
    ~state
    ~principal_id:(Operator_authorization.principal actor).id
    ~original_actor:(Some actor)
    ~request
    ~request_sha256
    ~authorize:(Run_command_authorization.check actor ~authorize)
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

let deferred_domain (t : deferred) = Int.equal t.domain (Stdlib.Domain.self () :> int)

let deferred_association (t : deferred) ~owner ~(state : Session_state.t) =
  if not (deferred_domain t)
  then conflict "deferred preparation belongs to another Eio domain"
  else if
    Custody.belongs_to t.preparation.custody ~owner
    && Option.exists t.preparation.deferred ~f:(phys_equal t)
    && P.Id.Session.equal t.preparation.request.session_id state.identity.session_id
    && Int.equal t.preparation.request.generation state.identity.generation
    && Int64.equal
         (Custody.acknowledged_revision t.preparation.custody)
         state.counters.revision
  then Ok ()
  else conflict "deferred preparation owner or acknowledged basis changed"
;;

let deferred_eligible (t : deferred) =
  match t.exclusion with
  | None -> Ok ()
  | Some _ -> conflict "deferred attempt entered execution or an admission writer"
;;

module Source_completion = struct
  type preparation = t
  type t = closed_attempt

  let preparation t = t.deferred.preparation

  let check_basis t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = deferred_association t.deferred ~owner ~state in
    let%bind () = deferred_eligible t.deferred in
    if
      t.deferred.sealed
      && Int64.equal
           t.revision
           (Custody.acknowledged_revision t.deferred.preparation.custody)
      &&
      match t.deferred.preparation.custody.phase with
      | Closed -> true
      | Open -> false
    then Ok ()
    else conflict "source completion lost its exact closed attempt"
  ;;
end

module Closed_attempt = struct
  type t = closed_attempt

  let complete t ~source_owner =
    let open Result.Let_syntax in
    let%bind () =
      if
        deferred_domain t.deferred
        && Runtime_source_owner.matches source_owner t.deferred.source_owner
      then Ok ()
      else conflict "closed attempt belongs to another source issuer"
    in
    let%bind () = deferred_eligible t.deferred in
    if t.deferred.sealed
    then Ok t
    else conflict "deferred attempt has not been factually closed"
  ;;
end

module Deferred = struct
  type preparation = t
  type t = deferred

  module Exclusion = struct
    type t = exclusion =
      | Ordinary_execution
      | Admission_attempted
      | Activation_consumed
      | Uncertain_write
      | Displaced
    [@@deriving equal, sexp_of]
  end

  let create_authenticated_request
        ~owner
        ~source_owner
        ~state
        ~actor
        ~request
        ~request_sha256
        ~authorize
    =
    let open Result.Let_syntax in
    let%map preparation =
      create_authenticated_request
        ~owner
        ~state
        ~actor
        ~request
        ~request_sha256
        ~authorize
    in
    let deferred =
      { preparation
      ; source_owner
      ; domain = (Stdlib.Domain.self () :> int)
      ; exclusion = None
      ; sealed = false
      }
    in
    preparation.deferred <- Some deferred;
    deferred
  ;;

  let preparation t = t.preparation
  let of_preparation (t : preparation) = t.deferred

  let exclude t ~reason =
    if not (deferred_domain t)
    then invalid_arg "deferred preparation belongs to another Eio domain";
    if Option.is_none t.exclusion then t.exclusion <- Some reason
  ;;

  let check_basis t ~owner ~state =
    let open Result.Let_syntax in
    let%bind () = deferred_association t ~owner ~state in
    let%bind () = deferred_eligible t in
    if t.sealed
    then conflict "deferred preparation is already sealed"
    else check_basis t.preparation ~owner ~state
  ;;

  let close t ~owner ~(state : Session_state.t) =
    if not (deferred_domain t)
    then conflict "deferred preparation belongs to another Eio domain"
    else if not (Custody.belongs_to t.preparation.custody ~owner)
    then conflict "deferred preparation belongs to another actor"
    else (
      (* Closure is factual even after displacement/exclusion. Eligibility is
         checked separately; no failure here may leave this owner's prep open. *)
      if Result.is_error (deferred_association t ~owner ~state)
      then exclude t ~reason:Displaced;
      close t.preparation;
      t.sealed <- true;
      Ok { deferred = t; revision = Custody.acknowledged_revision t.preparation.custody })
  ;;

  type deferred = t

  module Replacement = struct
    type t =
      { previous : Source_completion.t
      ; next : deferred
      }

    let check_basis t ~owner ~state =
      let open Result.Let_syntax in
      let%bind () = Source_completion.check_basis t.previous ~owner ~state in
      check_basis t.next ~owner ~state
    ;;

    let prepare
          previous
          ~owner
          ~source_owner
          ~state
          ~actor
          ~request
          ~request_sha256
          ~authorize
      =
      let open Result.Let_syntax in
      let%bind () = Source_completion.check_basis previous ~owner ~state in
      let%bind () =
        if Runtime_source_owner.Identity.equal previous.deferred.source_owner source_owner
        then Ok ()
        else conflict "retry completion belongs to another Factory"
      in
      let%bind next =
        create_authenticated_request
          ~owner
          ~source_owner
          ~state
          ~actor
          ~request
          ~request_sha256
          ~authorize
      in
      let t = { previous; next } in
      let%map () = check_basis t ~owner ~state in
      t
    ;;

    let check_current t ~owner ~state =
      let open Result.Let_syntax in
      let%bind () = check_basis t ~owner ~state in
      let%bind () = check_current t.next.preparation ~owner ~state in
      check_basis t ~owner ~state
    ;;

    let commit t ~owner ~state =
      let open Result.Let_syntax in
      let%map () = check_basis t ~owner ~state in
      exclude t.previous.deferred ~reason:Displaced;
      t.next
    ;;
  end

  module Decision = struct
    type deferred = t

    type t =
      | Retained of P.Run_receipt.t
      | Prepare of deferred
  end
end

module Constructor_jobs = struct
  type preparation = t
  type t = Custody.Constructor_jobs.t

  let prepare_add preparation ~owner ~state ~job =
    Custody.Constructor_jobs.prepare_add preparation.custody ~owner ~state ~job
  ;;

  let prepare_handoff preparation ~owner ~state =
    Custody.Constructor_jobs.prepare_handoff preparation.custody ~owner ~state
  ;;

  let check_before_commit = Custody.Constructor_jobs.check_before_commit
  let acknowledge = Custody.Constructor_jobs.acknowledge
end

module Decision = struct
  type preparation = t

  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Prepare of preparation
end
