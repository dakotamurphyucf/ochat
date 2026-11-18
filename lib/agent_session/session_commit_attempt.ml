open! Core

module Failure = struct
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

module Context = struct
  type t =
    | Constructor_model
    | Delegated_policy_claim
    | Causal_canonical of
        { slot : unit ref
        ; preparation : unit ref
        }
    | Causal_reserve of
        { attempt : unit ref
        ; set : unit ref
        }
    | Causal_terminal of
        { operation : unit ref
        ; reservation : unit ref
        }
end

type invocation = unit ref

type phase =
  | Awaiting
  | Invoking of invocation
  | Finished
  | Sealed_before_writer of invocation option

type t =
  { context : Context.t
  ; domain : int
  ; previous : Session_state.t
  ; candidate : Session_transition.t
  ; observation : Session_commit_observation.Owner.t
  ; mutable failure : Failure.t option
  ; mutable phase : phase
  }

let create ~context ~previous ~candidate =
  { context
  ; domain = (Stdlib.Domain.self () :> int)
  ; previous
  ; candidate
  ; observation = Session_commit_observation.Owner.create ~previous ~candidate
  ; failure = None
  ; phase = Awaiting
  }
;;

let observation t = t.observation

let no_writer t =
  match
    ( Session_commit_observation.progress t.observation
    , Session_commit_observation.outcome t.observation
    , t.failure )
  with
  | Unobserved, Not_called, Some _ | Before_writer, (Returned (Error _) | Raised _), _ ->
    true
  | ( (Unobserved | Before_writer | Writer_entered _ | Journal_acknowledged _)
    , (Not_called | In_progress | Returned _ | Raised _)
    , _ ) -> false
;;

let unresolved t =
  match t.failure with
  | Some (Protocol failure) -> Error failure
  | Some (Raised (exn, backtrace)) -> Exn.raise_with_original_backtrace exn backtrace
  | None ->
    (match Session_commit_observation.outcome t.observation with
     | Returned (Error failure) -> Error failure
     | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
     | Not_called | In_progress | Returned (Ok ()) ->
       Error
         (Agent_protocol.Error.create
            Conflict
            ~message:
              (match t.context with
               | Constructor_model ->
                 "constructor Model writer outcome remains unresolved"
               | Delegated_policy_claim ->
                 "delegated policy claim writer outcome remains unresolved"
               | Causal_canonical _ ->
                 "causal canonical writer outcome remains unresolved"
               | Causal_reserve _ ->
                 "causal reservation writer outcome remains unresolved"
               | Causal_terminal _ -> "causal terminal writer outcome remains unresolved")
            ~retryable:false
            ()))
;;

let perform t f =
  match f () with
  | Ok _ as result -> result
  | Error failure as result ->
    if Option.is_none t.failure then t.failure <- Some (Protocol failure);
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    if Option.is_none t.failure then t.failure <- Some (Raised (exn, backtrace));
    Exn.raise_with_original_backtrace exn backtrace
;;

let begin_invocation t =
  match t.phase, t.failure, Session_commit_observation.outcome t.observation with
  | Awaiting, None, Not_called ->
    let invocation = ref () in
    t.phase <- Invoking invocation;
    Ok invocation
  | (Awaiting | Invoking _ | Finished | Sealed_before_writer _), _, _ -> unresolved t
;;

let check_invocation t invocation =
  match t.phase, t.failure, Session_commit_observation.outcome t.observation with
  | Invoking active, None, Not_called when phys_equal invocation active -> Ok ()
  | ( (Awaiting | Invoking _ | Finished | Sealed_before_writer _)
    , _
    , (Not_called | In_progress | Returned _ | Raised _) ) -> unresolved t
;;

let finish_invocation t invocation =
  match t.phase with
  | Invoking active when phys_equal invocation active -> t.phase <- Finished
  | Sealed_before_writer (Some active) when phys_equal invocation active ->
    t.phase <- Sealed_before_writer None
  | Awaiting | Invoking _ | Finished | Sealed_before_writer _ ->
    invalid_arg "constructor Model invocation lost its elected attempt"
;;

module No_writer = struct
  type attempt = t
  type t = { attempt : attempt }

  let on_domain attempt = Int.equal attempt.domain (Stdlib.Domain.self () :> int)

  let conflict message =
    Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
  ;;

  let seal attempt =
    if not (on_domain attempt)
    then conflict "causal no-writer seal belongs to another domain"
    else (
      match attempt.context with
      | Constructor_model | Delegated_policy_claim ->
        conflict "causal no-writer seal requires its actual causal writer context"
      | Causal_canonical _ | Causal_reserve _ | Causal_terminal _ ->
        if no_writer attempt
        then (
          (* Never erase failure, progress or an Actor's attempted flag. This
             monotone phase prevents the same invocation from being elected or
             accepted again even if its original failure preceded callback entry. *)
          let pending_finalization =
            match attempt.phase with
            | Invoking invocation -> Some invocation
            | Awaiting | Finished -> None
            | Sealed_before_writer pending -> pending
          in
          attempt.phase <- Sealed_before_writer pending_finalization;
          Ok { attempt })
        else conflict "causal no-writer seal requires an actual pre-writer failure")
  ;;

  let matches t ~attempt ~previous ~candidate =
    on_domain t.attempt
    && on_domain attempt
    && phys_equal t.attempt attempt
    && phys_equal attempt.previous previous
    && phys_equal attempt.candidate candidate
    &&
    match attempt.phase with
    | Sealed_before_writer _ -> true
    | Awaiting | Invoking _ | Finished -> false
  ;;

  let matches_canonical t ~attempt ~slot ~preparation ~previous ~candidate =
    matches t ~attempt ~previous ~candidate
    &&
    match attempt.context with
    | Causal_canonical actual ->
      phys_equal actual.slot slot && phys_equal actual.preparation preparation
    | Constructor_model | Delegated_policy_claim | Causal_reserve _ | Causal_terminal _ ->
      false
  ;;

  let matches_reserve t ~attempt ~reservation_attempt ~set ~previous ~candidate =
    matches t ~attempt ~previous ~candidate
    &&
    match attempt.context with
    | Causal_reserve actual ->
      phys_equal actual.attempt reservation_attempt && phys_equal actual.set set
    | Constructor_model | Delegated_policy_claim | Causal_canonical _ | Causal_terminal _
      -> false
  ;;

  let matches_terminal t ~attempt ~operation ~reservation ~previous ~candidate =
    matches t ~attempt ~previous ~candidate
    &&
    match attempt.context with
    | Causal_terminal actual ->
      phys_equal actual.operation operation && phys_equal actual.reservation reservation
    | Constructor_model | Delegated_policy_claim | Causal_canonical _ | Causal_reserve _
      -> false
  ;;
end
