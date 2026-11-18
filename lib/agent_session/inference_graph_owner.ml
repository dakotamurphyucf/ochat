open! Core

module Phase = struct
  type t =
    | Open
    | Sealed
    | Finished
end

module Factual_usage = struct
  module Correction_outcome = struct
    type t =
      | Settled
      | Awaiting_initial_confirmation
  end

  module Drain_outcome = struct
    type t =
      | Settled
      | Deferred
  end

  module Failure = struct
    type t =
      | Protocol of Agent_protocol.Error.t
      | Raised of exn * Stdlib.Printexc.raw_backtrace

    let resume = function
      | Ok value -> Ok value
      | Error (Protocol error) -> Error error
      | Error (Raised (exn, backtrace)) -> Exn.raise_with_original_backtrace exn backtrace
    ;;
  end

  module Initial = struct
    type t =
      | Idle
      | Running of (unit, Failure.t) result Eio.Promise.t
      | Confirmed
  end

  module Correction = struct
    type t =
      | Idle
      | Running of (Drain_outcome.t, Failure.t) result Eio.Promise.t
  end

  type t =
    { domain : int
    ; confirm_initial : unit -> (unit, Agent_protocol.Error.t) result
    ; correct_usage : unit -> (Correction_outcome.t, Agent_protocol.Error.t) result
    ; mutable initial : Initial.t
    ; mutable correction : Correction.t
    ; mutable clean_revision : int64
    ; mutable current_revision : int64
    }

  let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

  let check_domain t =
    if on_domain t
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"factual graph usage belongs to another domain"
           ~retryable:false
           ())
  ;;

  let pending_on_domain t =
    match t.initial with
    | Idle | Running _ -> true
    | Confirmed ->
      (match t.correction with
       | Running _ -> true
       | Idle -> not (Int64.equal t.clean_revision t.current_revision))
  ;;

  (* A foreign-domain cell must never be pruned as settled. Actual owner writes
     and installers already require the issuing domain. *)
  let must_retain t = (not (on_domain t)) || pending_on_domain t

  module Host = struct
    let create ~confirm_initial ~correct_usage =
      { domain = (Stdlib.Domain.self () :> int)
      ; confirm_initial
      ; correct_usage
      ; initial = Idle
      ; correction = Idle
      ; clean_revision = 0L
      ; current_revision = 0L
      }
    ;;

    let invalidate t ~ledger_revision =
      if on_domain t && Int64.(ledger_revision > t.current_revision)
      then t.current_revision <- ledger_revision
    ;;

    let pending t =
      let%map.Result () = check_domain t in
      pending_on_domain t
    ;;

    let initial_confirmed t =
      let%map.Result () = check_domain t in
      match t.initial with
      | Confirmed -> true
      | Idle | Running _ -> false
    ;;
  end

  let confirm_initial t =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    match t.initial with
    | Confirmed -> Ok ()
    | Running promise -> Eio.Promise.await promise |> Failure.resume
    | Idle ->
      let promise, resolver = Eio.Promise.create () in
      let running = Initial.Running promise in
      t.initial <- running;
      let outcome =
        match t.confirm_initial () with
        | Ok () -> Ok ()
        | Error error -> Error (Failure.Protocol error)
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          Error (Failure.Raised (exn, backtrace))
      in
      Eio.Cancel.protect (fun () ->
        t.initial
        <- (match outcome with
            | Ok () -> Confirmed
            | Error _ -> Idle);
        Eio.Promise.resolve resolver outcome);
      Failure.resume outcome
  ;;

  let settle_correction t =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    match t.initial with
    | Idle | Running _ -> Ok Drain_outcome.Deferred
    | Confirmed ->
      (match t.correction with
       | Running promise -> Eio.Promise.await promise |> Failure.resume
       | Idle ->
         if Int64.equal t.clean_revision t.current_revision
         then Ok Drain_outcome.Settled
         else (
           let revision = t.current_revision in
           let promise, resolver = Eio.Promise.create () in
           let running = Correction.Running promise in
           t.correction <- running;
           let corrected =
             match t.correct_usage () with
             | Ok outcome -> Ok outcome
             | Error error -> Error (Failure.Protocol error)
             | exception exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               Error (Failure.Raised (exn, backtrace))
           in
           let outcome =
             Eio.Cancel.protect (fun () ->
               let outcome =
                 match corrected with
                 | Ok Correction_outcome.Settled ->
                   if Int64.equal revision t.current_revision
                   then Ok Drain_outcome.Settled
                   else Ok Drain_outcome.Deferred
                 | Ok Awaiting_initial_confirmation -> Ok Drain_outcome.Deferred
                 | Error failure -> Error failure
               in
               (match outcome with
                | Ok Settled -> t.clean_revision <- revision
                | Ok Deferred | Error _ -> ());
               t.correction <- Idle;
               Eio.Promise.resolve resolver outcome;
               outcome)
           in
           Failure.resume outcome))
  ;;
end

module Registration = struct
  type phase =
    | Active
    | Joined

  type t =
    { handle : Inference_ledger.Handle.t
    ; configuration : Inference.Observation.Configuration.t
    ; mutable attempt : Inference_runtime.Attempt.t option
    ; goal_id : Agent_protocol.Id.Goal.t option
    ; policy_live : bool Atomic.t
    ; initial_preparation : Run_preparation.t option
    ; factual_usage : Factual_usage.t option
    ; mutable phase : phase
    }

  let initial_preparation t = t.initial_preparation
  let factual_usage t = t.factual_usage

  let is_active t =
    match t.phase with
    | Active -> true
    | Joined -> false
  ;;

  let join t =
    t.phase <- Joined;
    t.attempt <- None;
    Atomic.set t.policy_live false
  ;;

  let handle t = t.handle
  let configuration t = t.configuration
  let attempt t = t.attempt
  let acknowledge t attempt = t.attempt <- Some attempt
  let goal_id t = t.goal_id
  let policy_is_current t = Atomic.get t.policy_live
  let invalidate_policy t = Atomic.set t.policy_live false
end

type t =
  { domain : int
  ; issuing_actor : unit ref
  ; source : Transcript.Source_id.t
  ; generation : int
  ; mutable qualified_source : Transcript.Source_id.t option
  ; mutable phase : Phase.t
  ; mutable registrations : Registration.t Int64.Map.t
  ; mutable native_join : unit ref option
  }

module Host = struct
  type nonrec owner = t

  let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

  let invalid message =
    Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
  ;;

  let check_domain t =
    if on_domain t
    then Ok ()
    else invalid "inference graph owner belongs to another domain"
  ;;

  let create ~issuing_actor ~source ~generation ~qualified_source =
    { domain = (Stdlib.Domain.self () :> int)
    ; issuing_actor
    ; source
    ; generation
    ; qualified_source = Some qualified_source
    ; phase = Open
    ; registrations = Int64.Map.empty
    ; native_join = None
    }
  ;;

  let issuing_actor t = t.issuing_actor
  let source t = t.source
  let generation t = t.generation
  let phase t = t.phase
  let qualified_source t = t.qualified_source

  let registration t ~ordinal =
    Map.find t.registrations ordinal |> Option.filter ~f:Registration.is_active
  ;;

  let retained_registration t handle =
    Map.find t.registrations (Inference_ledger.Handle.ordinal handle)
    |> Option.filter ~f:(fun registration ->
      Inference_ledger.Handle.equal (Registration.handle registration) handle)
  ;;

  let retained_row ledger registration =
    let handle = Registration.handle registration in
    Option.exists
      (Inference_ledger.find ledger ~ordinal:(Inference_ledger.Handle.ordinal handle))
      ~f:(fun row ->
        Inference_ledger.Handle.equal (Inference_ledger.Row.handle row) handle)
  ;;

  let pending_facts registration =
    Option.exists (Registration.factual_usage registration) ~f:Factual_usage.must_retain
  ;;

  let keep_registration ledger registration =
    Registration.is_active registration
    || retained_row ledger registration
    || pending_facts registration
  ;;

  let retain_joined t ledger =
    t.registrations <- Map.filter t.registrations ~f:(keep_registration ledger)
  ;;

  let register t ~initial_preparation ~handle ~configuration ~goal_id =
    t.qualified_source
    <- Some (Transcript.Scope.key (Inference_ledger.Handle.scope handle)).source;
    t.registrations
    <- Map.set
         t.registrations
         ~key:(Inference_ledger.Handle.ordinal handle)
         ~data:
           { handle
           ; configuration
           ; attempt = None
           ; goal_id
           ; policy_live = Atomic.make true
           ; initial_preparation
           ; factual_usage = None
           ; phase = Active
           }
  ;;

  module Prepared_registration = struct
    type installation =
      | Prepared of
          { before : Registration.t Int64.Map.t
          ; qualified_before : Transcript.Source_id.t option
          ; registrations : Registration.t Int64.Map.t
          ; qualified_source : Transcript.Source_id.t option
          }
      | Installed

    type t =
      { owner : owner
      ; registration : Registration.t
      ; mutable installation : installation
      }

    let registration t = t.registration

    let check_basis t =
      let%bind.Result () = check_domain t.owner in
      match t.installation with
      | Installed -> invalid "prepared graph registration is already installed"
      | Prepared prepared ->
        (match t.owner.phase with
         | Open
           when phys_equal t.owner.registrations prepared.before
                && Option.equal
                     Transcript.Source_id.equal
                     t.owner.qualified_source
                     prepared.qualified_before -> Ok ()
         | Open | Sealed | Finished ->
           invalid "prepared graph registration lost its original open map")
    ;;

    let install t =
      (* Exact Actor check_basis/writer precedes this same-State-ACK hook. Its
         own common ACK pruning may already have replaced the old map. Drop
         spent preparation maps so retained Plans cannot chain old facts. *)
      if on_domain t.owner
      then (
        match t.installation with
        | Prepared prepared ->
          (match t.owner.phase with
           | Open ->
             t.owner.registrations <- prepared.registrations;
             t.owner.qualified_source <- prepared.qualified_source;
             t.installation <- Installed
           | Sealed | Finished -> ())
        | Installed -> ())
    ;;
  end

  let prepare_registration
        t
        ~ledger_after
        ~initial_preparation
        ~handle
        ~configuration
        ~goal_id
        ~factual_usage
    =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    let%bind _ = Factual_usage.Host.pending factual_usage in
    let%bind () =
      match t.phase with
      | Open -> Ok ()
      | Sealed | Finished -> invalid "governed registration requires an open graph"
    in
    let ordinal = Inference_ledger.Handle.ordinal handle in
    if Map.mem t.registrations ordinal
    then invalid "governed registration already owns this canonical ordinal"
    else (
      let registration : Registration.t =
        { handle
        ; configuration
        ; attempt = None
        ; goal_id
        ; policy_live = Atomic.make true
        ; initial_preparation
        ; factual_usage = Some factual_usage
        ; phase = Active
        }
      in
      (* This candidate ACK invalidates existing facts. Keep even currently
         clean absent-row cells until ACK -> drain -> later pruning. *)
      let registrations =
        Map.filter t.registrations ~f:(fun registration ->
          keep_registration ledger_after registration
          || Option.is_some (Registration.factual_usage registration))
        |> Map.set ~key:ordinal ~data:registration
      in
      let qualified_source =
        Some (Transcript.Scope.key (Inference_ledger.Handle.scope handle)).source
      in
      let installation =
        Prepared_registration.Prepared
          { before = t.registrations
          ; qualified_before = t.qualified_source
          ; registrations
          ; qualified_source
          }
      in
      Ok ({ owner = t; registration; installation } : Prepared_registration.t))
  ;;

  module Native_join = struct
    type acknowledgement = unit ref

    type installation =
      | Prepared of
          { before : Registration.t Int64.Map.t
          ; registrations : Registration.t Int64.Map.t
          ; joined : Registration.t list
          }
      | Installed

    type t =
      { owner : owner
      ; acknowledgement : acknowledgement
      ; installed_acknowledgement : acknowledgement option
      ; mutable installation : installation
      }

    let prepare owner ~ledger_after =
      let%bind.Result () = check_domain owner in
      match owner.phase, owner.native_join with
      | Sealed, None ->
        let acknowledgement = ref () in
        let installed_acknowledgement = Some acknowledgement in
        let registrations =
          Map.filter owner.registrations ~f:(fun registration ->
            retained_row ledger_after registration
            || Option.is_some (Registration.factual_usage registration))
        in
        let joined = Map.data owner.registrations in
        let installation =
          Prepared { before = owner.registrations; registrations; joined }
        in
        Ok { owner; acknowledgement; installed_acknowledgement; installation }
      | (Open | Finished), (None | Some _) | Sealed, Some _ ->
        invalid "native graph join requires a sealed owner without an earlier ACK"
    ;;

    let check_basis t =
      let%bind.Result () = check_domain t.owner in
      match t.installation with
      | Installed -> invalid "native graph join plan is already installed"
      | Prepared prepared ->
        (match t.owner.phase, t.owner.native_join with
         | Sealed, None when phys_equal t.owner.registrations prepared.before -> Ok ()
         | (Open | Sealed | Finished), (None | Some _) ->
           invalid "native graph join lost its original sealed map")
    ;;

    let install t =
      if on_domain t.owner
      then (
        match t.installation with
        | Prepared prepared ->
          (match t.owner.phase with
           | Sealed ->
             (match t.owner.native_join with
              | None ->
                List.iter prepared.joined ~f:Registration.join;
                t.owner.registrations <- prepared.registrations;
                t.owner.native_join <- t.installed_acknowledgement;
                t.installation <- Installed
              | Some _ -> ())
           | Open | Finished -> ())
        | Installed -> ())
    ;;

    let acknowledgement t = t.acknowledgement

    let current t =
      let%map.Result () = check_domain t in
      t.native_join
    ;;

    let check_acknowledgement t acknowledgement =
      let%bind.Result () = check_domain t in
      match t.native_join with
      | Some actual when phys_equal actual acknowledgement ->
        (match t.phase with
         | Sealed | Finished -> Ok ()
         | Open -> invalid "native graph join ACK has an open owner")
      | None | Some _ ->
        invalid "native graph join ACK was not installed on this actual owner"
    ;;
  end

  let iter_registrations t ~f =
    Map.iter t.registrations ~f:(fun registration ->
      if Registration.is_active registration then f registration)
  ;;

  let iter_retained_registrations t ~f = Map.iter t.registrations ~f

  let iter_factual_usage t ~f =
    Map.iter t.registrations ~f:(fun registration ->
      Option.iter (Registration.factual_usage registration) ~f)
  ;;

  let release t ~ledger ~ordinal =
    match Map.find t.registrations ordinal with
    | None -> ()
    | Some registration ->
      if retained_row ledger registration || pending_facts registration
      then Registration.join registration
      else t.registrations <- Map.remove t.registrations ordinal
  ;;

  let seal t =
    match t.phase with
    | Open | Sealed -> t.phase <- Sealed
    | Finished -> ()
  ;;

  let finish t =
    t.phase <- Finished;
    (* The installed native identity is a unit ref only. Retain it for exact
       idempotent final-clear reentry, never a map/callback/owner backreference. *)
    t.registrations <- Int64.Map.empty
  ;;
end
