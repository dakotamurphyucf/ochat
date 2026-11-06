open! Core

(* Private admission custody only. Actual Actor capture, Store/policy checks,
   strong resource ownership and State-to-ACK install sequencing stay in Actor. *)

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

module Child_accounting_basis = struct
  type t =
    { issuing_actor : unit ref
    ; domain : int
    ; state : Session_state.t
    ; controller : Recurrence_controller.t
    ; source_owner : Runtime_source_owner.t
    }
end

module Child_usage_observation = struct
  type t =
    { controller : Recurrence_controller.t
    ; source_owner : Runtime_source_owner.Identity.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; snapshot : Child_usage_snapshot.t option
    ; refresh_read :
        unit
        -> (unit -> (unit, Agent_protocol.Error.t) result, Agent_protocol.Error.t) result
    }
end

module Descendant_preparation = struct
  module P = Agent_protocol
  module O = Agent_store.Delegation_goal_origin

  module Phase = struct
    type t =
      | Open
      | Acknowledged of unit ref
      | Sealed_before_writer
  end

  type t =
    { issuing_actor : unit ref
    ; domain : int
    ; source_owner : Runtime_source_owner.Identity.t
    ; before : Session_state.t
    ; session : P.Session_ref.t
    ; controller : Recurrence_controller.t
    ; origin : O.t
    ; occurrence : Goal_descendant_occurrence.t
    ; writer_attempted : bool ref
    ; phase : Phase.t ref
    ; mutable reservation_set_claim : unit ref option
    ; check_owner : unit -> (unit, P.Error.t) result
    ; check_before : unit -> (unit, P.Error.t) result
    }

  let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
  let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)
  let occurrence t = t.occurrence
  let scopes t = O.scopes t.origin
  let delegation t = t.before.spec.delegation

  let issued_by t ~source_owner =
    on_domain t && Runtime_source_owner.matches source_owner t.source_owner
  ;;

  let same_store t ~controller =
    on_domain t && Recurrence_controller.same_store t.controller controller
  ;;

  let check_owner t =
    if on_domain t
    then t.check_owner ()
    else conflict "child preparation belongs to another domain"
  ;;

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind () = t.check_before () in
    match !(t.phase) with
    | Open when not !(t.writer_attempted) -> Ok ()
    | Open | Acknowledged _ | Sealed_before_writer ->
      conflict "child preparation is sealed, acknowledged or writer-attempted"
  ;;

  let matches_delegation t reference =
    Option.exists
      t.before.spec.delegation
      ~f:(Agent_store.Delegation_store.Reference.equal reference)
  ;;

  let acknowledged_by t ~acknowledgement =
    match !(t.phase) with
    | Acknowledged actual -> phys_equal actual acknowledgement
    | Open | Sealed_before_writer -> false
  ;;

  module Acknowledgement_plan = struct
    type preparation = t

    type t =
      { preparation : preparation
      ; acknowledged : Phase.t
      }

    let prepare preparation ~acknowledgement =
      let%map.Result () = check_basis preparation in
      { preparation; acknowledged = Acknowledged acknowledgement }
    ;;

    (* Only the actual native child canonical ACK owner invokes this after exact
       State installation; positive object remains private until that event. *)
    let install t = t.preparation.phase := t.acknowledged
  end

  module Closed_before_writer = struct
    type preparation = t
    type t = { preparation : preparation }

    let belongs_to t ~preparation = phys_equal t.preparation preparation
  end

  let seal_before_writer t ~owner =
    if not (on_domain t && phys_equal owner t.issuing_actor)
    then conflict "child seal belongs to another actual owner or domain"
    else (
      match !(t.phase) with
      | Open when not !(t.writer_attempted) ->
        t.phase := Sealed_before_writer;
        Ok Closed_before_writer.{ preparation = t }
      | Open | Acknowledged _ | Sealed_before_writer ->
        conflict "child seal requires the same unattempted open preparation")
  ;;
end

module Descendant_admission_data = struct
  module Preparation = Descendant_preparation

  let metadata_only_candidate (candidate : Session_transition.t) =
    List.for_all candidate.events ~f:(fun event ->
      match event.Agent_protocol.Event.Durable.kind with
      | Operation_started -> false
      | _ -> true)
  ;;

  module O = Agent_store.Delegation_goal_origin

  module Root_route = struct
    type t =
      { association : Goal_descendant_root_route.t
      ; controller : Recurrence_controller.t
      ; root_issuer : unit ref
      ; source_owner : Runtime_source_owner.t
      ; domain : int
      ; preparation : Preparation.t
      ; records : Agent_store.Delegation_store.record list
      }

    let scope t = Goal_descendant_root_route.scope t.association
  end

  module Reservation = struct
    type t =
      { attempt_identity : unit ref
      ; route : Root_route.t
      ; preparation : Preparation.t
      ; policy : Goal_root_policy.t
      ; pending : Goal_descendant_pending.t
      ; mutable closed : Descendant_preparation.Closed_before_writer.t option
      }
  end

  module Reservation_attempt = struct
    type t =
      { identity : unit ref
      ; basis : Child_accounting_basis.t
      ; route : Root_route.t
      ; preparation : Preparation.t
      ; policy : Goal_root_policy.t
      ; writer_attempted : bool ref
      ; mutable planned_pending : Goal_descendant_pending.t option
      ; mutable acknowledged : Reservation.t option
      }

    type outcome =
      | Not_attempted
      | Acknowledged of Reservation.t
      | Attempt_unconfirmed

    let scope t = Root_route.scope t.route
    let acknowledged t = t.acknowledged

    let outcome t =
      match t.acknowledged with
      | Some reservation -> Acknowledged reservation
      | None -> if !(t.writer_attempted) then Attempt_unconfirmed else Not_attempted
    ;;
  end

  module Reserve_result = struct
    type t =
      | Reserved of Reservation.t
      | Blocked of Goal_descendants.Availability.t
      | Attempt_unconfirmed of Reservation_attempt.t
  end

  module Reservation_set = struct
    type phase =
      | Acquiring
      | Complete

    type t =
      { identity : unit ref
      ; preparation : Preparation.t
      ; mutable attempts : Reservation_attempt.t list
      ; mutable phase : phase
      }

    let conflict message =
      Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
    ;;

    (* One physical preparation owns exactly one Set for its entire lifetime.
       This claim is never reset by source retirement, sealing or factual ACK. *)
    let create preparation =
      let open Result.Let_syntax in
      let%bind () = Preparation.check_basis preparation in
      match preparation.reservation_set_claim with
      | Some _ -> conflict "child preparation already owns its reservation set"
      | None ->
        let identity = ref () in
        let set : t = { identity; preparation; attempts = []; phase = Acquiring } in
        preparation.reservation_set_claim <- Some identity;
        Ok set
    ;;

    let check_claim t =
      if
        Preparation.on_domain t.preparation
        && Option.exists t.preparation.reservation_set_claim ~f:(phys_equal t.identity)
      then Ok ()
      else conflict "root reservation set differs from its physical preparation claim"
    ;;

    let attempts t = t.attempts
    let preparation t = t.preparation

    let check_next t ~scope =
      let open Result.Let_syntax in
      let%bind () = check_claim t in
      match t.phase with
      | Complete -> conflict "root reservation vector is already complete"
      | Acquiring ->
        (match List.nth (Preparation.scopes t.preparation) (List.length t.attempts) with
         | Some expected when O.Scope.equal expected scope -> Ok ()
         | Some _ | None ->
           conflict "root attempts must cover the bounded canonical vector exactly once")
    ;;

    (* Called only by finite Begin, before its attempt can be returned or written. *)
    let retain_next t (attempt : Reservation_attempt.t) =
      let open Result.Let_syntax in
      let%bind () = check_next t ~scope:(Reservation_attempt.scope attempt) in
      if
        phys_equal t.preparation attempt.preparation
        && not (List.exists t.attempts ~f:(phys_equal attempt))
      then (
        t.attempts <- t.attempts @ [ attempt ];
        Ok ())
      else conflict "root attempt belongs to another physical child preparation"
    ;;
  end

  module Root_before_writer = struct
    type t =
      { reservation : Reservation.t
      ; check_current : unit -> (unit, Agent_protocol.Error.t) result
      }
  end

  module Before_writer = struct
    type t =
      { preparation : Preparation.t
      ; reservations : Reservation_set.t
      ; views : Root_before_writer.t list
      }
  end

  module Canonical_edge = struct
    type t =
      | Attempt of
          { before : Inference_ledger.t
          ; after : Inference_ledger.t
          ; canonical : Goal_descendants.Canonical.t
          }
      | Host_turn of Host_turn_tracking.Inherited_edge.t

    let before = function
      | Attempt edge -> edge.before
      | Host_turn edge -> Host_turn_tracking.Inherited_edge.before edge
    ;;

    let after = function
      | Attempt edge -> edge.after
      | Host_turn edge -> Host_turn_tracking.Inherited_edge.after edge
    ;;

    let canonical = function
      | Attempt edge -> edge.canonical
      | Host_turn edge -> Host_turn_tracking.Inherited_edge.canonical edge
    ;;
  end

  module Acknowledgement = struct
    (* Only actual child canonical install owner prepares this with genuine
       Handle/Turn_handle and complete receipts, and installs SAME phase identity
       through Preparation.Acknowledgement_plan before exposing the ACK. *)
    type t =
      { preparation : Preparation.t
      ; identity : unit ref
      ; installed : Session_state.t
      ; candidate : Session_transition.t
      ; edge : Canonical_edge.t
      ; receipts : Goal_descendant_included.t list
      ; reservations : Reservation_set.t
      }

    let validate_content (ack : t) =
      let module O = Agent_store.Delegation_goal_origin in
      let error code message =
        Agent_protocol.Error.create code ~message ~retryable:false ()
      in
      let open Result.Let_syntax in
      let%bind () = Reservation_set.check_claim ack.reservations in
      let%bind () =
        if Int.equal ack.preparation.domain (Stdlib.Domain.self () :> int)
        then Ok ()
        else Error (error Conflict "child ACK belongs to another actual owner domain")
      in
      let%bind () =
        if
          phys_equal ack.reservations.Reservation_set.preparation ack.preparation
          && phys_equal ack.candidate.state ack.installed
        then Ok ()
        else Error (error Conflict "child canonical ACK reservation owner changed")
      in
      let%bind _ =
        Goal_descendants.Canonical.check
          ~before:(Canonical_edge.before ack.edge)
          ~after:(Canonical_edge.after ack.edge)
          ~canonical:(Canonical_edge.canonical ack.edge)
          ~occurrence:(Descendant_preparation.occurrence ack.preparation)
      in
      let%bind accounting =
        Result.of_option
          ack.installed.goal_descendants
          ~error:(error Permission_denied "actual descendant component is absent")
      in
      let%bind _ =
        Result.of_option
          ack.installed.child_contributions
          ~error:(error Permission_denied "actual descendant child frontier is absent")
      in
      if
        List.equal
          Goal_descendant_included.equal
          (Goal_descendants.local_included accounting)
          ack.receipts
        && List.equal
             O.Scope.equal
             (List.map ack.receipts ~f:(fun receipt ->
                Goal_descendant_pending.scope (Goal_descendant_included.pending receipt)))
             (Descendant_preparation.scopes ack.preparation)
      then Ok ()
      else
        Error
          (error Conflict "child canonical ACK does not retain complete Included vector")
    ;;
  end

  module Import_acknowledgement = struct
    (* Same actual Child_usage_observation carrier, never a second snapshot.
       Constructor belongs to exact accepted factual import install owner or
       positive actual already-Included re-observation, not duplicate disposition
       or decoded receipt alone. Facts survive control/source/revocation changes. *)
    type t =
      { issuing_actor : unit ref
      ; controller : Recurrence_controller.t
      ; source_owner : Runtime_source_owner.Identity.t
      ; observation : Child_usage_observation.t
      ; installed : Session_state.t
      ; included : Goal_descendant_included.t list
      }

    let delegation t = t.installed.spec.delegation
  end

  module Confirmation = struct
    type t =
      { acknowledgement : Acknowledgement.t
      ; imports : Import_acknowledgement.t list
      }
  end

  module Root_admitted = struct
    type t =
      { reservation : Reservation.t
      ; acknowledgement : Acknowledgement.t
      ; confirmation : Confirmation.t
      ; check_current : unit -> (unit, Agent_protocol.Error.t) result
      }
  end

  module Admitted_views = struct
    type t =
      { acknowledgement : Acknowledgement.t
      ; confirmation : Confirmation.t
      ; views : Root_admitted.t list
      }
  end
end

module Descendant_host_data = struct
  type t =
    { prepare :
        Descendant_admission_data.Preparation.t
        -> Descendant_admission_data.Reservation_set.t
        -> (Descendant_admission_data.Before_writer.t, Agent_protocol.Error.t) result
    ; close :
        Descendant_admission_data.Reservation_set.t
        -> Descendant_preparation.Closed_before_writer.t
        -> (unit, Agent_protocol.Error.t) result
    ; confirm :
        Descendant_admission_data.Acknowledgement.t
        -> (Descendant_admission_data.Confirmation.t, Agent_protocol.Error.t) result
    ; refresh_admitted :
        Descendant_admission_data.Acknowledgement.t
        -> Descendant_admission_data.Confirmation.t
        -> (Descendant_admission_data.Admitted_views.t, Agent_protocol.Error.t) result
    }

  module Binding = struct
    type host = t

    type t =
      { issuing_actor : unit ref
      ; domain : int
      ; source_owner : Runtime_source_owner.t
      ; controller : Recurrence_controller.t
      ; check_ancestry :
          (unit
           -> (Delegation_authority.Checked_ancestry.t, Agent_protocol.Error.t) result)
            option
      ; host : host
      ; mutable closed : bool
      }

    let check_domain t =
      if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
      then invalid_arg "descendant host binding belongs to another Eio domain"
    ;;

    let close t =
      check_domain t;
      t.closed <- true
    ;;

    (* Facts retain their original host after fresh source retirement. *)
    let host t =
      check_domain t;
      t.host
    ;;
  end
end

module Descendant_admission_slot = struct
  module R = Descendant_admission_data

  module Phase = struct
    type t =
      | Active
      | Sealed of Descendant_preparation.Closed_before_writer.t
      | Released of Descendant_preparation.Closed_before_writer.t
      | Acknowledged of unit ref
  end

  module Cleanup = struct
    type t =
      | Idle
      | Running of (unit, Agent_protocol.Error.t) result Eio.Promise.t
  end

  type t =
    { preparation : Descendant_preparation.t
    ; reservations : R.Reservation_set.t
    ; binding : Descendant_host_data.Binding.t
    ; mutable phase : Phase.t
    ; mutable cleanup : Cleanup.t
    }

  let preparation t = t.preparation
  let reservations t = t.reservations
  let binding t = t.binding
  let host t = Descendant_host_data.Binding.host t.binding

  module Acknowledgement_plan = struct
    type slot = t

    type t =
      { slot : slot
      ; acknowledgement : R.Acknowledgement.t
      ; preparation : Descendant_preparation.Acknowledgement_plan.t
      ; acknowledged : Phase.t
      }
  end
end

module Root_reserve_acknowledgement = struct
  module R = Descendant_admission_data
  module GD = Goal_descendants
  module Pending = Goal_descendant_pending
  module Route = Goal_descendant_root_route
  module Scope = Agent_store.Delegation_goal_origin.Scope

  type t =
    { attempt : R.Reservation_attempt.t
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; positive_ack : R.Reservation.t option
    }

  let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()
  let conflict message = Error (error Conflict message)

  let prepare
        (attempt : R.Reservation_attempt.t)
        ~previous
        ~(candidate : Session_transition.t)
    =
    let open Result.Let_syntax in
    let%bind () =
      match R.Reservation_attempt.outcome attempt with
      | Not_attempted when phys_equal previous attempt.basis.state -> Ok ()
      | Not_attempted | Acknowledged _ | Attempt_unconfirmed ->
        conflict "root reserve ACK requires its same unattempted original basis"
    in
    let scope = Route.scope attempt.route.association in
    let occurrence = Descendant_preparation.occurrence attempt.preparation in
    let%bind incoming =
      (* Direct metadata leaf only. An alternate Goal Hold/Turn/Batch candidate
         cannot install this pending ACK, even if a branch happens to match. *)
      match candidate.delta with
      | Session_delta.Goal_descendants_changed update ->
        (match GD.Update.reason update with
         | Reserve reason
           when Scope.equal reason.scope scope
                && Goal_descendant_occurrence.equal reason.occurrence occurrence ->
           Ok (GD.Update.accounting update)
         | Register_new_root _
         | Register_first_unrelated_child _
         | Register_child _
         | Register_branch _
         | Register_retained_branch _
         | Reserve _
         | Record_included _
         | Import _
         | Close_before_writer _ -> conflict "root ACK candidate is not its exact reserve")
      | _ -> conflict "root ACK requires the direct reserve metadata leaf"
    in
    let%bind pending =
      List.find
        (GD.branches incoming ~goal_id:(Scope.goal_id scope))
        ~f:(fun branch -> Scope.equal (GD.Branch.scope branch) scope)
      |> Option.bind ~f:GD.Branch.pending
      |> Result.of_option
           ~error:(error Invalid_state "root reserve candidate has no pending")
    in
    let%bind () =
      if
        Goal_descendant_occurrence.equal (Pending.occurrence pending) occurrence
        && Agent_store.Delegation_store.Reference.equal
             (Pending.branch_reference pending)
             (Route.reference attempt.route.association)
      then Ok ()
      else conflict "root reserve candidate pending differs from its actual attempt"
    in
    let reservation : R.Reservation.t =
      { attempt_identity = attempt.identity
      ; route = attempt.route
      ; preparation = attempt.preparation
      ; policy = attempt.policy
      ; pending
      ; closed = None
      }
    in
    attempt.planned_pending <- Some pending;
    Ok { attempt; previous; candidate; positive_ack = Some reservation }
  ;;

  let check_basis t ~state ~candidate =
    match R.Reservation_attempt.outcome t.attempt with
    | Not_attempted
      when phys_equal state t.previous
           && phys_equal candidate t.candidate
           && Descendant_admission_data.metadata_only_candidate candidate -> Ok ()
    | Not_attempted | Acknowledged _ | Attempt_unconfirmed ->
      conflict "root reserve ACK candidate or actual attempt changed before writer"
  ;;

  (* Called only immediately after exact State assignment on successful persist.
     No callback, allocation, policy recheck or failure can erase this known ACK.
     Positive reservation object was prepared privately and remained unexposed. *)
  let install t = t.attempt.acknowledged <- t.positive_ack
end

module Root_close_acknowledgement = struct
  module R = Descendant_admission_data
  module Closed = Descendant_preparation.Closed_before_writer
  module GD = Goal_descendants
  module Pending = Goal_descendant_pending
  module Scope = Agent_store.Delegation_goal_origin.Scope

  type t =
    { reservation : R.Reservation.t
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; closed : Closed.t option
    }

  let conflict message =
    Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
  ;;

  let acknowledged (reservation : R.Reservation.t) ~closed =
    if
      not
        (Int.equal reservation.preparation.domain (Stdlib.Domain.self () :> int)
         && Int.equal reservation.route.domain (Stdlib.Domain.self () :> int)
         && Closed.belongs_to closed ~preparation:reservation.preparation)
    then conflict "root close belongs to another domain or physical child seal"
    else (
      match reservation.closed with
      | None -> Ok false
      | Some actual ->
        if phys_equal actual closed
        then Ok true
        else conflict "root close was acknowledged under another physical child seal")
  ;;

  let validate_candidate
        (reservation : R.Reservation.t)
        (candidate : Session_transition.t)
    =
    let open Result.Let_syntax in
    let%bind accounting =
      match candidate.delta with
      | Session_delta.Goal_descendants_changed update ->
        (match GD.Update.reason update with
         | Close_before_writer pending when Pending.equal pending reservation.pending ->
           Ok (GD.Update.accounting update)
         | Register_new_root _
         | Register_first_unrelated_child _
         | Register_child _
         | Register_branch _
         | Register_retained_branch _
         | Reserve _
         | Record_included _
         | Import _
         | Close_before_writer _ ->
           conflict "root close ACK is not its exact pending close")
      | _ -> conflict "root close ACK requires its direct metadata leaf"
    in
    let scope = R.Root_route.scope reservation.route in
    if
      R.metadata_only_candidate candidate
      && List.exists
           (GD.branches accounting ~goal_id:(Scope.goal_id scope))
           ~f:(fun branch ->
             Scope.equal (GD.Branch.scope branch) scope
             &&
             match GD.Branch.last branch with
             | Some (No_writer_closed actual) -> Pending.equal actual reservation.pending
             | Some (Included _) | None -> false)
    then Ok ()
    else conflict "root close ACK candidate has no exact terminal no-writer relation"
  ;;

  let prepare reservation ~closed ~previous ~candidate =
    let open Result.Let_syntax in
    let%bind known = acknowledged reservation ~closed in
    let%bind () =
      if known then conflict "root close is already acknowledged" else Ok ()
    in
    let%map () = validate_candidate reservation candidate in
    { reservation; previous; candidate; closed = Some closed }
  ;;

  let check_basis t ~state ~candidate =
    let open Result.Let_syntax in
    let%bind () =
      if
        phys_equal state t.previous
        && phys_equal candidate t.candidate
        && Option.is_none t.reservation.closed
      then Ok ()
      else conflict "root close ACK original basis or candidate changed"
    in
    validate_candidate t.reservation candidate
  ;;

  (* Exact State installation only; this positive outcome survives publication
     failure, later terminal-head replacement and policy/Actor retirement. *)
  let install t = t.reservation.closed <- t.closed
end

module Descendant_import_acknowledgement = struct
  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; acknowledgement : Descendant_admission_data.Import_acknowledgement.t option
    ; installed : Descendant_admission_data.Import_acknowledgement.t option ref
    }

  let check_basis t ~state ~candidate =
    if
      phys_equal t.previous state
      && phys_equal t.candidate candidate
      && Descendant_admission_data.metadata_only_candidate candidate
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"factual import ACK candidate or actual original basis changed"
           ~retryable:false
           ())
  ;;

  let install t = t.installed := t.acknowledgement
end

let descendant_reservations (set : Descendant_admission_data.Reservation_set.t) =
  let module R = Descendant_admission_data in
  let module O = Agent_store.Delegation_goal_origin in
  let open Result.Let_syntax in
  let%bind () = R.Reservation_set.check_claim set in
  let%bind () =
    if
      List.equal
        O.Scope.equal
        (List.map set.attempts ~f:R.Reservation_attempt.scope)
        (Descendant_preparation.scopes set.preparation)
    then Ok ()
    else
      Error (error Conflict "root reservations do not cover the complete frozen vector")
  in
  List.map set.attempts ~f:(fun attempt ->
    match R.Reservation_attempt.acknowledged attempt with
    | Some reservation
      when phys_equal reservation.preparation set.preparation
           && phys_equal reservation.attempt_identity attempt.identity -> Ok reservation
    | Some _ | None ->
      Error (error Conflict "root vector contains an unacknowledged or foreign attempt"))
  |> Result.all
;;

let check_descendant_before_writer (view : Descendant_admission_data.Before_writer.t) =
  let module R = Descendant_admission_data in
  let open Result.Let_syntax in
  let%bind () = Descendant_preparation.check_basis view.preparation in
  let%bind reservations = descendant_reservations view.reservations in
  let%bind () =
    if
      phys_equal view.preparation view.reservations.preparation
      && List.equal
           phys_equal
           reservations
           (List.map view.views ~f:(fun (root : R.Root_before_writer.t) ->
              root.reservation))
    then Ok ()
    else
      Error (error Conflict "prewriter views differ from exact retained root ACK vector")
  in
  let%bind () =
    List.fold_result view.views ~init:() ~f:(fun () root ->
      root.R.Root_before_writer.check_current ())
  in
  Descendant_preparation.check_basis view.preparation
;;

let prepare_descendant_before_writer ~preparation ~reservations ~views =
  let view =
    Descendant_admission_data.Before_writer.{ preparation; reservations; views }
  in
  let%map.Result () = check_descendant_before_writer view in
  reservations.phase <- Complete;
  view
;;

let check_descendant_admitted_views (views : Descendant_admission_data.Admitted_views.t) =
  let module R = Descendant_admission_data in
  let open Result.Let_syntax in
  let%bind () = Descendant_preparation.check_owner views.acknowledgement.preparation in
  let%bind reservations = descendant_reservations views.acknowledgement.reservations in
  let%bind () =
    if
      phys_equal views.confirmation.acknowledgement views.acknowledgement
      && List.equal
           phys_equal
           reservations
           (List.map views.views ~f:(fun (root : R.Root_admitted.t) -> root.reservation))
      && List.for_all views.views ~f:(fun (root : R.Root_admitted.t) ->
        phys_equal root.acknowledgement views.acknowledgement
        && phys_equal root.confirmation views.confirmation)
    then Ok ()
    else Error (error Conflict "admitted views differ from exact child ACK/root vector")
  in
  let%bind () =
    List.fold_result views.views ~init:() ~f:(fun () view ->
      view.R.Root_admitted.check_current ())
  in
  Descendant_preparation.check_owner views.acknowledgement.preparation
;;

let descendant_admitted_views ~acknowledgement ~confirmation ~views =
  let view =
    Descendant_admission_data.Admitted_views.{ acknowledgement; confirmation; views }
  in
  let%map.Result () = check_descendant_admitted_views view in
  view
;;

let check_descendant_ack (ack : Descendant_admission_data.Acknowledgement.t) =
  let open Result.Let_syntax in
  let%bind () = Descendant_admission_data.Acknowledgement.validate_content ack in
  if Descendant_preparation.acknowledged_by ack.preparation ~acknowledgement:ack.identity
  then Ok ()
  else Error (error Conflict "child canonical ACK has not been installed")
;;

let descendant_confirmation_import
      (reservation : Descendant_admission_data.Reservation.t)
      (ack : Descendant_admission_data.Acknowledgement.t)
      ~imports
  =
  let module R = Descendant_admission_data in
  let module GD = Goal_descendants in
  let module O = Agent_store.Delegation_goal_origin in
  let open Result.Let_syntax in
  let%bind () = check_descendant_ack ack in
  let receipt = Goal_descendant_included.create reservation.pending in
  let%bind () =
    if
      phys_equal reservation.preparation ack.preparation
      && List.mem ack.receipts receipt ~equal:Goal_descendant_included.equal
    then Ok ()
    else Error (error Conflict "root confirmation has a different actual child ACK")
  in
  let scope = R.Root_route.scope reservation.route in
  let positive (import : R.Import_acknowledgement.t) =
    phys_equal import.issuing_actor reservation.route.root_issuer
    && Recurrence_controller.same_store import.controller reservation.route.controller
    && Agent_protocol.Session_ref.equal
         (Recurrence_controller.session import.controller)
         (O.Scope.owner scope)
    && Runtime_source_owner.matches reservation.route.source_owner import.source_owner
    && Agent_store.Delegation_store.Reference.equal
         import.observation.reference
         (Goal_descendant_pending.branch_reference reservation.pending)
    && List.mem import.included receipt ~equal:Goal_descendant_included.equal
    && Option.exists import.installed.goal_descendants ~f:(fun accounting ->
      List.exists
        (GD.branches accounting ~goal_id:(O.Scope.goal_id scope))
        ~f:(fun branch ->
          O.Scope.equal (GD.Branch.scope branch) scope
          &&
          match GD.Branch.last branch with
          | Some (Included actual) -> Goal_descendant_included.equal actual receipt
          | Some (No_writer_closed _) | None -> false))
  in
  List.find imports ~f:positive
  |> Result.of_option
       ~error:
         (error
            Conflict
            "root receipt has no positive actual immediate-parent import ACK")
;;

let check_descendant_confirmation
      reservation
      acknowledgement
      (confirmation : Descendant_admission_data.Confirmation.t)
  =
  if phys_equal confirmation.acknowledgement acknowledgement
  then
    descendant_confirmation_import
      reservation
      acknowledgement
      ~imports:confirmation.imports
    |> Result.map ~f:ignore
  else Error (error Conflict "root confirmation has a different actual child ACK")
;;

let descendant_confirmation acknowledgement ~imports =
  let module R = Descendant_admission_data in
  let open Result.Let_syntax in
  let%bind reservations =
    descendant_reservations acknowledgement.R.Acknowledgement.reservations
  in
  let%map retained =
    List.fold_result reservations ~init:[] ~f:(fun retained reservation ->
      let%map actual =
        descendant_confirmation_import reservation acknowledgement ~imports
      in
      if List.mem retained actual ~equal:phys_equal
      then retained
      else retained @ [ actual ])
  in
  (* Each retained proof serves an exact root reservation. Canonical root order
     and physical dedup retain no more owners than the frozen scope vector. *)
  R.Confirmation.{ acknowledgement; imports = retained }
;;
