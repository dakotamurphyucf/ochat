open! Core
module A = Agent_session
module D = Agent_store.Delegation_store
module P = Agent_protocol

let unavailable code message = P.Error.create code ~message ~retryable:false ()

module Moderation_identity = struct
  module A = Agent_session
  module P = Agent_protocol

  let unavailable () =
    P.Error.create
      Permission_denied
      ~message:
        "delegation.owner_mediation_unavailable: native source identity lacks current \
         installed extensibility evidence"
      ~retryable:false
      ()
  ;;

  let checkpoint_observer checkpoint =
    let open Result.Let_syntax in
    let%bind checkpoint = A.Moderator_checkpoint.of_jsonaf checkpoint in
    let snapshot = A.Moderator_checkpoint.snapshot checkpoint in
    if snapshot.halted
    then Error (unavailable ())
    else
      Ok
        P.Invocation.
          { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  ;;

  let source_of_index (state : A.Session_state.t) index =
    A.Run_source_installation.captured
      (A.Run_state.installation index)
      ~generation:state.identity.generation
  ;;

  let installed_source (state : A.Session_state.t) =
    match state.run_state with
    | None -> Ok None
    | Some index ->
      (match (A.Run_state.installation index).source with
       | None -> Ok None
       | Some _ -> source_of_index state index |> Result.map ~f:Option.some)
  ;;

  let installed (state : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind index = Result.of_option state.run_state ~error:(unavailable ()) in
    let%bind source = source_of_index state index in
    let%bind pointer =
      Result.of_option (A.Run_state.moderator_lifecycle index) ~error:(unavailable ())
    in
    let%map () =
      A.Moderator_lifecycle.validate
        pointer
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        ~current_source:source
        ~executions:state.moderator_executions
    in
    (* Actual modern lifecycle claim provenance is identity DATA regardless of
       outcome. It is deliberately not Completed policy-restoration eligibility. *)
    source.observer
  ;;

  let of_state (state : A.Session_state.t) =
    let open Result.Let_syntax in
    match state.moderator with
    | None -> Ok None
    | Some checkpoint ->
      let%bind expected = installed state in
      let%bind actual = checkpoint_observer checkpoint in
      if P.Invocation.equal_observer expected actual
      then Ok (Some actual)
      else Error (unavailable ())
  ;;

  let of_moderator
        (moderator : A.Runtime_builder.Source.Moderator.t)
        ~(state : A.Session_state.t)
    =
    let open Result.Let_syntax in
    match state.moderator with
    | None -> Ok None
    | Some checkpoint ->
      (match moderator with
       | No_moderator | Legacy _ -> Error (unavailable ())
       | Extension expected ->
         let%bind actual = checkpoint_observer checkpoint in
         let%bind () =
           match state.run_state with
           | None -> Ok ()
           | Some index ->
             (match (A.Run_state.installation index).source with
              | None -> Ok ()
              | Some _ ->
                let%bind installed = source_of_index state index in
                if P.Invocation.equal_observer installed.observer actual
                then Ok ()
                else Error (unavailable ()))
         in
         if P.Invocation.equal_observer expected actual
         then Ok (Some actual)
         else Error (unavailable ()))
  ;;

  let of_source source ~state =
    let open Result.Let_syntax in
    let%bind () = A.Runtime_builder.Source.validate_target source state in
    of_moderator (A.Runtime_builder.Source.moderator source) ~state
  ;;
end

let requires_installed_source (state : A.Session_state.t) =
  match state.moderator, state.run_state with
  | None, _ -> false
  | Some _, None -> true
  | Some _, Some index -> Option.is_none (A.Run_state.moderator_lifecycle index)
;;

(* Installed current-use evidence is distinct from genuine construction Source
   admission below. Absence of a lifecycle pointer is a structural Source-proof
   mode, even when the Run source is installed. Present pointers retain strict
   State validation; invalid evidence never selects another proof. *)
let check_present_lifecycle (state : A.Session_state.t) =
  match state.run_state with
  | None -> Ok ()
  | Some index ->
    (match A.Run_state.moderator_lifecycle index with
     | None -> Ok ()
     | Some _ -> Moderation_identity.of_state state |> Result.map ~f:ignore)
;;

let moderation_source ?installed_source (state : A.Session_state.t) =
  match installed_source with
  | None -> Moderation_identity.of_state state
  | Some source ->
    let open Result.Let_syntax in
    let%bind () = check_present_lifecycle state in
    Moderation_identity.of_source source ~state
;;

let current_fingerprint ?installed_source state =
  let open Result.Let_syntax in
  let%bind moderator = moderation_source ?installed_source state in
  A.Delegation_authority.fingerprint ?moderator state
;;

let eligibility_fingerprint ~moderator state =
  let open Result.Let_syntax in
  let%bind () = check_present_lifecycle state in
  let%bind moderator = Moderation_identity.of_moderator moderator ~state in
  A.Delegation_authority.fingerprint ?moderator state
;;

let fingerprint ?source (state : Agent_session.Session_state.t) =
  let open Result.Let_syntax in
  let%bind moderator =
    match source with
    | None -> Moderation_identity.of_state state
    | Some source -> Moderation_identity.of_source source ~state
  in
  Agent_session.Delegation_authority.fingerprint ?moderator state
;;

let check_active (record : D.record) (expected : A.Session_state.t) =
  match record.admission.lifetime with
  | Independent _ -> Ok ()
  | Owned | Invocation_owned _ ->
    (match
       ( expected.lifecycle.desired
       , expected.lifecycle.observed
       , expected.halted
       , expected.failure )
     with
     | Running, (Idle | Running_turn _ | Waiting_for_permission _), false, None -> Ok ()
     | (Running | Stopped), _, _, _ ->
       Error (unavailable Permission_denied "native resource owned parent is inactive"))
;;

module Source_proof = struct
  type t =
    | State
    | Installed of A.Runtime_builder.Source.t

  let of_option = function
    | None -> State
    | Some source -> Installed source
  ;;

  (* These are the SAME immutable Source selected by an actual physical Cell,
     not interchangeable observer/hash payloads. Factory separately checks
     the observation's current physical membership around every wait. *)
  let equal t other =
    match t, other with
    | State, State -> true
    | Installed source, Installed other -> phys_equal source other
    | State, Installed _ | Installed _, State -> false
  ;;
end

let changed () =
  P.Error.create
    Conflict
    ~message:"native resource authority changed during validation"
    ~retryable:false
    ()
;;

module State_identity = struct
  type t =
    { domain : int
    ; session_id : P.Id.Session.t
    ; generation : int
    ; creating_principal : P.Id.Principal.t option
    ; spec : A.Session_state.Spec.Resource_identity.t
    ; source : P.Run_source.t option
    ; source_proof : Source_proof.t
    ; authority_sha256 : string
    ; initialization : A.Session_state.Runtime_initialization.t
    ; pending_initial_start : bool
    ; owner_lease_generation : int64
    }

  let capture ?installed_source (state : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind source = Moderation_identity.installed_source state in
    let%map authority_sha256 = current_fingerprint ?installed_source state in
    { domain = (Stdlib.Domain.self () :> int)
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; creating_principal = state.identity.creating_principal
    ; spec = A.Session_state.Spec.Resource_identity.capture state.spec
    ; source
    ; source_proof = Source_proof.of_option installed_source
    ; authority_sha256
    ; initialization = state.runtime_initialization
    ; pending_initial_start = state.pending_initial_start
    ; owner_lease_generation = state.counters.owner_lease_generation
    }
  ;;

  let check ?installed_source t ~(expected : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (P.Error.invalid_request "native authority basis belongs to another Eio domain")
    in
    let%bind () =
      if Source_proof.equal t.source_proof (Source_proof.of_option installed_source)
      then Ok ()
      else Error (changed ())
    in
    let%bind source = Moderation_identity.installed_source expected in
    let%bind authority_sha256 = current_fingerprint ?installed_source expected in
    if
      P.Id.Session.equal t.session_id expected.identity.session_id
      && Int.equal t.generation expected.identity.generation
      && Option.equal
           P.Id.Principal.equal
           t.creating_principal
           expected.identity.creating_principal
      && A.Session_state.Spec.Resource_identity.equal
           t.spec
           (A.Session_state.Spec.Resource_identity.capture expected.spec)
      && Option.equal P.Run_source.equal t.source source
      && String.equal t.authority_sha256 authority_sha256
      && A.Session_state.Runtime_initialization.equal
           t.initialization
           expected.runtime_initialization
      && Bool.equal t.pending_initial_start expected.pending_initial_start
      && Int64.equal t.owner_lease_generation expected.counters.owner_lease_generation
    then Ok ()
    else Error (changed ())
  ;;
end

module Source_window = struct
  module Liveness = struct
    type t =
      | Running
      | Stopped
    [@@deriving equal]

    let capture (state : A.Session_state.t) =
      match
        state.lifecycle.desired, state.lifecycle.observed, state.halted, state.failure
      with
      | Running, (Idle | Running_turn _ | Waiting_for_permission _), false, None ->
        Ok Running
      | Stopped, Stopped, false, None -> Ok Stopped
      | (Running | Stopped), _, _, _ -> Error (changed ())
    ;;
  end

  type t =
    { identity : State_identity.t
    ; source : A.Runtime_builder.Source.t
    ; stop_epoch : int64
    ; parent_stop_epoch : int64 option
    ; liveness : Liveness.t
    }

  let capture ~source (state : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind identity = State_identity.capture ~installed_source:source state in
    let%map liveness = Liveness.capture state in
    { identity
    ; source
    ; stop_epoch = state.stop_epoch
    ; parent_stop_epoch = state.parent_stop_epoch
    ; liveness
    }
  ;;

  let check t ~(expected : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = State_identity.check ~installed_source:t.source t.identity ~expected in
    let%bind liveness = Liveness.capture expected in
    if
      Int64.equal t.stop_epoch expected.stop_epoch
      && Option.equal Int64.equal t.parent_stop_epoch expected.parent_stop_epoch
      && Liveness.equal t.liveness liveness
    then Ok ()
    else Error (changed ())
  ;;
end

module Basis = struct
  module Stop_boundary = struct
    type t =
      | Independent
      | Bound of
          { stop_epoch : int64
          ; parent_stop_epoch : int64 option
          }

    let capture (record : D.record) (state : A.Session_state.t) =
      match record.admission.lifetime with
      | Independent _ -> Independent
      | Owned | Invocation_owned _ ->
        Bound
          { stop_epoch = state.stop_epoch; parent_stop_epoch = state.parent_stop_epoch }
    ;;

    let capture_resource_path observation =
      let state = A.Delegation_authority.Resource_path.Observation.state observation in
      match
        A.Delegation_authority.Resource_path.Observation.stop_boundary observation
      with
      | Ordinary ->
        Bound
          { stop_epoch = state.stop_epoch; parent_stop_epoch = state.parent_stop_epoch }
      | Independent_resource -> Independent
    ;;

    let check_liveness t record state =
      match t with
      | Independent -> Ok ()
      | Bound _ -> check_active record state
    ;;

    let check t (state : A.Session_state.t) =
      match t with
      | Independent -> true
      | Bound { stop_epoch; parent_stop_epoch } ->
        Int64.equal stop_epoch state.stop_epoch
        && Option.equal Int64.equal parent_stop_epoch state.parent_stop_epoch
    ;;
  end

  type t =
    { record : D.record
    ; identity : State_identity.t
    ; stop_boundary : Stop_boundary.t
    }

  let capture_with_boundary
        ?installed_source
        ~(record : D.record)
        ~stop_boundary
        (state : A.Session_state.t)
    =
    let open Result.Let_syntax in
    let%bind () =
      match record.stage, record.revocation with
      | Linked, None
        when P.Id.Session.equal record.key.parent_session_id state.identity.session_id
             && Int.equal record.key.parent_generation state.identity.generation -> Ok ()
      | (Reserved | Artifact_installed | Child_installed | Linked), _ ->
        Error (changed ())
    in
    let%bind () = A.Delegation_authority.check_invocation_owner record state in
    let%bind () = Stop_boundary.check_liveness stop_boundary record state in
    let%bind identity = State_identity.capture ?installed_source state in
    if String.equal identity.authority_sha256 record.admission.authority_sha256
    then Ok { record; identity; stop_boundary }
    else Error (changed ())
  ;;

  let capture ?installed_source ~record state =
    capture_with_boundary
      ?installed_source
      ~record
      ~stop_boundary:(Stop_boundary.capture record state)
      state
  ;;

  let capture_resource_path ?installed_source observation =
    capture_with_boundary
      ?installed_source
      ~record:(A.Delegation_authority.Resource_path.Observation.record observation)
      ~stop_boundary:(Stop_boundary.capture_resource_path observation)
      (A.Delegation_authority.Resource_path.Observation.state observation)
  ;;

  let check ?installed_source t ~(expected : A.Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = State_identity.check ?installed_source t.identity ~expected in
    let%bind () =
      if
        String.equal t.record.admission.authority_sha256 t.identity.authority_sha256
        && Stop_boundary.check t.stop_boundary expected
      then Ok ()
      else Error (changed ())
    in
    let%bind () = A.Delegation_authority.check_invocation_owner t.record expected in
    Stop_boundary.check_liveness t.stop_boundary t.record expected
  ;;
end
