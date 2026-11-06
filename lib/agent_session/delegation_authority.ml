open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module C = Chat_response.Tool_capability
module Origin = Agent_store.Delegation_goal_origin

type host =
  { state : P.Id.Session.t -> (Session_state.t, P.Error.t) result
  ; resolve : D.Reference.t -> (D.record, P.Error.t) result
  ; capabilities : P.Id.Session.t -> (C.t, P.Error.t) result
  }

type t =
  { host : host
  ; reference : D.Reference.t
  ; capabilities : C.t
  ; max_depth : int
  ; parent_stop_epoch : int64 option
  ; moderation : P.Id.Session.t -> (P.Invocation.observer option, P.Error.t) result
  ; authorize_independent : (D.record -> (unit, P.Error.t) result) option
  ; authored_capabilities : (D.record -> public:C.t -> (C.t, P.Error.t) result) option
  ; source_owner : Runtime_source_owner.Identity.t option
  }

let create
      ?(max_depth = 32)
      ?parent_stop_epoch
      ?(moderation = fun _ -> Ok None)
      ?authorize_independent
      ?authored_capabilities
      ?source_owner
      ~host
      ~reference
      ~capabilities
      ()
  =
  { host
  ; reference
  ; capabilities
  ; max_depth
  ; parent_stop_epoch
  ; moderation
  ; authorize_independent
  ; authored_capabilities
  ; source_owner = Option.map source_owner ~f:Runtime_source_owner.identity
  }
;;

let reference t = t.reference
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let unavailable message =
  Error (P.Error.create Invalid_state ~message ~retryable:false ())
;;

module Checked_ancestry = struct
  type t =
    { records : D.record list
    ; source_owner : Runtime_source_owner.Identity.t
    }

  let records t = t.records
  let issued_by t ~source_owner = Runtime_source_owner.matches source_owner t.source_owner
end

let fingerprint ?moderator (parent : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    match parent.moderator, moderator with
    | None, None -> Ok ()
    | Some _, Some expected ->
      let%bind actual = Moderator_checkpoint.observer parent.moderator in
      let%bind halted = Moderator_checkpoint.is_halted parent.moderator in
      (match Option.equal P.Invocation.equal_observer actual (Some expected), halted with
       | true, false -> Ok ()
       | _ ->
         unavailable
           "delegation.owner_mediation_unavailable: parent moderator identity or state \
            changed")
    | _ ->
      unavailable
        "delegation.owner_mediation_unavailable: parent moderator restrictions require \
         owner-aware enforcement"
  in
  match parent.spec.protocol.persistence, parent.moderator with
  | Transient, _ ->
    unavailable "delegation.unavailable: a durable parent host is required"
  | Durable, _ ->
    let base =
      [%sexp
        ("ochat.delegation-authority.v1" : string)
      , (parent.identity.session_id : P.Id.Session.t)
      , (parent.identity.generation : int)
      , (parent.spec.prompt_revision_id : P.Id.Prompt_revision.t)
      , (parent.spec.permission_profile : string)
      , (parent.spec.permission_profile_digest : string)
      , (parent.spec.workspace_instance : Workspace_instance.t)
      , (parent.spec.runtime_policy : string option)
      , (parent.spec.delegation : D.Reference.t option)
      , (Option.map parent.automatic_turn_budget ~f:(fun budget ->
           budget.Automatic_turn_budget.policy)
         : Chat_response.Runtime_semantics.policy option)]
      |> Sexp.to_string_mach
      |> Chatmd_shell_spec.Source_ref.digest
    in
    Ok
      (match moderator with
       | None -> base
       | Some source ->
         [%sexp
           ("ochat.delegation-moderated-authority.v1" : string)
         , (base : string)
         , (source : P.Invocation.observer)]
         |> Sexp.to_string_mach
         |> Chatmd_shell_spec.Source_ref.digest)
;;

let active (parent : Session_state.t) =
  match
    parent.lifecycle.desired, parent.lifecycle.observed, parent.halted, parent.failure
  with
  | Running, (Idle | Running_turn _ | Waiting_for_permission _), false, None -> Ok ()
  | _ -> denied "delegation.parent_inactive: parent execution authority is unavailable"
;;

let independent_authorized t (record : D.record) =
  match record.admission.lifetime, t.authorize_independent with
  | (Owned | Invocation_owned _), _ -> Ok ()
  | Independent _, Some authorize -> authorize record
  | Independent _, None ->
    unavailable
      "delegation.lifetime_unavailable: independent resource ownership is not installed"
;;

let check_invocation_owner (record : D.record) (parent : Session_state.t) =
  match record.admission.lifetime with
  | Owned | Independent _ -> Ok ()
  | Invocation_owned { invocation_id } ->
    (match
       ( P.Id.Session.equal parent.identity.session_id record.key.parent_session_id
         && Int.equal parent.identity.generation record.key.parent_generation
       , List.find parent.invocations ~f:(fun invocation ->
           P.Id.Invocation.equal invocation.P.Invocation.context.id invocation_id) )
     with
     | true, Some { status = Dispatching; _ } -> Ok ()
     | _ -> denied "delegation.invocation_ended: one-off caller is no longer executing")
;;

let authored_source (record : D.record) =
  D.Admission.authored_source record.admission
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
;;

let selected_bindings t record ~public ~expected =
  let open Result.Let_syntax in
  let%bind source = authored_source record in
  let%bind ceiling =
    match source, t.authored_capabilities with
    | None, _ -> Ok public
    | Some _, Some resolve -> resolve record ~public
    | Some _, None ->
      denied
        "delegation.authored_unavailable: authored private resources are not admitted"
  in
  let%bind selected =
    Chat_response.Background_request.rebind_capabilities
      ~pins:record.admission.capability_pins
      ~capabilities:ceiling
  in
  match String.equal (C.fingerprint selected) (C.fingerprint expected) with
  | true -> Ok ()
  | false -> denied "delegation.bindings_changed: inherited runtime needs fresh admission"
;;

let rec read_chain t ~visited ~depth ~expected ~require_execution reference =
  let open Result.Let_syntax in
  let%bind () =
    match
      depth < t.max_depth
      && not
           (List.mem
              visited
              reference.D.Reference.child_session_id
              ~equal:P.Id.Session.equal)
    with
    | true -> Ok ()
    | false -> denied "delegation.ancestry_limit: cyclic or excessive delegation ancestry"
  in
  let visited = reference.child_session_id :: visited in
  let%bind record = t.host.resolve reference in
  let%bind () =
    match
      ( D.Reference.equal reference (D.reference record)
      , record.revocation
      , record.admission.lifetime )
    with
    | false, _, _ ->
      denied "delegation.identity_changed: private admission does not match"
    | _, Some _, _ -> denied "delegation.revoked: child execution authority was revoked"
    | true, None, (Owned | Invocation_owned _ | Independent _) ->
      independent_authorized t record
  in
  let require_execution =
    match record.admission.lifetime with
    | Owned | Invocation_owned _ -> require_execution
    | Independent _ -> false
  in
  let%bind parent = t.host.state record.key.parent_session_id in
  let%bind () = check_invocation_owner record parent in
  let check_active parent = if require_execution then active parent else Ok () in
  let%bind () = check_active parent in
  let%bind () =
    let expected =
      Option.value
        t.parent_stop_epoch
        ~default:(Option.value record.admission.parent_stop_epoch ~default:0L)
    in
    match
      require_execution && depth = 0 && not (Int64.equal parent.stop_epoch expected)
    with
    | false -> Ok ()
    | true -> denied "delegation.parent_stopped: parent stopped since runtime admission"
  in
  let%bind moderator = t.moderation parent.identity.session_id in
  let%bind current_fingerprint = fingerprint ?moderator parent in
  let%bind () =
    match
      P.Id.Session.equal parent.identity.session_id record.key.parent_session_id
      && Int.equal parent.identity.generation record.key.parent_generation
      && P.Id.Prompt_revision.equal
           parent.spec.prompt_revision_id
           record.admission.parent_revision_id
      && String.equal current_fingerprint record.admission.authority_sha256
    with
    | true -> Ok ()
    | false ->
      denied
        "delegation.authority_changed: parent source, generation, policy or workspace \
         changed"
  in
  let%bind current = t.host.capabilities parent.identity.session_id in
  let%bind () = selected_bindings t record ~public:current ~expected in
  let%bind ancestors =
    match parent.spec.delegation with
    | None -> Ok []
    | Some ancestor ->
      let%bind () =
        match
          P.Id.Session.equal ancestor.child_session_id parent.identity.session_id
          && P.Id.Prompt_revision.equal
               ancestor.revision_id
               parent.spec.prompt_revision_id
        with
        | true -> Ok ()
        | false ->
          denied "delegation.ancestry_changed: parent has a foreign delegation reference"
      in
      let%bind ancestor_record, _, ancestors =
        read_chain
          t
          ~visited
          ~depth:(depth + 1)
          ~expected:current
          ~require_execution
          ancestor
      in
      (match ancestor_record.D.stage with
       | Linked -> Ok ancestors
       | _ ->
         denied
           "delegation.ancestor_not_linked: parent management relationship is not \
            committed")
  in
  (* Parent reads and live resource lookup may yield. Repeat both durable and
     actor-backed checks before returning; lifecycle coordination owns the lease
     through actual execution and is responsible for cancellation after this point. *)
  let%bind () = independent_authorized t record in
  let%bind source = authored_source record in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some _ ->
      (* The private closure is scoped to this edge. Never pass it as the public
         selection when walking the parent's own ancestry. Recheck its source-bound
         grant after yielding ancestor lookups, before final durable/state checks. *)
      let%bind current = t.host.capabilities parent.identity.session_id in
      selected_bindings t record ~public:current ~expected
  in
  let%bind latest = t.host.state parent.identity.session_id in
  let%bind () = check_invocation_owner record latest in
  let%bind () = check_active latest in
  let%bind latest_moderator = t.moderation latest.identity.session_id in
  let%bind latest_fingerprint = fingerprint ?moderator:latest_moderator latest in
  let%bind retained = t.host.resolve reference in
  match
    String.equal latest_fingerprint current_fingerprint
    && D.equal_record record retained
    && ((not require_execution) || Int64.equal latest.stop_epoch parent.stop_epoch)
  with
  | true -> Ok (retained, latest, retained :: ancestors)
  | false -> denied "delegation.authority_changed: admission changed during validation"
;;

let read_ancestry t =
  read_chain
    t
    ~visited:[]
    ~depth:0
    ~expected:t.capabilities
    ~require_execution:true
    t.reference
;;

let read t =
  let%map.Result record, parent, _ = read_ancestry t in
  record, parent
;;

let anchor_matches (record : D.record) anchor =
  let module A = Origin.Anchor in
  P.Id.Session.equal (A.parent_session_id anchor) record.key.parent_session_id
  && Int.equal (A.parent_generation anchor) record.key.parent_generation
  && P.Id.Principal.equal (A.principal_id anchor) record.key.principal_id
  && P.Idempotency_key.equal (A.key anchor) record.key.idempotency_key
  && P.Id.Session.equal (A.child_session_id anchor) record.admission.child_session_id
  && P.Id.Transaction.equal (A.transaction_id anchor) record.admission.transaction_id
;;

let validate_origin_edge (record : D.record) ancestor =
  let module Scope = Origin.Scope in
  let open Result.Let_syntax in
  match record.admission.goal_origin with
  | None -> Ok ()
  | Some origin ->
    let scopes = Origin.scopes origin in
    (* Positive Unrelated is an explicit birth boundary, including ordinary
       independently authorized management creation. Unknown is never one. *)
    if List.is_empty scopes
    then Ok ()
    else (
      let%bind inherited =
        match ancestor with
        | None -> Ok []
        | Some (ancestor : D.record) ->
          (match ancestor.admission.goal_origin with
           | None ->
             unavailable
               "delegation.goal_origin_unknown: inherited Goal origin is unavailable"
           | Some origin -> Ok (Origin.scopes origin))
      in
      let%bind () =
        List.fold_result inherited ~init:() ~f:(fun () expected ->
          if List.exists scopes ~f:(Scope.equal expected)
          then Ok ()
          else
            denied
              "delegation.goal_origin_changed: inherited Goal scope was changed or \
               omitted")
      in
      List.fold_result scopes ~init:() ~f:(fun () scope ->
        if
          List.exists inherited ~f:(Scope.equal scope)
          || anchor_matches record (Scope.branch scope)
        then Ok ()
        else
          denied
            "delegation.goal_origin_changed: Goal scope does not name its actual \
             first-child edge"))
;;

let rec validate_origin_chain = function
  | [] -> Ok ()
  | record :: ancestors ->
    let open Result.Let_syntax in
    let%bind () = validate_origin_edge record (List.hd ancestors) in
    validate_origin_chain ancestors
;;

let check_ancestry t =
  let open Result.Let_syntax in
  let%bind source_owner =
    Result.of_option
      t.source_owner
      ~error:
        (P.Error.create
           Invalid_state
           ~message:"delegation.issuer_unavailable: ancestry guard has no Factory issuer"
           ~retryable:false
           ())
  in
  let%bind record, _, observed_records = read_ancestry t in
  let%bind () =
    match record.stage with
    | Linked -> Ok ()
    | Reserved | Artifact_installed | Child_installed ->
      denied "delegation.not_linked: child management relationship is not committed"
  in
  let%map () = validate_origin_chain observed_records in
  Checked_ancestry.{ records = observed_records; source_owner }
;;

let check_preparation_record
      t
      (record : D.record)
      (parent : Session_state.t)
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind () =
    match record.stage with
    | Artifact_installed | Child_installed | Linked -> Ok ()
    | Reserved ->
      unavailable "delegation.artifact_pending: child artifact is not installed"
  in
  match
    P.Id.Session.equal session_id t.reference.child_session_id
    && P.Id.Prompt_revision.equal revision_id t.reference.revision_id
    && String.equal manifest_sha256 record.admission.manifest_sha256
    && String.equal permission_profile.Permission_policy.id parent.spec.permission_profile
    && String.equal
         permission_profile.revision_digest
         parent.spec.permission_profile_digest
  with
  | true -> Ok ()
  | false ->
    denied "delegation.child_identity: child or permission profile differs from admission"
;;

let check_preparation t ~session_id ~revision_id ~manifest_sha256 ~permission_profile =
  let open Result.Let_syntax in
  let%bind record, parent = read t in
  check_preparation_record
    t
    record
    parent
    ~session_id
    ~revision_id
    ~manifest_sha256
    ~permission_profile
;;

let check_captured_artifact_preparation
      t
      ~origin
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind record, parent = read t in
  let%bind () =
    match Recurrence_child_origin.source_kind origin with
    | Artifact when Recurrence_child_origin.matches_record origin record -> Ok ()
    | Artifact | Authored_tool _ ->
      denied "delegation.captured_artifact: source differs from actual admission"
  in
  check_preparation_record
    t
    record
    parent
    ~session_id
    ~revision_id
    ~manifest_sha256
    ~permission_profile
;;

let check_authored_preparation
      t
      ~origin
      ~capabilities
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind record, parent = read t in
  let%bind source = authored_source record in
  let%bind () =
    match
      Option.equal D.Admission.equal_authored_tool source (Some origin)
      && String.equal (C.fingerprint capabilities) (C.fingerprint t.capabilities)
    with
    | true -> Ok ()
    | false ->
      denied
        "delegation.authored_resources: prepared source or resources differ from \
         admission"
  in
  check_preparation_record
    t
    record
    parent
    ~session_id
    ~revision_id
    ~manifest_sha256
    ~permission_profile
;;

let check_execution t =
  let open Result.Let_syntax in
  let%bind record, _ = read t in
  match record.stage with
  | Linked -> Ok ()
  | Reserved | Artifact_installed | Child_installed ->
    denied "delegation.not_linked: child management relationship is not committed"
;;
