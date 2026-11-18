open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module C = Chat_response.Tool_capability
module Origin = Agent_store.Delegation_goal_origin

module Binding_evidence = struct
  type t = C.t

  let of_registry registry = registry
end

type host =
  { state : P.Id.Session.t -> (Session_state.t, P.Error.t) result
  ; resolve : D.Reference.t -> (D.record, P.Error.t) result
  ; capabilities : P.Id.Session.t -> (C.t, P.Error.t) result
  }

module Moderation_current = struct
  type t =
    D.record
    -> expected:Session_state.t
    -> (P.Invocation.observer option, P.Error.t) result
end

type t =
  { host : host
  ; reference : D.Reference.t
  ; capabilities : C.t
  ; max_depth : int
  ; parent_stop_epoch : int64 option
  ; moderation : Moderation_current.t
  ; authorize_independent : (D.record -> (unit, P.Error.t) result) option
  ; authored_capabilities : (D.record -> public:C.t -> (C.t, P.Error.t) result) option
  ; policy_restoration_bindings :
      (promoting:P.Id.Session.t
       -> P.Id.Session.t
       -> (Binding_evidence.t, P.Error.t) result)
        option
  ; source_owner : Runtime_source_owner.Identity.t option
  }

let create
      ?(max_depth = 32)
      ?parent_stop_epoch
      ?(moderation = fun _ -> Ok None)
      ?moderation_current
      ?authorize_independent
      ?authored_capabilities
      ?policy_restoration_bindings
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
  ; moderation =
      Option.value moderation_current ~default:(fun _record ~expected ->
        moderation expected.Session_state.identity.session_id)
  ; authorize_independent
  ; authored_capabilities
  ; policy_restoration_bindings
  ; source_owner = Option.map source_owner ~f:Runtime_source_owner.identity
  }
;;

let reference t = t.reference
let original_capabilities t = t.capabilities
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

module Resource_path_data = struct
  type path =
    { source_owner : Runtime_source_owner.Identity.t
    ; max_depth : int
    ; reference : D.Reference.t
    ; records : D.record list
    ; leaf_parent_stop_epoch : int64 option
    }

  type t = path

  let issued_by t ~source_owner = Runtime_source_owner.matches source_owner t.source_owner
  let reference t = t.reference
  let records t = t.records

  let of_records
        ~source_owner
        ~max_depth
        ~(actual_leaf : Session_state.t)
        ~reference
        ~records
    =
    let open Result.Let_syntax in
    let%bind () =
      if max_depth > 0 && List.length records <= max_depth
      then Ok ()
      else denied "delegation.ancestry_limit: cyclic or excessive delegation ancestry"
    in
    let%bind () =
      if
        P.Id.Session.equal
          actual_leaf.identity.session_id
          reference.D.Reference.child_session_id
        && P.Id.Prompt_revision.equal
             actual_leaf.spec.prompt_revision_id
             reference.revision_id
        && Option.equal D.Reference.equal actual_leaf.spec.delegation (Some reference)
      then Ok ()
      else denied "delegation.resource_path: actual leaf differs from the private path"
    in
    let rec validate visited expected = function
      | [] ->
        if List.mem visited expected ~equal:P.Id.Session.equal
        then denied "delegation.ancestry_limit: cyclic or excessive delegation ancestry"
        else Ok ()
      | (record : D.record) :: rest ->
        let child = record.admission.child_session_id in
        let%bind () =
          if
            P.Id.Session.equal expected child
            && not (List.mem visited child ~equal:P.Id.Session.equal)
          then Ok ()
          else denied "delegation.resource_path: records are cyclic or noncontiguous"
        in
        validate (child :: visited) record.key.parent_session_id rest
    in
    let%bind () =
      match records with
      | [] -> denied "delegation.resource_path: a nonempty private path is required"
      | first :: _ ->
        if D.Reference.equal reference (D.reference first)
        then validate [] reference.child_session_id records
        else denied "delegation.resource_path: initial reference differs from the path"
    in
    Ok
      { source_owner = Runtime_source_owner.identity source_owner
      ; max_depth
      ; reference
      ; records
      ; leaf_parent_stop_epoch = actual_leaf.parent_stop_epoch
      }
  ;;

  module Prefix = struct
    type t =
      { path : path
      ; length : int
      }

    let path t = t.path
    let records t = List.take t.path.records t.length
    let terminal t = List.nth_exn t.path.records (t.length - 1)
    let reference t = D.reference (terminal t)
    let endpoint t = (terminal t).key.parent_session_id

    let parent t =
      if t.length < List.length t.path.records
      then Ok (Some { t with length = t.length + 1 })
      else Ok None
    ;;
  end

  let first path : Prefix.t = { path; length = 1 }

  module Host = struct
    type t =
      { state : P.Id.Session.t -> (Session_state.t, P.Error.t) result
      ; resolve : D.Reference.t -> (D.record, P.Error.t) result
      ; moderation : P.Id.Session.t -> (P.Invocation.observer option, P.Error.t) result
      ; authorize_independent : D.record -> (unit, P.Error.t) result
      }
  end

  module Observation = struct
    module Stop_boundary = struct
      type t =
        | Ordinary
        | Independent_resource
    end

    type t =
      { prefix : Prefix.t
      ; record : D.record
      ; state : Session_state.t
      ; stop_boundary : Stop_boundary.t
      }

    let record t = t.record
    let state t = t.state
    let stop_boundary t = t.stop_boundary

    let check_prefix t ~source_owner ~(prefix : Prefix.t) =
      if
        issued_by (Prefix.path prefix) ~source_owner
        && phys_equal (Prefix.path t.prefix) (Prefix.path prefix)
        && Int.equal t.prefix.length prefix.length
      then Ok ()
      else denied "delegation.resource_path: observation belongs to another prefix"
    ;;
  end
end

module Read_purpose = struct
  type t =
    | Ordinary
    | Resource_metadata of Resource_path_data.t
    | Native_preparation of
        { prefix : Resource_path_data.Prefix.t
        ; capabilities : C.t
        }

  let path = function
    | Ordinary -> None
    | Resource_metadata path -> Some path
    | Native_preparation { prefix; _ } -> Some (Resource_path_data.Prefix.path prefix)
  ;;

  let selection t reference expected =
    match t with
    | Ordinary | Resource_metadata _ -> expected
    | Native_preparation { prefix; capabilities } ->
      if D.Reference.equal reference (Resource_path_data.Prefix.reference prefix)
      then Some capabilities
      else expected
  ;;

  let frozen_record t ~depth =
    Option.bind (path t) ~f:(fun path -> List.nth (Resource_path_data.records path) depth)
  ;;

  let check_record t ~depth record =
    match path t, frozen_record t ~depth with
    | None, None -> Ok ()
    | Some _, Some expected when D.equal_record expected record ->
      (match record.D.stage with
       | Linked -> Ok ()
       | Reserved | Artifact_installed | Child_installed ->
         denied "delegation.not_linked: resource path is not committed")
    | (Some _ | None), (Some _ | None) ->
      denied "delegation.resource_path: private admission changed from the frozen path"
  ;;

  let check_parent t ~depth reference =
    match path t with
    | None -> Ok ()
    | Some _ ->
      let expected = Option.map (frozen_record t ~depth:(depth + 1)) ~f:D.reference in
      if Option.equal D.Reference.equal expected reference
      then Ok ()
      else
        denied "delegation.resource_path: parent topology differs from the complete path"
  ;;
end

module Read_context = struct
  module Bindings = struct
    type t =
      | Metadata_only
      | Live of
          { capabilities : P.Id.Session.t -> (C.t, P.Error.t) result
          ; selected : D.record -> public:C.t -> expected:C.t -> (unit, P.Error.t) result
          }
  end

  type t =
    { state : P.Id.Session.t -> (Session_state.t, P.Error.t) result
    ; resolve : D.Reference.t -> (D.record, P.Error.t) result
    ; bindings : Bindings.t
    ; moderation : Moderation_current.t
    ; independent_authorized : D.record -> (unit, P.Error.t) result
    ; max_depth : int
    ; original_stop : (D.Reference.t * int64 option) option
    ; leaf_stop : (D.Reference.t * int64 option) option
    }
end

module Read_frame = struct
  type t =
    { record : D.record
    ; parent : Session_state.t
    ; stop_boundary : Resource_path_data.Observation.Stop_boundary.t
    }
end

let read_context ?retained_bindings (t : t) max_depth =
  let capabilities =
    match retained_bindings with
    | None -> t.host.capabilities
    | Some (lookup : P.Id.Session.t -> (Binding_evidence.t, P.Error.t) result) -> lookup
  in
  Read_context.
    { state = t.host.state
    ; resolve = t.host.resolve
    ; bindings = Live { capabilities; selected = selected_bindings t }
    ; moderation = t.moderation
    ; independent_authorized = independent_authorized t
    ; max_depth
    ; original_stop = Some (t.reference, t.parent_stop_epoch)
    ; leaf_stop = None
    }
;;

let rec read_chain context ~purpose ~visited ~depth ~expected ~stop_boundary reference =
  let module Boundary = Resource_path_data.Observation.Stop_boundary in
  let open Result.Let_syntax in
  let%bind () =
    match
      depth < context.Read_context.max_depth
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
  let expected = Read_purpose.selection purpose reference expected in
  let%bind record = context.resolve reference in
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
      context.independent_authorized record
  in
  let%bind () = Read_purpose.check_record purpose ~depth record in
  let stop_boundary =
    match record.admission.lifetime with
    | Owned | Invocation_owned _ -> stop_boundary
    | Independent _ -> Boundary.Independent_resource
  in
  let%bind parent = context.state record.key.parent_session_id in
  let%bind () = check_invocation_owner record parent in
  let check_active parent =
    match stop_boundary with
    | Boundary.Ordinary -> active parent
    | Independent_resource -> Ok ()
  in
  let%bind () = check_active parent in
  let%bind () =
    let original =
      Option.bind context.original_stop ~f:(fun (original, epoch) ->
        if D.Reference.equal original reference then Some epoch else None)
    in
    let leaf =
      Option.bind context.leaf_stop ~f:(fun (leaf, epoch) ->
        if D.Reference.equal leaf reference then Some epoch else None)
    in
    (* The original authority's current epoch takes precedence when its terminal
       is also the path leaf. A path cannot revive a stale original authority. *)
    let expected_epoch =
      Option.value
        (Option.join (Option.first_some original leaf))
        ~default:(Option.value record.admission.parent_stop_epoch ~default:0L)
    in
    match stop_boundary, depth = 0 || Option.is_some original with
    | Boundary.Ordinary, true when not (Int64.equal parent.stop_epoch expected_epoch) ->
      denied "delegation.parent_stopped: parent stopped since runtime admission"
    | Ordinary, (true | false) | Independent_resource, (true | false) -> Ok ()
  in
  let%bind moderator = context.moderation record ~expected:parent in
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
  let%bind current =
    match expected, context.bindings with
    | None, (Metadata_only | Live _) -> Ok None
    | Some expected, Live bindings ->
      let%bind current = bindings.capabilities parent.identity.session_id in
      let%map () = bindings.selected record ~public:current ~expected in
      Some current
    | Some _, Metadata_only ->
      invalid_arg "native metadata cannot approve live capability bindings"
  in
  let%bind () = Read_purpose.check_parent purpose ~depth parent.spec.delegation in
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
      let%bind ancestor_frame, ancestors =
        read_chain
          context
          ~purpose
          ~visited
          ~depth:(depth + 1)
          ~expected:current
          ~stop_boundary
          ancestor
      in
      (match ancestor_frame.Read_frame.record.stage with
       | Linked -> Ok ancestors
       | _ ->
         denied
           "delegation.ancestor_not_linked: parent management relationship is not \
            committed")
  in
  (* The SAME frame-final revalidation runs after native lower-prefix frames
     return from real live binding checks at their terminal authority and above. *)
  let%bind () = context.independent_authorized record in
  let%bind source = authored_source record in
  let%bind () =
    match source, expected, context.bindings with
    | None, _, _ | Some _, None, (Metadata_only | Live _) -> Ok ()
    | Some _, Some expected, Live bindings ->
      let%bind current = bindings.capabilities parent.identity.session_id in
      bindings.selected record ~public:current ~expected
    | Some _, Some _, Metadata_only ->
      invalid_arg "native metadata cannot approve authored private bindings"
  in
  let%bind latest = context.state parent.identity.session_id in
  let%bind () = check_invocation_owner record latest in
  let%bind () = check_active latest in
  let%bind latest_moderator = context.moderation record ~expected:latest in
  let%bind latest_fingerprint = fingerprint ?moderator:latest_moderator latest in
  let%bind retained = context.resolve reference in
  match
    String.equal latest_fingerprint current_fingerprint
    && D.equal_record record retained
    &&
    match stop_boundary with
    | Boundary.Ordinary -> Int64.equal latest.stop_epoch parent.stop_epoch
    | Independent_resource -> true
  with
  | true ->
    let frame = Read_frame.{ record = retained; parent = latest; stop_boundary } in
    Ok (frame, frame :: ancestors)
  | false -> denied "delegation.authority_changed: admission changed during validation"
;;

let read_ancestry ?retained_bindings t =
  let%map.Result frame, frames =
    read_chain
      (read_context ?retained_bindings t t.max_depth)
      ~purpose:Ordinary
      ~visited:[]
      ~depth:0
      ~expected:(Some t.capabilities)
      ~stop_boundary:Ordinary
      t.reference
  in
  ( frame.Read_frame.record
  , frame.parent
  , List.map frames ~f:(fun frame -> frame.Read_frame.record) )
;;

let read t =
  let%map.Result record, parent, _ = read_ancestry t in
  record, parent
;;

module Resource_path = struct
  include Resource_path_data

  let observe_prefix
        ?moderation_current
        prefix
        ~source_owner
        ~(host : Host.t)
        ~(actual_leaf : Session_state.t)
    =
    let open Result.Let_syntax in
    let path = Prefix.path prefix in
    let%bind () =
      if
        issued_by path ~source_owner
        && P.Id.Session.equal
             actual_leaf.identity.session_id
             path.reference.child_session_id
        && P.Id.Prompt_revision.equal
             actual_leaf.spec.prompt_revision_id
             path.reference.revision_id
        && Option.equal
             D.Reference.equal
             actual_leaf.spec.delegation
             (Some path.reference)
        && Option.equal
             Int64.equal
             actual_leaf.parent_stop_epoch
             path.leaf_parent_stop_epoch
      then Ok ()
      else denied "delegation.resource_path: original Factory or actual leaf differs"
    in
    let context =
      Read_context.
        { state = host.state
        ; resolve = host.resolve
        ; bindings = Metadata_only
        ; moderation =
            Option.value moderation_current ~default:(fun _record ~expected ->
              host.moderation expected.Session_state.identity.session_id)
        ; independent_authorized =
            (fun record ->
              match record.D.admission.lifetime with
              | Owned | Invocation_owned _ -> Ok ()
              | Independent _ -> host.authorize_independent record)
        ; max_depth = path.max_depth
        ; original_stop = None
        ; leaf_stop = Some (path.reference, path.leaf_parent_stop_epoch)
        }
    in
    let%bind _, frames =
      read_chain
        context
        ~purpose:(Resource_metadata path)
        ~visited:[]
        ~depth:0
        ~expected:None
        ~stop_boundary:Ordinary
        path.reference
    in
    match List.nth frames (prefix.Prefix.length - 1) with
    | None -> denied "delegation.resource_path: verified endpoint is unavailable"
    | Some frame ->
      Ok
        Observation.
          { prefix
          ; record = frame.record
          ; state = frame.parent
          ; stop_boundary = frame.stop_boundary
          }
  ;;
end

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

let check_ancestry_with ?retained_bindings t =
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
  let%bind record, _, observed_records = read_ancestry ?retained_bindings t in
  let%bind () =
    match record.stage with
    | Linked -> Ok ()
    | Reserved | Artifact_installed | Child_installed ->
      denied "delegation.not_linked: child management relationship is not committed"
  in
  let%map () = validate_origin_chain observed_records in
  Checked_ancestry.{ records = observed_records; source_owner }
;;

let check_ancestry t = check_ancestry_with t

let check_policy_restoration_ancestry t ~promoting =
  let retained_bindings =
    Option.map t.policy_restoration_bindings ~f:(fun lookup -> lookup ~promoting)
  in
  check_ancestry_with t ?retained_bindings
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

let check_captured_artifact_record (record : D.record) ~origin =
  match Recurrence_child_origin.source_kind origin with
  | Artifact when Recurrence_child_origin.matches_record origin record -> Ok ()
  | Artifact | Authored_tool _ ->
    denied "delegation.captured_artifact: source differs from actual admission"
;;

let check_authored_preparation_record t record ~origin ~capabilities =
  let open Result.Let_syntax in
  let%bind source = authored_source record in
  match
    Option.equal D.Admission.equal_authored_tool source (Some origin)
    && String.equal (C.fingerprint capabilities) (C.fingerprint t.capabilities)
  with
  | true -> Ok ()
  | false ->
    denied
      "delegation.authored_resources: prepared source or resources differ from admission"
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
  let%bind () = check_captured_artifact_record record ~origin in
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
  let%bind () = check_authored_preparation_record t record ~origin ~capabilities in
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

let read_native_preparation t ~prefix ~check_resource ~session_id ~revision_id =
  let open Result.Let_syntax in
  let path = Resource_path.Prefix.path prefix in
  let%bind () =
    if
      Option.exists t.source_owner ~f:(fun original ->
        Runtime_source_owner.Identity.equal original path.Resource_path_data.source_owner)
      && D.Reference.equal (Resource_path.Prefix.reference prefix) t.reference
      && P.Id.Session.equal session_id t.reference.child_session_id
      && P.Id.Prompt_revision.equal revision_id t.reference.revision_id
    then Ok ()
    else denied "delegation.resource_path: terminal authority or prepared child differs"
  in
  let%bind () = check_resource () in
  let purpose =
    Read_purpose.Native_preparation { prefix; capabilities = t.capabilities }
  in
  let context = read_context t (Int.min t.max_depth path.max_depth) in
  let context =
    { context with leaf_stop = Some (path.reference, path.leaf_parent_stop_epoch) }
  in
  let%bind _, frames =
    read_chain
      context
      ~purpose
      ~visited:[]
      ~depth:0
      ~expected:None
      ~stop_boundary:Ordinary
      path.reference
  in
  let%bind frame =
    Result.of_option
      (List.nth frames (prefix.Resource_path_data.Prefix.length - 1))
      ~error:
        (P.Error.create
           Permission_denied
           ~message:
             "delegation.resource_path: verified preparation endpoint is unavailable"
           ~retryable:false
           ())
  in
  let%map () = check_resource () in
  frame.Read_frame.record, frame.parent
;;

let check_native_resource_preparation
      t
      ~prefix
      ~check_resource
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind record, parent =
    read_native_preparation t ~prefix ~check_resource ~session_id ~revision_id
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

let check_native_resource_authored_preparation
      t
      ~prefix
      ~check_resource
      ~origin
      ~capabilities
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind record, parent =
    read_native_preparation t ~prefix ~check_resource ~session_id ~revision_id
  in
  let%bind () = check_authored_preparation_record t record ~origin ~capabilities in
  check_preparation_record
    t
    record
    parent
    ~session_id
    ~revision_id
    ~manifest_sha256
    ~permission_profile
;;

let check_native_resource_captured_artifact_preparation
      t
      ~prefix
      ~check_resource
      ~origin
      ~session_id
      ~revision_id
      ~manifest_sha256
      ~permission_profile
  =
  let open Result.Let_syntax in
  let%bind record, parent =
    read_native_preparation t ~prefix ~check_resource ~session_id ~revision_id
  in
  let%bind () = check_captured_artifact_record record ~origin in
  check_preparation_record
    t
    record
    parent
    ~session_id
    ~revision_id
    ~manifest_sha256
    ~permission_profile
;;
