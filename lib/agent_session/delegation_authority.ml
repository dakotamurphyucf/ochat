open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module C = Chat_response.Tool_capability

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
  }

let create ?(max_depth = 32) ~host ~reference ~capabilities () =
  { host; reference; capabilities; max_depth }
;;

let reference t = t.reference
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let unavailable message =
  Error (P.Error.create Invalid_state ~message ~retryable:false ())
;;

let fingerprint (parent : Session_state.t) =
  match parent.spec.protocol.persistence, parent.moderator with
  | Transient, _ ->
    unavailable "delegation.unavailable: a durable parent host is required"
  | _, Some _ ->
    unavailable
      "delegation.owner_mediation_unavailable: parent moderator restrictions require \
       owner-aware enforcement"
  | Durable, None ->
    Ok
      ([%sexp
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
       |> Chatmd_shell_spec.Source_ref.digest)
;;

let active (parent : Session_state.t) =
  match
    parent.lifecycle.desired, parent.lifecycle.observed, parent.halted, parent.failure
  with
  | Running, (Idle | Running_turn _ | Waiting_for_permission _), false, None -> Ok ()
  | _ -> denied "delegation.parent_inactive: parent execution authority is unavailable"
;;

let rec read_chain t ~visited ~depth ~expected reference =
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
    | true, None, Independent _ ->
      unavailable
        "delegation.lifetime_unavailable: independent resource ownership is not installed"
    | true, None, Owned -> Ok ()
  in
  let%bind parent = t.host.state record.key.parent_session_id in
  let%bind () = active parent in
  let%bind current_fingerprint = fingerprint parent in
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
  let%bind selected =
    Chat_response.Background_request.rebind_capabilities
      ~pins:record.admission.capability_pins
      ~capabilities:current
  in
  let%bind () =
    match String.equal (C.fingerprint selected) (C.fingerprint expected) with
    | true -> Ok ()
    | false ->
      denied "delegation.bindings_changed: inherited runtime needs fresh admission"
  in
  let%bind () =
    match parent.spec.delegation with
    | None -> Ok ()
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
      let%bind ancestor_record, _ =
        read_chain t ~visited ~depth:(depth + 1) ~expected:current ancestor
      in
      (match ancestor_record.D.stage with
       | Linked -> Ok ()
       | _ ->
         denied
           "delegation.ancestor_not_linked: parent management relationship is not \
            committed")
  in
  (* Parent reads and live resource lookup may yield. Repeat both durable and
     actor-backed checks before returning; lifecycle coordination owns the lease
     through actual execution and is responsible for cancellation after this point. *)
  let%bind latest = t.host.state parent.identity.session_id in
  let%bind () = active latest in
  let%bind latest_fingerprint = fingerprint latest in
  let%bind retained = t.host.resolve reference in
  match
    String.equal latest_fingerprint current_fingerprint && D.equal_record record retained
  with
  | true -> Ok (retained, latest)
  | false -> denied "delegation.authority_changed: admission changed during validation"
;;

let read t = read_chain t ~visited:[] ~depth:0 ~expected:t.capabilities t.reference

let check_preparation t ~session_id ~revision_id ~manifest_sha256 ~permission_profile =
  let open Result.Let_syntax in
  let%bind record, parent = read t in
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

let check_execution t =
  let open Result.Let_syntax in
  let%bind record, _ = read t in
  match record.stage with
  | Linked -> Ok ()
  | Reserved | Artifact_installed | Child_installed ->
    denied "delegation.not_linked: child management relationship is not committed"
;;
