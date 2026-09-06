open! Core

type update =
  | Generic of Agent_protocol.Grant.t
  | Shell of Session.Shell_state.t * Agent_protocol.Grant.t

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let opaque_id kind raw =
  let digest = Digestif.SHA256.(digest_string (kind ^ ":" ^ raw) |> to_hex) in
  Agent_protocol.Id.Grant.of_string ("grt_" ^ digest) |> Result.ok |> Option.value_exn
;;

let timestamp_of_ns nanoseconds =
  nanoseconds
  |> Int63.of_int64_exn
  |> Time_ns.of_int63_ns_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let ns_of_timestamp timestamp =
  timestamp
  |> Agent_protocol.Timestamp.to_time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
;;

let projected_state ~now_ns ~expires_at_ns ~revoked_at_ns =
  if Option.is_some revoked_at_ns
  then Agent_protocol.Grant.Revoked
  else if Option.exists expires_at_ns ~f:(fun expires -> Int64.(expires < now_ns))
  then Expired
  else Active
;;

let principal_id ~session_id ~creating_principal user_id =
  let parsed =
    Option.bind user_id ~f:(fun value ->
      Agent_protocol.Id.Principal.of_string value |> Result.ok)
  in
  match parsed, creating_principal with
  | Some principal, _ -> principal
  | None, Some principal -> principal
  | None, None ->
    let session = Agent_protocol.Id.Session.to_string session_id in
    let digest = Digestif.SHA256.(digest_string ("system:" ^ session) |> to_hex) in
    Agent_protocol.Id.Principal.of_string ("pri_" ^ digest)
    |> Result.ok
    |> Option.value_exn
;;

let optional_timestamp = Option.map ~f:timestamp_of_ns

let normalize_generic ~now grant =
  match grant.Agent_protocol.Grant.state, grant.expires_at with
  | Active, Some expires_at when Agent_protocol.Timestamp.compare expires_at now < 0 ->
    { grant with state = Expired }
  | Active, _ | Revoked, _ | Expired, _ -> grant
;;

let approval_scope = function
  | Session.Shell_state.Approval_scope.Exact_session -> Agent_protocol.Grant.Exact_session
  | Prefix_session _ -> Prefix_session
  | Durable_exact -> Durable_exact
;;

let approval ~now ~session_id ~creating_principal grant =
  let module Persisted = Session.Shell_state.Approval_grant in
  let now_ns = ns_of_timestamp now in
  Agent_protocol.Grant.
    { id = opaque_id "shell-approval" grant.Persisted.grant_id
    ; session_id
    ; principal_id = principal_id ~session_id ~creating_principal grant.user_id
    ; tool_name = "shell"
    ; identity_digest = grant.command_sha256
    ; scope = approval_scope grant.scope
    ; state =
        projected_state
          ~now_ns
          ~expires_at_ns:grant.expires_at_ns
          ~revoked_at_ns:grant.revoked_at_ns
    ; created_at = timestamp_of_ns grant.created_at_ns
    ; expires_at = optional_timestamp grant.expires_at_ns
    ; revoked_at = optional_timestamp grant.revoked_at_ns
    ; revocation_reason = grant.revocation_reason
    }
;;

let manifest ~now ~session_id ~creating_principal grant =
  let module Persisted = Session.Shell_state.Manifest_grant in
  let now_ns = ns_of_timestamp now in
  Agent_protocol.Grant.
    { id = opaque_id "shell-manifest" grant.Persisted.grant_id
    ; session_id
    ; principal_id = principal_id ~session_id ~creating_principal grant.user_id
    ; tool_name = "shell.manifest"
    ; identity_digest = grant.manifest_sha256
    ; scope = Exact_session
    ; state =
        projected_state
          ~now_ns
          ~expires_at_ns:grant.expires_at_ns
          ~revoked_at_ns:grant.revoked_at_ns
    ; created_at = timestamp_of_ns grant.created_at_ns
    ; expires_at = optional_timestamp grant.expires_at_ns
    ; revoked_at = optional_timestamp grant.revoked_at_ns
    ; revocation_reason = grant.revocation_reason
    }
;;

let project_manifest = manifest

let list ~now ~session_id ~creating_principal ~generic ~shell =
  List.map generic ~f:(normalize_generic ~now)
  @ List.map shell.Session.Shell_state.approval_grants ~f:(fun grant ->
    approval ~now ~session_id ~creating_principal grant)
  @ List.map shell.manifest_grants ~f:(fun grant ->
    manifest ~now ~session_id ~creating_principal grant)
;;

let revoke_generic ~now ~reason grant =
  let grant = normalize_generic ~now grant in
  if not (Agent_protocol.Grant.equal_state grant.state Active)
  then Error (error Already_resolved "grant is not active")
  else
    Ok
      (Generic
         { grant with
           state = Revoked
         ; revoked_at = Some now
         ; revocation_reason = Some reason
         })
;;

let revoke_approval ~now ~reason ~session_id ~creating_principal shell raw_id =
  let module Persisted = Session.Shell_state.Approval_grant in
  let found =
    List.find shell.Session.Shell_state.approval_grants ~f:(fun grant ->
      String.equal grant.Persisted.grant_id raw_id)
  in
  match found with
  | None -> None
  | Some grant ->
    let projected = approval ~now ~session_id ~creating_principal grant in
    if not (Agent_protocol.Grant.equal_state projected.state Active)
    then Some (Error (error Already_resolved "grant is not active"))
    else (
      let revoked_at_ns = Some (ns_of_timestamp now) in
      let approval_grants =
        List.map shell.approval_grants ~f:(fun candidate ->
          if String.equal candidate.Persisted.grant_id raw_id
          then { candidate with revoked_at_ns; revocation_reason = Some reason }
          else candidate)
      in
      let shell = { shell with approval_grants } in
      let grant =
        approval
          ~now
          ~session_id
          ~creating_principal
          { grant with revoked_at_ns; revocation_reason = Some reason }
      in
      Some (Ok (Shell (shell, grant))))
;;

let revoke_manifest ~now ~reason ~session_id ~creating_principal shell raw_id =
  let module Persisted = Session.Shell_state.Manifest_grant in
  let found =
    List.find shell.Session.Shell_state.manifest_grants ~f:(fun grant ->
      String.equal grant.Persisted.grant_id raw_id)
  in
  match found with
  | None -> None
  | Some grant ->
    let projected = manifest ~now ~session_id ~creating_principal grant in
    if not (Agent_protocol.Grant.equal_state projected.state Active)
    then Some (Error (error Already_resolved "grant is not active"))
    else (
      let revoked_at_ns = Some (ns_of_timestamp now) in
      let manifest_grants =
        List.map shell.manifest_grants ~f:(fun candidate ->
          if String.equal candidate.Persisted.grant_id raw_id
          then { candidate with revoked_at_ns; revocation_reason = Some reason }
          else candidate)
      in
      let shell = { shell with manifest_grants } in
      let grant =
        manifest
          ~now
          ~session_id
          ~creating_principal
          { grant with revoked_at_ns; revocation_reason = Some reason }
      in
      Some (Ok (Shell (shell, grant))))
;;

let revoke_shell ~now ~reason ~session_id ~creating_principal shell grant_id =
  let approval =
    List.find shell.Session.Shell_state.approval_grants ~f:(fun grant ->
      Agent_protocol.Id.Grant.compare (opaque_id "shell-approval" grant.grant_id) grant_id
      = 0)
  in
  match approval with
  | Some grant ->
    revoke_approval ~now ~reason ~session_id ~creating_principal shell grant.grant_id
    |> Option.value_exn
  | None ->
    let manifest =
      List.find shell.manifest_grants ~f:(fun grant ->
        Agent_protocol.Id.Grant.compare
          (opaque_id "shell-manifest" grant.grant_id)
          grant_id
        = 0)
    in
    (match manifest with
     | Some grant ->
       revoke_manifest ~now ~reason ~session_id ~creating_principal shell grant.grant_id
       |> Option.value_exn
     | None -> Error (error Invalid_request "grant does not exist"))
;;

let revoke ~now ~reason ~session_id ~creating_principal ~generic ~shell grant_id =
  match
    List.find generic ~f:(fun grant ->
      Agent_protocol.Id.Grant.compare grant.Agent_protocol.Grant.id grant_id = 0)
  with
  | Some grant -> revoke_generic ~now ~reason grant
  | None -> revoke_shell ~now ~reason ~session_id ~creating_principal shell grant_id
;;
