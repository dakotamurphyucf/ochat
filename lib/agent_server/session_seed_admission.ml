open! Core
module P = Agent_protocol
module S = Agent_session
module R = P.Session_seed.Request
module D = Document_schema

type t =
  { request : R.t
  ; definition : S.Prompt_definition.t
  ; workspace : S.Workspace_definition.t
  ; revision_id : P.Id.Prompt_revision.t
  ; root_invocation :
      Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  ; inference_target : Inference.Selection.t
  ; profile : S.Permission_policy.t
  ; configuration_revision : int64
  }

let reject message = Error (P.Error.invalid_request message)
let workspace t = t.workspace

let common request ~definition ~workspace ~profile ~checkpoint =
  checkpoint ();
  let spec = R.spec request in
  if not definition.S.Prompt_definition.enabled
  then reject "seed prompt definition is disabled"
  else if not (String.equal definition.permission_profile profile.S.Permission_policy.id)
  then reject "seed permission profile differs from the current definition"
  else if
    not
      (S.Prompt_definition.allows_workspace
         definition
         workspace.S.Workspace_definition.id)
  then reject "seed prompt is not permitted in the requested workspace"
  else (
    match spec.P.Session.Spec.prompt, spec.workspace with
    | Catalog prompt_id, Configured workspace_id ->
      if not (P.Id.Prompt_definition.equal prompt_id definition.id)
      then reject "seed selector differs from the current definition"
      else if not (P.Id.Workspace_definition.equal workspace_id workspace.id)
      then reject "seed workspace differs from the admitted definition"
      else if
        not
          (Option.value_map
             spec.permission_profile
             ~default:true
             ~f:(String.equal profile.id))
      then reject "seed explicitly requested another permission profile"
      else Ok ()
    | (Local_path _ | Generated _), _ | Catalog _, (Current | Local_path _) ->
      reject "seed requires an authored catalog root and configured workspace")
;;

let captured selection =
  match Inference.Selection.view selection with
  | Captured _ -> Ok ()
  | Unresolved -> reject "seed requires a captured inference selection"
;;

let catalog
      request
      ~definition
      ~revision
      ~workspace
      ~profile
      ~inference_target
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind () = common request ~definition ~workspace ~profile ~checkpoint in
  let%bind () = captured inference_target in
  let actual_definition = S.Prompt_revision.definition revision in
  if not (P.Id.Prompt_definition.equal definition.id actual_definition.id)
  then reject "seed revision belongs to another definition"
  else (
    match R.code request with
    | Catalog { definition_id; expected_revision } ->
      if
        (not (P.Id.Prompt_definition.equal definition_id definition.id))
        || not
             (P.Id.Prompt_revision.equal
                expected_revision
                (S.Prompt_revision.id revision))
      then reject "seed catalog revision changed"
      else (
        checkpoint ();
        Ok
          { request
          ; definition
          ; workspace
          ; revision_id = S.Prompt_revision.id revision
          ; root_invocation = S.Prompt_definition.root_invocation definition
          ; inference_target
          ; profile
          ; configuration_revision = 0L
          })
    | Retained _ -> reject "retained seed selection requires retained admission")
;;

let retained
      request
      ~(state : S.Session_state.t)
      ~definition
      ~workspace
      ~current_profile
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind () =
    common request ~definition ~workspace ~profile:current_profile ~checkpoint
  in
  let%bind () = captured state.spec.inference_target in
  match R.code request, state.spec.protocol.prompt, state.spec.delegation with
  | Retained _, Catalog definition_id, None ->
    if
      (not (P.Id.Prompt_definition.equal definition_id definition.id))
      || not
           (Option.equal
              P.Id.Prompt_definition.equal
              state.spec.prompt_definition_id
              (Some definition.id))
    then reject "retained seed is not the current authored catalog root"
    else if
      not
        (P.Session.equal_execution_host
           state.spec.protocol.execution_host
           (R.spec request).execution_host)
    then reject "retained seed execution host differs"
    else if
      (not (String.equal state.spec.permission_profile current_profile.id))
      || (not
            (String.equal
               state.spec.permission_profile_digest
               current_profile.revision_digest))
      || not
           (Option.equal String.equal state.spec.runtime_policy definition.runtime_policy)
    then
      reject
        "current definition no longer authorizes the retained profile or runtime policy"
    else (
      checkpoint ();
      Ok
        { request
        ; definition
        ; workspace
        ; revision_id = state.spec.prompt_revision_id
        ; root_invocation = state.spec.root_invocation
        ; inference_target = state.spec.inference_target
        ; profile = current_profile
        ; configuration_revision = state.spec.configuration_revision
        })
  | Catalog _, _, _ -> reject "catalog seed selection requires current revision admission"
  | Retained _, (Generated _ | Local_path _), _ | Retained _, Catalog _, Some _ ->
    reject "generated and delegated sources cannot seed an independent authored root"
;;

let document_error error =
  P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

module Prelayout = struct
  type recipe = t

  type t =
    { recipe : recipe
    ; identity : S.Session_state.Identity.t
    ; workspace_instance_id : P.Id.Workspace_instance.t
    ; workspace_projection : Jsonaf.t
    ; limits : D.Limits.t
    }

  let with_work ~max_work_steps ~checkpoint f =
    let module Invocation = struct
      exception Work_limit
    end
    in
    if max_work_steps <= 0
    then reject "seed creation work bound must be positive"
    else (
      let steps = ref 0 in
      let poll () =
        if !steps >= max_work_steps then raise Invocation.Work_limit;
        Int.incr steps;
        checkpoint ()
      in
      try f poll with
      | Invocation.Work_limit -> reject "seed creation work bound exceeded")
  ;;

  let admit_json json ~limits ~poll =
    D.Encoding.render
      ~limits
      ~byte_limit_context:"seed creation document exceeds bounds"
      ~checkpoint:poll
      ~f:(fun encoding -> D.Encoding.raw encoding json)
    |> Result.map ~f:Fn.ignore
    |> Result.map_error ~f:document_error
  ;;

  let spec recipe workspace_instance conflict_domain =
    S.Session_state.Spec.
      { protocol = R.spec recipe.request
      ; prompt_definition_id = Some recipe.definition.id
      ; root_invocation = recipe.root_invocation
      ; prompt_revision_id = recipe.revision_id
      ; configuration_revision = recipe.configuration_revision
      ; inference_target = recipe.inference_target
      ; delegation = None
      ; workspace_instance
      ; permission_profile = recipe.profile.id
      ; permission_profile_digest = recipe.profile.revision_digest
      ; runtime_policy = recipe.definition.runtime_policy
      ; quota_key = Some { S.Quota_key.conflict_domain; prompt_id = recipe.definition.id }
      }
  ;;

  let fields recipe identity spec =
    let transcript = R.transcript recipe.request in
    let state =
      S.Session_state.create_fields
        ~identity
        ~spec
        ~desired:P.Session.Stopped
        ~initial_history:(P.Session_seed.Transcript.entries transcript)
    in
    let next_history_sequence =
      P.Session_seed.Transcript.next_history_sequence transcript
    in
    { state with
      runtime_initialization = Pending { fresh_history = false }
    ; conversation =
        { state.conversation with
          initial_prompt_entry_count = 0
        ; next_history_sequence
        ; reserved_history_through = next_history_sequence
        }
    }
  ;;

  let admit
        recipe
        ~identity
        ~workspace_instance_id
        ~workspace_projection
        ~command_audit
        ~limits
        ~max_work_steps
        ~checkpoint
    =
    with_work ~max_work_steps ~checkpoint (fun poll ->
      let open Result.Let_syntax in
      let%bind () = admit_json (R.to_json recipe.request) ~limits ~poll in
      let%bind () = admit_json workspace_projection ~limits ~poll in
      let%bind conflict_domain =
        match D.Json.field workspace_projection ~name:"conflict_domain" with
        | Value (`String domain) -> Ok domain
        | Absent | Null | Value _ ->
          reject "workspace sizing projection has no conflict domain"
      in
      poll ();
      let state =
        fields recipe identity (spec recipe workspace_projection conflict_domain)
      in
      let%bind payload =
        S.Session_state_document.project_fields
          state
          ~spec:(fun spec -> S.Session_state_document.project_spec spec ~workspace:Fn.id)
          ~limits
      in
      let%bind () = admit_json payload ~limits ~poll in
      let%bind document =
        D.Document.create
          ~limits
          ~kind:"session.state"
          ~version:S.Session_state_document.current_version
          ~payload
        |> Result.map_error ~f:document_error
      in
      let%bind delta =
        S.Session_delta_document.project_created document ~limits
        |> Result.map_error ~f:document_error
      in
      let summary =
        S.Session_state.summary_fields state ~workspace:(fun _ -> workspace_instance_id)
      in
      let event =
        P.Event.Durable.of_payload
          ~session_id:identity.session_id
          ~sequence:1L
          ~revision:1L
          ~timestamp:identity.updated_at
          (Session_created summary)
      in
      let%bind () = admit_json (S.Durable_event_document.to_jsonaf event) ~limits ~poll in
      let%bind event =
        S.Durable_event_document.create event ~limits
        |> Result.map_error ~f:document_error
      in
      poll ();
      let%bind transaction =
        Agent_store.Transaction.create
          ~limits
          ~session_id:identity.session_id
          ~generation:identity.generation
          ~transaction_sequence:1L
          ~previous_transaction_hash:None
          ~session_revision:1L
          ~first_event_sequence:(Some 1L)
          ~last_event_sequence:(Some 1L)
          ~accepted_at_ns:Int64.max_value
          ~command_audit:(Some command_audit)
          ~delta
          ~durable_events:[ S.Durable_event_document.document event ]
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      let%bind () =
        admit_json
          (D.Document.json
             (Agent_store.Document_record.document
                (Agent_store.Transaction.Stored.record
                   (Agent_store.Transaction.stored transaction))))
          ~limits
          ~poll
      in
      Ok { recipe; identity; workspace_instance_id; workspace_projection; limits })
  ;;

  (* Paired fixed-schema traversal checks every scalar envelope, not merely a
     total byte count which could conceal a longer path or added nesting. *)
  let rec fits projection actual ~limits ~checkpoint =
    checkpoint ();
    match projection, actual with
    | `Object expected, `Object actual ->
      let rec fields expected actual =
        checkpoint ();
        match expected, actual with
        | [], [] -> Ok ()
        | (key, expected) :: rest, (actual_key, actual) :: actual_rest ->
          if not (String.equal key actual_key)
          then reject "resolved workspace shape differs from its sizing projection"
          else
            let open Result.Let_syntax in
            let%bind () = fits expected actual ~limits ~checkpoint in
            fields rest actual_rest
        | [], _ :: _ | _ :: _, [] ->
          reject "resolved workspace shape differs from its sizing projection"
      in
      fields expected actual
    | `String _, `String _
    | `Number _, `Number _
    | `True, `True
    | `False, `False
    | `Null, `Null ->
      let open Result.Let_syntax in
      let%bind maximum =
        D.Json.validate_and_measure ~limits projection
        |> Result.map_error ~f:document_error
      in
      let%bind actual =
        D.Json.validate_and_measure ~limits actual |> Result.map_error ~f:document_error
      in
      if actual <= maximum
      then Ok ()
      else reject "resolved workspace scalar exceeds its sizing projection"
    | _ -> reject "resolved workspace shape differs from its sizing projection"
  ;;

  let state t ~(workspace : S.Workspace_instance.t) ~checkpoint =
    let open Result.Let_syntax in
    checkpoint ();
    if
      (not (P.Id.Workspace_instance.equal workspace.id t.workspace_instance_id))
      || not
           (Option.equal
              P.Id.Workspace_definition.equal
              workspace.definition_id
              (Some t.recipe.workspace.id))
    then reject "resolved seed workspace identity differs from its admitted route"
    else (
      let%bind () =
        fits
          t.workspace_projection
          (S.Workspace_instance.to_jsonaf workspace)
          ~limits:t.limits
          ~checkpoint
      in
      let state =
        fields t.recipe t.identity (spec t.recipe workspace workspace.conflict_domain)
      in
      let%bind () = S.Session_state.validate state in
      checkpoint ();
      Ok state)
  ;;
end
