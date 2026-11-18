open! Core
module A = Agent_session
module Actor = A.Session_actor
module Observation = Actor.Resource_observation
module Source = A.Runtime_builder.Source
module D = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Artifacts = Agent_store.Prompt_artifact_store
module P = Agent_protocol
module Registry = Session_registry
module State = A.Session_state
module Id = P.Id.Session
module Revision = A.Prompt_revision
module Authored = A.Authored_agent_source
module Plan = Authored_resources.Source_plan
module Script = A.Runtime_script_preparation
module Generated = A.Generated_definition.Metadata_source

type host =
  { registry : Registry.t
  ; store : Store.t
  ; env : Eio_unix.Stdenv.base
  ; prompts : A.Prompt_catalog.t
  ; script_preparation : Script.t option
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; max_depth : int
  ; generated_limits : Chatml_compilation.limits option
  ; generated_source_limits : Chatmd_source_bundle.limits option
  ; target : Id.t -> (Registry.entry, P.Error.t) result
  ; profile : State.t -> (A.Permission_policy.t, P.Error.t) result
  }

let refuse code message = Error (P.Error.create code ~message ~retryable:false ())
let changed () = refuse Conflict "delegation.ancestry_changed: source observation changed"

let missing () =
  refuse
    Permission_denied
    "delegation.parent_missing: independent ancestor is unavailable"
;;

let unavailable_source () =
  refuse
    Permission_denied
    "delegation.independent_moderation_unavailable: no exact original moderator source"
;;

let diagnostic messages = refuse Prompt_unavailable (String.concat messages ~sep:"\n")

let revision_diagnostic diagnostics =
  List.map diagnostics ~f:(fun (d : A.Prompt_revision_builder.Diagnostic.t) -> d.message)
  |> diagnostic
;;

module Fact = struct
  type t =
    { entry : Registry.entry
    ; handle : Store.Handle.t
    ; observation : Observation.t
    ; record : D.record option
    }

  let state t = Observation.state t.observation
  let id t = (state t).identity.session_id
  let owner t = t.entry.runtime

  let same_entry (entry : Registry.entry) (current : Registry.entry) =
    phys_equal entry.actor current.actor
    && phys_equal entry.runtime current.runtime
    && Option.equal phys_equal entry.store_handle current.store_handle
  ;;

  (* Observation checks the Actor's committing domain before Registry/Handle reads.
     Existing resource/operation guards still own their individual reservations. *)
  let current t host =
    let open Result.Let_syntax in
    let%bind current = Observation.check_current t.observation in
    let id = id t in
    if
      Registry.is_closing host.registry
      || Registry.has_pending_cleanup host.registry id
      || not
           (Option.exists (Registry.find host.registry id) ~f:(same_entry t.entry)
            && Runtime_owner.belongs_to_actor t.entry.runtime t.entry.actor
            && Id.equal id (Store.Handle.session_id t.handle)
            && Store.owns_handle host.store t.handle
            && Store.retained_lifecycle_available host.store t.handle)
    then changed ()
    else Ok current
  ;;

  let from_entry host walk entry expected_id =
    let open Result.Let_syntax in
    let%bind observation = Actor.observe_resource_state entry.Registry.actor in
    let state = Observation.state observation in
    let%bind handle =
      Result.of_option
        entry.store_handle
        ~error:(P.Error.invalid_request "independent ancestor has no durable handle")
    in
    let%bind record =
      match state.spec.delegation, state.spec.protocol.prompt with
      | None, (Catalog _ | Local_path _) -> Ok None
      | Some reference, Generated revision_id ->
        let%bind record = walk.Independent_resources.resolve reference in
        (match
           ( D.Reference.equal reference (D.reference record)
           , record.stage
           , record.revocation )
         with
         | true, Linked, None ->
           let target_matches =
             Option.for_all record.admission.inference_target ~f:(fun expected ->
               match Inference.Selection.view state.spec.inference_target with
               | Captured actual -> Inference.Request.Target.equal expected actual
               | Unresolved -> false)
           in
           if
             Id.equal expected_id reference.child_session_id
             && P.Id.Prompt_revision.equal revision_id reference.revision_id
             && P.Id.Prompt_revision.equal
                  state.spec.prompt_revision_id
                  reference.revision_id
             && Option.is_none state.spec.prompt_definition_id
             && target_matches
           then Ok (Some record)
           else changed ()
         | _, _, Some _ ->
           refuse Permission_denied "delegation.revoked: ancestor delegation was revoked"
         | _ ->
           refuse
             Permission_denied
             "delegation.not_linked: independent ancestry is not privately linked")
      | None, Generated _ | Some _, (Catalog _ | Local_path _) -> changed ()
    in
    let fact = { entry; handle; observation; record } in
    let%bind () = if Id.equal expected_id (id fact) then Ok () else changed () in
    let%map _ = current fact host in
    fact
  ;;
end

let collect_facts host walk ~parent ~check_request =
  let open Result.Let_syntax in
  let%bind initial = Actor.state parent.Registry.actor in
  let parent_id = initial.identity.session_id in
  let rec loop facts id =
    if
      List.length facts >= host.max_depth
      || List.exists facts ~f:(fun f -> Id.equal (Fact.id f) id)
    then
      refuse
        Permission_denied
        "delegation.ancestry_limit: independent ancestry is cyclic or excessive"
    else (
      let%bind () = check_request () in
      let%bind entry = host.target id in
      let%bind () =
        if Id.equal id parent_id && not (Fact.same_entry entry parent)
        then changed ()
        else Ok ()
      in
      let%bind fact = Fact.from_entry host walk entry id in
      let%bind () =
        match facts with
        | [] -> Ok ()
        | child :: _ ->
          let record = Option.value_exn child.Fact.record in
          let state = Fact.state fact in
          if
            Int.equal state.identity.generation record.key.parent_generation
            && P.Id.Prompt_revision.equal
                 state.spec.prompt_revision_id
                 record.admission.parent_revision_id
          then Ok ()
          else changed ()
      in
      let%bind () = check_request () in
      let facts = fact :: facts in
      match fact.record with
      | None -> Ok (parent_id, facts)
      | Some record -> loop facts record.key.parent_session_id)
  in
  loop [] parent_id
;;

type retained =
  { observation : Runtime_owner.Native_source.t
  ; profile : A.Permission_policy.t
  }

type proof =
  | State_only
  | Retained of retained
  | Cold of Source.Moderator.t

type acquisition =
  | Proved of proof
  | Need_metadata

let acquire host ~retained_sources ~parent_id fact =
  let open Result.Let_syntax in
  let%bind expected = Fact.current fact host in
  match expected.moderator with
  | None -> Ok (Proved State_only)
  | Some _ ->
    let%bind profile = host.profile expected in
    let observe () =
      Runtime_owner.observe_native_source
        (Fact.owner fact)
        ~handle:fact.handle
        ~profile
        ~expected
    in
    let%bind acquisition =
      match retained_sources with
      | Some (access : A.Runtime_builder.ancestor_sources)
        when not (Id.equal parent_id (Fact.id fact)) ->
        access.with_source (Fact.id fact) ~f:(fun ~source ->
          let%bind actual = observe () in
          match actual with
          | Some observation
            when phys_equal source (Runtime_owner.Native_source.source observation) ->
            Ok (Proved (Retained { observation; profile }))
          | Some _ | None -> changed ())
      | Some _ | None ->
        let%map actual = observe () in
        (match actual with
         | Some observation -> Proved (Retained { observation; profile })
         | None -> Need_metadata)
    in
    let%bind current = Fact.current fact host in
    let%map () =
      match acquisition with
      | Proved (Retained retained) ->
        Runtime_owner.Native_source.check retained.observation ~profile ~expected:current
      | Need_metadata | Proved (State_only | Cold _) -> Ok ()
    in
    acquisition
;;

module Metadata = struct
  type scope =
    { forest : Plan.t option
    ; inherited : String.Set.t option
    }

  let empty = { forest = None; inherited = None }

  let capture host startup revision =
    let open Result.Let_syntax in
    let%map forest =
      Plan.capture
        ~max_depth:host.max_depth
        ~revision
        ~capture:(fun ~parent_revision ~tool_name ->
          A.Authored_source_preparation.with_prepared
            ~startup:(Some startup)
            ~script_preparation:host.script_preparation
            ~content_limits:host.content_limits
            ~parent:parent_revision
            ~tool_name
            ~env:host.env
            ~checkpoint:Eio.Fiber.check
            ~f:(fun _ source -> Ok source))
    in
    { forest = Some forest; inherited = None }
  ;;

  let select scope ~(selector : Agent_store.Captured_source_kind.Authored_tool.t) =
    let open Result.Let_syntax in
    match scope.forest with
    | Some forest
      when Option.for_all scope.inherited ~f:(fun names -> Set.mem names selector.name) ->
      let%map prepared, children = Plan.find_direct forest ~selector in
      ( A.Authored_source_preparation.source prepared
      , { forest = Some children; inherited = None } )
    | Some _ | None -> unavailable_source ()
  ;;

  let inherit_names scope pins =
    let names = String.Set.of_list (List.map pins ~f:fst) in
    { scope with
      inherited =
        Some (Option.value_map scope.inherited ~default:names ~f:(Set.inter names))
    }
  ;;

  let artifacts host =
    Artifacts.open_existing
      ~env:host.env
      ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root host.store))
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  ;;

  let moderator host startup revision invocation =
    Script.with_revision_plan
      host.script_preparation
      ~startup
      revision
      ~env:host.env
      ~invocation
      ~checkpoint:Eio.Fiber.check
      ~f:(fun startup ~compilation_limits ~plan ~programs ->
        Source.Prepared_revision.capture
          revision
          ~invocation
          ~startup
          ~compilation_limits
          ~plan
          ~programs
          ~checkpoint:Eio.Fiber.check
        |> Result.map ~f:Source.Prepared_revision.moderator)
  ;;

  let root host (state : State.t) =
    let open Result.Let_syntax in
    let%bind definition_id =
      Result.of_option
        state.spec.prompt_definition_id
        ~error:
          (P.Error.invalid_request "independent root has no verified prompt definition")
    in
    match
      A.Prompt_catalog.restore_verified_revision
        host.prompts
        ~definition_id
        ~revision_id:state.spec.prompt_revision_id
    with
    | Error errors -> revision_diagnostic errors
    | Ok revision -> Ok (revision, Script.Invocation.of_root state.spec.root_invocation)
  ;;

  let authored host artifacts source ~selector record =
    let open Result.Let_syntax in
    let%bind artifact =
      Authored.load_recorded_artifact ~artifact_store:artifacts ~reservation:record
    in
    let definition = Revision.definition (Authored.defining_revision source) in
    let%bind () =
      match Authored.defining_reference source with
      | None ->
        Recurrence_authored_source.check_source_artifact source ~selector ~artifact
      | Some _ ->
        if
          String.equal
            artifact.root_relative_path
            (Authored.identity source).root_relative_path
          && Option.for_all
               artifact.prompt_definition_id
               ~f:(P.Id.Prompt_definition.equal definition.id)
          && Option.for_all artifact.source_receipt ~f:(fun receipt ->
            P.Id.Prompt_definition.equal
              (Agent_store.Prompt_source_receipt.definition_id receipt)
              definition.id
            && String.equal
                 (Agent_store.Prompt_source_receipt.root_file receipt)
                 definition.root_file)
        then Ok ()
        else unavailable_source ()
    in
    match
      A.Prompt_revision_builder.reparse
        ~checkpoint:Eio.Fiber.check
        ?content_limits:host.content_limits
        ~definition
        ~artifact
        ~materialized_tree:(Artifacts.materialized_tree artifacts artifact.revision_id)
        ()
    with
    | Error errors -> revision_diagnostic errors
    | Ok revision -> Ok revision
  ;;

  let pick host startup scope fact ~needed ~scope_needed =
    let open Result.Let_syntax in
    let state = Fact.state fact in
    let finish scope revision invocation =
      let%map moderator =
        if needed
        then moderator host startup revision invocation
        else Ok Source.Moderator.No_moderator
      in
      scope, moderator
    in
    match fact.Fact.record with
    | None ->
      let%bind revision, invocation = root host state in
      let%bind scope = if scope_needed then capture host startup revision else Ok empty in
      finish scope revision invocation
    | Some record ->
      let%bind selector =
        D.Admission.authored_source record.admission
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      let%bind artifacts = artifacts host in
      (match record.admission.captured_recurrence, selector with
       | None, None ->
         (match
            Generated.load
              ?limits:host.generated_limits
              ?source_limits:host.generated_source_limits
              ~artifact_store:artifacts
              ~record
              ()
          with
          | Error errors ->
            List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> diagnostic
          | Ok metadata ->
            if D.Reference.equal (Generated.reference metadata) (D.reference record)
            then (
              let scope =
                if scope_needed
                then inherit_names scope (Generated.capability_pins metadata)
                else empty
              in
              let moderator =
                Option.value_map
                  (Generated.moderator metadata)
                  ~default:Source.Moderator.No_moderator
                  ~f:(fun observer -> Source.Moderator.Extension observer)
              in
              Ok (scope, moderator))
            else changed ())
       | None, Some selector ->
         let%bind source, scope = select scope ~selector in
         let%bind revision = authored host artifacts source ~selector record in
         finish scope revision Script.Invocation.Delegated_ui
       | Some _, selector ->
         let%bind retained =
           A.Captured_recurrence_source.restore
             ~artifact_store:artifacts
             ~content_limits:host.content_limits
             ~record
             ~state
             ~checkpoint:Eio.Fiber.check
         in
         let revision = A.Captured_recurrence_source.revision retained in
         (match selector with
          | Some selector ->
            let%bind source, scope = select scope ~selector in
            let%bind () =
              Recurrence_authored_source.check_source_artifact
                source
                ~selector
                ~artifact:(Revision.artifact revision)
            in
            finish scope revision Script.Invocation.Delegated_ui
          | None ->
            let origin = A.Captured_recurrence_source.origin retained in
            let%bind scope =
              if scope_needed then capture host startup revision else Ok empty
            in
            finish
              scope
              revision
              (Script.Invocation.of_root
                 (A.Recurrence_child_origin.root_invocation origin))))
  ;;
end

type member =
  { fact : Fact.t
  ; proof : proof
  ; digest : string
  }

type t =
  { domain : int
  ; host : host
  ; parent_id : Id.t
  ; members : member list
  }

let fingerprint proof (state : State.t) =
  let open Result.Let_syntax in
  match proof with
  | State_only ->
    (match state.moderator with
     | None -> A.Delegation_authority.fingerprint state
     | Some _ -> changed ())
  | Retained { observation; profile } ->
    let%bind () =
      Runtime_owner.Native_source.check observation ~profile ~expected:state
    in
    Native_resource_authority.current_fingerprint
      ~installed_source:(Runtime_owner.Native_source.source observation)
      state
  | Cold moderator -> Native_resource_authority.eligibility_fingerprint ~moderator state
;;

let check_member host { fact; proof; digest } =
  let open Result.Let_syntax in
  let%bind state = Fact.current fact host in
  let%bind current = fingerprint proof state in
  if String.equal current digest then Ok state else changed ()
;;

let check_domain t =
  if Int.equal t.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else refuse Conflict "independent source observation belongs to another domain"
;;

let check_current t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  List.fold_result t.members ~init:() ~f:(fun () member ->
    check_member t.host member |> Result.map ~f:ignore)
;;

let check_reservations t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  List.fold_result t.members ~init:() ~f:(fun () member ->
    Registry.check_native_metadata_publication t.host.registry member.fact.entry)
;;

let fingerprint_state t ~owner (state : State.t) =
  let open Result.Let_syntax in
  let module Identity = State.Spec.Resource_identity in
  let%bind () = check_domain t in
  let%bind member =
    List.find t.members ~f:(fun member -> phys_equal owner (Fact.owner member.fact))
    |> Result.of_option
         ~error:(P.Error.invalid_request "independent source owner was not observed")
  in
  let%bind current = check_member t.host member in
  let%bind state_digest = fingerprint member.proof state in
  if
    Id.equal state.identity.session_id current.identity.session_id
    && Int.equal state.identity.generation current.identity.generation
    && Identity.equal (Identity.capture state.spec) (Identity.capture current.spec)
    && String.equal state_digest member.digest
  then Ok member.digest
  else changed ()
;;

let prepare_proofs host acquisitions ~check_request =
  let open Result.Let_syntax in
  let authored fact =
    Option.exists fact.Fact.record ~f:(fun record ->
      Option.is_some record.admission.authored_tool)
  in
  let rec needs_scope = function
    | [] -> false
    | (fact, proof) :: rest ->
      (match proof with
       | Need_metadata when authored fact -> true
       | Proved _ | Need_metadata -> needs_scope rest)
  in
  let rec load startup scope proofs = function
    | [] -> Ok (List.rev proofs)
    | (fact, acquisition) :: rest ->
      let%bind () = check_request () in
      let scope_needed = needs_scope rest in
      let%bind scope, proof =
        match acquisition, scope_needed with
        | Proved proof, false -> Ok (Metadata.empty, proof)
        | _ ->
          let needed =
            match acquisition with
            | Need_metadata -> true
            | Proved _ -> false
          in
          let%map scope, moderator =
            Metadata.pick host startup scope fact ~needed ~scope_needed
          in
          ( scope
          , (match acquisition with
             | Proved proof -> proof
             | Need_metadata -> Cold moderator) )
      in
      let%bind _ = Fact.current fact host in
      load startup scope ((fact, proof) :: proofs) rest
  in
  if
    List.exists acquisitions ~f:(function
      | _, Need_metadata -> true
      | _, Proved _ -> false)
  then (
    let limits =
      Option.value_map
        host.script_preparation
        ~default:Chatml_compilation.default_limits
        ~f:Script.compilation
    in
    Chat_response.Startup_compilation.with_context
      ~limits
      ~env:host.env
      ~checkpoint:Eio.Fiber.check
      ~map_error:(fun error ->
        P.Error.create Prompt_unavailable ~message:error.message ~retryable:false ())
      ~f:(fun startup -> load startup Metadata.empty [] acquisitions)
      ())
  else
    Ok
      (List.filter_map acquisitions ~f:(function
         | fact, Proved proof -> Some (fact, proof)
         | _, Need_metadata -> None))
;;

let check_record t record =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  if
    List.exists t.members ~f:(fun member ->
      Option.exists member.fact.record ~f:(D.equal_record record))
  then Ok ()
  else changed ()
;;

let recheck t ~walk =
  let open Result.Let_syntax in
  let%bind () = check_reservations t in
  let%bind () = check_current t in
  let find id =
    match List.find t.members ~f:(fun member -> Id.equal id (Fact.id member.fact)) with
    | None -> missing ()
    | Some member ->
      let%map state = check_member t.host member in
      { Independent_resources.state; owner = Fact.owner member.fact }
  in
  let resolve reference =
    let%bind record = walk.Independent_resources.resolve reference in
    let%map () = check_record t record in
    record
  in
  let source_fingerprint ({ state; owner } : Independent_resources.ancestor) =
    fingerprint_state t ~owner state
  in
  let%bind () =
    Independent_resources.check_parent_with_source
      ~max_depth:t.host.max_depth
      ~host:{ walk with find; resolve }
      ~source_fingerprint
      ~parent_id:t.parent_id
  in
  let%bind () = check_current t in
  check_reservations t
;;

let prepare ?retained_sources host ~walk ~parent ~check_request =
  let open Result.Let_syntax in
  let%bind parent_id, facts = collect_facts host walk ~parent ~check_request in
  let%bind acquisitions =
    List.fold_result facts ~init:[] ~f:(fun acquisitions fact ->
      let%bind () = check_request () in
      let%map acquisition = acquire host ~retained_sources ~parent_id fact in
      (fact, acquisition) :: acquisitions)
  in
  let%bind proofs = prepare_proofs host (List.rev acquisitions) ~check_request in
  let%bind () = check_request () in
  let%bind members =
    List.map proofs ~f:(fun (fact, proof) ->
      let%bind state = Fact.current fact host in
      let%map digest = fingerprint proof state in
      { fact; proof; digest })
    |> Result.all
  in
  let t = { domain = (Stdlib.Domain.self () :> int); host; parent_id; members } in
  let%bind () = check_current t in
  let%map () = check_reservations t in
  t
;;
