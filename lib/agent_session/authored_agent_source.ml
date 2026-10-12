open Core
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module Store = Agent_store.Prompt_artifact_store

module Identity = struct
  type t =
    { parent_revision_id : P.Id.Prompt_revision.t
    ; parent_manifest_sha256 : string
    ; tool_name : string
    ; root_relative_path : string
    ; policy : CM.agent_persistence
    }
  [@@deriving equal, sexp_of]
end

type t =
  { identity : Identity.t
  ; declaration : CM.agent_tool
  ; source : Captured_child_source.t
  ; defining_revision : Prompt_revision.t
  ; defining_reference : CM.Agent_reference.t option
  }

let identity t = t.identity
let declaration t = t.declaration
let defining_revision t = t.defining_revision
let defining_reference t = t.defining_reference

let program_fingerprint t =
  (match Captured_child_source.programs_sha256 t.source with
   | None ->
     [%sexp ("ochat.authored-agent-source.v1" : string), (t.identity : Identity.t)]
   | Some programs_sha256 ->
     [%sexp
       ("ochat.authored-agent-source.v2" : string)
     , (t.identity : Identity.t)
     , (programs_sha256 : string)])
  |> Sexp.to_string_mach
  |> Chatmd_shell_spec.Source_ref.digest
;;

let fingerprint t =
  match Captured_child_source.content_sha256 t.source with
  | None -> program_fingerprint t
  | Some content_sha256 ->
    [%sexp
      ("ochat.authored-agent-source.v3" : string)
    , (program_fingerprint t : string)
    , (content_sha256 : string)]
    |> Sexp.to_string_mach
    |> Chatmd_shell_spec.Source_ref.digest
;;

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let capture ?(checkpoint = Eio.Fiber.check) ?content_limits ~parent ~tool_name () =
  let open Result.Let_syntax in
  let%bind declaration, policy =
    List.filter_map (Prompt_revision.elements parent) ~f:(function
      | CM.Tool (Persistent_agent (agent, policy)) when String.equal agent.name tool_name
        -> Some (agent, Some policy)
      | Tool (Agent agent) when String.equal agent.name tool_name -> Some (agent, None)
      | _ -> None)
    |> function
    | [ (agent, Some policy) ] -> Ok (agent, policy)
    | _ ->
      denied "authored agent requires one unambiguous persistence-enabled declaration"
  in
  let%bind () =
    match declaration.is_local with
    | true -> Ok ()
    | false -> denied "persistent authored agent source must be captured locally"
  in
  let%map source, defining_reference =
    match Prompt_revision.parser_context parent with
    | None ->
      Captured_child_source.capture_legacy_tool
        ~parent
        ~declaration
        ~content_limits
        ~checkpoint
      |> Result.map ~f:(fun source -> source, None)
    | Some parsed ->
      let references =
        List.filter (CM.Parsed.agent_references parsed) ~f:(fun reference ->
          checkpoint ();
          match CM.Agent_reference.invocation reference with
          | Persistent_tool (candidate, candidate_policy) ->
            phys_equal candidate declaration
            && CM.equal_agent_persistence candidate_policy policy
          | Tool _ | Content _ -> false)
      in
      (match references with
       | [ reference ] ->
         Captured_child_source.capture ~parent ~reference ~content_limits ~checkpoint
         |> Result.map ~f:(fun source -> source, Some reference)
       | [] | _ :: _ ->
         denied "persistent authored declaration lacks one exact owned parser reference")
  in
  let parent_artifact = Prompt_revision.artifact parent in
  let root_relative_path = Captured_child_source.root_relative_path source in
  let identity : Identity.t =
    { parent_revision_id = parent_artifact.revision_id
    ; parent_manifest_sha256 = parent_artifact.manifest_sha256
    ; tool_name
    ; root_relative_path
    ; policy
    }
  in
  { identity
  ; declaration = { declaration with agent = root_relative_path }
  ; source
  ; defining_revision = parent
  ; defining_reference
  }
;;

let authored_sources t = Captured_child_source.authored_sources t.source

let with_programs ?(checkpoint = Eio.Fiber.check) t programs =
  Result.map
    (Captured_child_source.with_programs t.source programs ~checkpoint)
    ~f:(fun source -> { t with source })
;;

let artifact t ~revision_id ~created_at =
  Captured_child_source.artifact t.source ~revision_id ~created_at
;;

let resource_revision ?(checkpoint = Eio.Fiber.check) ~parent t =
  let open Result.Let_syntax in
  let%bind revision_id =
    P.Id.Prompt_revision.of_string ("prv_authored_" ^ fingerprint t)
  in
  Captured_child_source.resource_revision t.source ~parent ~revision_id ~checkpoint
;;

let store_result result =
  Result.map_error result ~f:Agent_store.Store_error.to_protocol_error
;;

let load_artifact ~artifact_store ~(reservation : Agent_store.Delegation_store.record) =
  let open Result.Let_syntax in
  let%bind () =
    match reservation.admission.authored_tool with
    | Some _ -> Ok ()
    | None -> denied "authored artifact requires an authored reservation"
  in
  let%bind stored =
    Store.load artifact_store reservation.admission.revision_id |> store_result
  in
  match
    ( String.equal stored.manifest_sha256 reservation.admission.manifest_sha256
    , stored.parser_schema_version
    , stored.runtime_schema_version
    , stored.prompt_definition_id
    , stored.canonical_source
    , stored.shell_manifest_sha256 )
  with
  | true, 5, 1, None, None, None -> Ok stored
  | _ -> denied "authored artifact differs from its reserved source contract"
;;

let install_reserved ~delegations ~reservation ~artifact_store ~capability_pins t =
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let%bind current = D.resolve delegations (D.reference reservation) |> store_result in
  let%bind () =
    match current.revocation, current.admission.authored_tool with
    | None, Some origin
      when String.equal origin.name t.identity.tool_name
           && String.equal origin.source_sha256 (fingerprint t)
           && List.equal
                (fun (name, pin) (other_name, other_pin) ->
                   String.equal name other_name && String.equal pin other_pin)
                current.admission.capability_pins
                capability_pins -> Ok ()
    | _ -> denied "authored source or private bindings differ from the live reservation"
  in
  (* The source's defining revision is covered by its fingerprint. The actual
     caller may inherit this wrapper and have a different revision; admission of
     that caller belongs to the common delegation authority service. *)
  let%bind expected =
    artifact
      t
      ~revision_id:current.admission.revision_id
      ~created_at:current.admission.created_at
    |> store_result
  in
  let%bind () =
    match String.equal expected.manifest_sha256 current.admission.manifest_sha256 with
    | true -> Ok ()
    | false -> denied "authored source differs from its reserved manifest"
  in
  let verify () =
    load_artifact ~artifact_store ~reservation:current |> Result.map ~f:ignore
  in
  let%bind () =
    match Store.exists artifact_store expected.revision_id with
    | true -> verify ()
    | false ->
      (match
         Store.install
           artifact_store
           ~transaction_id:current.admission.transaction_id
           expected
       with
       | Ok () -> verify ()
       | Error failure ->
         (* Concurrent replays may finish the same installation while IO yields.
            Only the exact reserved, verified tree permits acknowledgement. *)
         (match Store.exists artifact_store expected.revision_id with
          | true -> verify ()
          | false -> store_result (Error failure)))
  in
  (* Advancing rechecks the current record and revocation after all yielding IO.
     A revoked installation stays reserved for recovery, never published here. *)
  D.advance delegations current Artifact_installed |> store_result
;;
