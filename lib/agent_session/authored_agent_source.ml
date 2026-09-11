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
  ; parent_artifact : Store.Artifact.t
  ; root_chatmd : string
  ; sources : Store.Source.t list
  }

let identity t = t.identity
let declaration t = t.declaration

let fingerprint t =
  [%sexp ("ochat.authored-agent-source.v1" : string), (t.identity : Identity.t)]
  |> Sexp.to_string_mach
  |> Chatmd_shell_spec.Source_ref.digest
;;

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let capture ~parent ~tool_name =
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
  let parent_artifact = Prompt_revision.artifact parent in
  let%bind root_source =
    Store.Source.create
      ~relative_path:parent_artifact.root_relative_path
      ~contents:parent_artifact.root_chatmd
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let all_sources = root_source :: parent_artifact.sources in
  let tree = Prompt_revision.materialized_tree parent in
  let relative_path =
    let open Option.Let_syntax in
    let%bind root = Eio.Path.native tree in
    let%bind reference = String.chop_prefix declaration.agent ~prefix:(root ^ "/") in
    (* Authored imports preserve paths such as definitions/../agents/child.chatmd.
       Normalize only the portion beneath the verified artifact tree; never consult
       realpath or follow an external live-file alias to decide source identity. *)
    let loader = Source_loader.confined_filesystem ~root:tree in
    Source_loader.root loader ~file:reference
    |> Result.ok
    |> Option.map ~f:Source_loader.relative_path
  in
  let%bind selected =
    List.find all_sources ~f:(fun source ->
      Option.value_map
        relative_path
        ~default:false
        ~f:(String.equal source.Store.Source.relative_path))
    |> Result.of_option
         ~error:
           (P.Error.create
              Permission_denied
              ~message:
                "persistent authored agent is not in the parent's captured source closure"
              ~retryable:false
              ())
  in
  let identity : Identity.t =
    { parent_revision_id = parent_artifact.revision_id
    ; parent_manifest_sha256 = parent_artifact.manifest_sha256
    ; tool_name
    ; root_relative_path = selected.relative_path
    ; policy
    }
  in
  Ok
    { identity
    ; declaration = { declaration with agent = selected.relative_path }
    ; parent_artifact
    ; root_chatmd = selected.contents
    ; sources =
        List.filter all_sources ~f:(fun source ->
          not (String.equal source.relative_path selected.relative_path))
    }
;;

let artifact t ~revision_id ~created_at =
  Store.Artifact.create
    ~revision_id
    ~root_relative_path:t.identity.root_relative_path
    ~root_chatmd:t.root_chatmd
    ~sources:t.sources
    ~parser_schema_version:t.parent_artifact.parser_schema_version
    ~runtime_schema_version:t.parent_artifact.runtime_schema_version
    ~created_at
    ()
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
