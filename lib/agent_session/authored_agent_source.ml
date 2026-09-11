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
