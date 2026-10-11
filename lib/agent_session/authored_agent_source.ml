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
  ; programs : Chatml_prompt_programs.t option
  ; programs_sha256 : string option
  }

let identity t = t.identity
let declaration t = t.declaration

let fingerprint t =
  (match t.programs_sha256 with
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
  let sources =
    List.filter all_sources ~f:(fun source ->
      not (String.equal source.Store.Source.relative_path selected.relative_path))
  in
  let%bind programs =
    match parent_artifact.program_bundle with
    | None -> Ok None
    | Some storage ->
      let%bind parent_authored =
        Prompt_program_custody.authored
          ~root_file:parent_artifact.root_relative_path
          ~sources:
            (List.map all_sources ~f:(fun s -> s.Store.Source.relative_path, s.contents))
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained authored inventory"
            ~retryable:false
            ())
      in
      let%bind custody =
        Prompt_program_custody.admit
          storage
          ~authored:parent_authored
          ~checkpoint:Eio.Fiber.check
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained program custody"
            ~retryable:false
            ())
      in
      let%bind root_input =
        List.find (Chatml_prompt_programs.parser_inputs custody) ~f:(fun input ->
          Chatmd_parser_input.Role.equal (Chatmd_parser_input.role input) Root
          && String.equal (Chatmd_parser_input.path input) selected.relative_path)
        |> Result.of_option
             ~error:
               (P.Error.create
                  Prompt_unavailable
                  ~message:"Retained child has no captured effective Root"
                  ~retryable:false
                  ())
      in
      let%bind authored =
        Prompt_program_custody.authored
          ~root_file:selected.relative_path
          ~sources:
            (List.map all_sources ~f:(fun s -> s.Store.Source.relative_path, s.contents))
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained child inventory"
            ~retryable:false
            ())
      in
      let%bind parsed =
        Prompt_program_custody.replay root_input ~authored ~dir:tree
        |> Result.map_error ~f:(fun error ->
          P.Error.create
            Prompt_unavailable
            ~message:(Error.to_string_hum error)
            ~retryable:false
            ())
      in
      let%map child =
        Chatml_prompt_programs.reroot
          custody
          ~authored
          ~parsed
          ~programs:[]
          ~limits:(Prompt_program_custody.limits ())
          ~checkpoint:Eio.Fiber.check
        |> Result.map_error ~f:(fun _ ->
          P.Error.create
            Prompt_unavailable
            ~message:"Invalid retained child selection"
            ~retryable:false
            ())
      in
      Some child
  in
  let%bind programs_sha256 =
    match programs with
    | None -> Ok None
    | Some programs ->
      Prompt_program_custody.fingerprint programs ~checkpoint:Eio.Fiber.check
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun _ ->
        P.Error.create
          Prompt_unavailable
          ~message:"Complete child program manifest exceeds artifact policy"
          ~retryable:false
          ())
  in
  Ok
    { identity
    ; declaration = { declaration with agent = selected.relative_path }
    ; parent_artifact
    ; root_chatmd = selected.contents
    ; sources
    ; programs
    ; programs_sha256
    }
;;

let authored_sources t =
  Prompt_program_custody.authored
    ~root_file:t.identity.root_relative_path
    ~sources:
      ((t.identity.root_relative_path, t.root_chatmd)
       :: List.map t.sources ~f:(fun s -> s.Store.Source.relative_path, s.contents))
  |> Or_error.ok_exn
;;

let with_programs t incoming =
  let open Result.Let_syntax in
  let%bind previous =
    match t.programs with
    | Some previous -> Ok previous
    | None -> denied "Legacy source has no retained effective child Root"
  in
  let expected = Chatml_prompt_programs.root_input previous
  and actual = Chatml_prompt_programs.root_input incoming in
  let%bind () =
    if
      Chatmd_parser_input.Role.equal (Chatmd_parser_input.role actual) Root
      && String.equal (Chatmd_parser_input.path actual) t.identity.root_relative_path
      && Chatmd_parser_input.authored_matches
           expected
           (Chatmd_parser_input.authored actual)
           ~checkpoint:Eio.Fiber.check
      && Chatmd_parser_input.effective_matches
           expected
           (Chatmd_parser_input.effective actual)
           ~checkpoint:Eio.Fiber.check
    then Ok ()
    else denied "Incoming custody does not select this retained child's exact Root"
  in
  let%bind programs =
    Chatml_prompt_programs.adopt
      incoming
      ~previous
      ~authored:(authored_sources t)
      ~limits:(Prompt_program_custody.limits ())
      ~checkpoint:Eio.Fiber.check
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Prompt_unavailable
        ~message:"Incoming custody changed retained source inventory or unknown ownership"
        ~retryable:false
        ())
  in
  let%map programs_sha256 =
    Prompt_program_custody.fingerprint programs ~checkpoint:Eio.Fiber.check
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Prompt_unavailable
        ~message:"Complete child program manifest exceeds artifact policy"
        ~retryable:false
        ())
  in
  { t with programs = Some programs; programs_sha256 = Some programs_sha256 }
;;

let artifact t ~revision_id ~created_at =
  Store.Artifact.create
    ~revision_id
    ?source_receipt:t.parent_artifact.source_receipt
    ~root_relative_path:t.identity.root_relative_path
    ~root_chatmd:t.root_chatmd
    ~sources:t.sources
    ~parser_schema_version:t.parent_artifact.parser_schema_version
    ~runtime_schema_version:t.parent_artifact.runtime_schema_version
    ?program_bundle:(Option.map t.programs ~f:Chatml_prompt_programs.to_storage)
    ~created_at
    ()
;;

let resource_revision ~parent t =
  let open Result.Let_syntax in
  let%bind () =
    match
      P.Id.Prompt_revision.equal (Prompt_revision.id parent) t.identity.parent_revision_id
      && String.equal
           (Prompt_revision.artifact parent).manifest_sha256
           t.identity.parent_manifest_sha256
    with
    | true -> Ok ()
    | false -> denied "authored resource preparation requires its original source owner"
  in
  let%bind revision_id =
    P.Id.Prompt_revision.of_string ("prv_authored_" ^ fingerprint t)
  in
  let%bind artifact =
    artifact t ~revision_id ~created_at:t.parent_artifact.created_at
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  Prompt_revision_builder.reparse
    ~definition:(Prompt_revision.definition parent)
    ~artifact
    ~materialized_tree:(Prompt_revision.materialized_tree parent)
  |> Result.map_error ~f:(fun diagnostics ->
    P.Error.create
      Prompt_unavailable
      ~message:
        (List.map diagnostics ~f:(fun diagnostic ->
           diagnostic.Prompt_revision_builder.Diagnostic.message)
         |> String.concat ~sep:"\n")
      ~retryable:false
      ())
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
