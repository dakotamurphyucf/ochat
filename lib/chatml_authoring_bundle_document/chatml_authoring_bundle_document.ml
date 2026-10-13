open Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Project = Chatml_authoring_bundle_project
module Binding = Chatml_prompt_binding
module Contract = Chatml.Chatml_compiler_contract
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Input = Chatmd_parser_input
module Inputs = Authoring_bundle_document_inputs
module Stage = Project.Attempt.Stage

module Selection = struct
  type t =
    { contract : Contract.t
    ; previous : Chatml_project_build.t option
    }

  let create ~contract ~previous = { contract; previous }
  let contract t = t.contract
  let previous t = t.previous
end

module Error = struct
  type problem =
    | Wrong_document_basis
    | Snapshot_mismatch
    | Document_bundle_limit
    | Parser of Prompt.Chat_markdown.Generated.error
    | Missing_or_surplus_document_member
    | Missing_or_surplus_script
    | Stale_or_foreign_selector
    | Script_body_mismatch
    | External_source_unavailable
    | Project_route_unavailable
    | Project_root_mismatch
    | Wrong_authored_environment
    | Dynamic_project_unavailable
    | Selected_contract_unavailable
    | Unexpected_selected_contract
    | Project of Project.Error.t

  type t = { problem : problem }

  let problem t = t.problem
end

open Error

let error problem = Error problem

let option_result option ~error =
  match option with
  | Some value -> Ok value
  | None -> Error error
;;

module Validated = struct
  module Script = struct
    type t =
      { selector : Bundle.Manifest.Script.Selector.t
      ; binding : Binding.t
      ; project : Project.Validated.t option
      }

    let selector t = t.selector
    let binding t = t.binding
    let project t = t.project
  end

  type t =
    { manifest_fingerprint : Captured.Digest.t
    ; root_source : Captured.Id.t
    ; document_bundle_fingerprint : Captured.Digest.t
    ; scripts : Script.t list
    }

  let manifest_fingerprint t = t.manifest_fingerprint
  let root_source t = t.root_source
  let document_bundle_fingerprint t = t.document_bundle_fingerprint
  let scripts t = t.scripts
end

type task =
  { selector : Bundle.Manifest.Script.Selector.t
  ; binding : Binding.t
  ; project : Bundle.Manifest.Project.t option
  ; selection : Selection.t option
  ; derived_entry : Captured.t option
  }

let bytes_equal left right ~stage =
  if String.length left <> String.length right
  then false
  else (
    let offset = ref 0 in
    let equal = ref true in
    while !equal && !offset < String.length left do
      if !offset mod 4096 = 0 then Stage.checkpoint stage;
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    Stage.checkpoint stage;
    !equal)
;;

let find_and_remove_selector scripts selector ~stage =
  let rec loop prefix = function
    | [] -> None
    | script :: rest ->
      Stage.checkpoint stage;
      if
        Bundle.Manifest.Script.Selector.equal
          selector
          (Bundle.Manifest.Script.selector script)
      then Some (script, List.rev_append prefix rest)
      else loop (script :: prefix) rest
  in
  loop [] scripts
;;

let find_and_remove_selection selections selector ~stage =
  let rec loop prefix = function
    | [] -> None
    | ((other, value) as pair) :: rest ->
      Stage.checkpoint stage;
      if Bundle.Manifest.Script.Selector.equal selector other
      then Some (value, List.rev_append prefix rest)
      else loop (pair :: prefix) rest
  in
  loop [] selections
;;

let selector_of_binding inputs binding declaration ~stage =
  let open Result.Let_syntax in
  let%bind registration_source =
    Inputs.input_source_id inputs (Binding.registration_input binding) ~stage
    |> option_result ~error:Stale_or_foreign_selector
  in
  let%bind declaration_source =
    Inputs.input_source_id inputs (Binding.parser_input binding) ~stage
    |> option_result ~error:Stale_or_foreign_selector
  in
  Bundle.Manifest.Script.Selector.create
    ~registration_source
    ~declaration_source
    ~start_byte:(Binding.start_byte binding)
    ~end_byte:(Binding.end_byte binding)
    ~script_id:(Binding.script_id binding)
    ~body_digest:(Binding.script_digest binding)
    ~body_byte_length:(String.length (Declaration.script_body declaration))
  |> Result.map_error ~f:(fun _ -> Stale_or_foreign_selector)
;;

let external_body_matches inputs binding declaration ~stage =
  match Declaration.script_path declaration with
  | None -> Ok None
  | Some reference ->
    let open Result.Let_syntax in
    let%bind path =
      Source_loader.resolve_member
        (Binding.declaration_path binding)
        ~reference
        ~max_reference_bytes:1024
        ~checkpoint:(fun () -> Stage.checkpoint stage)
      |> Result.map_error ~f:(fun _ -> External_source_unavailable)
    in
    let%bind source =
      Inputs.source_at inputs path ~stage
      |> option_result ~error:External_source_unavailable
    in
    if bytes_equal (Captured.contents source) (Declaration.script_body declaration) ~stage
    then Ok (Inputs.source_id inputs path)
    else error Script_body_mismatch
;;

let authored_project inputs binding declaration project ~stage =
  let open Result.Let_syntax in
  let%bind authored =
    Declaration.project_selection declaration
    |> option_result ~error:Dynamic_project_unavailable
  in
  let%bind path =
    Source_loader.resolve_member
      (Binding.declaration_path binding)
      ~reference:authored.project
      ~max_reference_bytes:1024
      ~checkpoint:(fun () -> Stage.checkpoint stage)
    |> Result.map_error ~f:(fun _ -> Project_route_unavailable)
  in
  let%bind root_id =
    Inputs.source_id inputs path |> option_result ~error:Project_route_unavailable
  in
  if not (Captured.Id.equal root_id (Bundle.Manifest.Project.root_source project))
  then error Project_root_mismatch
  else (
    let selected = Bundle.Manifest.Project.selection project in
    if not (Option.equal String.equal authored.environment selected.environment)
    then error Wrong_authored_environment
    else Ok root_id)
;;

let derived_entry project declaration ~stage =
  match Bundle.Manifest.Project.entry project with
  | Declared -> Ok None
  | Captured captured ->
    (match Bundle.Manifest.Project.Captured_entry.delivery captured with
     | Uploaded_member -> Ok None
     | Derived_script ->
       let metadata = Bundle.Manifest.Project.Captured_entry.source captured in
       let body = Declaration.script_body declaration in
       Stage.debit_input stage ~bytes:(String.length body);
       Captured.create
         (Agent_protocol.Authoring_common.Source.id metadata)
         ~contents:body
         ~expected_digest:(Some (Agent_protocol.Authoring_common.Source.digest metadata))
         ~max_bytes:(Some Bundle.Wire_limits.max_member_bytes)
         ~checkpoint:(fun () -> Stage.checkpoint stage)
       |> Result.map ~f:Option.some
       |> Result.map_error ~f:(fun _ -> Script_body_mismatch))
;;

let task_for_declaration inputs script selection binding selector declaration ~stage =
  let open Result.Let_syntax in
  if
    not
      (Bundle.Manifest.Script.Selector.equal
         selector
         (Bundle.Manifest.Script.selector script))
  then error Stale_or_foreign_selector
  else (
    let%bind external_source = external_body_matches inputs binding declaration ~stage in
    match Bundle.Manifest.Script.choice script, selection with
    | Source_only, None ->
      (match Declaration.project_selection declaration with
       | None ->
         Ok
           ( { selector; binding; project = None; selection = None; derived_entry = None }
           , None )
       | Some _ -> error Dynamic_project_unavailable)
    | Source_only, Some _ -> error Unexpected_selected_contract
    | Selected_project _, None -> error Selected_contract_unavailable
    | Selected_project project, Some selection ->
      let%bind () =
        match external_source, Bundle.Manifest.Project.entry project with
        | Some source_id, Captured captured
          when Bundle.Manifest.Project.Captured_entry.equal_delivery
                 (Bundle.Manifest.Project.Captured_entry.delivery captured)
                 Uploaded_member ->
          if
            Captured.Id.equal
              source_id
              (Agent_protocol.Authoring_common.Source.id
                 (Bundle.Manifest.Project.Captured_entry.source captured))
          then Ok ()
          else error Script_body_mismatch
        | None, _ | Some _, _ -> Ok ()
      in
      let%bind root = authored_project inputs binding declaration project ~stage in
      let%map derived_entry = derived_entry project declaration ~stage in
      ( { selector
        ; binding
        ; project = Some project
        ; selection = Some selection
        ; derived_entry
        }
      , Some root ))
;;

let collect_tasks inputs parsed_bundle document selected_projects ~stage =
  let open Result.Let_syntax in
  let parseds =
    parsed_bundle.Prompt.Chat_markdown.parsed_root
    :: List.map parsed_bundle.parsed_agents ~f:snd
  in
  let actual_count =
    List.fold parseds ~init:0 ~f:(fun count parsed ->
      Stage.checkpoint stage;
      count + List.length (Parsed.declarations parsed))
  in
  if actual_count <> List.length (Bundle.Manifest.Chatmd_document.scripts document)
  then error Missing_or_surplus_script
  else (
    let%bind tasks, roots, remaining_scripts, remaining_selections =
      List.fold_result
        parseds
        ~init:([], [], Bundle.Manifest.Chatmd_document.scripts document, selected_projects)
        ~f:(fun (tasks, roots, scripts, selections) parsed ->
          List.fold_result
            (Parsed.declarations parsed)
            ~init:(tasks, roots, scripts, selections)
            ~f:(fun (tasks, roots, scripts, selections) declaration ->
              Stage.checkpoint stage;
              let%bind binding =
                Binding.of_declaration parsed declaration ~checkpoint:(fun () ->
                  Stage.checkpoint stage)
                |> Result.map_error ~f:(fun _ -> Stale_or_foreign_selector)
              in
              let%bind selector = selector_of_binding inputs binding declaration ~stage in
              let%bind script, scripts =
                find_and_remove_selector scripts selector ~stage
                |> option_result ~error:Missing_or_surplus_script
              in
              let selection, selections =
                match find_and_remove_selection selections selector ~stage with
                | None -> None, selections
                | Some (selection, selections) -> Some selection, selections
              in
              let%map task, root =
                task_for_declaration
                  inputs
                  script
                  selection
                  binding
                  selector
                  declaration
                  ~stage
              in
              task :: tasks, Option.to_list root @ roots, scripts, selections))
    in
    if (not (List.is_empty remaining_scripts)) || not (List.is_empty remaining_selections)
    then error Missing_or_surplus_script
    else Ok (List.rev tasks, roots))
;;

let covers_inventory manifest document tasks ~stage =
  let ids =
    List.fold
      (Bundle.Manifest.Chatmd_document.document_members document)
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun map member ->
        Stage.checkpoint stage;
        Map.set map ~key:(Bundle.Manifest.Document_member.source member) ~data:())
  in
  let ids =
    List.fold tasks ~init:ids ~f:(fun map task ->
      Stage.checkpoint stage;
      match task.project with
      | None -> map
      | Some project ->
        List.fold
          (Bundle.Manifest.Project.source_members project)
          ~init:map
          ~f:(fun map id ->
            Stage.checkpoint stage;
            Map.set map ~key:id ~data:()))
  in
  List.length (Bundle.Manifest.members manifest) = Map.length ids
  && List.for_all (Bundle.Manifest.members manifest) ~f:(fun member ->
    Stage.checkpoint stage;
    Map.mem ids (Agent_protocol.Authoring_common.Source.id (Bundle.Member.source member)))
;;

let validate manifest ~sources ~dir ~selected_projects ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind document =
    match Bundle.Manifest.basis manifest with
    | Project _ -> Error { Error.problem = Wrong_document_basis }
    | Chatmd_document document -> Ok document
  in
  let%bind attempt =
    Project.Attempt.admit manifest ~sources ~limits ~checkpoint
    |> Result.map_error ~f:(fun problem -> { Error.problem = Project problem })
  in
  let%bind tasks, bundle_fingerprint =
    Project.Attempt.run_stage attempt ~checkpoint ~f:(fun stage ->
      let%bind inputs =
        Inputs.create document ~stage
        |> Result.map_error ~f:(fun () -> Missing_or_surplus_document_member)
      in
      let document_sources = Inputs.bundle_sources inputs in
      List.iter document_sources ~f:(fun (_, contents) ->
        Stage.checkpoint stage;
        Stage.debit_input stage ~bytes:(String.length contents));
      let%bind bundle =
        Chatmd_source_bundle.create
          ~limits:
            { max_source_bytes = 1024 * 1024
            ; max_bundle_bytes = 8 * 1024 * 1024
            ; max_files = 256
            }
          ~checkpoint:(fun () -> Stage.checkpoint stage)
          ~root_file:(Inputs.root_file inputs)
          ~sources:document_sources
          ()
        |> Result.map_error ~f:(fun _ -> Document_bundle_limit)
      in
      let%bind receipt =
        Prompt.Chat_markdown.Generated.parse_source_bundle
          ~dir
          bundle
          ~checkpoint:(fun () -> Stage.checkpoint stage)
          ~before_read:(fun ~member:_ ~byte_length ->
            Stage.debit_input stage ~bytes:byte_length)
        |> Result.map_error ~f:(fun parser -> Parser parser)
      in
      let%bind tasks, roots =
        collect_tasks
          inputs
          (Prompt.Chat_markdown.Generated.parsed receipt)
          document
          selected_projects
          ~stage
      in
      if
        not
          (Inputs.covers_reads_or_roots
             inputs
             ~read_members:(Prompt.Chat_markdown.Generated.read_members receipt)
             ~selected_roots:roots
             ~stage)
      then error Missing_or_surplus_document_member
      else if not (covers_inventory manifest document tasks ~stage)
      then error Snapshot_mismatch
      else (
        let fingerprint =
          Chatmd_source_bundle.fingerprint bundle
          |> Captured.Digest.of_hex
          |> Or_error.ok_exn
        in
        Ok (tasks, fingerprint)))
    |> Result.map_error ~f:(function
      | Stage_error problem -> { Error.problem }
      | Budget_error problem -> { Error.problem = Project problem })
  in
  let%map scripts =
    List.fold_result tasks ~init:[] ~f:(fun scripts task ->
      match task.project, task.selection with
      | None, None ->
        Ok
          ({ Validated.Script.selector = task.selector
           ; binding = task.binding
           ; project = None
           }
           :: scripts)
      | Some _, Some selection ->
        Project.Attempt.validate_selected_script
          attempt
          ~selector:task.selector
          ~derived_entry:task.derived_entry
          ~previous:(Selection.previous selection)
          ~contract:(Selection.contract selection)
          ~checkpoint
        |> Result.map_error ~f:(fun problem -> { Error.problem = Project problem })
        |> Result.map ~f:(fun project ->
          { Validated.Script.selector = task.selector
          ; binding = task.binding
          ; project = Some project
          }
          :: scripts)
      | None, Some _ | Some _, None ->
        Error { Error.problem = Selected_contract_unavailable })
    |> Result.map ~f:List.rev
  in
  { Validated.manifest_fingerprint = Bundle.Manifest.fingerprint manifest
  ; root_source = Bundle.Manifest.Chatmd_document.root_source document
  ; document_bundle_fingerprint = bundle_fingerprint
  ; scripts
  }
;;
