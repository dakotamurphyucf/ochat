open! Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Actor = Operator_authorization
module Project = Chatml_authoring_bundle_project
module Document = Chatml_authoring_bundle_document
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Captured = Chatml_source

module Selected = struct
  type t =
    { environment : string
    ; contract : Chatml.Chatml_compiler_contract.t
    ; previous : Build.t option
    }

  let create ~environment ~contract ~previous =
    if String.is_empty environment || String.length environment > 1024
    then Or_error.error_string "invalid selected environment"
    else Ok { environment; contract; previous }
  ;;

  let environment t = t.environment
  let contract t = t.contract
  let previous t = t.previous
end

module Policy = struct
  type t =
    { select :
        Actor.t
        -> Bundle.Manifest.t
        -> Bundle.Manifest.Script.Selector.t option
        -> Selected.t option
    }

  let create ~select = { select }
end

module Error = struct
  type t =
    | Unavailable
    | Invalid_closure
    | Resource_limit
end

type target =
  { selector : Bundle.Manifest.Script.Selector.t option
  ; selected : Selected.t
  }

type t =
  { manifest : Bundle.Manifest.t
  ; targets : target list
  }

let ( let* ) result f = Result.bind result ~f

let selectors manifest =
  match Bundle.Manifest.basis manifest with
  | Project _ -> [ None ]
  | Chatmd_document document ->
    List.filter_map (Bundle.Manifest.Chatmd_document.scripts document) ~f:(fun script ->
      match Bundle.Manifest.Script.choice script with
      | Source_only -> None
      | Selected_project _ -> Some (Some (Bundle.Manifest.Script.selector script)))
;;

let admit policy ~actor ~manifest ~checkpoint =
  let* targets =
    List.fold (selectors manifest) ~init:(Ok []) ~f:(fun result selector ->
      let* targets = result in
      checkpoint ();
      match policy.Policy.select actor manifest selector with
      | None -> Error Error.Unavailable
      | Some selected -> Ok ({ selector; selected } :: targets))
  in
  Ok { manifest; targets = List.rev targets }
;;

let same_source left right =
  Captured.Id.equal (Captured.id left) (Captured.id right)
  && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && Captured.byte_length left = Captured.byte_length right
;;

let same_previous left right =
  match left, right with
  | None, None -> true
  | Some left, Some right ->
    Captured.Digest.equal
      (Prepared.fingerprint (Build.prepared left))
      (Prepared.fingerprint (Build.prepared right))
    && same_source (Build.lock_source left) (Build.lock_source right)
  | None, Some _ | Some _, None -> false
;;

let same_selected left right ~checkpoint =
  checkpoint ();
  String.equal left.Selected.environment right.Selected.environment
  && Captured.Digest.equal
       (Chatml.Chatml_compiler_contract.fingerprint left.contract ~checkpoint)
       (Chatml.Chatml_compiler_contract.fingerprint right.contract ~checkpoint)
  && same_previous left.previous right.previous
;;

let check_current t ~policy ~actor ~checkpoint =
  List.for_all t.targets ~f:(fun target ->
    checkpoint ();
    match policy.Policy.select actor t.manifest target.selector with
    | None -> false
    | Some current -> same_selected target.selected current ~checkpoint)
;;

let target t selector =
  List.find t.targets ~f:(fun target ->
    Option.equal Bundle.Manifest.Script.Selector.equal target.selector selector)
  |> Option.map ~f:(fun target -> target.selected)
;;

let checked_build selected validated =
  let prepared = Build.prepared (Project.Validated.build validated) in
  String.equal
    (Prepared.Environment.name (Prepared.environment prepared))
    selected.Selected.environment
;;

let project_error error =
  match Project.Error.problem error with
  | Work_limit | Input_limit | Accounting_overflow -> Error.Resource_limit
  | Snapshot_mismatch
  | Missing_or_surplus_member
  | Wrong_project_basis
  | Missing_selected_script
  | Derived_entry_mismatch
  | Previous_lock_mismatch
  | Unexpected_previous_build
  | Result_mismatch
  | Attempt_closed
  | Attempt_busy
  | Builder _
  | Project_declaration _ -> Error.Invalid_closure
;;

let document_error error =
  match Document.Error.problem error with
  | Project project -> project_error project
  | Document_bundle_limit -> Error.Resource_limit
  | Wrong_document_basis
  | Snapshot_mismatch
  | Parser _
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
  | Unexpected_selected_contract -> Error.Invalid_closure
;;

let validate t ~snapshot ~document_dir ~limits ~checkpoint =
  let manifest = Store.Snapshot.manifest snapshot in
  if
    not
      (Captured.Digest.equal
         (Bundle.Manifest.fingerprint t.manifest)
         (Bundle.Manifest.fingerprint manifest))
  then Error Error.Invalid_closure
  else (
    match Bundle.Manifest.basis manifest with
    | Project _ ->
      let* selected = target t None |> Result.of_option ~error:Error.Unavailable in
      let* attempt =
        Project.Attempt.admit
          manifest
          ~sources:(Store.Snapshot.sources snapshot)
          ~limits
          ~checkpoint
        |> Result.map_error ~f:project_error
      in
      let* validated =
        Project.Attempt.validate_top_project
          attempt
          ~previous:selected.previous
          ~contract:selected.contract
          ~checkpoint
        |> Result.map_error ~f:project_error
      in
      if checked_build selected validated
      then Ok (Store.Proof.Project validated)
      else Error Error.Invalid_closure
    | Chatmd_document document ->
      let* selected_projects =
        List.fold
          (Bundle.Manifest.Chatmd_document.scripts document)
          ~init:(Ok [])
          ~f:(fun result script ->
            let* projects = result in
            checkpoint ();
            match Bundle.Manifest.Script.choice script with
            | Source_only -> Ok projects
            | Selected_project _ ->
              let selector = Bundle.Manifest.Script.selector script in
              let* selected =
                target t (Some selector) |> Result.of_option ~error:Error.Unavailable
              in
              Ok
                (( selector
                 , Document.Selection.create
                     ~contract:selected.contract
                     ~previous:selected.previous )
                 :: projects))
      in
      let* validated =
        Document.validate
          manifest
          ~sources:(Store.Snapshot.sources snapshot)
          ~dir:document_dir
          ~selected_projects:(List.rev selected_projects)
          ~limits
          ~checkpoint
        |> Result.map_error ~f:document_error
      in
      if
        List.for_all (Document.Validated.scripts validated) ~f:(fun script ->
          match Document.Validated.Script.project script with
          | None -> true
          | Some project ->
            (match target t (Some (Document.Validated.Script.selector script)) with
             | None -> false
             | Some selected -> checked_build selected project))
      then Ok (Store.Proof.Chatmd_document validated)
      else Error Error.Invalid_closure)
;;
