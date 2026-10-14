open! Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Actor = Operator_authorization
module Project = Chatml_authoring_bundle_project
module Document = Chatml_authoring_bundle_document
module Captured = Chatml_source
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

exception Contract_limit

module Selected = struct
  type t =
    { environment : string
    ; contract : Contract.t
    ; fingerprint : Captured.Digest.t
    ; identity_bytes : int
    }

  let create ~environment ~contract =
    if String.is_empty environment || String.length environment > 1024
    then Or_error.error_string "invalid selected environment"
    else (
      let maximum_work = 64 * 1024 * 1024 in
      let maximum_identity = 8 * 1024 * 1024 in
      let allocated = ref 0 in
      try
        let identity_bytes =
          Contract.input_bytes contract ~checkpoint:Fn.id ~allocate:(fun bytes ->
            if bytes < 0 || bytes > maximum_work - !allocated then raise Contract_limit;
            allocated := !allocated + bytes)
        in
        if identity_bytes < 0 || identity_bytes > maximum_identity
        then raise Contract_limit;
        Ok
          { environment
          ; contract
          ; fingerprint = Contract.fingerprint contract ~checkpoint:Fn.id
          ; identity_bytes
          }
      with
      | Contract_limit ->
        Or_error.error_string "selected compiler contract exceeds rebind bound")
  ;;

  let environment t = t.environment
  let contract t = t.contract
end

module Policy = struct
  type t =
    { authorize_read : Actor.t -> bool
    ; select :
        Actor.t
        -> Bundle.Manifest.t
        -> Bundle.Manifest.Script.Selector.t option
        -> Selected.t option
    }

  let create ~authorize_read ~select = { authorize_read; select }
  let authorize_read t actor = t.authorize_read actor
end

module Rebound = struct
  type basis =
    | Project of Project.Validated.t
    | Chatmd_document of Document.Validated.t

  type t =
    { receipt : Bundle.Receipt.t
    ; basis : basis
    }

  let receipt t = t.receipt
  let basis t = t.basis
end

module Error = struct
  type t =
    | Selection_unavailable
    | Stale_capture
    | Invalid_closure
    | Resource_limit
end

type target =
  { selector : Bundle.Manifest.Script.Selector.t option
  ; selected : Selected.t
  }

type t =
  { manifest : Bundle.Manifest.t
  ; receipt : Bundle.Receipt.t
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

let saved_project receipt selector =
  match Bundle.Receipt.basis receipt, selector with
  | Project project, None -> Some project
  | Chatmd_document document, Some selector ->
    List.find_map (Bundle.Receipt.Chatmd_document.scripts document) ~f:(function
      | Source_only _ -> None
      | Selected_project { selector = current; project; _ } ->
        if Bundle.Manifest.Script.Selector.equal selector current
        then Some project
        else None)
  | Project _, Some _ | Chatmd_document _, None -> None
;;

let admit policy ~actor ~receipt ~manifest ~checkpoint =
  if
    not
      (Captured.Digest.equal
         (Bundle.Manifest.fingerprint manifest)
         (Bundle.Receipt.manifest_fingerprint receipt))
  then Error Error.Stale_capture
  else
    let* targets, _ =
      List.fold
        (selectors manifest)
        ~init:(Ok ([], 0))
        ~f:(fun result selector ->
          let* targets, bytes = result in
          checkpoint ();
          match
            policy.Policy.select actor manifest selector, saved_project receipt selector
          with
          | None, _ -> Error Error.Selection_unavailable
          | Some _, None -> Error Error.Stale_capture
          | Some selected, Some saved ->
            if selected.identity_bytes > (8 * 1024 * 1024) - bytes
            then Error Error.Resource_limit
            else if
              String.equal
                (Selected.environment selected)
                (Bundle.Receipt.Project.environment saved)
              && Captured.Digest.equal
                   selected.fingerprint
                   (Bundle.Receipt.Project.compiler_contract saved)
            then Ok ({ selector; selected } :: targets, bytes + selected.identity_bytes)
            else Error Error.Stale_capture)
    in
    Ok { manifest; receipt; targets = List.rev targets }
;;

let check_current t ~policy ~actor ~checkpoint =
  List.for_all t.targets ~f:(fun target ->
    checkpoint ();
    match policy.Policy.select actor t.manifest target.selector with
    | None -> false
    | Some current ->
      String.equal current.environment target.selected.environment
      && Captured.Digest.equal current.fingerprint target.selected.fingerprint)
;;

let target t selector =
  List.find t.targets ~f:(fun target ->
    Option.equal Bundle.Manifest.Script.Selector.equal target.selector selector)
  |> Option.map ~f:(fun target -> target.selected)
;;

let project_contract t =
  match Bundle.Manifest.basis t.manifest with
  | Project _ -> Option.map (target t None) ~f:Selected.contract
  | Chatmd_document _ -> None
;;

let project_error problem =
  match Project.Error.problem problem with
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
  | Project_declaration _ -> Error.Stale_capture
;;

let document_error problem =
  match Document.Error.problem problem with
  | Project problem -> project_error problem
  | Document_bundle_limit -> Error.Resource_limit
  | Parser _ -> Error.Invalid_closure
  | Wrong_document_basis
  | Snapshot_mismatch
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
  | Unexpected_selected_contract -> Error.Stale_capture
;;

let selected_environment selected validated =
  String.equal
    (Selected.environment selected)
    (Prepared.Environment.name
       (Prepared.environment (Build.prepared (Project.Validated.build validated))))
;;

let validate t ~capture ~document_dir ~limits ~checkpoint =
  let manifest = Store.Published_capture.manifest capture in
  let receipt = Store.Published_capture.receipt capture in
  if
    (not (Bundle.Manifest.equal manifest t.manifest))
    || not (Bundle.Receipt.equal receipt t.receipt)
  then Error Error.Stale_capture
  else
    let* basis, proof =
      match Bundle.Manifest.basis manifest with
      | Project _ ->
        (match Store.Published_capture.graphs capture, target t None with
         | [ graph ], Some selected
           when Option.is_none (Store.Published_capture.Graph.selector graph) ->
           let* attempt =
             Project.Attempt.admit
               manifest
               ~sources:(Store.Published_capture.uploaded_sources capture)
               ~limits
               ~checkpoint
             |> Result.map_error ~f:project_error
           in
           let* validated =
             Project.Attempt.restore_published_top
               attempt
               ~lock_source:(Store.Published_capture.Graph.lock_source graph)
               ~sources:(Store.Published_capture.Graph.sources graph)
               ~contract:(Selected.contract selected)
               ~checkpoint
             |> Result.map_error ~f:project_error
           in
           if selected_environment selected validated
           then Ok (Rebound.Project validated, Store.Proof.Project validated)
           else Error Error.Stale_capture
         | _ -> Error Error.Stale_capture)
      | Chatmd_document _ ->
        let* selected_projects =
          List.fold
            (Store.Published_capture.graphs capture)
            ~init:(Ok [])
            ~f:(fun result graph ->
              let* selected_projects = result in
              checkpoint ();
              match Store.Published_capture.Graph.selector graph with
              | None -> Error Error.Stale_capture
              | Some selector ->
                (match target t (Some selector) with
                 | None -> Error Error.Stale_capture
                 | Some selected ->
                   Ok
                     (( selector
                      , Document.Published_selection.create
                          ~contract:(Selected.contract selected)
                          ~lock_source:(Store.Published_capture.Graph.lock_source graph)
                          ~sources:(Store.Published_capture.Graph.sources graph) )
                      :: selected_projects)))
        in
        let* validated =
          Document.rebind
            manifest
            ~sources:(Store.Published_capture.uploaded_sources capture)
            ~dir:document_dir
            ~selected_projects:(List.rev selected_projects)
            ~limits
            ~checkpoint
          |> Result.map_error ~f:document_error
        in
        let correct_environments =
          List.for_all (Document.Validated.scripts validated) ~f:(fun script ->
            checkpoint ();
            match Document.Validated.Script.project script with
            | None -> true
            | Some project ->
              Option.value_map
                (target t (Some (Document.Validated.Script.selector script)))
                ~default:false
                ~f:(fun selected -> selected_environment selected project))
        in
        if correct_environments
        then Ok (Rebound.Chatmd_document validated, Store.Proof.Chatmd_document validated)
        else Error Error.Stale_capture
    in
    if Store.Published_capture.agrees_with_proof capture proof ~checkpoint
    then Ok { Rebound.receipt; basis }
    else Error Error.Stale_capture
;;
