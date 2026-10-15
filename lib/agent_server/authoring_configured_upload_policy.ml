open Core
module P = Agent_protocol
module Bundle = P.Authoring_bundle.Bundle
module Config = Authoring_upload_access_configuration
module Contracts = Authoring_standard_contracts
module Host = Authoring_upload_host
module Actor = Operator_authorization
module Protocol_selection = P.Authoring.Context.Selection

module Error = struct
  type t =
    | Invalid_configuration
    | Missing_standard_target
    | Selection_limit
  [@@deriving sexp_of]
end

type selected =
  { principal_id : P.Id.Principal.t
  ; authentication_kind : string
  ; request : Protocol_selection.t
  ; publication : Host.Publication_policy.Selected.t
  ; rebind : Host.Rebind_policy.Selected.t
  }

type t =
  { grants : Config.Grant.t list
  ; selected : selected list
  }

let request = function
  | Authoring_project_access_configuration.Selection.Library { environment } ->
    Protocol_selection.create ~target:Protocol_selection.Library ~environment
  | Entry { name; environment } ->
    Protocol_selection.create ~target:(Protocol_selection.Entry name) ~environment
;;

let bind configuration ~contracts =
  let open Result.Let_syntax in
  let%bind () =
    Config.validate configuration
    |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
  in
  let%map selected =
    List.map configuration.selections ~f:(fun row ->
      let%bind request =
        request row.request |> Result.map_error ~f:(fun _ -> Error.Invalid_configuration)
      in
      let%bind admitted =
        Contracts.find contracts row.target
        |> Result.of_option ~error:Error.Missing_standard_target
      in
      let contract = Contracts.Entry.contract admitted in
      let%bind publication =
        Host.Publication_policy.Selected.create
          ~environment:row.selected_environment
          ~contract
          ~previous:None
        |> Result.map_error ~f:(fun _ -> Error.Selection_limit)
      in
      let%map rebind =
        Host.Rebind_policy.Selected.create_standard
          ~environment:row.selected_environment
          ~admitted
        |> Result.map_error ~f:(fun _ -> Error.Selection_limit)
      in
      { principal_id = row.principal_id
      ; authentication_kind = row.authentication_kind
      ; request
      ; publication
      ; rebind
      })
    |> Result.all
  in
  { grants = configuration.grants; selected }
;;

let current_actor actor = Actor.is_current actor

let grant t actor operation =
  current_actor actor
  &&
  let principal = Actor.principal actor in
  List.exists t.grants ~f:(fun row ->
    P.Id.Principal.equal principal.id row.principal_id
    && String.equal principal.authentication_kind row.authentication_kind
    && Config.Grant.equal_operation row.operation operation)
;;

let upload_policy t =
  Host.Upload_policy.create
    ~authorize_write:(fun actor -> grant t actor Config.Grant.Upload_write)
    ~authorize_publish:(fun actor -> grant t actor Config.Grant.Publish)
    ~authorize_parent:None
;;

let project_from_manifest manifest selector =
  match Bundle.Manifest.basis manifest, selector with
  | Project project, None -> Some project
  | Chatmd_document document, Some selector ->
    List.find_map (Bundle.Manifest.Chatmd_document.scripts document) ~f:(fun script ->
      if
        Bundle.Manifest.Script.Selector.equal
          selector
          (Bundle.Manifest.Script.selector script)
      then (
        match Bundle.Manifest.Script.choice script with
        | Source_only -> None
        | Selected_project project -> Some project)
      else None)
  | Project _, Some _ | Chatmd_document _, None -> None
;;

let no_previous_required project =
  match Bundle.Manifest.Project.mode project with
  | Exact_lock _ -> true
  | Compose { selected_previous_lock = None; generated_lock = _ } -> true
  | Compose { selected_previous_lock = Some _; generated_lock = _ } -> false
;;

let selection t actor project =
  let principal = Actor.principal actor in
  let request = Bundle.Manifest.Project.selection project in
  List.find t.selected ~f:(fun row ->
    P.Id.Principal.equal row.principal_id principal.id
    && String.equal row.authentication_kind principal.authentication_kind
    && Protocol_selection.equal row.request request)
;;

let publication_policy t =
  Host.Publication_policy.create ~select:(fun actor manifest selector ->
    if not (grant t actor Config.Grant.Publish)
    then None
    else (
      match project_from_manifest manifest selector with
      | None -> None
      | Some project ->
        if not (no_previous_required project)
        then None
        else Option.map (selection t actor project) ~f:(fun row -> row.publication)))
;;

let rebind_policy t =
  Host.Rebind_policy.create
    ~authorize_read:(fun actor -> grant t actor Config.Grant.Bundle_read)
    ~authorize_rebind:(fun actor -> grant t actor Config.Grant.Rebind)
    ~select:(fun actor manifest selector ->
      if not (grant t actor Config.Grant.Bundle_read && grant t actor Config.Grant.Rebind)
      then None
      else
        Option.bind (project_from_manifest manifest selector) ~f:(fun project ->
          Option.map (selection t actor project) ~f:(fun row -> row.rebind)))
;;
