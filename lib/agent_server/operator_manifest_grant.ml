open! Core

type t =
  { id : string
  ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t
  ; workspace_definition_ids : Agent_protocol.Id.Workspace_definition.t list
  ; manifest_sha256 : string
  ; source_sha256 : string
  ; principal_ids : Agent_protocol.Id.Principal.t list
  }

let create value =
  let open Result.Let_syntax in
  let%map principal_ids =
    Result.all
      (List.map value.Config.Manifest_grant.principals ~f:(fun principal ->
         Agent_protocol.Id.Principal.of_string principal))
  in
  { id = value.id
  ; prompt_definition_id = Catalog_identity.prompt_definition value.prompt
  ; workspace_definition_ids =
      List.map value.workspaces ~f:Catalog_identity.workspace_definition
  ; manifest_sha256 = value.manifest_sha256
  ; source_sha256 = value.source_sha256
  ; principal_ids
  }
;;

let principal_matches t principal_id =
  List.is_empty t.principal_ids
  || List.mem t.principal_ids principal_id ~equal:(fun left right ->
    Agent_protocol.Id.Principal.compare left right = 0)
;;

let authorizes
      t
      ~prompt_definition_id
      ~workspace_definition_id
      ~principal_id
      ~source_sha256
      ~manifest_sha256
  =
  Agent_protocol.Id.Prompt_definition.compare t.prompt_definition_id prompt_definition_id
  = 0
  && List.mem t.workspace_definition_ids workspace_definition_id ~equal:(fun left right ->
    Agent_protocol.Id.Workspace_definition.compare left right = 0)
  && principal_matches t principal_id
  && String.equal t.source_sha256 source_sha256
  && String.equal t.manifest_sha256 manifest_sha256
;;

let id t = t.id
