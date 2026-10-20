open! Core
include Authoring_context_service_core
include Authoring_context_owner

type owner = t

module Selection_policy = struct
  include Authoring_context_service_core.Selection_policy

  let with_project_contract t ~project_contract = { t with project_contract }
end

let with_current t ~actor ~connection ~expected ~checkpoint ~f =
  Authoring_context_owner.with_current t ~actor ~connection ~expected ~checkpoint ~f
;;

let historical_supplied_identity t =
  Option.map t.supplied_document ~f:Supplied_document_policy.identity
;;

let check_historical_owner t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  if t.closed then Error Error.Closed else Ok ()
;;

let check_historical_origin_now t ~actor origin =
  let open Result.Let_syntax in
  let%bind () = check_historical_owner t in
  match origin with
  | Developer_historical_source.Origin.Roots roots ->
    (match t.sources with
     | None -> Error Error.Uninstalled
     | Some source ->
       Sources.Historical_roots.check_read_now source ~actor roots
       |> Result.map_error ~f:Authoring_context_owner.source_host_error)
  | Developer_historical_source.Origin.Published_upload pin ->
    (match t.upload with
     | None -> Error Error.Uninstalled
     | Some upload ->
       Upload.check_historical_pin_now upload ~actor pin
       |> Result.map_error ~f:Authoring_context_owner.upload_host_error)
  | Developer_historical_source.Origin.Supplied_document identity ->
    (match t.supplied_document with
     | None -> Error Error.Uninstalled
     | Some policy ->
       if
         Supplied_document_policy.Identity.equal
           (Supplied_document_policy.identity policy)
           identity
         && Operator_authorization.is_current actor
         && Supplied_document_policy.read_admitted policy actor
       then Ok ()
       else Error Error.Unauthorized)
;;

let check_historical_origin t ~actor origin ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_historical_owner t in
  match origin with
  | Developer_historical_source.Origin.Published_upload pin ->
    (match t.upload with
     | None -> Error Error.Uninstalled
     | Some upload ->
       let%bind () =
         Upload.check_historical_pin upload ~actor pin ~checkpoint
         |> Result.map_error ~f:Authoring_context_owner.upload_host_error
       in
       check_historical_origin_now t ~actor origin)
  | Developer_historical_source.Origin.Roots _
  | Developer_historical_source.Origin.Supplied_document _ ->
    check_historical_origin_now t ~actor origin
;;

let handle_core = Authoring_context_methods.handle_core
let handle_bundle = Authoring_context_bundle_service.handle_bundle

let handle_capabilities t ~actor ~connection ~make =
  let open Result.Let_syntax in
  let feature =
    match Connection_context.authoring_negotiation connection with
    | Some negotiation
      when Connection_context.Authoring_negotiation.selected
             negotiation
             P.Authoring.Features.core -> Ok Selected_feature.Core_authoring
    | Some negotiation
      when Connection_context.Authoring_negotiation.selected
             negotiation
             P.Authoring.Features.bundle -> Ok Selected_feature.Bundle_upload
    | None | Some _ -> Error Error.Unselected_feature
  in
  let%bind feature = feature in
  Authoring_context_service_core.with_request
    t
    ~feature
    ~actor
    ~connection
    ~checkpoint:Eio.Fiber.check
    (fun entry ~checkpoint ->
       checkpoint ();
       match entry.negotiation with
       | None -> Error Error.Stale_negotiation
       | Some negotiation ->
         let response = W.Response.Capabilities (make negotiation) in
         if
           String.length (Jsonaf.to_string (W.Response.to_json response))
           > t.limits.max_response_bytes
         then Error Error.Resource_limit
         else Ok response)
;;

module Semantic = struct
  include Authoring_semantic_service
  module Script_selection = Authoring_script_selection
  module Project_metadata = Authoring_project_metadata
end

module Assistance = Authoring_context_assistance_service

module Developer = struct
  include Developer_capture_host

  let create ~historical_recording ~owner ~policy ~limits =
    Developer_capture_host.create ~historical_recording ~authoring:owner ~policy ~limits
  ;;
end

module For_testing = Authoring_context_testing
