open! Core
include Authoring_context_service_core
include Authoring_context_owner

type owner = t

let with_current t ~actor ~connection ~expected ~checkpoint ~f =
  Authoring_context_owner.with_current t ~actor ~connection ~expected ~checkpoint ~f
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
module For_testing = Authoring_context_testing
