open! Core
include Authoring_context_service_core
include Authoring_context_owner

type owner = t

let with_current t ~actor ~connection ~expected ~checkpoint ~f =
  Authoring_context_owner.with_current t ~actor ~connection ~expected ~checkpoint ~f
;;

let handle_core = Authoring_context_methods.handle_core
let handle_bundle = Authoring_context_bundle_service.handle_bundle

module Semantic = struct
  include Authoring_semantic_service
  module Script_selection = Authoring_script_selection
  module Project_metadata = Authoring_project_metadata
end

module Assistance = Authoring_context_assistance_service
module For_testing = Authoring_context_testing
