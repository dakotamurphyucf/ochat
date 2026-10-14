open! Core
include Authoring_context_service_core
include Authoring_context_owner

let with_current t ~actor ~connection ~expected ~checkpoint ~f =
  Authoring_context_owner.with_current t ~actor ~connection ~expected ~checkpoint ~f
;;

let handle_core = Authoring_context_methods.handle_core
let handle_bundle = Authoring_context_bundle_service.handle_bundle
