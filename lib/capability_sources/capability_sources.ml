open! Core
include Capability_source_inventory

let installed ~limits ~checkpoint =
  Capability_source_inventory.admit
    [ { id = "lifecycle"
      ; title = "Lifecycle composition and scoped Run adapter"
      ; entry = "examples/learning/lifecycle-composition/agent.chatmd"
      ; members = Bundled_capability_sources.documents
      }
    ]
    ~limits
    ~checkpoint
;;
