open! Core
include Capability_source_inventory

(* One generated immutable byte inventory, partitioned into explicit ordinary
   source closures. Shared libraries have one canonical source and exact bytes. *)
let lifecycle_members =
  [ "examples/learning/lifecycle-composition/agent.chatmd"
  ; "examples/learning/lifecycle-composition/chatml.project.json"
  ; "examples/learning/lifecycle-composition/components/counter/chatml.project.json"
  ; "examples/learning/lifecycle-composition/components/counter/src/counter.chatml"
  ; "examples/learning/lifecycle-composition/components/counter/src/counter.chatmli"
  ; "examples/learning/lifecycle-composition/components/review/chatml.project.json"
  ; "examples/learning/lifecycle-composition/components/review/src/review.chatml"
  ; "examples/learning/lifecycle-composition/components/review/src/review.chatmli"
  ; "examples/learning/lifecycle-composition/moderator.chatml"
  ; "examples/libraries/lifecycle-run/chatml.project.json"
  ; "examples/libraries/lifecycle-run/src/lifecycle_run.chatml"
  ; "examples/libraries/lifecycle-run/src/lifecycle_run.chatmli"
  ; "examples/libraries/lifecycle/chatml.project.json"
  ; "examples/libraries/lifecycle/src/lifecycle.chatml"
  ; "examples/libraries/lifecycle/src/lifecycle.chatmli"
  ; "guide/chatml-lifecycle-composition.md"
  ; "guide/chatml-installed-libraries.md"
  ; "guide/chatml-authoring-runs.md"
  ]
;;

let instruction_only =
  [ "examples/learning/instruction-selection/agent.chatmd"
  ; "examples/learning/instruction-selection/chatml.project.json"
  ; "examples/learning/instruction-selection/moderator.chatml"
  ; "examples/libraries/instruction-requests/chatml.project.json"
  ; "examples/libraries/instruction-requests/src/instruction_requests.chatml"
  ; "examples/libraries/instruction-requests/src/instruction_requests.chatmli"
  ; "guide/admitted-instructions.md"
  ]
;;

let instruction_members =
  instruction_only
  @ [ "examples/libraries/lifecycle/chatml.project.json"
    ; "examples/libraries/lifecycle/src/lifecycle.chatml"
    ; "examples/libraries/lifecycle/src/lifecycle.chatmli"
    ; "guide/chatml-installed-libraries.md"
    ]
;;

let recurrence_members =
  [ "examples/libraries/recurrence-requests/chatml.project.json"
  ; "examples/libraries/recurrence-requests/src/recurrence_requests.chatml"
  ; "examples/libraries/recurrence-requests/src/recurrence_requests.chatmli"
  ; "agent-server/recurring-automation.md"
  ]
;;

let installed ~limits ~checkpoint =
  Capability_source_inventory.admit
    [ { id = "lifecycle"
      ; title = "Lifecycle composition and scoped Run adapter"
      ; entry = "examples/learning/lifecycle-composition/agent.chatmd"
      ; members =
          List.filter Bundled_capability_sources.documents ~f:(fun (path, _) ->
            List.mem lifecycle_members path ~equal:String.equal)
      }
    ; { id = "recurrence-requests"
      ; title = "Pure explicit recurring automation request constructors"
      ; entry = "examples/libraries/recurrence-requests/chatml.project.json"
      ; members =
          List.filter Bundled_capability_sources.documents ~f:(fun (path, _) ->
            List.mem recurrence_members path ~equal:String.equal)
      }
    ; { id = "instruction-selection"
      ; title = "Explicit instruction requests and Lifecycle review"
      ; entry = "examples/learning/instruction-selection/agent.chatmd"
      ; members =
          List.filter Bundled_capability_sources.documents ~f:(fun (path, _) ->
            List.mem instruction_members path ~equal:String.equal)
      }
    ]
    ~limits
    ~checkpoint
;;
