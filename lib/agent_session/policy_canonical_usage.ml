open! Core
module Carrier = Policy_canonical_usage_carrier
module Selected = Policy_canonical_usage_selection
module Updated = Policy_canonical_usage_update
module Exported = Policy_canonical_usage_projection

type t = Carrier.t

let limits = Carrier.limits
let max_causes = Carrier.max_causes
let max_memberships = Carrier.max_memberships
let max_streams = Carrier.max_streams
let max_bindings = Carrier.max_bindings
let callee t = (Carrier.value t).Policy_canonical_usage_data.callee

module Choice = Policy_canonical_usage_data.Choice
module Binding = Policy_canonical_usage_data.Binding

module Stream = struct
  include Policy_canonical_usage_data.Stream

  let for_target cause ~target =
    if
      List.mem
        (Policy_cause_data.targets cause)
        target
        ~equal:Policy_cause_data.Goal_target.equal
    then Ok (of_cause cause target)
    else
      Error
        (Agent_protocol.Error.invalid_request
           "policy stream target is absent from its cause")
  ;;
end

module Selection = Selected.Selection
module Admission = Selected.Admission

module Update = struct
  include Updated.Update

  let selection t =
    match t.reason with
    | Reconcile selected -> selected
    | Register_event _ | Register_work _ | Prune_work -> None
  ;;
end

module Head = Exported.Head
module Projection = Exported.Projection
module Ordered = Policy_canonical_usage_ordered.Ordered

let bindings = Carrier.bindings
let validate = Carrier.validate
let protected_ordinals = Carrier.protected_ordinals
let reserved_growth = Carrier.reserved_growth
let required_depth = Carrier.required_depth
let validate_encoded_capacity = Carrier.validate_encoded_capacity
let of_document = Carrier.of_document
let to_document = Carrier.to_document
let adopt = Carrier.adopt
let register_event = Updated.register_event
let register_work = Updated.register_work
let reconcile = Updated.reconcile
let reconcile_factual = Updated.reconcile_factual
let prune_work = Updated.prune_work
let projections = Exported.projections
let capture_forwarding = Policy_canonical_usage_forwarding.capture
let check_forwarding_current = Policy_canonical_usage_forwarding.check_current

module Source = Policy_canonical_usage_source
