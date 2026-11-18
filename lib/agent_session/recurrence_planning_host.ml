open! Core

type t =
  { authorize :
      controller:Agent_protocol.Session_ref.t
      -> (Operator_authorization.t, Recurrence_planning_status.Reason.t) result
  ; load_zone :
      name:string
      -> (Recurrence_rules.Zone.t option, Recurrence_zone_loader.Error.t) result
  }

let create ~authorize ~load_zone = { authorize; load_zone }
let authorize t ~controller = t.authorize ~controller
let load_zone t ~name = t.load_zone ~name
