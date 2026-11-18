(** Trusted composition-root port for planning only. The real daemon adapter
    binds explicit administrative grants and its admitted directory owner. No
    decoder, principal rematch or stored creator may construct that authority.
    This port supplies neither target visibility nor child/Run admission. *)
type t

(** Authorize issues a proof of this original host admission, whose non-yielding
    currentness guard includes host lifetime, exact controller grant and original
    expiry. The actor retains and rechecks that same proof after every yield.
    Missing zone data returns None; loader cancellation propagates unchanged. *)
val create
  :  authorize:
       (controller:Agent_protocol.Session_ref.t
        -> (Operator_authorization.t, Recurrence_planning_status.Reason.t) result)
  -> load_zone:
       (name:string
        -> (Recurrence_rules.Zone.t option, Recurrence_zone_loader.Error.t) result)
  -> t

val authorize
  :  t
  -> controller:Agent_protocol.Session_ref.t
  -> (Operator_authorization.t, Recurrence_planning_status.Reason.t) result

val load_zone
  :  t
  -> name:string
  -> (Recurrence_rules.Zone.t option, Recurrence_zone_loader.Error.t) result
