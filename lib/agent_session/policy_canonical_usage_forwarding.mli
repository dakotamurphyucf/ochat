(** Read-only source-work export from the CURRENT actual canonical carrier.
    Only this exporting work is revalidated; earlier flat frames remain immutable
    historical facts. Neither output nor currentness grants live authority. *)
val capture
  :  Policy_canonical_usage_carrier.t
  -> context:Policy_cause_data.Context.t
  -> work:Policy_cause_data.Allocation_work.t
  -> (Policy_cause_data.Forwarding.t, Agent_protocol.Error.t) result

val check_current
  :  Policy_canonical_usage_carrier.t
  -> context:Policy_cause_data.Context.t
  -> work:Policy_cause_data.Allocation_work.t
  -> Policy_cause_data.Forwarding.t
  -> (unit, Agent_protocol.Error.t) result
