(** Cross-subcarrier proof membership. Pure data validation grants no host
    authority and depends on neither Session_state nor Session_delta. *)
val validate
  :  catalog:Goal_catalog.t option
  -> runs:Run_state.t option
  -> (unit, Agent_protocol.Error.t) result

(** Whole ordered Batch fence, after its complete final projection. A newly
    associated Run must have the reciprocal current Enabled goal episode; old
    historical links remain immutable even after the goal advances or closes. *)
val validate_transition
  :  previous_runs:Run_state.t option
  -> catalog:Goal_catalog.t option
  -> runs:Run_state.t option
  -> (unit, Agent_protocol.Error.t) result
