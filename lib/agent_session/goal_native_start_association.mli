(** Pure native Start command and historical admission association checks.
    These values and successful checks are factual DATA, never execution custody.
    Actor callsites retain private original/source/owner and ACK sequencing. *)
val intent
  :  Agent_protocol.Goal_native_start_request.t
  -> (Goal_episode_admission.intent, Agent_protocol.Error.t) result

(** Check immutable target, exact current CAS/source and native no-operator-pause
    rule. A previous episode needs its actual terminal receipt. This neither
    enables the Goal nor creates a Run; actual admission repeats the planner. *)
val check_request
  :  Agent_protocol.Goal_native_start_request.t
  -> state:Session_state.t
  -> session:Agent_protocol.Session_ref.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> (unit, Agent_protocol.Error.t) result

(** Check the same actual admission against its immutable native command and
    retained historical Run/Goal link, independently of the current episode. *)
val check_admission
  :  Agent_protocol.Goal_native_start_intent.t
  -> state:Session_state.t
  -> session:Agent_protocol.Session_ref.t
  -> admission:Agent_protocol.Run_receipt.t
  -> (unit, Agent_protocol.Error.t) result
