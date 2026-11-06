(** Pure retirement of future executable Goal work. These plans carry no
    authority: the Actor authenticates commands, owns prewriter revocation and
    saves the complete returned delta with the actual command/safety transition. *)

(** Augment an already prepared fresh Hold command without changing its planned
    Goal/catalog revisions or receipt. Retire only that Goal's Pending Continue
    and pending Turn/Compact follow-ups, including historical episode evidence.
    Preserve Wait/Finish, admitted work, immutable outcomes and stop-dominant
    End_session requests. Retained commands bypass this function. *)
val prepare_hold
  :  Session_state.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> delta:Session_delta.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Same selective future-execution retirement for a fresh Abandon. Does not
    cancel admitted Run work, alter accounting or manufacture terminal evidence.
    Retained commands bypass this function. *)
val prepare_abandon
  :  Session_state.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> delta:Session_delta.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Stop safety planning, independent of user command receipt capacity. Enabled
    Goals become Held Operator_pause once; existing holds and closed Goals retain
    their metadata. All open Goals' future executable requests are retired.
    No accounts, actual work, passive Waits or receipts are removed. None means
    no durable change, not permission to omit Actor-owned explicit revocation. *)
val prepare_stop
  :  Session_state.t
  -> (Session_delta.t option, Agent_protocol.Error.t) result
