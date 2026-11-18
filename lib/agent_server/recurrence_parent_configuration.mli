(** Compare the frozen parent configuration with the current session. Host
    permission profiles, workspace availability and creation policy are
    checked independently by the execution owner. *)
val matches
  :  Agent_session.Recurrence_target.Parent.t
  -> Agent_session.Session_state.Spec.t
  -> bool
