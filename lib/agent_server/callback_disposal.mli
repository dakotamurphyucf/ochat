(** Pure observations of actual callback producer/finish/release states. These
    are not whole-runtime join proofs or permission to repeat callbacks. *)
val completion_settled : Agent_session.Callback_cleanup.Completion_snapshot.t -> bool

(** Also requires the actual parent's registration closure. An original Init End
    or Operation terminal and factual/resource duties stay separate. *)
val parent_settled : Agent_session.Callback_cleanup.Cleanup_snapshot.t -> bool

(** Exact original parent diagnostic completeness at this immutable observation.
    Later handler completion cannot retroactively bless an earlier sealed report.
    This does not require successful callback cleanup or grant custody release. *)
val parent_diagnostics_sealed : Agent_session.Callback_cleanup.Cleanup_snapshot.t -> bool
