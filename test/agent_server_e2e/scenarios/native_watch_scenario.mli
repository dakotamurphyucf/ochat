(** Real-clock response-watch qualification through native managed-session tools
    and the confined helper adapter. Preserves the maintained 30-second watch
    deadline and production 100-event/5000-ms checkpoint cadence. Synthetic child
    dispatch is classified only by its actual developer input, never text copied
    into parent tool arguments. The HTTP client fixture uses the shared load
    harness's 120-second idle connection window; this does not change watch or
    subscription deadlines. Helper manifests are explicitly authorized only in
    the helper fixture; declared executable/session confinement remains enforced.
    Bounded phase checkpoints locate failures. Failed
    fixture cleanup releases owned synthetic barriers; purposeful restart leaves
    the child blocked until real daemon interruption is durably observed. *)
val run : Eio_unix.Stdenv.base -> Support.Load_report.t -> unit
