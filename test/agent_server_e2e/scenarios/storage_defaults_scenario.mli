(** Fixed-workload production storage measurements through the existing daemon
    harness. Uses real clocks and the default 100-event/5000-ms checkpoint policy.
    Reports acknowledged mutation, checkpoint-inclusive mutation, nested-tool,
    notification, restart and replay phases; no wall-time pass thresholds.
    Synthetic inference selects real local tools without external requests.
    Native helper/watch qualification remains a separately reported gap. *)
val run : Eio_unix.Stdenv.base -> Support.Load_report.t -> unit
