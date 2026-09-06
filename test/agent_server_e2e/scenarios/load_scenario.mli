(** [run env ~case] runs explicit bounded load with retained resource reports.
    Defaults: 100 sessions/1000 commands, 25 sessions with 20 SSE clients each,
    1000 reconnects across 50 sessions, and three actor-unload settling cycles.
    OCHAT_E2E_LOAD_* positive integer overrides are recorded in reports. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** [report env name f] retains a passed/failed resource report for [f]. *)
val report : Eio_unix.Stdenv.base -> string -> (Support.Load_report.t -> unit) -> unit

(** [unload env report] verifies three stopped-actor settling cycles on a
    separate isolated stock daemon, identified by PID in each sample. *)
val unload : Eio_unix.Stdenv.base -> Support.Load_report.t -> unit
