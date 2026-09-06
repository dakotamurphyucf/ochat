(** Persistent metadata-only reports. Resource commands inspect only fixture PIDs.
    Sampling currently requires macOS [/bin/ps] and [/usr/sbin/lsof]. Reports
    default to [_build/agent-e2e-reports] beneath [DUNE_SOURCEROOT] or the launch
    directory, not a disposable Dune action directory. An absolute
    [OCHAT_E2E_REPORT_ROOT] overrides that location. Atomic replacement is
    serialized across reporting fibers; this is not a power-loss durability test. *)
open Core

type t

(** [create env name] selects a fresh report path; the first [record] writes it.
    The name must be a harness-controlled scenario label, not user input. *)
val create : Eio_unix.Stdenv.base -> string -> t

(** [record t env label fields] atomically replaces the metadata checkpoint;
    interruption leaves the previous complete JSON report available. *)
val record : t -> Eio_unix.Stdenv.base -> string -> (string * Jsonaf.t) list -> unit

(** [sample t env fixture daemon client label fields] records process resources,
    ready-state actor count and store sizes, returning [(rss_kib, descriptors)].
    Disk traversal does not follow symbolic links. Samples are observations over
    time, not an atomic filesystem snapshot or a continuous peak measurement. *)
val sample
  :  t
  -> Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> Daemon_process.t
  -> Http_driver.t
  -> string
  -> (string * Jsonaf.t) list
  -> int * int

(** [finish t] writes passed status and returns the report's native path.
    Call only after all reporting fibers and scenario assertions have completed. *)
val finish : t -> string

(** [fail t] preserves collected samples with failed status. Call after reporting
    fibers have stopped; failure never converts partial execution into a pass. *)
val fail : t -> unit
