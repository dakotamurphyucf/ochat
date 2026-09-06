(** [run env environment] tests the production Maintenance.run_once coordinator
    against real filesystem stores with explicit timestamps. Inject the active
    protection set; do not claim coverage of the daemon's registry-derived active
    detection or periodic timer. Check both sides of the response cutoff, inclusive
    blob/receipt expiration, protected receipts, adopted blobs, persisted receipts,
    non-response isolation, protection release and repeated-cycle idempotence. *)
val run : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
