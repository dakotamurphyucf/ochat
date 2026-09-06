(** [run env environment] drives the production blob validator and atomic
    installer over real HTTP to a deliberately faulty Eio TCP peer. Every fault
    occurs after a valid chunk is written to the actual sibling staging file.
    Check interrupted framing, malformed JSON/base64, digest, offset, metadata,
    premature EOF and no-progress failures with both existing and absent targets.
    Include successful two-chunk controls for both target states to distinguish
    corruption failures from an inherently broken peer or second-request path.
    This is an adversarial HTTP-peer test, not a daemon fault-injection test. *)
val run : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit

(** [run_cancellation env environment] checks callback-raised cancellation,
    ordinary callback exceptions and real fiber cancellation after staging a
    nonterminal blob chunk obtained over HTTP. Require cancellation to escape
    the installer with its original cause, rather than an Error result, and
    verify protected cleanup and preservation of existing/absent targets.
    The HTTP callback intentionally suspends before requesting further chunks;
    this tests installer cancellation, not cancellation inside the HTTP client. *)
val run_cancellation : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
