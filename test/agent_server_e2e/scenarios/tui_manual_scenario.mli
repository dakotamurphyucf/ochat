(** [run env ~case] hosts an isolated manual fixture until operator stdin says
    [quit] or reaches EOF. Prints launcher paths and metadata-only observations,
    never credentials or raw conversation content. [Some "self-check"] instead
    validates the local launcher with a PTY and exits, cleaning all private roots.
    [Some "orchestration"] selects the durable-job moderator fixture;
    [Some "orchestration-self-check"] checks its detached delivery and later
    scheduled wake without a user terminal. This entry point has no Dune runtest
    alias. [Some "observer-shutdown-self-check"] verifies that operator-scoped
    fibers finish cancellation before the observer connection and daemon close.
    [Some "headless-self-check"] launches a setsid child, runs the real PTY
    approval/child/parent/quit trace, verifies no controlling terminal was
    acquired, and requires normal exit zero rather than a passed marker alone.
    [Some "headless-child"] is the internal subprocess entry point.
    [Some "typeahead"] automatically replies to nonstreaming gpt-5.6-luna
    suggestion requests with deterministic multiline/wide text. Append
    [--typeahead manual] or [--typeahead auto] to a printed launcher command.
    [Some "typeahead-self-check"] verifies that automatic reply path with a
    real PTY and clean terminal restoration, without user interaction. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
