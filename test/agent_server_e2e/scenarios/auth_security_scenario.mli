open Core

(** [run env ~case] runs [auth-security] against the production HTTP transport
    in [Daemon_host], with injected OAuth validators and configured proxy trust.
    Require callback exceptions to fail closed with redacted unauthenticated
    errors while preserving cancellation in focused direct-daemon regressions.

    Exercise OAuth success, rejection, expiry, exceptions and backend errors;
    static expiry and malformed/duplicate Authorization headers; trusted proxy
    assertions and header errors; untrusted forwarded-address spoofing; and
    scope denial before session registry, index or directory mutation.

    Missing OAuth resolvers are startup failures, not HTTP response cases.
    OAuth expiry and backend availability are injected validator results, not
    real-provider token-signature, discovery, refresh or network-outage tests.
    Cancellation is checked directly, not through HTTP client disconnects. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
