open! Core

(** Host-configured remote document/image acquisition for one selected child.
    No default is installed. Authored URLs are requests, not grants. The
    selected child projector supplies its currentness check and remaining
    aggregate byte allowance for each read; this module owns URL/transport
    admission and its own finite response bound. It is private to the trusted
    agent_server composition. *)
type t

module Policy : sig
  type t

  (** Admit exact scheme/host/port origins with finite response and wall-time
      limits. HTTPS requires the system CA authenticator. Plain HTTP is allowed
      only for explicitly listed literal loopback addresses. No wildcard
      suffixes, userinfo, fragments or non-HTTP schemes. An empty origin list
      denies remote content before network I/O. *)
  val create
    :  allowed_origins:string list
    -> max_response_bytes:int
    -> max_wall_seconds:float
    -> t Or_error.t
end

(** Borrow the daemon's Eio network/clock capabilities. Construction obtains
    the same CA/TLS setup used by the bounded provider transport; failure is
    typed and does not silently select [Io.Net]'s null authenticator. *)
val create
  :  policy:Policy.t
  -> net:_ Eio.Net.t
  -> clock:_ Eio.Time.Mono.t
  -> (t, Agent_protocol.Error.t) result

(** Parse and compare the full URL with its configured origin before network
    I/O. Use that SAME URI for the request, never a separately re-parsed host
    and path. Reject redirects, non-success statuses and compressed responses;
    read raw body under both [max_bytes] and policy max before retaining it.
    [max_bytes] is the caller's remaining aggregate content allowance, not a
    fresh per-occurrence budget. Check current owner before and after every
    yielding phase. Transport/timeout/body-limit failures are typed;
    cancellation and unexpected checkpoint exceptions retain identity and
    backtrace. A selected source/content owner is required by the trusted
    caller; [check_owner] alone is not a source grant. *)
val read
  :  t
  -> url:string
  -> max_bytes:int
  -> check_owner:(unit -> (unit, Agent_protocol.Error.t) result)
  -> checkpoint:(unit -> unit)
  -> on_acquire:(unit -> unit)
  -> (string, Agent_protocol.Error.t) result
