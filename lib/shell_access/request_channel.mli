open Core

(** Bounded, private line-framed request channel. The transport carries no bearer
    credential; the host lends its handler only for the owning process lifetime.
    Requests/responses are single lines (typically compact JSON). *)
type limits =
  { max_request_bytes : int
  ; max_response_bytes : int
  ; max_requests : int
  }

val default_limits : limits

type error =
  | Denied
  | Request_too_large
  | Response_too_large
  | Too_many_requests
  | Invalid_response_frame
  | Handler_failed
[@@deriving equal, sexp]

val error_to_string : error -> string

type t

(** Trusted host construction. [check] verifies the live owner before handling and
    before disclosure, including after a yielding handler. Application failures
    should be encoded in normal response frames. Raised non-cancellation exceptions
    close the channel without exposing exception text or request data. *)
val create
  :  limits:limits
  -> check:(unit -> bool)
  -> handle:(string -> string)
  -> (t, string) result

val request_fd : int
val response_fd : int

(** Serve until EOF, cancellation or a channel error. Caller owns both flows and
    must cancel/join this fiber when the process exits. Does not log payloads. *)
val serve
  :  t
  -> source:_ Eio.Flow.source
  -> sink:_ Eio.Flow.sink
  -> check:(unit -> bool)
  -> on_activity:(unit -> unit)
  -> (unit, error) result

module Client : sig
  (** Blocking client for a dedicated helper process. Uses only inherited pipe
      descriptors 3/4, with no socket path, environment token or credential fallback.
      Exactly one bounded response frame is read. Host process supervision owns
      timeout/cancellation. Does not close or reopen another owner's descriptors. *)
  val exchange : limits:limits -> string -> (string, string) result
end
