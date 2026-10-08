open! Core

(** Host-owned direct-Codex OAuth foundation. No credential cache, automatic
    login, storage authority, alternate route, or billing fallback. *)
module Error : sig
  type stage =
    | Configure
    | Listen
    | Callback
    | Device_challenge
    | Device_poll
    | Exchange
    | Identity
  [@@deriving sexp_of]

  type code =
    | Invalid_configuration
    | Unsupported_route
    | Closed
    | Timed_out
    | Invalid_callback
    | State_mismatch
    | Authorization_denied
    | Protocol_error
    | Response_too_large
    | Transport_unavailable
    | Identity_mismatch
    | Identity_unverifiable
    | Submission_uncertain
  [@@deriving equal, sexp_of]

  type identity_failure =
    | Invalid_token
    | Issuer
    | Audience
    | Account
    | Subject
    | Nonce
    | Expiry
    | Scopes
  [@@deriving equal, sexp_of]

  type t

  (** Optional closed failed identity check; absent for non-identity failures.
      Contains no claim/token values and grants no authentication authority. *)
  val identity_failure : t -> identity_failure option

  val stage : t -> stage
  val code : t -> code
  val sexp_of_t : t -> Sexp.t
end

module Policy : sig
  type t

  (** Trusted host registration policy, not a per-request client override.
      Fixed issuer/client/direct Codex route; access-token audience opaque
      until an authoritative provider policy defines it.
      Callback ports are only pinned client allowlist1455/1457, no ephemeral
      fallback; direct route eligibility remains live qualification.
      access_scope_claim enables only an explicitly qualified authenticated JWT
      scope claim; default false. Never infer device scopes from browser request. *)
  val direct_codex
    :  ?access_scope_claim:bool
    -> expected_account:string option
    -> callback_port:int
    -> unit
    -> (t, Error.t) result

  val issuer : t -> string
  val client_registration : t -> string
  val resource : t -> string
end

module Transport : sig
  type t

  val create : net:_ Eio.Net.t -> clock:_ Eio.Time.Mono.t -> (t, Error.t) result

  (** Joins serialized active exchange and rejects future exchanges. Per-call
      switches own sockets; strictCA/hostname validation and no redirects. *)
  val close : t -> unit
end

module Challenge : sig
  type t

  (** Authorized operator interaction only. No serializers: state-bearing URL
      and device one-time code must not enter logs/status/history/export. *)
  val with_browser_uri : t -> f:(Uri.t -> 'a) -> ('a, Error.t) result

  val with_device_prompt
    :  t
    -> f:(verification_uri:Uri.t -> user_code:string -> 'a)
    -> ('a, Error.t) result
end

module Verified : sig
  type scope_source =
    | Token_response
    | Browser_request
    | Prior_exact
    | Qualified_access_token_claim
  [@@deriving equal, sexp_of]

  type t

  val account : t -> string
  val subject : t -> string
  val scopes : t -> string list
  val scope_presence : t -> string list Provider_oauth_protocol.Presence.t
  val scope_source : t -> scope_source

  (** Actual latest token-response presence, preserved in protected continuity.
      Separate from authenticated JWT absolute expiry; never synthesized. *)
  val response_expires_in : t -> int64 Provider_oauth_protocol.Presence.t

  (** Original bounded nonce/audience/auth_time/azp proof. Only protected163
      material may persist it; never public registry metadata/status/history.
      Every refresh preserves this original proof completely and updates only
      separate latest token-response expiry evidence. *)
  val with_continuity : t -> f:(Provider_secret_store.Secret.t -> 'a) -> 'a

  val expires_at : t -> float

  (** Host-only163 adapter borrows material; no public secret serialization.
      Refresh absence/null/value is retained for declared grant handling. *)
  val with_material
    :  t
    -> f:
         (access:Provider_secret_store.Secret.t
          -> refresh:Provider_secret_store.Secret.t Provider_oauth_protocol.Presence.t
          -> 'a)
    -> 'a
end

module Login : sig
  type t

  type phase =
    | Starting
    | Awaiting_callback
    | Polling
    | Exchanging
    | Validating
    | Ready
    | Failed
    | Cancelled
  [@@deriving equal, sexp_of]

  (** Caller switch owns one worker; inner switch owns listener/accepted sockets.
      close cancels+joins worker. Whole monotonic bound includes listener setup,
      pending requests, exchange and validation. Positive maximum<=900seconds.
      Browser listener uses SO_REUSEADDR for immediate restart after a completed
      callback, with SO_REUSEPORT disabled: an active listener at the same pinned
      endpoint rejects, without alternative-port fallback. Owned listener closes
      before worker completion is published.
      Browser listener binds literal127.0.0.1 and selected allowed port BEFORE
      exposing URI; browser opener belongs operator frontend. Randomness borrowed.
      ID-token nonce must match requested browser nonce. *)
  val start_browser
    :  transport:Transport.t
    -> policy:Policy.t
    -> sw:Eio.Switch.t
    -> net:_ Eio.Net.t
    -> secure_random:_ Eio.Flow.source
    -> clock:_ Eio.Time.Mono.t
    -> wall_clock:_ Eio.Time.clock
    -> maximum_wait:Time_ns.Span.t
    -> (t * Challenge.t, Error.t) result

  val start_device
    :  transport:Transport.t
    -> policy:Policy.t
    -> sw:Eio.Switch.t
    -> clock:_ Eio.Time.Mono.t
    -> wall_clock:_ Eio.Time.clock
    -> maximum_wait:Time_ns.Span.t
    -> (t * Challenge.t, Error.t) result

  val phase : t -> phase

  (** Nonblocking, non-consuming read of a completed typed failure. [None] means
      pending, success, or an unexpected exception; it is not success evidence.
      Never returns verified material or changes [await]/[close] ownership.
      Unexpected exceptions remain for existing [await]/[close] propagation. *)
  val error : t -> Error.t option

  val await : t -> (Verified.t, Error.t) result

  (** Cancels and joins the owned worker on every call. An unexpected worker
      exception is re-raised by the first completed close only; later closes
      remain safe for switch finalizers. [await] independently preserves it. *)
  val close : t -> unit
end

module Existing : sig
  (** Trusted163 adapter only; inputs must come from the exact committed registry
      identity/grant/material under refresh ownership. No JWT/cache import.
      Validates fixed issuer/client/resource and cross-checks protected proof's
      account/subject against that metadata; missing/corrupt proof refuses renewal.
      Expired access is allowed here because existing authority may need renewal. *)
  val of_registry
    :  issuer:string
    -> client_registration:string
    -> resource:string
    -> account:string
    -> subject:string
    -> scopes:string list
    -> expires_at:float
    -> access:Provider_secret_store.Secret.t
    -> refresh:Provider_secret_store.Secret.t Provider_oauth_protocol.Presence.t
    -> continuity:Provider_secret_store.Secret.t
    -> (Verified.t, Error.t) result
end

module Refresh : sig
  type policy =
    | Preserve_omitted
    | Require_rotated
  [@@deriving equal, sexp_of]

  type outcome =
    | Verified of Verified.t
    | Definitely_not_submitted
    | Authoritative_rejection
    | Possibly_consumed

  (** Trusted163 exchange port only: invoke after durable Possibly_sent intent.
      No storage/login/fallback. Existing exact identity is retained only for
      omitted ID token; supplied ID token is revalidated, null rejected. Refresh
      omission stays Absent for163 to merge according to declared grant policy.
      Cancellation propagates;163 already owns uncertainty. No automatic retry. *)
  val exchange
    :  transport:Transport.t
    -> policy:Policy.t
    -> refresh_policy:policy
    -> wall_clock:_ Eio.Time.clock
    -> Verified.t
    -> outcome
end

module Direct_headers : sig
  (** Host-only private-header builder for the fixed direct Responses endpoint.
      Inference driver integration must compose62/163 currentness guards and
      account-aware transport identity; this builder alone grants no lease. *)
  val with_headers
    :  Verified.t
    -> endpoint:string
    -> f:((string * string) list -> 'a)
    -> ('a, Error.t) result
end

module For_testing : sig
  type endpoint =
    | User_code
    | Device_poll
    | Token

  type transport_error =
    | Closed
    | Connection
    | Tls
    | Invalid_http
    | Body_limit
    | Timeout (** Explicit synthetic transport; never live provider qualification. *)

  val scripted_transport
    :  clock:_ Eio.Time.Mono.t
    -> (endpoint
        -> body:string
        -> on_possible_submission:(unit -> unit)
        -> (int * string, transport_error) result)
    -> Transport.t

  val parse_response : string -> (int * int, transport_error) result
end
