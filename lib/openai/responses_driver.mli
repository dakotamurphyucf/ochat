open! Core

(** Explicit, stateless Responses preparation and HTTP/SSE dispatch. No ambient
    model, endpoint, settings or credentials; no host history/tool execution. *)
module Capability : sig
  type feature =
    | Text_input
    | Image_input
    | Document_input
    | Function_tools
    | Custom_tools
    | Opaque_replay
    | Setting of string
  [@@deriving equal, compare, sexp_of]

  type support =
    | Supported
    | Unsupported
    | Unknown
  [@@deriving equal, sexp_of]

  type t

  (** Declared baseline applies to arbitrary model strings. Model declarations
      refine it; explicit Unsupported always wins. Optional features default to
      Unknown. Duplicate declarations and unknown setting names reject. This is
      a declaration seam, not a maintained model catalog or live probe. *)
  val create
    :  baseline:(feature * support) list
    -> models:(string * (feature * support) list) list
    -> t Or_error.t

  val resolve : t -> model:string -> feature:feature -> support
end

module Setting : sig
  type provenance =
    | Execution_override
    | Captured_prompt
    | Profile_default
  [@@deriving equal, sexp_of]

  type t

  (** Selected codec request options only. Null remains explicit, absence means
      no override. Codec validation checks each effective value before dispatch.
      Unknown names and duplicate keys within a precedence layer reject. *)
  val create
    :  name:string
    -> value:Jsonaf.t Responses_codec.Request.Field.t
    -> provenance:provenance
    -> t Or_error.t

  val name : t -> string
  val value : t -> Jsonaf.t Responses_codec.Request.Field.t
  val provenance : t -> provenance
end

module Profile : sig
  type t

  (** Endpoint is an absolute HTTPS Responses URL with a DNS hostname and canonical URI spelling, without userinfo, query or
      fragment. HTTP is allowed only for literal loopback test endpoints.
      Nonempty profile/account labels are nonsecret host identities. No default
      model: the execution supplies it. Profile defaults are immutable. *)
  val create
    :  id:string
    -> account:string option
    -> endpoint:string
    -> capabilities:Capability.t
    -> defaults:Setting.t list
    -> t Or_error.t

  val id : t -> string
  val account : t -> string option
  val endpoint : t -> string

  (** Pure capture of effective precedence before durable selection. No auth or
      I/O. Execution layers may not masquerade as profile defaults. *)
  val effective_settings : t -> Setting.t list -> Setting.t list Or_error.t

  val capability : t -> model:string -> feature:Capability.feature -> Capability.support
end

module Prepared : sig
  type t

  (** Capture after the host has formed its final effective history (including
      final guidance) and resolved assets to inline immutable data. Precedence:
      execution override > captured prompt > profile default > omission.
      Explicit optional features require Supported; unsupported/unknown fail.
      Images use base64 data URIs; files use base64 or base64 data URIs, never URLs
      or provider file IDs. Tools
      are schemas only; names must be unique and async/native hosted tools reject.
      Origin compatibility of opaque captures remains the host's responsibility.
      Final admission follows preparation; changing inputs requires reprepare. *)
  val create
    :  Profile.t
    -> model:string
    -> history:Jsonaf.t list
    -> tools:Responses_codec.Request.Tool.t list
    -> settings:Setting.t list
    -> t Or_error.t

  (** Lower an already captured effective selection without consulting current
      profile defaults. Names must be unique; provenance is retained. Absent
      values remain omitted. This is the durable neutral adapter entry point. *)
  val of_captured_settings
    :  Profile.t
    -> model:string
    -> history:Jsonaf.t list
    -> tools:Responses_codec.Request.Tool.t list
    -> settings:Setting.t list
    -> t Or_error.t

  val profile : t -> Profile.t
  val model : t -> string
  val request : t -> Responses_codec.Request.t
  val settings : t -> Setting.t list
  val fingerprint : t -> string
end

module Auth : sig
  type identity =
    { owner : string
    ; generation : int64
    }
  [@@deriving equal, sexp_of]

  type lease

  type error =
    | Missing
    | Denied
    | Profile_changed
    | Reauthorization_required
    | Invalid_credential
    | Timed_out
  [@@deriving equal, sexp_of]

  (** Host-only currentness guard; rechecked after connection acquisition before
      writing credentials. Wrapping composes the existing source guard first, then
    the supplied guard; neither can remove the other's revocation. An existing
    owner/generation must exactly match or Invalid_credential is returned. Guards
    are non-yielding host policy snapshots. The owner/generation identify auth lifecycle, contain
      no token, and permit WS channel invalidation. No serializer is provided. *)
  val with_identity
    :  lease
    -> owner:string
    -> generation:int64
    -> check_current:(unit -> (unit, error) Result.t)
    -> (lease, error) Result.t

  val identity : lease -> identity option

  (** Secret host lease, deliberately without serialization or secret accessor.
      Validates header-safe nonempty bytes. Resolver is called at dispatch with
      exactly the captured identity; it may silently renew but never start login. *)
  val bearer : string -> (lease, error) Result.t

  type resolver = sw:Eio.Switch.t -> Profile.t -> (lease, error) Result.t
end

module Terminal : sig
  type delivery =
    | Definitely_not_submitted
    | Possibly_submitted
    | Response_started
  [@@deriving equal, sexp_of]

  type failure =
    | Http_status of int
    | Invalid_http
    | Invalid_content_type
    | Body_limit
    | Framing_limit
    | Protocol
    | Connection
    | Timeout
  [@@deriving equal, sexp_of]

  type t =
    | Provider of Responses_codec.Wire.Tracker.completion
    | Failed of
        { delivery : delivery
        ; reason : failure
        }
    (** Provider captures are private. Failed diagnostics contain no body, endpoint,
      credential or exception text. Previous callback events remain evidence. *)
end

module Event : sig
  type t =
    | Update of Responses_codec.Stream.update
    | Finalized of (int * Responses_codec.Wire.Item.t) list
    | Terminal of Terminal.t
end

type t

(** Secure HTTP/1.1 client using system CA certificate and hostname verification.
    Per-attempt nested Eio switch owns the socket under the caller fiber's
    cancellation context. Limits bound request, headers,
    aggregate entity bytes, cumulative transfer-framing bytes and individual SSE
    frames. HTTP status/header/chunk/trailer lines require CRLF; fields and chunk
    extensions are validated. [max_framing_bytes] defaults to 1 MiB and includes
    chunk sizes/extensions, separators and trailers. Deadline bounds I/O including
    DNS/TLS/auth. Host must initialize Mirage_crypto_rng before HTTPS use (as the
    repository binaries already do), once in the embedding host. No redirects, retries, transport fallback, logging or lazy stream.
    [create] performs CA setup, without fetching credentials or network access. *)
val create
  :  net:_ Eio.Net.t
  -> clock:_ Eio.Time.clock
  -> ?max_request_bytes:int
  -> ?max_header_bytes:int
  -> ?max_body_bytes:int
  -> ?max_framing_bytes:int
  -> ?max_frame_bytes:int
  -> ?timeout_seconds:float
  -> unit
  -> t Or_error.t

(** Tighten aggregate response and SSE frame byte bounds without changing network,
    TLS, authentication, deadline or request limits. Positive values only; a larger
    supplied value cannot enlarge either existing bound. Pure immutable copy. *)
val with_response_limit : t -> max_body_bytes:int -> t Or_error.t

(** Auth Error emits no events. Every normal Ok return delivers exactly one
    matching Terminal. Nonterminal validated codec updates arrive incrementally;
    provider terminals occur only in Terminal. Callback exceptions/cancellation
    propagate, including during terminal delivery. Failures preserve publication
    evidence and never retry, even when submission is uncertain. *)
val run
  :  t
  -> auth:Auth.resolver
  -> prepared:Prepared.t
  -> on_event:(Event.t -> unit)
  -> (Terminal.t, Auth.error) Result.t
