open! Core

module Error : sig
  type t =
    | Invalid_token
    | Issuer
    | Audience
    | Account
    | Subject
    | Nonce
    | Expiry
    | Scopes
  [@@deriving equal, sexp_of]
end

type t

(** Private boundary: only tokens returned directly by fixed authenticated TLS
    transport qualify as input. This is not a JWT signature-verification API.
    Access audience remains opaque: no provider audience declaration is assumed.
    Resource is bound by fixed registration/endpoint provenance. Scope omission is accepted
    only with the exact explicitly requested scope list, qualified access scope
    claim, or existing refresh grant. Supplied refresh ID tokens are revalidated;
    omitted ID token can retain only the exact prior authenticated identity. *)
val validate
  :  Provider_oauth_protocol.Token.t
  -> now:float
  -> client:string
  -> expected_account:string option
  -> expected_subject:string option
  -> nonce:string option
  -> requested_scopes:string list option
  -> access_scope_claim:bool
  -> prior:t option
  -> (t, Error.t) result

val account : t -> string
val subject : t -> string
val scopes : t -> string list
val expires_at : t -> float
val response_expires_in : t -> int64 Provider_oauth_protocol.Presence.t
val continuity : t -> string

val restore_continuity
  :  string
  -> account:string
  -> subject:string
  -> scopes:string list
  -> expires_at:float
  -> (t, Error.t) result
