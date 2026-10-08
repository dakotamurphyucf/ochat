open! Core

(** Pure bounded direct-Codex protocol. Decoding is NOT identity verification;
    the host must obtain tokens from its fixed authenticated token endpoint. *)
module Error : sig
  type t =
    | Invalid_input
    | Invalid_callback
    | State_mismatch
    | Authorization_denied
    | Invalid_json
    | Response_too_large
    | Invalid_pkce
    | Invalid_grant
  [@@deriving equal, sexp_of]
end

module Presence : sig
  type 'a t =
    | Absent
    | Null
    | Value of 'a
end

module Pkce : sig
  type t

  val of_verifier : string -> (t, Error.t) result
  val verifier : t -> string
  val challenge : t -> string
  val of_pair : verifier:string -> challenge:string -> (t, Error.t) result
end

module Callback : sig
  type t =
    | Code of string
    | Denied
    (** Exact origin-form callback path, bounded strict percent-decoding, no
      duplicates or code/error conflict. State is checked before accepting data. *)

  val parse : expected_state:string -> target:string -> (t, Error.t) result
end

module Device : sig
  type challenge

  val decode_challenge : string -> (challenge, Error.t) result
  val id : challenge -> string
  val user_code : challenge -> string
  val interval_seconds : challenge -> int

  type grant

  val decode_grant : string -> (grant, Error.t) result
  val authorization_code : grant -> string
  val pkce : grant -> Pkce.t
end

module Token : sig
  type t

  val decode : string -> (t, Error.t) result
  val access : t -> string
  val refresh : t -> string Presence.t
  val id_token : t -> string Presence.t
  val scopes : t -> string list Presence.t
  val expires_in : t -> int64 Presence.t
end

val form : (string * string) list -> string
