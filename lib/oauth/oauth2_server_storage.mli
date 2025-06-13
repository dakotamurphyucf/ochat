open! Core

module Token = Oauth2_server_types.Token

val insert : Token.t -> unit

val find_valid : string -> bool
(** [find_valid access_token] returns [true] if the token exists and has not
    expired. Expired tokens are evicted eagerly. *)

val pp_table : Format.formatter -> unit -> unit
(** Debug helper *)

