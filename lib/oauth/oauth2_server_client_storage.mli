open! Core

module Client = Oauth2_server_types.Client

(** Register a new client.
    If [client_secret] is [None] we treat the registration as a *public*
    client (PKCE only).  A fresh [client_id] is always generated; if
    [client_secret = None] we leave it absent in the returned record. *)
val register : ?client_name:string -> ?redirect_uris:string list -> ?confidential:bool -> unit -> Client.t

val find : string -> Client.t option

val validate_secret : client_id:string -> client_secret:string option -> bool
(** [validate_secret ~client_id ~client_secret] returns [true] when a client
    exists and either:
    - the stored client has no secret (public client) and [client_secret] is
      [None]; or
    - the stored [client_secret] matches the given value. *)

(** Register a confidential client using the supplied [client_id] and
    [client_secret].  Primarily intended for deterministic credentials in
    development and test environments.  Any pre-existing entry with the same
    identifier is overwritten. *)
val insert_fixed :
  client_id:string ->
  client_secret:string ->
  ?client_name:string ->
  ?redirect_uris:string list ->
  unit -> Client.t

val pp_table : Format.formatter -> unit -> unit
