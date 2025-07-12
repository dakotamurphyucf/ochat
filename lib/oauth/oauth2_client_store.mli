open! Core

(** Persistent storage of OAuth client credentials that have been obtained
    via Dynamic Client Registration (RFC 7591).  The file is stored under
    the XDG config directory so that multiple invocations of the CLI can
    reuse the same client identifier when talking to the same issuer.  *)

module Credential : sig
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"] [@jsonaf.option]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

(** [lookup ~issuer] returns the stored credentials for [issuer] if any. *)
val lookup : env:Eio_unix.Stdenv.base -> issuer:string -> Credential.t option

(** [store ~issuer cred] persistently stores [cred] as the credentials to use
    for [issuer], overwriting any previously saved value. *)
val store : env:Eio_unix.Stdenv.base -> issuer:string -> Credential.t -> unit
