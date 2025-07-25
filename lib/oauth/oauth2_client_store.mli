open! Core

(** Cache OAuth 2.0 dynamic-registration credentials on disk.

    A successful Dynamic Client Registration (RFC 7591) returns a
    [client_id] – and sometimes a [client_secret] – that should be
    re-used on subsequent exchanges with the same issuer.  Creating a new
    client for every invocation is wasteful and often rate-limited.  This
    module persistently stores the credentials so that independent CLI
    runs share a single identifier.

    {1 Storage layout}

    • Location   : [$XDG_CONFIG_HOME/ocamlochat/registered.json]
      (or [$HOME/.config/ocamlochat/registered.json] as a fallback).

    • Structure  : a JSON object mapping issuer URLs → credential – e.g.

    {[
      "https://auth.example" : {
        "client_id"     : "abc",
        "client_secret" : "s3cr3t"
      }
    ]}  *)

module Credential : sig
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"] [@jsonaf.option]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

(** [lookup ~env ~issuer] returns the cached credentials for [issuer] or
    [None] when the issuer is unknown.

    The function never raises – malformed, unreadable, or missing files
    are treated as an empty cache. *)
val lookup
  :  env:Eio_unix.Stdenv.base
       (** Capability bundle obtained from
                                   [Eio_main.run]. *)
  -> issuer:string (** Issuer’s discovery URL (case-sensitive). *)
  -> Credential.t option

(** [store ~env ~issuer cred] writes [cred] as the credentials for
    [issuer], replacing any previous value.

    The operation is atomic: it writes to a [*.tmp] file first and then
    {!Eio.Path.rename}s the result.  The final file mode is [0o600]. *)
val store : env:Eio_unix.Stdenv.base -> issuer:string -> Credential.t -> unit
