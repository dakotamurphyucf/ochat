open! Core

(** OAuth acquisition and credential-isolated, best-effort disk caching.
    Operational errors return [Error]; Eio cancellation propagates. *)
type creds =
  | Client_secret of
      { id : string
      ; secret : string
      ; scope : string option
      }
  | Pkce of { client_id : string }

val cache_dir : unit -> string

(** [cache_file issuer creds] hashes the exact issuer and credential tuple,
    including grant type, secret and requested scope, into a versioned filename.
    Raw credentials are not written to filenames or token JSON. Old issuer-only
    cache files are never imported: their credential identity is unknown.
    PKCE has no account selector; use separate cache roots for separate users
    of the same public client. Scope order/whitespace is not normalized. *)
val cache_file : string -> creds -> string

(** [load ~env issuer creds] reads only this identity's token. Files with
    group/other permissions are rejected. Missing/invalid files return errors.
    The cache directory must be trusted; this is not an adversarial-filesystem
    sandbox, credential vault, or token revocation check. *)
val load
  :  env:Eio_unix.Stdenv.base
  -> string
  -> creds
  -> (Oauth2_types.Token.t, string) result

(** [store ~env issuer creds token] publishes through an exclusive 0600 temporary
    file and rename. Concurrent writers use different temporary files; last
    successful rename wins. Failures are best-effort and cancellation propagates.
    No fsync durability or single-flight acquisition is promised. *)
val store : env:Eio_unix.Stdenv.base -> string -> creds -> Oauth2_types.Token.t -> unit

val fallback_metadata : issuer:string -> Oauth2_types.Metadata.t

val fetch_metadata
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> issuer:string
  -> (Oauth2_types.Metadata.t, string) result

val refresh_access_token
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> issuer:string
  -> creds
  -> Oauth2_types.Token.t
  -> (Oauth2_types.Token.t, string) result

val obtain
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> creds
  -> (Oauth2_types.Token.t, string) result

(** [get ~env ~sw ~issuer creds] reuses a fresh identity-matching cache entry,
    refreshes an expiring token, or reacquires after a cache/refresh failure.
    The cached-token margin is 60 seconds; a newly acquired token is returned
    with its issuer-provided lifetime. Client-secret grants use [/token]; PKCE
    discovers its endpoint. No forced refresh on HTTP 401 is provided here. *)
val get
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> issuer:string
  -> creds
  -> (Oauth2_types.Token.t, string) result
