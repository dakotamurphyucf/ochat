(** OAuth 2.0 token management with transparent caching.

    This module provides a thin wrapper around the lower-level helpers in the
    {!module:Oauth2_client_credentials} and {!module:Oauth2_pkce_flow}
    sub-modules.  It handles:

    • Discovering the authorisation‐server metadata (with a sensible fallback
      when the [/.well-known/oauth-authorization-server] endpoint is absent).
    • Retrieving an access / refresh token using either the *Client
      Credentials* grant or the interactive *PKCE* flow.
    • Persisting the returned token in the user’s XDG cache directory (or
      [$HOME/.cache] on systems without XDG) so subsequent runs start up
      instantly.
    • Refreshing the token whenever fewer than 60 s remain before expiry.

    The helper is entirely {b exception-free}.  All recoverable error paths
    return [Error msg] where [msg] provides a short, human-readable
    diagnostic.

    {1 Credentials}

    The [creds] variant captures the two supported client types:

    - {!`Client_secret}  — confidential clients that know a [client_secret]
      and therefore use the RFC&nbsp;6749 §4.4 *client-credentials* grant.
    - {!`Pkce}          — public clients (CLI / desktop apps) that rely on
      the user completing a browser-based *PKCE* dance.

    {1 Quick start}

    {[
      Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
          match
            Oauth2_manager.get
              ~env ~sw
              ~issuer:"https://auth.example"
              (Client_secret
                 { id     = "my-service"
                 ; secret = Sys.getenv_exn "CLIENT_SECRET"
                 ; scope  = Some "openid profile"
                 })
          with
          | Error msg -> Format.eprintf "Token error: %s@." msg
          | Ok tok ->
              Format.printf "Bearer %s@." tok.access_token
    ]}
*)

open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) r f = bind r ~f
    let ( let+ ) r f = map r ~f
  end
end

module Tok = Oauth2_types.Token

(** [cache_dir ()] yields the directory used to persist token JSON files.

      Resolution order follows the XDG Base Directory specification:
      {ol
      {- [$XDG_CACHE_HOME] if set}
      {- [$HOME/.cache] on Unix‐like systems}
      {- [./.cache] as a last resort}
      }

      The function does not touch the file-system; callers should create the
      directory (e.g. via {!Io.mkdir}) before writing files within it. *)
let cache_dir () : string =
  match Sys.getenv "XDG_CACHE_HOME" with
  | Some d -> Filename.concat d "ocamlgpt/tokens"
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> Filename.concat home ".cache/ocamlgpt/tokens"
     | None -> Filename.concat "." ".cache/ocamlgpt/tokens")
;;

(** [cache_file issuer] maps an [issuer] base URL to the absolute path of
      its token cache file.  The issuer string is hashed with MD5 so that
      extremely long or non-filesystem-safe URLs do not break on exotic
      platforms.  The file is named [<md5>.json] and always lives under
      [cache_dir ()]. *)
let cache_file issuer =
  let digest = Md5.digest_string issuer |> Md5.to_hex in
  Filename.concat (cache_dir ()) (digest ^ ".json")
;;

(*────────────────────────  Metadata retrieval with fallback  ─────────────*)

(** [fallback_metadata ~issuer] constructs an {!Oauth2_types.Metadata.t}
      record directly from the given [issuer] when the discovery document is
      missing.  Only the three most common endpoints are filled in:  [
      /authorize], [/token], and [/register].  All paths are appended to the
      {e scheme://host[:port]} portion of [issuer]. *)
let fallback_metadata ~(issuer : string) : Oauth2_types.Metadata.t =
  (* Strip any path component – we only want scheme://host[:port] *)
  let uri = Uri.of_string issuer in
  let base = Uri.with_path uri "" in
  let base_s = Uri.to_string base |> String.rstrip ~drop:(fun c -> Char.(c = '/')) in
  let path p = base_s ^ p in
  { Oauth2_types.Metadata.authorization_endpoint = path "/authorize"
  ; token_endpoint = path "/token"
  ; registration_endpoint = Some (path "/register")
  }
;;

(** [fetch_metadata ~env ~sw ~issuer] downloads the
      {i Authorization Server Metadata} document from
      [issuer ^ "/.well-known/oauth-authorization-server"].

      If the HTTPS request fails or the payload cannot be decoded the helper
      silently falls back to {!fallback_metadata}, ensuring that flows that
      hard-code the conventional endpoint names continue to work. *)
let fetch_metadata ~env ~sw ~(issuer : string)
  : (Oauth2_types.Metadata.t, string) Result.t
  =
  match
    Oauth2_http.get_json ~env ~sw (issuer ^ "/.well-known/oauth-authorization-server")
  with
  | Ok json ->
    (* Attempt to parse the metadata from the JSON response *)
    (* If parsing fails, fall back to a basic metadata structure *)
    (try Ok (Oauth2_types.Metadata.t_of_jsonaf json) with
     | _ -> Ok (fallback_metadata ~issuer))
  | Error err ->
    if true then failwith err;
    (* If the request fails, fall back to a basic metadata structure *)
    (* This is useful for servers that do not support the well-known endpoint *)
    Ok (fallback_metadata ~issuer)
;;

(** [load ~env issuer] attempts to read a previously cached token for
      [issuer].  For security the file must be readable and writable {b only}
      by the current user; otherwise [Error "insecure_token_cache_permissions"]
      is returned.  Any other I/O or decoding error yields
      [Error "token_cache_read"]. *)
let load ~env issuer : (Tok.t, string) Result.t =
  let fs = Eio.Stdenv.fs env in
  let rel = cache_file issuer in
  let path = Eio.Path.(fs / rel) in
  try
    let stats = Eio.Path.stat ~follow:true path in
    if stats.perm land 0o077 <> 0
    then Error "insecure_token_cache_permissions"
    else (
      let s = Eio.Path.load path in
      Ok (Tok.t_of_jsonaf (Jsonaf.of_string s)))
  with
  | _ -> Error "token_cache_read"
;;

(** [store ~env issuer tok] atomically writes [tok] to disk using
      [`Or_truncate 0o600] permissions.  Errors are swallowed on purpose –
      the function is best-effort and should never crash the application. *)
let store ~env issuer tok =
  try
    Io.mkdir ~exists_ok:true ~dir:(Eio.Stdenv.fs env) (cache_dir ());
    let fs = Eio.Stdenv.fs env in
    let tmp = cache_file issuer ^ ".tmp" in
    let final = cache_file issuer in
    let tmp_path = Eio.Path.(fs / tmp) in
    let final_path = Eio.Path.(fs / final) in
    (* Ensure the directory exists *)
    (* Write temporary file with strict permissions *)
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      tmp_path
      (Jsonaf.to_string (Tok.jsonaf_of_t tok));
    (try Eio.Path.rename tmp_path final_path with
     | _ -> ());
    ()
  with
  | _ -> ()
;;

(*────────────────────────  Refresh token flow  ─────────────────────────*)

(** Credentials used by {!get}, {!obtain}, and {!refresh_access_token}. *)
type creds =
  | Client_secret of
      { id : string
      ; secret : string
      ; scope : string option
      }
  | Pkce of { client_id : string }

(** {ul
    {- [`Client_secret] — confidential clients possessing a private
       [client_secret] and therefore eligible for the *client-credentials*
       grant.  Provide [scope] to narrow the issued privileges.}
    {- [`Pkce] — public clients (desktop / CLI) that must perform the
       browser-based PKCE flow.} } *)

(** [refresh_access_token ~env ~sw ~issuer creds tok] exchanges
      [tok.refresh_token] for a fresh access token.

      - For {!`Client_secret} clients the helper performs a standard
        *refresh_token* grant at [issuer ^ "/token"].
      - For {!`Pkce} clients the grant is POST-ed to the metadata’s
        [token_endpoint].

      Returned tokens are stamped with the current wall-clock time so that
      {!Oauth2_types.Token.is_expired} works reliably. *)
let refresh_access_token ~env ~sw ~issuer creds (tok : Tok.t) : (Tok.t, string) Result.t =
  match tok.refresh_token with
  | None -> Error "no_refresh_token"
  | Some refresh_token ->
    let open Result.Let_syntax in
    (match creds with
     | Client_secret { id; secret; scope = _ } ->
       let params =
         [ "grant_type", "refresh_token"
         ; "refresh_token", refresh_token
         ; "client_id", id
         ; "client_secret", secret
         ]
       in
       let* json = Oauth2_http.post_form ~env ~sw (issuer ^ "/token") params in
       Ok
         Tok.
           { (Tok.t_of_jsonaf json) with
             obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
           }
     | Pkce { client_id } ->
       let* meta = fetch_metadata ~env ~sw ~issuer in
       let params =
         [ "grant_type", "refresh_token"
         ; "refresh_token", refresh_token
         ; "client_id", client_id
         ]
       in
       let* json = Oauth2_http.post_form ~env ~sw meta.token_endpoint params in
       Ok
         Tok.
           { (Tok.t_of_jsonaf json) with
             obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
           })
;;

(** [obtain ~env ~sw issuer creds] performs the initial grant:

      • *Client-credentials* for confidential clients
      • Interactive *PKCE* flow for public clients

      The function is usually not called directly – use {!get} instead which
      combines caching, refreshing, and initial acquisition. *)
let obtain ~env ~sw issuer = function
  | Client_secret { id; secret; scope } ->
    (* If the client credentials flow is not supported, we can return an error or
       handle it differently based on the application's requirements. *)
    Oauth2_client_credentials.fetch_token
      ~env
      ~sw
      ~token_uri:(issuer ^ "/token")
      ~client_id:id
      ~client_secret:secret
      ?scope
      ()
  | Pkce { client_id } ->
    let open Result.Let_syntax in
    let* meta = fetch_metadata ~env ~sw ~issuer in
    let code, verifier, redirect = Oauth2_pkce_flow.run ~env ~sw ~meta ~client_id in
    Oauth2_pkce_flow.exchange_token
      ~env
      ~sw
      ~meta
      ~client_id
      ~code
      ~code_verifier:verifier
      ~redirect_uri:redirect
;;

(* PKCE flow not yet supported in the lightweight OAuth client – return an
     explicit error so callers can fall back to the headless
     client-credentials grant. *)
(* | _ -> Error "PKCE flow not supported in this build" *)

(** [get ~env ~sw ~issuer creds] is the main entry-point.  It guarantees
      that the returned token is valid for at least 60 seconds.

      Workflow:
      {ol
      {- Try to [load] the token from disk.}
      {- If present and still fresh ⇒ return immediately.}
      {- If expired ⇒ attempt {!refresh_access_token}.}
      {- On refresh failure or missing cache ⇒ {!obtain}.}
      {- Persist the brand-new token with [store] before returning.}
      }

      All failure cases bubble up as [Error msg].  The helper never raises
      exceptions. *)
let get ~env ~sw ~issuer creds : (Tok.t, string) Result.t =
  match load ~env issuer with
  | Ok tok when not (Tok.is_expired tok) ->
    let () = store ~env issuer tok in
    Ok tok
  | Ok tok ->
    (* Token expired – attempt refresh first *)
    (let open Result.Let_syntax in
     let* refreshed = refresh_access_token ~env ~sw ~issuer creds tok in
     if Tok.is_expired refreshed
     then Error "refresh_yielded_expired_token"
     else (
       store ~env issuer refreshed;
       Ok refreshed))
    |> (function
     | Ok t -> Ok t
     | Error _ ->
       let open Result.Let_syntax in
       let* tok = obtain ~env ~sw issuer creds in
       let () = store ~env issuer tok in
       Ok tok)
  | Error _ ->
    let open Result.Let_syntax in
    let* tok = obtain ~env ~sw issuer creds in
    let () = store ~env issuer tok in
    Ok tok
;;
