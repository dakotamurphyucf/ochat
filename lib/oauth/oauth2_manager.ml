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

    Eio cancellation propagates. All recoverable error paths
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
      let authenticate env sw secret =
        Oauth2_manager.get ~env ~sw ~issuer:"https://auth.example"
          (Oauth2_manager.Client_secret
             { id = "my-service"; secret; scope = Some "openid profile" })
    ]}

    Inspect the result without logging tokens or secrets.
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
  | Some d -> Filename.concat d "ocamlochat/tokens"
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> Filename.concat home ".cache/ocamlochat/tokens"
     | None -> Filename.concat "." ".cache/ocamlochat/tokens")
;;

let cache_file issuer creds =
  let identity =
    match creds with
    | Client_secret { id; secret; scope } ->
      [%sexp
        ("client_credentials" : string)
      , (id : string)
      , (secret : string)
      , (scope : string option)]
    | Pkce { client_id } -> [%sexp ("pkce" : string), (client_id : string)]
  in
  let encoded = Sexp.to_string_mach [%sexp (issuer : string), (identity : Sexp.t)] in
  let digest = Digestif.SHA256.(digest_string encoded |> to_hex) in
  Filename.concat (cache_dir ()) ("v2-" ^ digest ^ ".json")
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
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | _ -> Ok (fallback_metadata ~issuer))
  | Error _ -> Ok (fallback_metadata ~issuer)
;;

(** [load ~env issuer creds] attempts to read a previously cached token for
      [issuer].  For security the file must be readable and writable {b only}
      by the current user; otherwise [Error "insecure_token_cache_permissions"]
      is returned.  Any other I/O or decoding error yields
      [Error "token_cache_read"]. *)
let load ~env issuer creds : (Tok.t, string) Result.t =
  let fs = Eio.Stdenv.fs env in
  let rel = cache_file issuer creds in
  let path = Eio.Path.(fs / rel) in
  try
    let stats = Eio.Path.stat ~follow:true path in
    if stats.perm land 0o077 <> 0
    then Error "insecure_token_cache_permissions"
    else (
      let s = Eio.Path.load path in
      Ok (Tok.t_of_jsonaf (Jsonaf.of_string s)))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error "token_cache_read"
;;

let temporary_sequence = Atomic.make 0

let write_token ~env issuer creds tok =
  let fs = Eio.Stdenv.fs env in
  let final = Eio.Path.(fs / cache_file issuer creds) in
  let serial = Atomic.fetch_and_add temporary_sequence 1 in
  let suffix = sprintf ".%d.%d.tmp" (Core_unix.getpid () |> Pid.to_int) serial in
  let temporary = Eio.Path.(fs / (cache_file issuer creds ^ suffix)) in
  let owned = ref false in
  Fun.protect
    (fun () ->
       Eio.Path.with_open_out ~create:(`Exclusive 0o600) temporary (fun flow ->
         owned := true;
         Eio.Flow.copy_string (Jsonaf.to_string (Tok.jsonaf_of_t tok)) flow);
       Eio.Path.rename temporary final)
    ~finally:(fun () ->
      if !owned
      then
        Eio.Cancel.protect (fun () ->
          try Eio.Path.unlink temporary with
          | _ -> ()))
;;

let store ~env issuer creds tok =
  try
    Io.mkdir ~exists_ok:true ~dir:(Eio.Stdenv.fs env) (cache_dir ());
    write_token ~env issuer creds tok
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> ()
;;

(*────────────────────────  Refresh token flow  ─────────────────────────*)

(** [refresh_access_token ~env ~sw ~issuer creds tok] exchanges
      [tok.refresh_token] for a fresh access token.

      - For {!`Client_secret} clients the helper performs a standard
        *refresh_token* grant at [issuer ^ "/token"].
      - For {!`Pkce} clients the grant is POST-ed to the metadata’s
        [token_endpoint].

      Returned tokens are stamped with the current wall-clock time so that
      {!Oauth2_types.Token.is_expired} works reliably. *)
let refresh_endpoint ~env ~sw ~issuer = function
  | Client_secret { id; secret; scope = _ } ->
    Ok (issuer ^ "/token", [ "client_id", id; "client_secret", secret ])
  | Pkce { client_id } ->
    Result.map (fetch_metadata ~env ~sw ~issuer) ~f:(fun meta ->
      meta.token_endpoint, [ "client_id", client_id ])
;;

let refresh_access_token ~env ~sw ~issuer creds (tok : Tok.t) =
  match tok.refresh_token with
  | None -> Error "no_refresh_token"
  | Some refresh_token ->
    let open Result.Let_syntax in
    let* endpoint, credentials = refresh_endpoint ~env ~sw ~issuer creds in
    let params =
      [ "grant_type", "refresh_token"; "refresh_token", refresh_token ] @ credentials
    in
    let* json = Oauth2_http.post_form ~env ~sw endpoint params in
    Tok.of_response_json ~obtained_at:(Eio.Time.now (Eio.Stdenv.clock env)) json
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

(** [get ~env ~sw ~issuer creds] is the main entry-point.  It reuses cached tokens only
      while they pass the 60-second expiry margin. Newly acquired tokens use
      the lifetime supplied by the issuer.

      Workflow:
      {ol
      {- Try to [load] the token from disk.}
      {- If present and still fresh ⇒ return immediately.}
      {- If expired ⇒ attempt {!refresh_access_token}.}
      {- On refresh failure or missing cache ⇒ {!obtain}.}
      {- Persist the brand-new token with [store] before returning.}
      }

      Operational failures return [Error msg]. Eio cancellation propagates. *)
let get_unprotected ~env ~sw ~issuer creds : (Tok.t, string) Result.t =
  match load ~env issuer creds with
  | Ok tok when not (Tok.is_expired tok) -> Ok tok
  | Ok tok ->
    (* Token expired – attempt refresh first *)
    (let open Result.Let_syntax in
     let* refreshed = refresh_access_token ~env ~sw ~issuer creds tok in
     if Tok.is_expired refreshed
     then Error "refresh_yielded_expired_token"
     else (
       store ~env issuer creds refreshed;
       Ok refreshed))
    |> (function
     | Ok t -> Ok t
     | Error _ ->
       let open Result.Let_syntax in
       let* tok = obtain ~env ~sw issuer creds in
       let () = store ~env issuer creds tok in
       Ok tok)
  | Error _ ->
    let open Result.Let_syntax in
    let* tok = obtain ~env ~sw issuer creds in
    let () = store ~env issuer creds tok in
    Ok tok
;;

let get ~env ~sw ~issuer creds =
  Oauth2_http.protect (fun () -> get_unprotected ~env ~sw ~issuer creds)
;;
