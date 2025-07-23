# Oauth2_manager – Token caching and refresh wrapper

`Oauth2_manager` combines the low-level helpers in the `chatgpt.oauth2`
library into a {b plug-and-play} access-token provider that “just works” for
both headless background jobs and interactive command-line tools.

At a glance it delivers:

| Feature | How it works |
|---------|--------------|
| **Automatic discovery** | Attempts `/.well-known/oauth-authorization-server`, otherwise falls back to the conventional `/authorize` and `/token` endpoints. |
| **Client-credentials grant** | Uses {!module:Oauth2_client_credentials} when a `client_secret` is available. |
| **PKCE flow** | Falls back to the browser-based {!module:Oauth2_pkce_flow} for public clients. |
| **Local cache** | Persists the full [`Oauth2_types.Token.t`](oauth2_types.doc.md) as JSON under `$XDG_CACHE_HOME/ocamlgpt/tokens/`. |
| **Refresh logic** | Refreshes whenever less than 60 s remain before expiry and propagates any *refresh_token* errors to the caller. |

The module is exception-free.  Any transport, HTTP, or decoding failure is
reported as `Error "…"` while unrecoverable bugs (e.g. programmer mistakes)
continue to surface via regular exceptions.

---

## 1  Public API

```ocaml
type creds =
  | Client_secret of {
      id     : string;
      secret : string;
      scope  : string option;
    }
  | Pkce of { client_id : string }

val get :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  issuer:string ->
  creds ->
  (Oauth2_types.Token.t, string) result
```

*Everything else* (`cache_file`, `store`, `refresh_access_token` …) is
semi-public and documented in the source for users with more advanced needs.

---

## 2  Quick start

### 2.1  Headless service – client-credentials

```ocaml
Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    match
      Oauth2_manager.get
        ~env ~sw
        ~issuer:"https://auth.example"
        (Client_secret {
           id     = "build-bot";
           secret = Sys.getenv_exn "CLIENT_SECRET";
           scope  = Some "openid profile";
         })
    with
    | Error msg -> Format.eprintf "Token error: %s@." msg
    | Ok tok -> Format.printf "Bearer %s@." tok.access_token
```

`Bearer <token>` can now be attached to ordinary HTTP requests.

### 2.2  Interactive CLI – PKCE

```ocaml
let authenticate () =
  Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      match
        Oauth2_manager.get
          ~env ~sw
          ~issuer:"https://login.okta.com/oauth2/default"
          (Pkce { client_id = "0oa5abc123XYZ" })
      with
      | Error msg -> Error (`Auth msg)
      | Ok tok -> Ok tok
```

The helper opens the user’s browser, waits for the redirect, writes the token
to cache, refreshes on subsequent invocations – and stays entirely within the
terminal otherwise.

---

## 3  Function reference (abbreviated)

| Function | Purpose |
|----------|---------|
| `cache_dir` | Return `$XDG_CACHE_HOME`-compatible directory for token files. |
| `cache_file issuer` | Deterministic filename based on MD5 of the issuer URL. |
| `fallback_metadata` | Construct minimal `Metadata.t` when discovery fails. |
| `fetch_metadata` | Download and decode `/.well-known/oauth-authorization-server`. |
| `load` / `store` | *Exact* JSON round-trip for `Token.t` with strict `0600` perms. |
| `refresh_access_token` | Perform a refresh-token grant. |
| `obtain` | First-time acquisition via client-credentials or PKCE. |
| `get` | High-level cache + refresh + obtain pipeline. |

All helpers favour explicit parameters (`env`, `sw`, `issuer`) so that they
compose cleanly inside larger Eio applications.

---

## 4  Cache location & security

Token files are created with permissions `0600` and loaded only if the file is
{i still} private to the user.  Any stray `group` or `other` bits cause
`Error "insecure_token_cache_permissions"` and force a fresh network request.

---

## 5  Known limitations

1. **No retry/back-off.** The caller is expected to retry transient
   `Error` values.
2. **Single issuer per process.** There is no in-memory LRU; invoking
   `get` for multiple issuers concurrently is perfectly safe but heavy
   traffic may cause a surge in open/noisy connections.
3. **Discovery shortcut.** The fallback assumes standard endpoint paths; some
   proprietary setups may break.

---

## 6  Related modules

* [`Oauth2_client_credentials`](oauth2_client_credentials.doc.md) – raw
  client-credentials grant.
* [`Oauth2_pkce_flow`](oauth2_pkce_flow.doc.md) – interactive PKCE browser
  helper.
* [`Oauth2_http`](oauth2_http.doc.md) – tiny wrapper over *Piaf* for JSON and
  form HTTP requests.

