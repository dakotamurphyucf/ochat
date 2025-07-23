# Oauth2_server_routes – HTTP endpoints for a minimal OAuth&nbsp;2.0 server

`Oauth2_server_routes` bundles **four** ready-made request handlers that
turn any [Piaf](https://github.com/anmonteiro/piaf) server into a *very
light-weight* OAuth&nbsp;2.0 **Authorisation Server**.  The helpers build on
two in-memory tables –
[`Oauth2_server_client_storage`](oauth2_server_client_storage.doc.md) and
[`Oauth2_server_storage`](oauth2_server_storage.doc.md) – and therefore
require *no* external database.  Everything lives inside the OCaml
process and vanishes on restart, which makes the module ideal for unit
tests, demos and CI pipelines.

---

## Exposed routes

| Path | Spec | Handler |
|------|------|---------|
| `/.well-known/oauth-authorization-server` | RFC&nbsp;8414 (AS Metadata) | `handle_metadata` |
| `/token` | RFC&nbsp;6749 §4.4 (*Client Credentials* grant) | `handle_token` |
| `/authorize` | RFC&nbsp;6749 §4.1 (*Authorisation Code*) **placeholder** | `handle_authorize` |
| `/register` | RFC&nbsp;7591 (Dynamic Client Registration) | `handle_register` |

Each helper is **exception-free** and always returns a
`Piaf.Response.t` whose body is JSON and whose status code reflects the
outcome.

---

## Quick-start – embed the routes in an Eio/Piaf server

```ocaml
open Piaf
open Eio

Eio_main.run @@ fun env ->
  (* Minimal router – mount all routes under the root prefix. *)
  let callback (_client : Client.t) req =
    match Request.path req with
    | [".well-known"; "oauth-authorization-server"] ->
        Oauth2_server_routes.handle_metadata req 8080
    | ["token"] ->
        Oauth2_server_routes.handle_token ~env req
    | ["authorize"] ->
        Oauth2_server_routes.handle_authorize req
    | ["register"] ->
        Oauth2_server_routes.handle_register req
    | _ -> Response.create `Not_found
  in
  let server_config = Server.Config.create ~port:8080 () in
  Server.start ~config:server_config env callback
```

Run it, then request a token:

```console
$ curl -s \
       -d 'grant_type=client_credentials' \
       -d 'client_id=my-client' \
       -d 'client_secret=my-secret' \
       http://localhost:8080/token | jq
{
  "access_token": "xg0C9t…",
  "token_type": "Bearer",
  "expires_in": 3600,
  "obtained_at": 1.694e9
}
```

---

## API reference

### `handle_metadata : Piaf.Request.t -> int -> Piaf.Response.t`

Returns the server’s *Authorization-Server Metadata* document.  The
function derives the scheme and host from the incoming request and forces
the supplied TCP `port`, ensuring that all URLs are self-contained even
behind reverse proxies.

### `handle_token : env:Eio_unix.Stdenv.base -> Piaf.Request.t -> Piaf.Response.t`

Implements the *Client Credentials* grant:

* Accepts `application/x-www-form-urlencoded` with the fields
  `grant_type`, `client_id`, `client_secret`.
* Validates the credentials against the client store.
* Generates and persists a fresh access token.
* Responds with HTTP 200 and the JSON representation of
  `Oauth2_server_types.Token.t`.

Error responses follow [RFC&nbsp;6749 §5.2]:

| Status | JSON payload | Reason |
|--------|--------------|--------|
| 400 | `{ "error": "invalid_request" }` | Missing or malformed body |
| 400 | `{ "error": "unsupported_grant_type" }` | Any grant other than *client_credentials* |
| 401 | `{ "error": "invalid_client" }` | Unknown `client_id` or wrong secret |

### `handle_authorize : Piaf.Request.t -> Piaf.Response.t`

Placeholder for the upcoming *Authorisation Code* + PKCE flow.  Always
returns **501**.

### `handle_register : Piaf.Request.t -> Piaf.Response.t`

Processes a Dynamic Client Registration request.  Only a subset of the
specification is supported – enough for the rest of the repository:

* `client_name` – optional string.
* `redirect_uris` – optional array of strings.
* `token_endpoint_auth_method` – ignored; every client is registered as
  *confidential* and therefore receives a secret.

On success the function responds with **201** and the JSON encoding of
`Oauth2_server_types.Client.t`.

---

## Known limitations

1. **Single-process state.**  Neither the client nor the token store is
   persisted.  Restarting the server loses all registrations and issued
   tokens.
2. **One grant only.**  The `/token` endpoint recognises _only_
   *client_credentials*.
3. **No PKCE yet.**  `/authorize` is a dummy.
4. **No TLS helper.**  Serving over HTTPS (required by the spec in
   production) must be handled externally.

---

## Further reading

* [RFC 6749] – The OAuth 2.0 authorization framework.  
* [RFC 7591] – OAuth 2.0 Dynamic Client Registration.  
* [RFC 8414] – *OAuth 2.0 Authorization Server Metadata*.

[RFC 6749]: https://www.rfc-editor.org/rfc/rfc6749
[RFC 7591]: https://www.rfc-editor.org/rfc/rfc7591
[RFC 8414]: https://www.rfc-editor.org/rfc/rfc8414

