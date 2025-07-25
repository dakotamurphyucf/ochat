# Oauth2_client_credentials – Client-Credentials grant helper

`Oauth2_client_credentials` implements the
[RFC 6749 §4.4](https://datatracker.ietf.org/doc/html/rfc6749#section-4.4)
*Client Credentials* grant for OAuth 2.0.  The grant is intended for
server-to-server (machine-to-machine) scenarios where no human user is
involved – the application acts on its own behalf with a *client ID* and
*client secret* previously issued by the authorisation server.

The module exposes a single convenience function, {!val:fetch_token},
which takes care of:

1. Encoding the request as
   `application/x-www-form-urlencoded` in the format expected by most
   servers.
2. Performing the HTTPS `POST` using
   [`Piaf.Client.Oneshot`](https://github.com/plexus/ocaml-piaf).
3. Decoding the JSON payload into an [`Oauth2_types.Token.t`][] record.
4. Stamping the returned token with the time of acquisition, allowing
   callers to rely on [`Oauth2_types.Token.is_expired`][] for refresh
   decisions.

---

## API in a nutshell

```ocaml
val fetch_token :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  token_uri:string ->
  client_id:string ->
  client_secret:string ->
  ?scope:string ->
  unit ->
  (Oauth2_types.Token.t, string) result
```

### Example – obtain and use a bearer token

```ocaml
Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    match
      Oauth2_client_credentials.fetch_token
        ~env ~sw
        ~token_uri:"https://auth.example/token"
        ~client_id:"my-service"
        ~client_secret:(Sys.getenv_exn "CLIENT_SECRET")
        ~scope:"profile openid"
        ()
    with
    | Error msg -> Format.eprintf "Token error: %s@." msg
    | Ok tok ->
        let headers = [ "authorization", "Bearer " ^ tok.access_token ] in
        let uri = Uri.of_string "https://api.example/resource" in
        match Piaf.Client.Oneshot.get ~headers env ~sw uri with
        | Ok resp -> Format.printf "HTTP %d@." resp.status
        | Error err -> Format.eprintf "HTTP error: %s@." (Piaf.Error.to_string err)
```

---

## Parameters

| Argument        | Description                                                            |
|-----------------|------------------------------------------------------------------------|
| `env`           | The [`Eio_unix.Stdenv.base`] obtained from `Eio_main.run`.              |
| `sw`            | A [`Eio.Switch.t`] delimiting the lifetime of the network operations.  |
| `token_uri`     | Full URL of the `/token` endpoint on the authorisation server.         |
| `client_id`     | Public identifier issued during (dynamic) client registration.         |
| `client_secret` | Confidential shared secret bound to the client ID.                     |
| `?scope`        | Optional space-separated list restricting the privileges requested.     |

The unit `()` at the end keeps the label-led call site readable and leaves
room for future optional parameters.

---

## Error handling

The helper is *exception-free*: any transport-layer error (connection
refused, TLS handshake failure, …), HTTP error response, or JSON decoding
issue is returned as `Error "…"` with a descriptive message.  Inspect or
log the message before propagating it to the caller.

---

## Known limitations

1. **No built-in retries**.  Transient failures (e.g. 503 or network
   hiccups) must be retried by the caller or a higher-level wrapper.
2. **Plain-text secret**.  The `client_secret` parameter expects the raw
   secret; storing it securely is out of scope.
3. **No JWT support**.  Token responses are assumed to be opaque bearer
   strings; structured JWT introspection is left to the application.

---

## Related modules

* [`Oauth2_http`](oauth2_http.doc.md) – one-shot HTTP helpers used under
  the hood.
* [`Oauth2_types`](oauth2_types.doc.md) – JSON ↔ OCaml data models,
  including the [`Token`][] record returned by this grant.

[`Oauth2_types.Token.t`]: oauth2_types.doc.md
[`Oauth2_types.Token.is_expired`]: oauth2_types.doc.md

