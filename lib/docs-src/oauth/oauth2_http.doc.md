# Oauth2_http – minimal HTTP helpers for OAuth 2.0 work-flows

`Oauth2_http` is a *quality-of-life* wrapper around
[Piaf](https://github.com/anmonteiro/piaf) that keeps OAuth-related HTTP
requests down to a single line.  Instead of sprinkling `Piaf.Client.Oneshot`
calls and JSON decoding boiler-plate throughout your codebase you can just
re-use one of the three convenience functions.

| Function     | Method / body type                                | Typical usage (RFC) |
|--------------|---------------------------------------------------|---------------------|
| `get_json`   | `GET` – *accept: application/json*                | Discovery document (`/.well-known/oauth-authorization-server`), user-info end-point (RFC 8414/7662) |
| `post_form`  | `POST` – `application/x-www-form-urlencoded`      | Token exchange & refresh (RFC 6749) |
| `post_json`  | `POST` – `application/json`                       | Dynamic client registration (RFC 7591) |

All helpers are **one-shot** – a fresh TCP/TLS connection is established for
every request and torn down right after the response body is consumed.  This
fits well with the sporadic nature of OAuth client traffic and avoids
keeping idle connections around.

---

## Quick start

### Fetch the discovery document

```ocaml
Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let meta_json =
      Oauth2_http.get_json
        ~env ~sw
        "https://auth.example/.well-known/oauth-authorization-server"
    in
    match meta_json with
    | Error e -> eprintf "Discovery failed: %s\n" e
    | Ok json ->
        let meta = Oauth2_types.Metadata.t_of_jsonaf json in
        printf "Token endpoint = %s\n" meta.token_endpoint
```

### Exchange an authorisation code for a token

```ocaml
let exchange_code ~env ~sw ~code ~client_id ~redirect_uri ~token_endpoint =
  let params =
    [ "grant_type", "authorization_code";
      "code", code;
      "client_id", client_id;
      "redirect_uri", redirect_uri ]
  in
  Oauth2_http.post_form ~env ~sw token_endpoint params
  |> Result.map ~f:Oauth2_types.Token.t_of_jsonaf
```

### Register a public client

```ocaml
let register_public_client ~env ~sw registration_endpoint ~name ~redirect_uri =
  let payload =
    Jsonaf.of_list
      [ "client_name", Jsonaf.of_string name;
        "redirect_uris", Jsonaf.of_array [| Jsonaf.of_string redirect_uri |];
        "token_endpoint_auth_method", Jsonaf.of_string "none" ]
  in
  Oauth2_http.post_json ~env ~sw registration_endpoint payload
  |> Result.map ~f:Oauth2_types.Client_registration.t_of_jsonaf
```

---

## Error handling semantics

The helpers return `(Jsonaf.t, string) Result.t` where `Error msg` is a
human-readable description coming from:

1. `Piaf.Error.to_string` — network / TLS / HTTP problems.
2. `Exn.to_string` — JSON decoding failures (`get_json`, `post_json`).

`post_form` intentionally {em raises} the JSON parse exception instead of
wrapping it.  This mirrors historical code paths and avoids an extra match
layer in callers that pre-validate the content-type.

---

## Implementation notes

* `piaf_cfg` sets `allow_insecure = true` so that *localhost* development
  servers with self-signed certificates Just Work™.  Review this before
  shipping to production.
* Form-encoding is done manually (`Uri.pct_encode`) because Piaf currently
  lacks an explicit helper.

---

## Limitations

1. The full response body is buffered in memory; large downloads are
   inefficient.
2. Only `application/json` responses are supported.
3. No automatic retry / back-off strategy.


