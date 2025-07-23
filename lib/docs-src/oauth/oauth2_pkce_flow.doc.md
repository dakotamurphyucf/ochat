# Oauth2_pkce_flow – Interactive PKCE flow helper

`Oauth2_pkce_flow` wires together a **minimal, self-contained**
implementation of the *OAuth&nbsp;2.0 Authorisation Code* grant with
[*Proof-Key for Code Exchange* (PKCE, RFC&nbsp;7636)](https://datatracker.ietf.org/doc/html/rfc7636).

It is aimed at **native / command-line applications** where launching a
web browser is feasible but exposing a world-reachable redirect URI is
not.  The helper:

1. Creates a one-shot HTTP listener on
   `http://127.0.0.1:8876/cb`.
2. Generates a cryptographically-secure `code_verifier` /
   `code_challenge` pair.
3. Opens the user’s browser at the authorisation endpoint with the
   required PKCE query parameters.
4. Waits for the server to redirect back with `?code=…` and exposes the
   resulting triple `(code, verifier, redirect_uri)` to the caller.

A second convenience {!exchange_token} trades the code for an access /
refresh token.

Dependencies stay pleasantly light: only
[`Eio`](https://github.com/ocaml-multicore/eio) for concurrent I/O and
[`Oauth2_http`](oauth2_http.doc.md) for a single HTTPS request.

---

## Quick reference

| Function | Purpose |
|----------|---------|
| `run` | Launch browser, wait for callback, return `(code, verifier, redirect_uri)` |
| `exchange_token` | POST *authorization_code* grant and decode JSON into `Token.t` |


---

## 1  End-to-end example

```ocaml
open Oauth2_pkce_flow

let authenticate ~meta ~client_id () =
  Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      (* 1. Interactive browser dance *)
      let code, verifier, redirect_uri =
        run ~env ~sw ~meta ~client_id
      in

      (* 2. Code → token *)
      match
        exchange_token
          ~env ~sw ~meta ~client_id
          ~code ~code_verifier:verifier ~redirect_uri
      with
      | Error msg -> Error (`Auth msg)
      | Ok tok -> Ok tok
```

Compile & run the snippet, a browser window pops up asking the user to
sign in.  Once they approve access the function returns an
[`Oauth2_types.Token.t`](oauth2_types.doc.md) record stamped with
`obtained_at`.


---

## 2  API details

### `run`

```ocaml
val run :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  meta:Oauth2_types.Metadata.t ->
  client_id:string ->
  string * string * string
```

| Result | Meaning |
|--------|---------|
| `code` | One-time authorisation code from the AS |
| `verifier` | PKCE `code_verifier` (pass to `exchange_token`) |
| `redirect_uri` | Callback URI actually used (constant for now) |

Internally the helper binds a TCP socket with
`Eio.Net.listen ~reuse_addr:true ~reuse_port:true` and handles exactly
{b one} request before closing.


### `exchange_token`

```ocaml
val exchange_token :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  meta:Oauth2_types.Metadata.t ->
  client_id:string ->
  code:string ->
  code_verifier:string ->
  redirect_uri:string ->
  (Oauth2_types.Token.t, string) result
```

On success the returned `Token.t` field [`obtained_at`] is stamped with
`Eio.Time.now` so that [`Token.is_expired`] works out of the box.


---

## 3  Local HTTP callback mechanics

*   Listener bound to IPv4 wildcard `0.0.0.0` on port **8876** with
    backlog 1.
*   Only the query string is parsed, ignoring path & headers.
*   The browser receives a plain text response: *“You may close this
    tab”*.

Feel free to adapt the port, path or HTML response to fit your UX.


---

## 4  Error handling semantics

| Layer | Propagation |
|-------|-------------|
| Browser launch | Logged via `Logs.warn`; function continues |
| Socket bind / accept | Raises `Eio.Io` (let it crash philosophy) |
| Token exchange | `Ok token` / `Error message` |

`exchange_token` re-raises `Jsonaf.Parse_error` if the server returns
malformed JSON – mirroring `Oauth2_http.post_form`.


---

## 5  Limitations & TODOs

1. Port is **hard-coded** to 8876.  A random high port would avoid
   clashes with other local services.
2. Listener is IPv4 only (`127.0.0.1`).  IPv6 dual-stack would be more
   robust.
3. Only the first callback is handled – subsequent clicks yield a blank
   page.
4. No state/nonce parameter – CSRF protection left to the caller.

