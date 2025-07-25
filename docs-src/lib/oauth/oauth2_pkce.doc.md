# Oauth2_pkce – PKCE helper utilities

`Oauth2_pkce` is a **tiny, self-contained helper** that generates the
`code_verifier` / `code_challenge` pair required by the
[OAuth 2.0 Authorization Code flow with Proof Key for Code Exchange
(RFC 7636)](https://datatracker.ietf.org/doc/html/rfc7636).

The module stays deliberately minimal so that it can be embedded in
command-line tools, GUI apps or web servers without dragging in heavy
OAuth client libraries.  It depends only on:

* [`mirage-crypto-rng`](https://mirage.github.io/mirage-crypto/doc) –
  cryptographically-secure random numbers.
* [`digestif`](https://mirage.github.io/digestif/) – SHA-256.
* [`base64`](https://mirage.github.io/ocaml-base64/) – URL-safe encoding.

---

## Quick reference

| Function | Purpose |
|----------|---------|
| `gen_code_verifier` | Produce a random, RFC-7636-compliant verifier. |
| `challenge_of_verifier` | Derive the corresponding `S256` challenge. |


## 1  Generating a verifier / challenge pair

```ocaml
let verifier  = Oauth2_pkce.gen_code_verifier () in
let challenge = Oauth2_pkce.challenge_of_verifier verifier in

Printf.printf "Verifier  = %s\nChallenge = %s\n" verifier challenge
```

Sample output (hex digits elided for brevity):

```
Verifier  = fMCI...mnc
Challenge = CSt7...xpU
```

Both strings are already base64-URL encoded without padding and can be
sent verbatim:

* `code_challenge=<Challenge>&code_challenge_method=S256` (authorisation
  request)
* `code_verifier=<Verifier>` (token request)

### Length and character set

* 43–128 characters as mandated by RFC 7636 §4.1 (this implementation
  yields **43 or 44**).
* Alphabet: `A–Z a–z 0–9 - _` (base64url).


## 2  API details

### `gen_code_verifier : unit -> string`

Creates 32 bytes of entropy using `Mirage_crypto_rng.generate` and
base64url-encodes them without `=` padding.

Remarks:

* Requires a **seeded RNG**.  The helper calls
  `Mirage_crypto_rng_unix.use_default ()` as a best-effort fallback but
  will raise `Unseeded_generator` if that fails.
* 32 random bytes translate to 256 bits of entropy – well above the 128
  bits usually considered sufficient and within the length limits.

### `challenge_of_verifier : string -> string`

`challenge_of_verifier v` hashes `v` with SHA-256 and base64url-encodes
the 32-byte digest.  This corresponds to the *S256* transformation from
RFC 7636 §4.2.

The function **does not** validate that the input already conforms to
the verifier grammar – garbage in, garbage out.


## 3  Integrating into an OAuth flow

```ocaml
let open_browser uri = Sys.command ("xdg-open " ^ uri) |> ignore

let run_pkce_flow ~authorization_endpoint ~token_endpoint ~client_id () =
  let verifier  = Oauth2_pkce.gen_code_verifier () in
  let challenge = Oauth2_pkce.challenge_of_verifier verifier in

  (* 1. Ask the resource owner to authenticate *)
  let auth_uri =
    Uri.add_query_params'
      (Uri.of_string authorization_endpoint)
      [ "response_type", "code";
        "client_id",      client_id;
        "code_challenge", challenge;
        "code_challenge_method", "S256" ]
  in
  open_browser (Uri.to_string auth_uri);

  (* 2. Receive ?code=ABC via redirect … *)
  let code = wait_for_redirect () in

  (* 3. Exchange the code for a token *)
  let params =
    [ "grant_type",    "authorization_code";
      "code",          code;
      "client_id",     client_id;
      "code_verifier", verifier ]
  in
  Oauth2_http.post_form ~env ~sw token_endpoint params
  |> Result.map ~f:Oauth2_types.Token.t_of_jsonaf
```


## 4  Known limitations

1. No *plain* (SHA-1) support – only the recommended `S256` method.
2. The verifier length is fixed at 32 bytes of entropy; if interoperability
   with systems enforcing a different size is required you must roll your
   own generator.
3. Error handling is minimal; a failure to seed the RNG raises an
   exception.


---

© This documentation is released into the public domain.

