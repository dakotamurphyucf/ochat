# Oauth2_server_types – Small records living on the *server* side

`Oauth2_server_types` bundles **three tiny data-only records** that an OAuth
2.0 / OpenID Connect **Authorization Server** must hand out or persist during
normal operation.  The module ships as part of the `ochat.oauth2` library
and depends only on Jane-Street’s `core` plus the `jsonaf_ext` shim that
generates JSON converters.

| Sub-module | Purpose | Typical source / sink |
|------------|---------|-----------------------|
| `Metadata` | Minimal *Authorization-Server Metadata* document | `/.well-known/oauth-authorization-server` |
| `Client`   | Credentials produced by *Dynamic Client Registration* | `registration_endpoint` |
| `Token`    | Access token delivered by `token_endpoint` | `token_endpoint` |

All records derive `ppx_jsonaf_conv`, therefore you automatically get

```ocaml
val jsonaf_of_t :       t -> Jsonaf.t
val t_of_jsonaf   : Jsonaf.t -> t
```

plus the usual `_of_string_exn` / `_to_string` helpers stemming from
`Jsonaf`.  Unknown JSON keys are silently ignored so that forward-compatibility
with future spec revisions is preserved.

---

## API cheat-sheet

### `Metadata.t`

```ocaml
type t = {
  issuer                : string;         (* e.g. "https://auth.example" *)
  authorization_endpoint: string;         (* "/authorize" *)
  token_endpoint        : string;         (* "/token"      *)
  registration_endpoint : string option;  (* "/register" or None *)
}
```

Use it to advertise to clients where the various OAuth endpoints live.

### `Client.t`

```ocaml
type t = {
  client_id                : string;
  client_secret            : string option;
  client_name              : string option;
  redirect_uris            : string list option;
  client_id_issued_at      : int option;   (* seconds since epoch *)
  client_secret_expires_at : int option;   (* seconds since epoch *)
}
```

Opaque record that the server persists right after a successful **Dynamic
Client Registration** request.

### `Token.t`

```ocaml
type t = {
  access_token : string;  (* opaque, random *)
  token_type   : string;  (* "Bearer" *)
  expires_in   : int;     (* lifetime in seconds *)
  obtained_at  : float;   (* Unix epoch, seconds *)
}
```

---

## Usage examples

### 1. Serialising a freshly minted access token

```ocaml
let now = Unix.gettimeofday () in
let token : Oauth2_server_types.Token.t =
  { access_token = Crypto.random_token 24
  ; token_type   = "Bearer"
  ; expires_in   = 3600
  ; obtained_at  = now
  }

let body = token |> Oauth2_server_types.Token.jsonaf_of_t |> Jsonaf.to_string
in
Http_server.respond_string ~status:`OK body
```

### 2. Persisting a new client registration

```ocaml
let save_client db (c : Oauth2_server_types.Client.t) =
  let key   = c.client_id in
  let value = Jsonaf.to_string (Oauth2_server_types.Client.jsonaf_of_t c) in
  Database.set db ~key ~value
```

---

## Known limitations

1. The records intentionally cover **only the happy-path** fields required by
   the `ochat.oauth2` code base.  If your use-case needs additional
   metadata (scopes, JWK URIs, …) you are expected to extend the records in a
   fork.
2. The module does **not** provide helper functions such as `Token.is_expired`.
   Higher-level code can compute this easily: `Unix.gettimeofday () -. t.obtained_at
   > float t.expires_in`.

---

*Generated automatically by the project’s documentation tooling.*

