# Oauth2_types – Plain records for OAuth 2.0 flows

`Oauth2_types` is a tiny helper module that groups together three *data-only*
records that regularly pop up when talking to an OAuth 2.0 / OpenID Connect
server.  The module lives in the `ochat.oauth2` library and has **zero
runtime dependencies** beyond Jane-Street’s `core` and a small `jsonaf_ext`
shim.

| Sub-module | Purpose | Typical source |
|------------|---------|-----------------|
| `Metadata` | Sub-set of the *Authorization Server Metadata* document | `/.well-known/oauth-authorization-server` |
| `Client_registration` | Credentials issued by *Dynamic Client Registration* | POST to the *registration endpoint* |
| `Token` | Access token obtained from `token_endpoint` | Any OAuth 2.0 grant |

All three records derive `ppx_jsonaf_conv`, therefore the following functions
are generated for you:

```ocaml
val jsonaf_of_t :       t -> Jsonaf.t
val t_of_jsonaf   : Jsonaf.t -> t
```

plus the usual `_of_string_exn` / `_to_string` helpers provided by
`Jsonaf`.  The `Metadata` and `Client_registration` records are marked with
`[@@jsonaf.allow_extra_fields]` so that **forward-compatibility** is ensured:
unknown fields are silently ignored.

---

## API cheat-sheet

### `Metadata.t`

```ocaml
type t = {
  authorization_endpoint : string;
  token_endpoint         : string;
  registration_endpoint  : string option;
}
```

Use it for driving browser-based flows (PKCE) or to discover where to POST
client-registration / token-refresh requests.

### `Client_registration.t`

```ocaml
type t = {
  client_id     : string;
  client_secret : string option;
}
```

Many servers omit `client_secret` for *public* clients such as native or
SPA apps.

### `Token.t`

```ocaml
type t = {
  access_token  : string;
  token_type    : string;   (* e.g. "Bearer" *)
  expires_in    : int;      (* seconds *)
  refresh_token : string option;
  scope         : string option;
  obtained_at   : float;    (* Unix epoch, seconds *)
}

val is_expired : t -> bool
```

`is_expired` returns `true` if *less than 60 s* remain before the token hits
its `expires_in` deadline – giving the caller just enough time to refresh and
retry without a failed request.

---

## Usage examples

### 1. Loading server metadata

```ocaml
let fetch_metadata ~uri =
  Eio.Switch.run @@ fun sw ->
  let client = Piaf.Client.on_eio ~sw uri in
  match Piaf.Client.get client ~sw (Uri.with_path uri "/.well-known/oauth-authorization-server") with
  | Error _ as e -> e
  | Ok resp      ->
    Piaf.Body.to_string resp.body |> Result.map ~f:(fun body ->
      Jsonaf.of_string_exn body |> Oauth2_types.Metadata.t_of_jsonaf)
```

### 2. Persisting and checking a token

```ocaml
let save_token file t =
  Out_channel.write_all file ~data:(Oauth2_types.Token.jsonaf_of_t t |> Jsonaf.to_string)

let load_token file =
  In_channel.read_all file |> fun s ->
  Jsonaf.of_string_exn s |> Oauth2_types.Token.t_of_jsonaf

let with_fresh_token file ~refresh ~use =
  let token = load_token file in
  let token = if Oauth2_types.Token.is_expired token then refresh token else token in
  save_token file token;
  use token
```

---

## Known limitations

1. The types intentionally model only the *minimal* set of fields required by
   the rest of the `ochat.oauth2` stack.  If your use-case needs
   additional metadata, simply extend the records on a fork or wrap them in a
   separate record.
2. `Token.is_expired` assumes that clock skew with the server is negligible.
   If you operate in an environment with significant time drift, consider
   extending the safety window or syncing the local clock.

---

*Generated automatically by the project’s documentation tooling.*

