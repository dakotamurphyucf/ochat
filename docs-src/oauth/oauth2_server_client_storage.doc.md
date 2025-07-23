OAuth2 Server ‑ Client Storage
==============================

In-memory registry for credentials created by *Dynamic Client Registration*
(RFC 7591).  The module keeps a single [`Core.Hashtbl.t`](https://opensource.janestreet.com/stdlib/v0.15/core/Core/Hashtbl_intf/index.html)
indexed by `client_id` and exposes a minimal API that is sufficient for the
other server-side components in this repository.

Durability is **not** provided: every process restart wipes the table.  This is
acceptable for the intended use-cases — prototypes, CI tests and single
instance deployments — but you **must** back the interface with a real
database in production environments.

---

API Reference
-------------

### `register ?client_name ?redirect_uris ?confidential () → Client.t`

Create a new client and insert it into the table.

Arguments:

* `?client_name` – optional human-readable label for UI dialogs.
* `?redirect_uris` – white-list of allowed redirect URIs; `None` means *any*.
* `?confidential` – if `true` (default `false`) generate a secret and mark the
  client as *confidential*; otherwise the result is a *public* PKCE-only
  client with `client_secret = None`.

Returns the freshly created [`Oauth2_server_types.Client.t`](./oauth2_server_types.doc.md).

Example – **public** client

```ocaml
let c =
  Oauth2_server_client_storage.register
    ~client_name:"Web CLI"
    ()
in
assert (Option.is_none c.client_secret)
```

Example – **confidential** client

```ocaml
let c = Oauth2_server_client_storage.register ~confidential:true () in
match c.client_secret with
| Some _ -> ()  (* secret present *)
| None -> assert false
```

---

### `find client_id → Client.t option`

O(1) lookup by identifier.  Returns `None` when the client is unknown.

---

### `validate_secret ~client_id ~client_secret → bool`

Credential check used by the `token_endpoint`:

* returns `true` if the `client_id` exists **and**
  * the stored record is *public* and `client_secret = None`;
  * **or** the stored secret exactly matches `client_secret`.
* any other combination yields `false`.

---

### `insert_fixed ~client_id ~client_secret ?client_name ?redirect_uris () → Client.t`

Insert a deterministic **confidential** client.  Pre-existing entries with the
same `client_id` are overwritten.  Handy for tests and local development
set-ups where credentials must remain stable across restarts.

---

### `pp_table fmt ()`

Pretty-print one line per entry – convenient for REPL inspection.

---

Internal Details
----------------

* `client_id` and random secrets are generated with
  [`Mirage_crypto_rng.generate`](https://mirage.github.io/mirage-crypto/doc/mirage-crypto-rng/Mirage_crypto_rng/index.html)
  and encoded as *URL-safe* base64 without padding using the `Base64` package.
* Timestamps are wall-clock seconds via `Core_unix.time` and stored as ints.
* Development credentials are inserted at start-up.  Set the environment
  variables `MCP_DEV_CLIENT_ID` and `MCP_DEV_CLIENT_SECRET` to override the
  default *dev-client / dev-secret* pair.

Known Limitations
-----------------

1. **Volatile** – data resides in RAM only.  Persist it yourself for serious
   deployments.
2. **No expiry & rotation** – `client_secret_expires_at` is always `None`.
   Add a background job and extra fields if you require secret rollover.
3. **Single process** – sharing the table between multiple OS processes is not
   supported.

