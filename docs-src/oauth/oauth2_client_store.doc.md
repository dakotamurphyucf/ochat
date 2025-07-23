# Oauth2_client_store – disk-based cache for Dynamic Client Registration

`Oauth2_client_store` remembers the *client credentials* returned by an
OAuth 2.0 **Dynamic Client Registration** flow (RFC 7591).  Storing the
credentials in a well-known location allows multiple invocations of the
CLI — or completely different binaries — to re-use the same
`client_id` instead of creating a fresh client for every run.

---

## Configuration directory

| Environment variable     | Effective path                                   |
|--------------------------|--------------------------------------------------|
| `XDG_CONFIG_HOME` set    | `$XDG_CONFIG_HOME/ocamlgpt/registered.json`       |
| `XDG_CONFIG_HOME` unset  | `$HOME/.config/ocamlgpt/registered.json`          |

Permissions are locked down to `0o600`; only the current user can read or
modify the file.

The file is a single **JSON object** whose keys are issuer URLs:

```json
{
  "https://auth.example": {
    "client_id"     : "abc123",
    "client_secret" : "s3cr3t"
  },
  "https://login.foo": {
    "client_id" : "public-xyz"
  }
}
```

---

## API in a nutshell

```ocaml
module Credential : sig
  type t = {
    client_id     : string;
    client_secret : string option; (* may be [None] for public clients *)
  }
end

val lookup : env:Eio_unix.Stdenv.base -> issuer:string -> Credential.t option
val store  : env:Eio_unix.Stdenv.base -> issuer:string -> Credential.t -> unit
```

### Example – forget-and-register-once

```ocaml
let get_or_register_client ~env ~sw ~issuer registration_endpoint =
  match Oauth2_client_store.lookup ~env ~issuer with
  | Some cred -> Ok cred
  | None ->
      (* Registration is protocol-specific; simplified here *)
      match register_public_client ~env ~sw registration_endpoint with
      | Error _ as e -> e
      | Ok cred ->
          Oauth2_client_store.store ~env ~issuer cred;
          Ok cred
```

---

## Implementation details

* **Atomic writes** – updates go to `registered.json.tmp` first and are
  then `rename`-d into place, ensuring callers never observe partially
  written files.
* **Best-effort reads** – any IO failure or JSON parsing error yields an
  *empty map* instead of raising, making `lookup` exception-free.
* **No concurrency primitives** – simultaneous writers from multiple
  processes risk lost updates.  Consider wrapping `store` in an
  OS-level lock if your application runs concurrent instances.

---

## Limitations and future work

1. The cache is global per user.  Multi-tenant systems may require a
   more granular isolation strategy.
2. There is no migration logic; future schema changes must be backwards
   compatible or versioned.
3. Credential expiry is not tracked.  Callers remain responsible for
   rotating and invalidating stale registrations.

---

## Related modules

* [`Oauth2_http`](oauth2_http.doc.md) – one-shot HTTP helpers for OAuth
  calls.
* [`Oauth2_types`](oauth2_types.doc.md) – JSON ↔ OCaml data models for
  discovery documents, tokens, &c.

