# Oauth2_manager — credential-isolated token caching

The maintained outbound MCP client uses this helper for optional OAuth.
It is not the daemon's inbound authentication service. See the
[public interface](../../../lib/oauth/oauth2_manager.mli) and
[MCP HTTP adapter](../mcp/mcp_transport_http.doc.md).

## 1 Public API

`get ~env ~sw ~issuer creds` returns a token or operational Error; Eio
cancellation propagates. Credentials are either
`Client_secret { id; secret; scope }` or `Pkce { client_id }`.

Fresh cached tokens are reused. Expiring tokens attempt refresh; a returned
refresh failure triggers acquisition. Cache freshness uses a 60-second margin.
Newly acquired tokens retain the issuer's lifetime, which may be shorter.
Wire decoding assigns a local obtained_at; loading cached JSON preserves it.

## 2 Quick start

```ocaml
let authenticate env sw secret =
  Oauth2_manager.get ~env ~sw ~issuer:"https://auth.example"
    (Oauth2_manager.Client_secret
       { id = "build-bot"; secret; scope = Some "tools.read" })
```

This placeholder requires a real authorization service. Inspect the Result,
but do not print tokens or secrets. PKCE can open a browser and wait for a
callback; see its [known limitations](oauth2_pkce_flow.doc.md). It is not a
general unattended login fallback.

## 3 Function reference

- `fetch_metadata` tries issuer + `/.well-known/oauth-authorization-server`;
  transport/status/JSON/schema failures yield conventional endpoints.
- `fallback_metadata` builds /authorize, /token and /register from the origin.
- `obtain` and `refresh_access_token` use issuer + /token for confidential
  clients; PKCE uses the discovered token endpoint.
- `cache_dir ()` resolves XDG_CACHE_HOME, HOME/.cache or ./.cache, followed by
  `ocamlochat/tokens`.
- `cache_file issuer creds`, `load ~env issuer creds` and
  `store ~env issuer creds token` require explicit credential identity.

The latter three formerly accepted only issuer. Callers must now supply creds;
there is deliberately no issuer-only fallback API.

## 4 Cache location & security

Files are named `v2-<sha256>.json`. The digest binds the **exact** issuer,
grant type, client ID, confidential-client secret and requested scope.
Different IDs, secret rotations, scopes and grant types cannot reuse one
another's tokens. Scope ordering/whitespace is not normalized; equivalent but
differently spelled scopes may acquire separate tokens.

Old `<md5-of-issuer>.json` files are ignored and left untouched. They cannot
be safely migrated because they contain no credential identity. First use of
the new cache reacquires authorization (possibly interactive for PKCE).

Cache JSON contains the token, including any refresh token, but not the supplied
client secret. Load rejects group/other permission bits. Store uses a new
exclusive 0600 temporary file and rename, preventing concurrent writers from
sharing a temporary file; cleanup removes owned temporary files. Store failures
are best-effort, except cancellation. No fsync durability is promised.

Use a private, trusted cache directory. This is neither encryption nor protection
against a malicious same-user process, manipulated parent directories, or token
revocation. PKCE has no account selector: separate accounts of the same public
client need separate cache roots. A requested scope is not local enforcement
of the scopes the issuer actually grants.

## 5 Known limitations

No single-flight acquisition or refresh locking: concurrent misses may make
multiple network requests, and last completed cache publication wins for that
identity. There is no retry/backoff or forced invalidation on HTTP 401.
Discovery fallback may not match nonstandard servers.
Interactive callback hardening remains a [tracked limitation](../../development/code-documentation-audit.md).

## 6 Related modules

[Token codec](oauth2_types.doc.md), [client credentials](oauth2_client_credentials.doc.md),
[PKCE](oauth2_pkce_flow.doc.md), [HTTP helpers](oauth2_http.doc.md).
