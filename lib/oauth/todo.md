# Context: State of the OAuth 2.1 implementation in the MCP codebase

## Current OAuth 2.1 implementation – status & gaps (detailed analysis)

The following section provides a narrative assessment written on **2025-06-11**
and is retained for engineering reference.  It lists what is present in the
code-base, what is missing, and a richer explanation of why those gaps matter
for strict conformance with the Model Context Protocol.

### Helper stack already present

The repository contains a complete, self-contained OAuth client helper stack in
`lib/oauth/`:

* `oauth2_types.ml`           – token / metadata records
* `oauth2_http.ml`            – light HTTP util (Piaf one-shots)
* `oauth2_client_credentials.ml` – client-credentials grant
* `oauth2_pkce.ml` / `oauth2_pkce_flow.ml` – PKCE utilities & local redirect listener
* `oauth2_manager.ml`         – cache, refresh, single entry-point

`mcp_transport_http.ml` plugs this manager in, adding a *Bearer* header to all
requests when `MCP_CLIENT_ID` / `MCP_CLIENT_SECRET` are provided.

### Spec-compliant pieces already implemented

1. **Metadata discovery** (`/.well-known/oauth-authorization-server`)
2. **PKCE** (S256 challenge, local callback, interactive browser)
3. **Client-credentials grant**
4. **Token caching** with expiry check (`expires_in – 60 s` guard)
5. **Bearer header** applied to **every** HTTP request
6. Correctly skipped for **stdio** transport (per spec § 2.1)

### Missing / partial elements

1. **Server-side enforcement** – MCP server never checks `Authorization`, never
   responds 401/403, and provides no `/token` or `/authorize` endpoints.
2. **Dynamic client registration** (RFC 7591) – completely absent.
3. **Fallback endpoints** – client aborts when metadata gives 404; spec demands
   default `/authorize`, `/token`, `/register` paths.
4. **401-triggered flow** – client fetches token eagerly instead of after
   receiving 401 and does not retry the failed request.
5. **Refresh-token use** – `refresh_token` is stored but never exchanged.
6. **Scope propagation** – accepted in client-credentials path only; ignored elsewhere.
7. **Secure RNG** – PKCE uses `Random.self_init`, not cryptographically secure.
8. **open_browser portability** – shells out via `xdg-open`, brittle quoting.
9. **Token cache security** – plain-text, default `0644` permissions.
10. **Origin header validation** – HTTP server ignores `Origin`, risk of DNS-rebind.
11. **Error propagation** – transport hides OAuth failures behind a warn log.
12. **PKCE port collision** – listener picks random port but does not retry if busy.
13. **Third-party delegated flow** (§ 2.10) – not implemented.

### Concrete improvement checklist

Server-side
* Validate Bearer and return `WWW-Authenticate` with proper error JSON.
* Expose minimal `/token` & `/authorize` endpoints accepting `client_credentials` grant.
* Serve metadata (`/.well-known/oauth-authorization-server`).

Client-side
* Retry logic: on first 401 start auth dance, then resend JSON-RPC batch.
* Add dynamic registration (`POST /register`) when metadata advertises it.
* Implement refresh-token path in `oauth2_manager`.
* Build default endpoints when metadata missing.
* Use `Mirage_crypto_rng.generate` for PKCE verifier.
* Secure cache file `chmod 600`, atomic writes.
* Supply environment / CLI flags to switch between PKCE and secret flow.

Tests & docs
* Stub auth server in CI, verify 401 path, refresh path.
* Document required env-vars.

Addressing these items will move both client and server to full MCP 2025-03-26
compliance and harden the implementation for production use.



---



# OAuth 2.1 – implementation task-list

This file tracks the remaining work required to turn the reference modules in
`lib/oauth/` into a **fully spec-compliant** layer that satisfies the Model
Context Protocol (MCP) 2025-03-26 Authorization specification and the gaps
identified in *oauth.md*.

The roadmap is organised into **six incremental phases**.  Each phase is small
enough to land as its own pull-request while keeping the build green and the
test-suite passing.

---
detailed step-by-step execution plan for turning the outline in  
lib/oauth/oauth.md into a production-ready, MCP-compliant OAuth 2.1 layer
=========================================================================

The repository already contains most of the code blocks sketched in
oauth.md; however several integration steps, hardening tasks and server-side
counterparts are still missing.  
The roadmap below is split into six incremental phases so that every commit
keeps the tree green and testable.

---------------------------------------------------------------------------
PHASE 0 Inventory & gap analysis (​already done, serve as checklist)
---------------------------------------------------------------------------

1. Modules that are present and compile:
   • oauth2_types.ml / oauth2_http.ml  
   • oauth2_pkce.ml / oauth2_pkce_flow.ml  
   • oauth2_client_credentials.ml  
   • oauth2_manager.ml  
   • Bearer injection hook inside mcp_transport_http.ml

2. Still **missing / partial**:
   • Refresh-token path, dynamic registration, default-endpoint fallback,  
     401-triggered auth retry, TLS-grade RNG, secure token cache perms,  
     server-side enforcement (`/authorize`,`/token`,`WWW-Authenticate`),  
     Origin validation, error surfacing, etc.

---------------------------------------------------------------------------
PHASE 1 Client-side completeness (2 PRs)
---------------------------------------------------------------------------

1-A  Refresh-token & cache hardening
-----------------------------------
1. oauth2_manager.ml  
   a. Implement `refresh_access_token : … -> (Token.t, string) result`  
      – POST grant_type=refresh_token, store new token.  
   b. Update `get` to branch:
        • valid token → return  
        • expired + refresh_token present → try refresh; on failure fall back
          to full obtain.  
   c. Replace `Out_channel.write_all` with atomic write:
        • write to `<file>.tmp`, `Unix.fchmod 0o600`, then `Unix.rename`.  
   d. Unit-test: artificial token with expires_in = 1; ensure refresh path.

2-A  Cryptographically strong RNG
---------------------------------
2. Add *mirage-crypto-rng* to ochat.opam (no C-stubs).  
3. oauth2_pkce.ml – swap `Random` with `Mirage_crypto_rng.generate` (32 bytes).

3-A  Default endpoint fallback
-----------------------------
4. oauth2_manager.obtain:  
   • after `get_json` 404, synthesize Metadata record with
     `<issuer>/authorize` etc.  Covered by spec § 2.3.3.

4-A  401-triggered auth & retry
------------------------------
5. mcp_transport_http.ml  
   • When POST returns 401 + `WWW-Authenticate: Bearer`, call
     `Oauth2_manager.get` (may pop browser if needed) → retry original body
     once.  
   • Add exponential back-off guard to avoid loops.

---------------------------------------------------------------------------
PHASE 2 Server-side minimal auth endpoints + Bearer enforcement (1 PR)
---------------------------------------------------------------------------

1. lib/mcp/mcp_server_http.ml
   • Insert an early middleware:
     ```
     match Headers.get req.headers "authorization" with
     | Some ("Bearer " ^ tok) when Auth_cache.valid tok -> continue
     | _ ->
         let hdrs = Headers.of_list [ "www-authenticate", "Bearer" ] in
         Response.create ~headers:hdrs ~body:(Body.of_string err_json) `Unauthorized
     ```
   • Auth_cache for PoC: (token,expires) Hashtbl filled by `/token` endpoint.

2. New tiny module oauth2_server.ml
   • Hard-coded client-credentials grant for local dev / CI:
     POST /token
       – expects form (grant_type=client_credentials, client_id, client_secret)
       – returns JSON Token (1 hour)  
     POST /register => 501 Not Implemented (placeholder).  
   • Optional `/authorize` endpoint returning dummy HTML for PKCE flow.

3. Add route mounting in mcp_server_http.ml when `--auth-dev` CLI flag set.

---------------------------------------------------------------------------
PHASE 3 Dynamic client registration (client & server) (1 PR)
---------------------------------------------------------------------------

1. oauth2_http.post_json helper (reuse).  
2. oauth2_manager.obtain for PKCE path:  
   • If metadata.registration_endpoint present AND we have no cached client_id,
     POST software statement → persist client_id in
     `~/.config/ocamlochat/registered.json`.  
3. oauth2_server.ml – unprotected `/register` that returns fresh
   `{client_id, client_secret:null}`.

---------------------------------------------------------------------------
PHASE 4 Origin header & TLS hardening (server) (1 PR)
---------------------------------------------------------------------------

1. mcp_server_http.ml:  
   • Reject any request whose `Origin` header is present and not
     `http://127.0.0.1:<port>` or scheme-and-host of listen addr.  
   • Bind only to localhost unless `--bind 0.0.0.0` explicitly passed.

2. Add optional `--cert --key` flags; if supplied, build Piaf HTTPS config.

---------------------------------------------------------------------------
PHASE 5 Test suite & CI fixtures (2 PRs)
---------------------------------------------------------------------------

1. test/oauth_stub_server.ml – starts local `/token` & `/authorize`.  
2. test/oauth_client_test.ml –  
   • ensure client fetches metadata, exchanges token, retries after 401,  
   • verify refresh_token path, cache perms (0600).

3. Extend existing mcp_client_test.ml to run with Bearer header.

---------------------------------------------------------------------------
PHASE 6 Documentation & clean-up (1 PR)
---------------------------------------------------------------------------

• Update prompts/server_side_mcp_plan.md & *_history.md with each milestone.  
• Add README note on required env vars (`MCP_CLIENT_ID`, `MCP_CLIENT_SECRET`)
  and interactive PKCE fallback.

---------------------------------------------------------------------------
Implementation sequence inside each PR
---------------------------------------------------------------------------

1. Write/adjust types (`[@@deriving jsonaf ~stroke:"snake_case"]`).  
2. Add new dune libraries if any.  
3. Unit tests (`dune runtest --diff-command=diff -u`).  
4. Update history file with bullet-point summary.

---------------------------------------------------------------------------
By following the six phases above we will move from the current
happy-path OAuth layer to a fully spec-compliant implementation with secure
token handling, refresh flow, dynamic registration and optional local
authorization server – all without breaking existing functionality.