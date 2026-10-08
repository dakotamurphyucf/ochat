# Direct provider OAuth foundation

The provider OAuth libraries implement the fixed Codex browser loopback and custom
device protocol. They do not read authentication caches, choose API-key billing,
start a browser, or publish credentials. Credential lifecycle authority belongs to
the host registry; a successful protocol result is a candidate for that authority.

The protocol follows Codex revision
`fac5d0ba91b991ddb5a0f049289acd84135927b5`. The issuer, client registration and
Codex resource are fixed. Browser callbacks bind literal 127.0.0.1 on an explicitly
selected allowed port, validate state and PKCE, and bind the returned ID token to
the generated nonce. Unrelated callbacks do not consume the pending login. Device
polling uses the provider's custom endpoints and treats 403/404 as pending only at
the polling endpoint. Both flows have finite monotonic deadlines and owned workers
that cancellation and close join. Authorization-code exchanges are not retried
after possible submission. The callback returns fixed text directing the user
to OChat for the final status; receiving a callback does not itself prove that
login or credential publication succeeded. The listener permits immediate
restart after a completed callback while refusing another active listener on
the same fixed loopback port.

## Identity provenance

Identity assertions come only from responses received through the fixed token
endpoint with system CA and hostname validation. This follows the authenticated
token-endpoint rule in [OIDC Core 3.1.3.7](https://openid.net/specs/openid-connect-core-1_0.html#IDTokenValidation).
It does not admit caller JWTs or claim that decoding a JWT verifies its signature.
ID-token issuer, client audience, authorized party, subject, account and times are
validated. The access-token audience remains opaque because no qualified expected
value has been established for this route.

A refresh response may omit its ID token. Established exact identity and protected
continuity remain authoritative; a supplied ID token must match that identity.
Original nonce, audience and authentication time continuity follow
[OIDC Core 12.2](https://openid.net/specs/openid-connect-core-1_0.html#RefreshTokenResponse).
The protected bounded continuity payload contains the required proof fields,
not a copy of the original ID token, and has no public status representation.
Latest token-response `expires_in` presence is retained separately in that
protected payload. The registry absolute expiry declaration comes from the
authenticated access-token `exp` claim; it does not assert that the token response
contained `expires_in`. Scope provenance similarly distinguishes response fields,
browser requested scopes, exact prior grants and qualified access-token claims.

Scope omission follows [OAuth 2.0](https://www.rfc-editor.org/rfc/rfc6749.html#section-5.1):
browser requested scopes or the exact existing refresh grant can supply the
omitted value. Device polling cannot inherit browser scopes. Its optional
access-token scope-claim policy requires explicit host qualification and defaults
to refusal. Absent, null and supplied token fields remain distinct.

## Dispatch and failure boundaries

A direct driver lease permits only the literal fixed HTTPS Codex Responses route
and the exact captured profile account. HTTP and WS use the same account and OChat
identification header policy. Owner/currentness and credential revision fencing
remain required; header construction does not replace host authorization. No
arbitrary header interface or insecure TLS override is provided.

Errors retain finite classifications without server bodies, token bytes or private
callback parameters. Identity failures identify only the rejected check, such as
issuer, audience or nonce. `Login.error` reads a completed typed failure without
consuming the login result; absence is not evidence of successful login.
Transport headers and bodies are bounded before decoding.
Ambiguous code/refresh exchange results do not authorize automatic replay. The
registry adapter preserves the registry durable rotating intent. The registry owns
publication, refresh serialization, logout and late-result rejection.

## Qualification limits

All 21 synthetic protocol, flow and native registry adapter cases pass under the
strict unrestricted macOS runner, including an actual owned loopback callback.
They cover state rejection, pending polling, cancellation, whole-flow expiry,
bounded HTTP framing, identity validation, refresh omission and continuity,
restart, exact admission identity, publication uncertainty and revoked authority
during an exchange. The Responses driver suites also pass, including the two
new direct-route/header cases across HTTP and WebSocket policies. Repository
`@check` and offline documentation validation pass.

Live browser login, credential registration, restart with saved credentials and
cancellation of a subsequent login passed on 2026-10-08. The first direct Codex
SSE inference request returned HTTP 400; it does not qualify inference support.
Complete browser/device workflows, refresh and direct HTTP/WS route acceptance
still require provider qualification. Synthetic exchanges do not claim
that OpenAI permits this registration for every host or account.

## Registry adapter ownership

The separate `provider_oauth_registry` adapter begins the original durable
candidate before interactive work. Its owner switch closes and joins login work,
then cancels only that original pending candidate. Completing login constructs an
exact registry identity, grant and protected material before committing the
candidate. The identity keeps the operator expectation's configured required
scopes; the grant separately retains every verified granted scope and its original
presence and provenance. Extra grants do not redefine identity requirements.
Missing requirements still reject publication, and exact expectations retain
account and subject equality. Immediately before commit, a required trusted
nonyielding callback checks current operator ownership and scopes. Revocation returns typed `Denied`
without activation; close cancels the original candidate and preserves the active
credential. A commit whose publication may have happened remains uncertain for
explicit reconciliation; it does not retry the exchange or create a new operation.
The existing active credential remains authoritative during acquisition.

Renewal restores the provider's protected continuity from the exact committed
identity and grant. It runs only after registry rotating intent admission, honors
the supplied switch and maps definitely unsent, authoritative rejection,
possibly consumed and verified outcomes without publishing itself. Dispatch
leases require the actual admitted identity to equal the requested identity before
borrowing access material. Admission owner, epoch, currentness and credential
revision guards survive the additional profile guards composed by the host bridge.

The adapter native-registry regression sources cover restart and refresh,
cancellation and concurrent completion/close, original-candidate cleanup, foreign
identity pairing, and lost publication acknowledgment. All 21 focused OAuth
protocol, flow and native-registry expect cases passed strict comparison in the
composed host build. These checks use controlled external exchanges; real
device registration, expiry and renewal remain live qualification requirements.
