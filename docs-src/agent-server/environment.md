# Environment and host settings

Set runtime variables in the **host process** environment before launch. A remote
client's environment does not configure the daemon or change its paths. Do not
print credentials during troubleshooting. Shell command environments are separately
compiled from declarations and filtered by shell policy; inheriting a variable in
the daemon is not permission to disclose it to a tool.

## Agent host and provider

| Variable | Consumer and behavior |
|---|---|
| `OPENAI_API_KEY` | OpenAI provider credential; opt-in typeahead in every TUI mode requires a nonblank key in the client process. Suggestions default off and never borrow daemon credentials. No server authentication authority. Loaded by the provider module at process initialization; restart after changes. |
| `API_URL` | Responses endpoint host, default `api.openai.com`; the transport appends `/v1/responses`. For direct OpenAI use set `API_URL=api.openai.com` explicitly if your shell normally uses a proxy. Explicit origins are also accepted by the underlying transport. See its TLS limitation in [security](permissions-and-security.md). |
| `OCHAT_OPENAI_IDLE_TIMEOUT_SECONDS` | Stream idle timeout, not total turn deadline: default 600 seconds, positive finite values capped at 3600; invalid values fall back to 600. |
| `OCHAT_STREAM_TIMEOUT_SECONDS` | Compatibility fallback only when the more specific idle-timeout variable is absent. |
| `HOME` | Home expansion, cache/config defaults and `${home}`. An explicit embedding path can override the relevant context. |
| `PWD` | Launch-directory capture in executable adapters; no remote workspace override. Use a consistent physical cwd and environment. |
| `TMPDIR` | Embedded transient data-root base, default `/tmp`; it is not the configured daemon temporary-workspace catalog. |

Sources: [response provider](../../lib/openai/responses.ml),
[foreground timeout](../../lib/chat_response/agent_response_loop.ml),
[stream timeout](../../lib/chat_response/in_memory_stream.ml),
[embedded host](../../lib/agent_server/embedded.ml), and
[runtime builder](../../lib/agent_session/runtime_builder.ml).

## TUI configuration and diagnostics

`OCHAT_CHAT_TUI_CONFIG` selects a TUI config file; `XDG_CONFIG_HOME`/`HOME` supply
default search locations. Explicit flags/config/no-config precedence and every
host-specific flag are in the [TUI executable reference](../bin/chat_tui.doc.md).

| Variable | Meaning |
|---|---|
| `OCHAT_TUI_FPS` | Redraw rate, default 30; parsed rates are at least 1. This is not an agent throughput control. |
| `OCHAT_TUI_ASCII` | `1`, `true`, or `yes` selects ASCII shell borders. |
| `OCHAT_WRAP_SLOP_CELLS` | Low-level layout adjustment; see [wrapping utility](../../lib/chat_tui/util.ml), not a server setting. |
| `OCHAT_STREAM_BATCH_MS` | Legacy streaming presentation batching, default 12ms, parsed values clamped to 1–50ms. |
| `OCHAT_GRAMMAR_DIR` | Additional syntax grammar discovery location; see [discovery](../../lib/chat_tui/highlight_grammar_discovery.ml). |
| `OCHAT_TUI_STARTUP_TIMING`, `OCHAT_TUI_RENDER_METRICS` | Opt-in diagnostic instrumentation; see [app](../../lib/chat_tui/app.ml) for accepted values/output. |
| `OCHAT_TUI_SCROLL_TRACE` | Opt-in scroll trace destination/control; see [trace implementation](../../lib/chat_tui/live_scroll_trace.ml). Treat trace artifacts as potentially sensitive. |

## Shell host compatibility settings

These are consumed by hosts that invoke the existing shell policy/security
loaders. They do **not** replace daemon permission profiles/operator grants or
add unimplemented stock-daemon reviewer hooks. Follow the
[host matrix](../guide/chatmd-shell-host-integration.md).

| Variable | Meaning |
|---|---|
| `OCHAT_SHELL_ADMIN_POLICY` | JSON administrative policy path; absent means the loader's permissive policy, not automatic manifest authorization. |
| `OCHAT_SHELL_TRUSTED_SOURCES` | Trusted-source policy file; required when that policy requires source verification. |
| `OCHAT_SHELL_REPOSITORY_IDENTITY` | Optional identity bound into source verification/legacy host context. |
| `OCHAT_SHELL_MANIFEST_SIGNATURE`, `OCHAT_SHELL_MANIFEST_PUBLIC_KEYS` | Signature and public-key file pair. Supplying only one is an error. |
| `OCHAT_SHELL_SIGNATURE_AUDIENCE` | Expected signature audience, default `ochat`. |
| `USER` | Legacy host user identity metadata, not authenticated daemon principal identity. |
| `PATH` | Host executable resolution input where applicable; shell declarations may narrow/replace search paths. Prefer explicit trusted executable paths. |

Sources: [administrative loader](../../lib/shell_runtime/admin_policy_loader.ml),
[manifest verification](../../lib/shell_runtime/manifest_security.ml), and
[shell security guide](../guide/chatmd-shell-security.md).

## Other features and test-only controls

`EMBEDDINGS_HOST` and `EMBEDDINGS_MODEL` configure the embedding/search feature;
`OPENAI_EMBEDDINGS_STUB` selects its stub behavior, not the agent Responses
provider. Missing embedding credentials also select test vectors; see
[embedding configuration](../guide/search-and-indexing.md#embedding-configuration).
`CI` and `OAUTH_NO_BROWSER` suppress automatic browser launch in the interactive
OAuth PKCE helper by presence, not by truthy value; they do not bypass its
callback wait. See [PKCE limitations](../lib/oauth/oauth2_pkce_flow.doc.md).
Test-runner opt-ins,
cost guards, artifacts and soak controls belong to [testing](testing.md), not
production daemon configuration. Do not export test controls indiscriminately
into an operational service.
