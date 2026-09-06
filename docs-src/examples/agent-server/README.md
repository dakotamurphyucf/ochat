# Tracked agent-server examples

Run commands from the repository root with its opam environment active. The
OCaml example uses Core and Eio and compiles with the repository libraries.
No credentials or daemon state are stored here.

```sh
dune build bin/chat_tui.exe bin/ochat_agent_server.exe bin/ochat_agent_stdio.exe \
  docs-src/examples/agent-server/clients/docs_example.exe
OCHAT_DEMO=$(mktemp -d /tmp/ochat-docs.XXXXXX)
dune exec docs-src/examples/agent-server/clients/docs_example.exe -- \
  setup "$OCHAT_DEMO" gpt-5.6-sol
```

Keep the printed directory path. Setup refuses a nonempty directory and creates:

| File | Purpose |
|---|---|
| `hello.chatmd` | Tool-free prompt using the selected model; calls a provider only when work is requested. |
| `workspace/` | Empty physical workspace. |
| `unix.sexp`, `http.sexp` | Validated configs sharing one store/socket; run only one daemon at a time. HTTP binds 127.0.0.1:8787. |
| `admin.token` | Generated raw credential for the tutorial operator. Keep private. |
| `observer.token` | Different generated credential with transcript-read scope only. |
| `tokens.sexp` | Hashed credentials for the same principal with different scopes; not plaintext bearer tokens. |
| `admin.curl`, `observer.curl` | Private curl configs containing the respective raw bearer header; avoid tokens in process arguments. |
| `initialize.json` | Typed-codec-generated handshake envelope for raw HTTP use. |

The helper uses exclusive mode-0600 file creation. `mktemp` supplies the private
directory. The tutorial operator token has broad scopes for demonstrating
administration; narrow them for an actual deployment. Tokens have no expiry in
this disposable fixture. Configure expiry/rotation for normal use.

The observer shares the operator's principal identity because session visibility
is creator-scoped. Its token has only transcript-read scope. It must initialize
its own logical connection; reusing the operator connection ID is rejected.
Regenerate older fixtures that assigned different principals to these tokens.

`clients/discover.ndjson` is a complete initialization and catalog/session
discovery stream. It does not send model messages. Validate it with:

```sh
dune exec docs-src/examples/agent-server/clients/docs_example.exe -- \
  check-requests docs-src/examples/agent-server/clients/discover.ndjson
```

`docs_example client URI [TOKEN_FILE]` is a compiled full-duplex embedding example
equivalent to the stock stdio gateway. It accepts request envelopes on stdin and
forwards responses/notifications to stdout, diagnostics to stderr. EOF detaches
the connection. Initialization is supplied by the client, not silently injected.

See the [tutorials](../../agent-server/README.md). The static
[hello prompt](prompts/hello.chatmd) is also runnable directly. Model calls require
your credentials and incur provider charges; fixture setup/discovery does not.
After stopping the daemon and all clients, archive or remove only your recorded
temporary tutorial directory if you no longer need its state. Never target the
repository, home directory, or normal Ochat store for cleanup.
