# Process validation and limits

All successful builds/executions used the isolated OCH-108 worktree and its own `_build`. Existing default switch was read-only: OCaml 5.3.0, Dune 3.21.1, ocamlformat 0.28.1. No installation, switch/package change, provider call, GUI startup or shared checkout mutation was performed.

## Build and checks

```sh
opam exec --switch=default -- dune build --root . --build-dir _build bin/ochat_agent_helper.exe bin/ochat_agent_server.exe bin/ochat_agent_stdio.exe bin/main.exe bin/chat_tui.exe
opam exec --switch=default -- dune build --root . --build-dir _build test/request_channel_integration_test.exe test/request_channel_probe.exe
opam exec --switch=default -- dune exec --root . --build-dir _build test/request_channel_integration_test.exe -- _build/default/bin/ochat_agent_helper.exe _build/default/test/request_channel_probe.exe
opam exec --switch=default -- dune build --root . --build-dir _build test/agent_server_e2e/agent_server_e2e.exe
```

All exited 0. Build emitted existing Menhir conflict/precedence warnings and duplicate-link-library warnings. No unrelated warnings were changed. The helper integration passed actual confined exchange, backend denial, byte limits, expired scope/revocation, cancellation/reaping, descriptor/environment isolation and independent concurrent process limits.

The first attempted build omitted `--root .`; Dune selected the enclosing project and exited 1 (“Don't know how to build bin/ochat_agent_helper.exe”). This was a setup error, not an entrypoint failure. All subsequent commands selected the worktree root explicitly.

Sixteen subprocess probes used a five-second timeout, isolated child HOME/XDG config/cwd, absent TUI config, no display, provider keys removed and `API_URL=http://127.0.0.1:1`. [Normalized outcomes](process-cases.json) retain exact argv, exit statuses, stream sizes and observed created paths. Help/version of main, TUI, daemon and stdio exited 0; current main with no arguments exits 1. Parser/mode errors and invalid/unauthorized helper invocations exited 1. Every probe observed no newly created path under its isolated roots. The helper rejected options and a valid JSON request without inherited channel descriptors.

These probes measure stream/exit behavior and observed file creation only. They do not prove absence of file reads, environment capture, native/global initialization, network attempts or transient effects outside the observed roots. The source audit finds current provider environment capture and daemon RNG startup effects. OCH-110 must qualify the extracted final artifact, including transitive initialization.

## Existing process lifecycle regressions

Run the existing harness with its artifact directory and executable paths pointing inside this worktree:

```sh
OCHAT_E2E_ARTIFACT_ROOT="$PWD/scratch/agents/codex_cli_inventory/e2e-artifacts" OCHAT_E2E_SERVER_EXE="$PWD/_build/default/bin/ochat_agent_server.exe" OCHAT_E2E_STDIO_EXE="$PWD/_build/default/bin/ochat_agent_stdio.exe" opam exec --switch=default -- dune exec --root . --build-dir _build test/agent_server_e2e/agent_server_e2e.exe -- --scenario stdio-modes
OCHAT_E2E_ARTIFACT_ROOT="$PWD/scratch/agents/codex_cli_inventory/e2e-artifacts" OCHAT_E2E_SERVER_EXE="$PWD/_build/default/bin/ochat_agent_server.exe" opam exec --switch=default -- dune exec --root . --build-dir _build test/agent_server_e2e/agent_server_e2e.exe -- --scenario daemon-smoke --case daemon.graceful-sigterm
```

Both exited 0. The harness isolates child homes/config/stores, strips provider keys and uses localhost transports. Developer-only fixture prompts do not request model inference. The ten stdio cases passed: local initialization, transient bootstrap, process-bound EOF, malformed input, oversized input, gateway Unix events, gateway HTTP events, gateway EOF detachment, invalid bearer file and stdout purity. The focused daemon SIGTERM case passed shutdown and clean restart. No probe remains running.

## Design-only check and remaining acceptance

The following independent checks each exited 0:

```sh
opam exec --switch=default -- ocamlformat --check design/och-108/run_contract.mli
opam exec --switch=default -- ocamlc -stop-after parsing -c design/och-108/run_contract.mli
```

Both JSON artifacts parsed successfully; all follow-up source paths and internal artifact links exist. Four issue refinements, one generated task and sixteen normalized process outcomes were checked. `git diff --cached --check` verifies staged ticket-owned files.

`run_contract.mli` is a proposed interface sketch, outside Dune libraries. Existing external type references were verified directly. It is formatted with repository Jane Street style and parsed with `ocamlc -stop-after parsing`; this is not a compiled production interface or semantic typecheck.

The actual final unified installed artifact does not exist yet. No check here establishes confined dispatch of that artifact, complete side-effect-free help, single-turn output semantics or ChatML-owned workflow run completion. OCH-111 must repeat confined grant checks using the final executable digest/argv; OCH-110 must test extracted startup effects; OCH-112 must exercise committed run receipts, uncertainty/reconnect and lifecycle policy through the shared actor. Current legacy transcript completion must not be mistaken for that result. Follow-up proposals transfer these completion blockers before OCH-108 closes.
