# Shell access across Ochat hosts

Read the [security model](chatmd-shell-security.md) before authorizing a manifest.
Four checks are distinct: manifest authority, capabilities, per-command
policy/approval, and OS confinement. Allowing one does not bypass the others.

| Host | Bootstrap and state | Administration |
|---|---|---|
| Native local TUI | Embedded default requires grants; transient process-bound host. Native `--local` does not accept legacy authorization/persistence flags. | Client Shell Security projection; use a supported legacy/daemon mode if its bootstrap needs a CLI option absent here. |
| Legacy local TUI | `--authorize-shell-manifest` authorizes the exact manifest for that process; legacy session persistence is separate. | `ochat shell` legacy-store commands and local Shell Security views. |
| Local stdio | Embedded default profile requires grants; optional durable data root does not provide a manifest-authorize CLI flag. | Protocol views; custom embedding for different bootstrap policy. |
| Daemon | Pinned permission profile and exact persisted/operator grants, or explicit `assume_authorized`/`deny`. | Protocol `permission.*`, `grant.*`, `audit.read`; connected Shell Security views. |
| File-backed completion/legacy nested host | Existing host-supplied authorization/runtime policy; do not assume a TUI approver exists. | Corresponding legacy session/runtime integration. |
| OCaml embedder | Explicit host runtime options, identity/path context, policy/reviewer hooks and persistence owner. | Host APIs; own switch/close/cancellation correctly. |

`ochat shell grants ... SESSION_ID`, manifest-grant and interrupted commands load
the legacy `Session_store`. A daemon `ses_...` ID is not a way to select the daemon
store from that CLI. Use protocol commands or connected TUI instead. Never edit
a running daemon's journals or grants files to work around authorization.

## Inspect and authorize

```sh
ochat shell inspect /absolute/path/agent.chatmd
ochat shell inspect /absolute/path/agent.chatmd -canonical
```

Inspection compiles declarations; it does not run their tools. Check executable
identities, source/import versions, variables, capabilities, policy, backend,
secret descriptors, hooks/reviewers, resource limits and audit behavior.

For legacy interactive use, after reviewing the actual prompt:

```sh
chat-tui --no-config -file /absolute/path/agent.chatmd --authorize-shell-manifest
```

Do not add `--local`: authorization is a legacy-only mode flag. A successful
bootstrap is not approval for every future command. Hard denial still wins.

For a daemon, configure [operator manifest grants](../agent-server/configuration.md#operator-manifest-grants)
or deliberately select another manifest authorization mode. Grants compare the
exact compiled source and manifest identities in the daemon runtime context.
`${workspace}` and `${session_dir}` can differ from standalone inspection; do not
paste a digest from a different context and assume it authorizes the daemon.
Inspect the pinned prompt/runtime security projection and diagnostic identities
for the intended context. There is no stock generic `grant create` CLI or
daemon manifest-grant creation RPC: bootstrap entries belong to operator config.

For a reviewed deployment, derive the bootstrap inputs as follows:

1. Inspect the exact configured root source with the daemon account/platform and
   host security environment. Copy the `requested manifest:` SHA-256 from
   `ochat shell inspect FILE -canonical`; review the accompanying canonical JSON.
2. Compute the root source bytes' SHA-256, for example `shasum -a 256 FILE` with
   the real path quoted. `source_sha256` is this root hash, not the hash of a
   ChatMD export, the artifact's manifest file, or a directory listing. Imported
   source identity also participates in the compiled manifest/revision.
3. Obtain the intended principal ID from `protocol.initialize`'s principal field
   or the operator-owned static token record. Put it in `principals`; an empty
   list deliberately authorizes any otherwise-eligible authenticated principal.
4. Add a config grant with the configured prompt/workspace names and both hashes,
   validate, then reload the catalog or start the daemon. Create/start the intended
   session with `require_grant` and verify the runtime accepts the same identity.
5. If the runtime's compiled manifest differs from the pinned artifact/inspection,
   stop and investigate source context/platform/policy. Never substitute a broad
   grant or suppress the comparison just to make startup pass.

The daemon compares the grant against `Artifact.root_sha256`, the pinned shell
manifest digest, the runtime request's manifest digest, and the session's prompt,
workspace and creating principal. This is exact identity matching, not a wildcard
grant over future edits. Redacted `prompt.get` is not a full manifest download.

`assume_authorized` is an explicit host trust decision for reviewed prompts, not
interactive grant creation and not equivalent to YOLO. Shell capability/policy
and backend checks remain. Keep this choice visible in automated examples.

## Runtime paths

`${tool_dir}` is original host launch cwd; `${workspace}` is configured execution
root. `${prompt_dir}` belongs to the root source and `${source_dir}` to the
declaring/imported file. Session/cache/home paths come from the host. See the
[complete variable table](../agent-server/sessions-and-workspaces.md#workspaces-and-paths).
Changing any security-relevant resolved path can invalidate a manifest grant.

## Approval, audit, and interruption

The shell runtime offers only allowed once/exact-session/prefix-session/durable-
exact scopes. Identity includes executable, argv boundaries, cwd/environment,
manifest and input/script content where applicable. Rewrites restart validation;
reviewers cannot override hard deny or administrative ceilings. Server reviewer
resolvers and shell reviewer chains are different extension surfaces.

Connected TUI Shell Security tabs are Overview, Runtimes, Grants, Audit and
Interrupted. Use `h/l` to switch tabs, `j/k` to navigate, `r` to refresh where
offered, and Esc to return to chat. Audit selection updates recorded details;
replay is non-executing. Remote views are redacted projections, not direct file
access. Interrupted work after restart may have unknown external effects: inspect
before retrying. See [persistence/audit](chatmd-shell-persistence-and-audit.md).

The [shell tutorial](../agent-server/tutorials/shell-agent.md) separates a narrow
tool declaration from its host authorization. Optional OS limits use linked
[child process setup](../lib/shell_access/process_spawn.doc.md); they require no
companion executable or Ochat self-execution.
