# ochat chat-completion — managed headless execution

`ochat chat-completion` runs an ordinary conversation or a versioned ChatML
moderator through the native embedded host. The authored ChatMD definition and
managed conversation are separate. Execution keeps its captured code and provider
selection; an explicit transcript export contains the complete authorized
conversation history, rather than another executable definition.

Complete [installation and provider setup](../agent-server/quickstart.md) first.
Model requests require credentials and incur provider charges. The examples below
assume the repository root and the selected opam switch. This CLI owns its local
host; it does not attach to a remote daemon or use the legacy `ochat shell` store.

<a id="130-second-smoke-test"></a>

## Run an ordinary request

Copy the tracked, tool-free [hello prompt](../examples/agent-server/prompts/hello.chatmd)
and add the initial request to that separate definition:

```sh
OCHAT_BATCH=$(mktemp -d /tmp/ochat-batch.XXXXXX)
cp docs-src/examples/agent-server/prompts/hello.chatmd "$OCHAT_BATCH/definition.chatmd"
printf '%s\n' '<user>Greet a new Ochat user in one sentence.</user>' >> "$OCHAT_BATCH/definition.chatmd"
dune exec bin/main.exe -- chat-completion \
  -prompt-file "$OCHAT_BATCH/definition.chatmd" \
  -storage-root "$OCHAT_BATCH/store" \
  -output-file "$OCHAT_BATCH/transcript.chatmd"
```

Adjust the model in the private definition if your provider requires it. A
successful ordinary run exports the initial messages and the completed response.
Response wording and provider IDs vary. The definition remains unchanged.

Actual session IDs, managed run IDs when applicable, and bounded live draft/tool
observations are printed on stderr. Observations can be partial, replaced or
contain gaps; they are not the final transcript. Stdout is reserved for the
complete canonical export when `-output-file` is omitted or `/dev/stdout`.

## Select execution mode

`-run-mode single-turn` is the default. Ordinary and legacy prompts use their
actual foreground operation in this mode. `-run-mode workflow` requires a real
versioned moderator and waits for its managed run terminal, rather than ending
at the first completed model operation. It never silently becomes Single_turn. This rejection precedes run admission
and provider work; independently admitted session creation can already initialize
its configured hooks.

For a versioned moderator, a fresh conversation admits its genuine authored
startup in the selected mode. Single_turn can complete a successful inert startup
without inventing a user message; Workflow remains governed by the moderator's
actual continuation and finish decisions. Failed callbacks settle the actual run
with durable failure evidence. See [managed runs](../guide/chatml-authoring-runs.md).

## Resume captured execution

Reuse the reported session ID with the same storage root. A retained versioned
moderator accepts explicit new input:

```console
$ ochat chat-completion -prompt-file /work/prompts/definition.chatmd \
    -storage-root /work/private/agent-store -session-id SESSION_ID \
    -message 'Review the next change.' -run-mode single-turn \
    -output-file /work/private/next-transcript.chatmd
```

`-prompt-file` remains a required selector coordinate. Retained execution uses its
stored definition, invocation role and provider target even if the original file
has disappeared. It does not adopt edits from the current file. A consumed
versioned startup is never replayed just because `-session-id` was supplied: use
`-message` for genuine new input or `-run-id` for the original run's evidence.
Ordinary/legacy explicit `-message` is rejected; their initial input comes from
the authored definition or an explicitly imported transcript.

If a retained versioned session is stopped, explicit input first activates it
against the selected position, joins that retained runtime's actual resume
callback, then selects a new admission anchor for the appended message. The
current configuration at entry to that second phase is retained across the join;
source, generation, attachment or configuration changes during it conflict.
History changes committed by the authorized resume callback are expected.
There is no failed-command retry with a newer position.

To inspect an original managed run, pass both `-session-id` and its reported
`-run-id`, with the same `-run-mode` that admitted it. This observes the authorized
original receipt and terminal without starting another run or reloading source.
Unavailable evidence does not authorize rerunning it.

## Rebuild or import deliberately

`-rebuild -session-id SESSION_ID` adopts the current catalog definition into an
**already stopped** retained conversation. The command does not silently stop an
active session. A missing grant or invalid configured graph rejects before
initialization. Rebuilding is distinct from ordinary captured resume.

`-import-transcript FILE` creates a new managed conversation from an edited
ChatMD transcript and the separately selected definition. It cannot be combined
with `-session-id`. Import uses the existing bounded ChatMD conversion once;
executable declarations, imports, document directives, embedded agents and local
image reads are rejected. This is authored message interpretation, not exact
replay of opaque tool metadata, private runtime state or prior command receipts.
See [native transcript import](../agent-server/tutorials/local-tui.md).

A lost create/seed reply is reconciled using its original command key. Failed or
cancelled execution also reads newly retained original command receipts once before
closing the host. It reports their method/key, session and storage coordinates;
an authentic accepted-run receipt reports the original Run ID. Unknown outcomes
remain explicit. Do not repeat admission with a new key.

<a id="6-root-scoped-file-reads"></a>

## Source and project ownership

The launch directory is the workspace/tool directory. Definition-relative
imports, script `src`, documents and configured project declarations retain their
actual original parser/source context. Choosing a transcript output elsewhere
never rebases executable dependencies to that output directory.

Workspace-local project selection uses the existing local policy. External
project source access requires an explicit administrative grant:

```console
$ ochat chat-completion -prompt-file /work/prompts/definition.chatmd \
    -project-root /work/shared-project \
    -output-file /work/private/transcript.chatmd
```

`-project-root` is repeatable; relative values use the actual launch directory.
Omitting it grants no external project roots. This flag does not grant Git
transport, native tools or arbitrary escaped script reads. Each script's actual
project, own module scope, environment and companion signature are prepared
through the existing source/project owners. Source configuration selects the
provider profile before initializers; retained sessions preserve that target.

Shell-enabled prompts retain their administrative/trust/signature and exact
complete-manifest authorization checks. The CLI never falls back to direct spawn
when a grant is missing. Use [shell inspection](shell-runtime-management.md) and
[the shell security guide](../guide/chatmd-shell-security.md); inspection's resource
hash and complete prepared execution hash have different roles.

## Export, failure and cleanup

The final transcript is rendered from a complete authorized canonical export.
It is bounded to 64 MiB of ChatMD and 256 MiB of renderer work under the existing
document limits. Live observations have separate bounded presentation and do not
replace canonical history. The export can reflect legitimate managed compaction
or history rewriting; it is not reconstructed by appending to an old file.

A file export replaces a distinct destination atomically. Its parent must already
exist. The destination cannot alias the definition or imported transcript, even
through symlinks or a removed source coordinate. A failed, cancelled, limited or
interrupted run preserves its available terminal transcript and returns a nonzero
exit; a written file alone is not evidence of success.

One subscribed handle owns notification reduction, run/operation presentation
and lease renewal. The command closes and joins it before releasing the borrowed
connection, local host and provider owners. Durable native storage defaults to
`HOME/.ochat/agent-store`, or the explicit `-storage-root`. An exported transcript
is not a backup of captured code, private checkpoints or the command ledger.
Archive or remove a private store only after all owners using it have stopped.

For interactive operation, use [the native TUI](../agent-server/tutorials/local-tui.md).
For a long-lived shared service, use [private daemon hosting](../agent-server/tutorials/unix-daemon.md).
