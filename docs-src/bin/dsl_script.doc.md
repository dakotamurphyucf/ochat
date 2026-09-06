# dsl_script: embedded ChatML demonstrations

`dsl_script` is an experimental demonstration executable, not a CLI for loading
a user-supplied script file. It takes no parsed command-line options and runs
hard-coded programs in [its source](../../bin/dsl_script.ml).

```sh
dune exec bin/dsl_script.exe
```

## What it runs

The current source contains five demonstrations:

1. A task-orchestration state machine with task status updates and emitted actions.
2. An event-driven task runner with attempts, completion, failure, and stop.
3. A recursive arithmetic-expression interpreter with substitution.
4. Breadth-first graph traversal over an adjacency matrix.
5. Record extension, task construction, and hashing.

Each program gets a fresh `BuiltinModules.create_default_env ()` environment.
The local `parse` helper retains source text alongside the parsed statements;
`Chatml_resolver.eval_program` checks and evaluates the result. A diagnostic is
formatted against that source on failure. Successful demos print their output
and a completion line. Treat the source as the output specification rather than
expecting the old Alice/age demonstration.

## Use ChatML in an agent

For real agent orchestration, put a moderator declaration in ChatMD and use an
appropriate host. See [ChatML workflows](../chatml/README.md) and the
[runtime guide](../guide/chatml-moderator-runtime.md).

The `parse` function in this binary is an implementation helper, not an installed
library API to access via `open Dsl_script`. Embedders should use the
[ChatML parser](../lib/chatml/chatml_parser.doc.md),
[resolver](../lib/chatml/chatml_resolver.doc.md), and host-runtime interfaces.

Return to the [command index](README.md).
