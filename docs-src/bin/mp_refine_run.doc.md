# mp-refine-run: generate and improve prompts

Use this command to generate prompt packs or refine an existing prompt/tool
description. It offers local template generation, online prompt-factory work,
and classic recursive meta-prompting—not a single fixed refinement strategy.

## Start here

With Ochat installed, write a task description in `task.md`:

```sh
mp-refine-run -task-file task.md -output-file generated-prompt.md
```

The default strategy may call models and incur charges. To use the local
template factory without model calls:

```sh
mp-refine-run -task-file task.md -meta-factory true -output-file local-pack.md
```

Output is **appended**, not replaced, when `-output-file` is supplied. Use a
fresh file for a separate result. Without that flag, output goes to stdout.

## Options

| Flag | Default | Meaning |
|---|---|---|
| `-task-file FILE` | Absent | Task description. Recommended; despite the help saying required, the parser accepts omission and uses empty task text. |
| `-input-file FILE` | Absent | Existing prompt to iterate; absence selects creation paths. |
| `-output-file FILE` | Absent | Append result to this file; otherwise print it. |
| `-action generate\|update` | `generate` | Action supplied to the recursive flow. |
| `-prompt-type general\|tool` | `general` | Select general/tool behavior where the selected strategy distinguishes it. |
| `-meta-factory BOOL` | `false` | Local template factory; takes precedence over the other strategy flags. |
| `-meta-factory-online BOOL` | `true` | Enable online factory strategy. |
| `-classic-rmp BOOL` | `false` | Disable online factory mode; does not override `-meta-factory true`. |

These booleans take explicit `true` or `false` arguments.

## Strategy selection

1. With `-meta-factory true`, call `Prompt_factory.create_pack` without an
   input file or `iterate_pack` with one. This is local template generation.
2. Otherwise online mode is enabled only if `-meta-factory-online true` and
   `-classic-rmp false`.
3. With no input file and online mode enabled, try
   `Prompt_factory_online.create_pack_online` with the GPT-5 model constructor.
   If it returns no result, fall back to the general recursive flow with online
   factory mode disabled.
4. Other cases select `Mp_flow.first_flow` or `tool_flow` according to prompt
   type, passing the chosen online setting.

For example, refine a tool description using the classic strategy:

```sh
mp-refine-run -task-file task.md -input-file draft-tool.md \
  -prompt-type tool -action update -classic-rmp true \
  -output-file refined-tool.md
```

The binary does not expose a universal iteration, model, or spending-cap flag.
Review the selected library strategy before automating paid runs. Invalid
action/type values explicitly exit 1; parsing, file, or provider failures may
also exit nonzero. There is no documented fixed 20,000-token truncation contract.

## References

- [Meta-prompting library](../lib/meta_prompting.doc.md): evaluators and recursive flows.
- [Prompt factory](../meta_prompting/templates.doc.md): prompt-pack templates.
- [Provider environment](../agent-server/environment.md): component-specific settings.
- [Implementation](../../bin/mp_refine_run.ml): exact strategy dispatch.
- [Command index](README.md).
