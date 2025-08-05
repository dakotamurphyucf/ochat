# `mp-refine-run`

Recursive meta-prompt refinement from the command line

---

## 1  Purpose

`mp-refine-run` is a developer utility that repeatedly improves a draft
LLM prompt via **recursive meta-prompting**.  It is the CLI counterpart of
the higher-level OCaml helpers exposed by [`Mp_flow`](../../lib/meta_prompting/mp_flow.ml).
Instead of calling the library directly you can simply pass two Markdown
files and let the tool handle the optimisation loop, evaluator calls, model
selection and logging.

Typical scenarios include:

* turning a textual task description into a high-quality *system prompt*;
* refining an existing prompt based on reward-model feedback;
* iteratively polishing OpenAI *function-calling* tool descriptions.

## 2  Quick start

Generate a brand-new assistant prompt:

```bash
$ mp-refine-run -task-file task.md > prompt.txt
```

Update an existing tool description and append the refined version in-place:

```bash
$ mp-refine-run \
    -task-file translator_task.md \
    -input-file  draft_tool.md     \
    -output-file draft_tool.md     \
    -action      update            \
    -prompt-type tool
```

## 3  Command-line reference

| Flag               | Description                                                               |
|--------------------|---------------------------------------------------------------------------|
| `-task-file FILE`  | Markdown file that describes **what** you want the model to do.            |
| `-input-file FILE` | Draft prompt to start the refinement loop with (optional).                |
| `-output-file FILE`| If provided, the refined prompt is **appended** to the given file.         |
| `-action ACTION`   | `generate` (default) \| `update` – maps to `Context.Generate/Update`.     |
| `-prompt-type TYPE`| `general` (default) \| `tool` – switches evaluator rubric and templates.  |

All flags except `-task-file` are optional.  Omitting `-output-file` prints
the prompt to *stdout*.

## 4  How it works (high-level)

1. The two input files are read by `Io.load_doc` so that path capabilities
   are respected.
2. Flag values are turned into the corresponding `Context` variants.
3. Depending on `-prompt-type` either `Mp_flow.first_flow` (general) or
   `Mp_flow.tool_flow` (tool) is invoked.
4. `Mp_flow` runs a fixed-length iterative loop:
   * A *proposer* LLM (default **GPT-4o**) rewrites the prompt.
   * A reward model evaluates the candidate using the rubric selected in
     step 2.
   * A Thompson bandit keeps the statistically best candidate so far.
5. The final prompt is returned to the CLI layer and persisted / printed.

All network IO happens inside `Io.run_main`, which wraps `Eio_main.run` and
initialises the Mirage-crypto RNG required by `tls-eio`.

## 5  Environment variables

* `OPENAI_API_KEY` – required so that the helper libraries can call the OpenAI
  HTTP endpoints.

## 6  Exit codes

| Code | Meaning                                     |
|------|---------------------------------------------|
| 0    | Success                                     |
| 1    | Invalid flag value (unknown action / type)  |

## 7  Known limitations

* The reward-model RPCs are synchronous and may take several seconds for
  large prompts.
* Very large tasks or prompt bodies (> 20 000 tokens) will be truncated by
  the OpenAI back-end.
* The program intentionally appends to `-output-file` instead of overwriting
  it – remember to clear the file if you only want to keep the most recent
  result.

## 8  See also

* [`Mp_flow`](../../lib/meta_prompting/mp_flow.ml) – implementation details of
  the refinement loop.
* `mp-prompt` – interactive REPL for rapid experimentation with meta-prompting.
* `odoc-search` – semantic search over OCaml documentation, often used as an
  auxiliary tool during prompt engineering.

