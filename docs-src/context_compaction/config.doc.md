# `Context_compaction.Config`

Runtime parameters that control how the **context-compaction** pipeline
filters and trims a chat conversation.

---

## Overview

When pruning a long chat we want to keep as much of the useful history
as possible while staying within the model’s context window.  The
library therefore exposes two knobs:

* **`context_limit`** – maximum number of *tokens* the relevance judge
  is allowed to consider when deciding which messages to keep.
* **`relevance_threshold`** – minimum importance score on the unit
  interval **[0, 1]** a message needs in order to survive filtering.

`Config` stores those values in a record and provides a helper to load
user overrides from a JSON file.  If the file is missing or malformed
the module falls back on conservative defaults:

```ocaml
let default =
  { context_limit = 20_000;
    relevance_threshold = 0.5 }
```

The design goal is to remain *offline-friendly*: the module never
attempts network or file-system access unless the embedding
application explicitly grants the necessary Eio capabilities.

---

## JSON schema

The configuration file is a single JSON object with the following
optional keys:

```jsonc
{
  "context_limit": 4096,          // integer ≥ 0
  "relevance_threshold": 0.75     // float   ∈ [0, 1]
}
```

Unknown keys are ignored so that future versions can add more
parameters without breaking older setups.

### Search path

`Config.load` inspects the following locations in order and returns the
first file that parses successfully:

1. `$XDG_CONFIG_HOME/ochat/context_compaction.json`  
   *(falls back to `$HOME/.config/…` when `XDG_CONFIG_HOME` is unset)*
2. `$HOME/.ochat/context_compaction.json`

Failure to read or parse a location simply moves on to the next one;
the function never raises.

---

## Public interface

### `type t`

```ocaml
type t = {
  context_limit : int;
  relevance_threshold : float;
}
```

Record of all tunable parameters.

### `default`

```ocaml
val default : t
```

Built-in configuration shown in the Overview section.

### `load`

```ocaml
val load : unit -> t
```

Returns the merged configuration obtained by overlaying the first valid
JSON file found in the *search path* onto `default`.

---

## Usage examples

### Initialising the pipeline with user settings

```ocaml
open Context_compaction

let cfg = Config.load () in

let keep_message msg =
  Relevance_judge.is_relevant
    ~env:stdenv                (* Eio capabilities *)
    cfg                        (* uses both fields *)
    ~prompt:msg
in
...
```

### Tightening the relevance filter programmatically

```ocaml
let strict_cfg =
  { Config.default with relevance_threshold = 0.8 } in

(* apply strict_cfg to the compaction pipeline … *)
```

---

## Behaviour in offline mode

`Config` itself is pure and does not depend on the presence of an
`Eio_unix` environment.  Nevertheless, the values you choose directly
affect how other modules behave when network access is unavailable:

* A **low** `relevance_threshold` combined with the fallback score of
  **0.5** will keep most messages.
* A **high** threshold will discard them instead.

Choose threshold values carefully when writing unit tests that run in
CI.

---

## Known limitations

1. **No validation of extreme values** – numbers outside their natural
   range can be provided and will propagate downstream unchecked.  This
   is deliberate to avoid breaking existing setups but may change in
   the future.
2. **`read_file_if_exists` placeholder** – the current implementation
   always returns `None`, effectively disabling user overrides until
   the IO layer is integrated.  The API is stable and will not change
   when real IO is implemented.

---

## Change log

* **v0.1** – Initial scaffolding: in-memory defaults, JSON overlay,
  placeholder file IO.

