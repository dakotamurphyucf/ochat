# `Task_intf` — Minimal task description record

`Task_intf` provides the smallest possible data-structure that still fully
describes *what* the agent is expected to do.  It is designed to be:

* **Easy to serialise** – it is a plain OCaml record containing only
  primitive types.
* **Stable** – the type is unlikely to change even when higher-level
  layers (planners, schedulers, UI components …) evolve.
* **Self-contained** – the module ships with helper functions so callers
  do not have to know the exact Markdown formatting conventions used by
  the meta-prompting layer.

Although it lives under `lib/meta_prompting/`, the record can be created
and manipulated by any part of the code-base.  The module is *not*
coupled to the rest of the meta-prompting pipeline.

---

## Type definition

```ocaml
type t = {
  description : string;      (* required *)
  context     : string option; (* optional *)
  tags        : string list;   (* optional, default [] *)
}
```

Field semantics

* **`description`** – human-readable description of the task to perform.
  The string *should* be non-empty; the module does not enforce the
  invariant but downstream components might.
* **`context`** – additional background information.  When `None` or an
  empty string, the block is omitted from rendered Markdown.
* **`tags`** – free-form tags used for routing or prioritisation.  The
  list may be empty.

---

## API

### `make`

```ocaml
val make : ?context:string -> ?tags:string list -> string -> t
```

Creates a new task.

Default values:

* `context` – `None`
* `tags`    – `[]`

#### Example

```ocaml
open Meta_prompting

let task =
  Task_intf.make
    ~context:"The repository is large; focus on src/."
    ~tags:[ "refactor"; "high-prio" ]
    "Rename module X to Y everywhere"

(* [task] now contains:
   { description = "Rename module X to Y everywhere";
     context     = Some "The repository is large; focus on src/.";
     tags        = ["refactor"; "high-prio"] } *)
```

### `to_markdown`

```ocaml
val to_markdown : t -> string
```

Renders a task into a Markdown fragment following this layout (blocks are
omitted when empty):

```
## Task
<description>

### Context
<context>
Tags: tag1, tag2
```

#### Example

```ocaml
let md = Task_intf.to_markdown task in
print_endline md;
(*
  ## Task
  Rename module X to Y everywhere

  ### Context
  The repository is large; focus on src/.
  Tags: refactor, high-prio
*)
```

---

## Usage tips

1. **One task, one intent** – keep `description` focused; split large or
   multi-step requests into several tasks so that the planner can
   schedule them independently.
2. **Avoid Markdown formatting** in `description`/`context`; the helper
   does not escape special characters.
3. **Use tags consistently** – downstream tooling often relies on exact
   string matching.

---

## Limitations & Known issues

* **No validation** – the module trusts the caller.  Garbage in,
  garbage out.
* **No Markdown escaping** – injection is possible if untrusted input is
  passed directly.
* **No ordering guarantee for tags** – the list is rendered in the order
  provided by the caller.

---

## Change log

* 2025-08-05 – Initial documentation extracted and expanded.

