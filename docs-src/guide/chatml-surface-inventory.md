# ChatML compiler signature inventories

[Chatml_surface_inventory](../../lib/chatml/chatml_surface_inventory.mli) extracts
builtin signatures from the compiler's shared surface definitions. Reference
generators can use these signatures without evaluating a script, constructing an
agent runtime, or invoking a builtin implementation.

The `standard` function returns inventories for the core, legacy moderator, UI moderator,
seven shell surfaces, and four extensibility surfaces. Each inventory contains its
surface ID, module names, globals, module exports, type aliases and supplied
entrypoint contracts. The extensibility entrypoints come directly from
[Chatml_extension_surface](../../lib/chatml/chatml_extension_surface.mli): one-off
`main` takes one argument, standalone `run` takes two, and moderator `on_event`
takes three. `initial_state` is a value. Other hosts' entrypoint and event contracts
are documented separately; an empty entrypoint list in a builtin inventory does
not imply that a host accepts arbitrary script definitions.

The inventory preserves the differences between surfaces. For example, the
one-off and standalone surfaces have tool-mediated operations without ambient
`Process` or `Model` modules. The delegated moderator also excludes those modules,
while retaining its session-owned moderator operations. UI and shell capabilities
remain in their own surfaces. Use the actual target surface when preparing help;
combining every inventory would describe capabilities the target does not have.

`of_surface` supports a host-supplied surface and explicit entrypoint contracts.
It rejects duplicate value/module names, duplicate exports within a module,
duplicate aliases and duplicate entrypoints. Items have deterministic ordering;
reordering distinct declarations does not change the resulting metadata.

`to_json` emits version 1 structural metadata. Its `scheme_format` is
`chatml-builtin-sexp-v1`: each `scheme` is the canonical S-expression encoding of
the compiler's builtin type algebra. This is metadata for reference generators,
not executable ChatML annotation syntax. It preserves explicit argument lists,
open/closed row structure, recursive binders and constructor payloads. In
particular, `TTuple` in this representation does not advertise arbitrary tuple
expressions in the language.

Signature inventories describe compile-time availability. They do not install a
host operation, select a tool, supply permission, or enable an experimental
runtime feature. Semantic prose, operation phases, failure behavior, examples and
the model-facing authoring reference service require their own maintained coverage.
The inventories are a source for that work, not a complete authoring corpus.

See the [checked OCaml differences](chatml-ocaml-differences.md),
[language specification](chatml-language-spec.md),
[moderator runtime](chatml-moderator-runtime.md), and
[extensibility foundations](../agent-server/extensibility-foundations.md) for those
contracts. The [inventory tests](../../test/chatml_surface_inventory_test.ml)
check target distinctions, entrypoint arity and deterministic namespace handling.
