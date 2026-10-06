# Targeted design validation

Run `python3 design/och-49/validation/run.py` from the worktree. The harness copies the design-only declarations and probe into this worktree's ignored scratch validation project. It compiles Core/Jsonaf semantic declarations and Eio resource seams with ppx_jane, then runs six inline expect probes using ppx_expect/ppx_jsonaf_conv. No production library, switch/package/default mutation, model request, credential read or service startup is required.

The independently supplied `{}`, `{"field":null}` and `{"field":"value"}` inputs check decode acceptance and exact emitted key policy for optional non-null, required nullable and required non-null records. Two independent decode-state/exact-encoder checks prevent matching round-trip mistakes. An additional constructed input demonstrates that allow_extra_fields drops an extra field. These are evidence about the installed generator, not implementation of the proposed universal converter or triple-presence record codec. M1-T02/M1-T03a must separately test semantic decode state, exact emission and provider/profile-specific eligibility; a derived presence variant alone is not a record omission codec.

Checks run 2026-10-06:

- `opam exec --switch=default -- ocamlc -version`: 5.3.0.
- `opam exec --switch=default -- dune --version`: 3.21.1.
- `opam exec --switch=default -- ocamlformat --version`: 0.28.1; design-local version/profile pinned without changing root configuration.
- `opam exec --switch=default -- ocamlfind query -format '%p %v' core jsonaf ppx_jsonaf_conv ppx_jane ppx_expect eio`: Corev0.17.1, Jsonafv0.17.0, ppx_jsonaf_convv0.17.0, ppx_janev0.17.0, ppx_expectv0.17.2; Eio metadata has no version string.
- Isolated `dune build @all` and `dune runtest`: passed; two interface declarations compile and all six expect probes match manually supplied expectations.

First validation exposed ambiguous documentation comments and the probe's missing Jsonaf.Export open; both were corrected without suppressing warnings or promoting expectations. No production bug is claimed from those draft errors.

The authorized existing default toolchain matches the compiler but differs from .github/ci-toolchain.json's newer Dune/Core/PPX patch versions. These are local checks, not qualification of that locked CI environment. Downstream codec implementation must repeat generator acceptance with its actual supported package set. Do not install or modify a shared switch to hide this difference.

Limitations: compile checking establishes signature consistency, not implementation, soundness or stream/durability behavior. Source scenario walkthroughs are static evidence. No schema conversion, checkpoint failure, network/WS/auth, native execution, real restart or full-repository suite was run; those depend on the corresponding implementation tickets. Focused design checks do not mark M1 complete.
