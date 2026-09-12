# Installed authoring reference sources

The [Authoring_sources library](../../lib/authoring_sources/authoring_sources.mli)
provides offline source material for the authoring corpus. The build embeds shared
ChatML, ChatMD and extensibility reference documents directly from `docs-src`.
The installed library therefore needs no repository checkout, file-read tool,
network access, vector store or provider request to retrieve those documents.

The OCaml function `installed ()` returns the embedded documents and the
[compiler-owned signature inventories](chatml-surface-inventory.md), plus the
exact regular productions from the compiled ChatML parser.
`document` accepts an exact `docs-src`-relative path; it does not normalize paths,
read the filesystem or fetch another revision. `signatures` selects one exact
compiler surface and preserves its entrypoint contract. It does not combine
surfaces or install their operations. The build rule's explicit document list is
the source inventory; adding a maintained reference requires updating that list.

Format version 2 assigns each document a SHA-256 digest of its exact bytes, each
compiler surface a digest of its structural inventory, and each grammar
production a digest of its left/right symbols and semantic action. The bundle
identity hashes the format version and sorted document, surface and production
contracts. Changing prose, examples, membership, a builtin signature or parser
production therefore changes the source identity. The structural `manifest`
exposes these hashes without repeating document bodies or compiler action code.

The build reads Menhir's `.cmly` metadata through the build-only `menhirSdk`
dependency and embeds ordinary OCaml data. Installed execution does not load
Menhir SDK, read a `.cmly` file or invoke a generator. `grammar` includes all
regular productions, including structural, explicit rejection and never-reduced
branches; it is not a list of programs that necessarily parse or typecheck.
Production numbers and source locations are excluded from the contract. Lexer
rules, precedence/conflict resolution, helper implementation and type inference
are separate semantic review obligations.

This identity describes reference source material. A retrieval service must also
bind the installed runtime build/contract, actual target surface, effective tool
capabilities and resolved authoring policy. The bundle alone does not supply
audited topic coverage, task packages, paging, token budgets or model-context
insertion. Documents retain their implementation and qualification notes; embedding
them does not enable experimental features or mark their examples qualified on
every host. Custom conventions must not replace official semantics.

[Topic assembly](authoring-topic-corpus.md) builds source-pinned fragments and
prerequisite closures on this bundle; complete task packages and service policy
remain separate work.

The [offline executable fixture](../../test/authoring_sources/offline.ml)
runs from `/`, outside the checkout, and retrieves the embedded references and
target-specific entrypoint signatures. The documentation gate compares every
embedded body with its maintained source and checks the
[OCaml-differences examples](chatml-ocaml-differences.md) against the real compiler
and interpreter. The [child-session reference](chatml-authoring-children.md)
also checks a complete captured creation request against the real request decoder
and generated-definition validator, without creating a session. Broader semantic
coverage and authoring-service qualification
remain separate from this source-packaging check.
