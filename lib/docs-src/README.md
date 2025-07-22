# Internal prose (`docs-src/`)

This directory holds free-form Markdown files that go **beyond inline
`*.mli` comments**.  They capture design notes, usage examples, historical
decisions, and any other background that helps a human (or an indexing tool)
understand the code-base.

Naming rules
------------

* Use the same basename as the module you’re describing plus the suffix
  `.doc.md`  –  e.g.

      vector_db.doc.md   → relates to `vector_db.ml` / `vector_db.mli`

* Longer, thematic docs are welcome; pick a concise slug such as
  `embedding_pipeline.doc.md`.

* Keep language plain Markdown; no special tooling required.

Scope
-----

The files are **internal**; they are not shipped in the public API docs nor in
published packages.  Feel free to include TODOs, open questions, code
snippets, or links to external resources.

When adding a new module, consider whether a side-car `.doc.md` would help
future readers.  If so, drop it here under the same sub-directory structure
as the source code.

