# Internal developer documentation (`docs-src/`)

This directory is **not** part of the public API surface.  It collects all
Markdown/MLD files that provide background, design notes, tutorials or other
long-form explanations for maintainers of the chatgpt code-base.

Key points
-----------

* Every file typically has the suffix `.doc.md` (or `.mld`).  The suffix makes
  it easy to ignore them in editors or glob patterns while still signalling
  “text that should be built by odoc”.

* The folder mirrors the library layout (e.g. `oauth/`, `openai/`, etc.) so you
  can locate the prose for a module quickly.

* These files are **not** fed to odoc – they’re for human eyes only.  We keep
  them outside the published documentation to avoid confusing end-users and to
  keep the public docs lean.

Authoring conventions
---------------------

1. Keep short API explanations inside the corresponding `*.mli` doc-comments.
   Place longer examples or rationale here under the same relative path.

2. Start every standalone page with a level-0 or level-1 heading so odoc
   renders a proper title, e.g.

   ```markdown
   {0 Vector DB – storage design}
   ```

3. To reference modules inside the prose, use odoc links such as

   ```markdown
   See {!Vector_db.insert} for the insertion API.
   ```

4. If you need to include code snippets, wrap them in `{[ … ]}` so odoc applies
   OCaml syntax highlighting.

5. When a document grows large, consider moving it into its own sub-folder and
   splitting with odoc headings `{1 …}`, `{2 …}` etc.  odoc automatically
   builds a sidebar table of contents.

Happy documenting!

