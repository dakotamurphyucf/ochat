# Dune_describe — Structured access to `dune describe`

`Dune_describe` is a thin wrapper around the `dune describe` family of
sub-commands.  Instead of having to interpret the *(canonical S-expression)*
output yourself, you receive regular OCaml records that you can traverse with
ordinary functions from `Core`.

The high-level entry-point is `Dune_describe.run`, which:

1. Spawns

   ```console
   $ dune describe external-lib-deps --format csexp
   $ dune describe --format csexp
   ```

   in the current workspace using the `Eio` process-manager obtained from the
   standard environment.

2. Parses the output of both commands using the `csexp` library.

3. Merges the two data-sets into a single `project_details` value that contains
   everything most programs care about:

   * local libraries defined in the workspace;
   * executables built by the project;
   * direct local and external dependencies for each component;
   * the source files (implementation + interface) that make up every module.

The module still exposes the *raw* representations under the sub-modules
`Dune_describe.Deps` and `Dune_describe.Item` for advanced use-cases.

---

## API overview

```ocaml
val run : < process_mgr : _ ; .. > -> project_details
```

### `type project_details`

```ocaml
type project_details = {
  local_libraries : local_lib_info list;
  executables     : executable_info list;
}
```

*Each of the auxiliary record types (`local_lib_info`, `executable_info`,
`module_info`) is defined in `Dune_describe.mli` and documented inline.*

### Example — dumping external dependencies

```ocaml
open Core

Eio_main.run @@ fun env ->
  let details = Dune_describe.run env in
  List.iter details.local_libraries ~f:(fun lib ->
    printf "• %s → %s\n"
      lib.name
      (String.concat ~sep:", " lib.external_dependencies))
```

**Sample output**

```text
• chatgpt.io → eio, jsonaf, cohttp
• chatgpt.vector_db → owl, bm25, core
```

---

## Design notes

* The `_build/<context>` prefix returned by `dune describe` is replaced with
  the path of the workspace root.  This means that paths in the returned
  records always point to the *source* files rather than the build artefacts.

* All the heavy lifting (parsing and merging) happens synchronously in the
  calling fiber. if necessary the work could be off-loaded to a helper domain via
  `Eio.Domain_manager`.

---

## Limitations & future work

* **No transitive closure** — only direct dependencies are reported.  If you
  need full dependency graphs you will have to traverse recursively.

* **No support for promotion rules** (`(rule (action (with-outputs…)))`) or
  alias stanzas.  These are ignored by `dune describe` itself and therefore
  not available to the library.

* **Dune version coupling** — the parser understands the output format used by
  `dune ≥ 3.5`.  If the upstream format changes the library will raise at
  runtime until fixed.

Please open an issue or submit a PR should you encounter any of the above.

