# `Package_index` – Coarse-grained package selector for ODoc search

`Package_index` provides a very small **vector database** that maps OPAM package
names to the OpenAI embedding of (roughly) the first paragraph of the package’s
documentation.  At run-time the index lets you quickly filter the thousands of
packages present on an ODoc mirror down to a handful that are relevant to the
user’s query.  The heavy lifting for fine-grained ranking is subsequently
handled by `Vector_db` / `BM25`, but operating on a much smaller set.

Because the index contains only one vector per package, it remains tiny (≈200 kB
for the complete public opam archive).  Persistence relies on the fast
`bin_prot` serialisation format and uses the Eio-compatible helpers from
`Bin_prot_utils_eio`.

---

## API overview

| Function | Purpose |
|----------|---------|
| `build` | Ask OpenAI for embeddings and build an in-memory index |
| `query` | Return the *k* closest package names to a query vector |
| `save` / `load` | Persist the index to `package_index.binio` |
| `build_and_save` | Convenience `build` → `save` combo |

See the inline interface documentation in
[`package_index.mli`](./package_index.mli) for complete type signatures.

---

## Usage examples

### Build an index once and save it

```ocaml
open Eio_main
open Chatochat.Package_index  (* the public name declared in dune *)

let () =
  Eio_main.run @@ fun env ->
  (* 1. Gather (package, blurb) pairs – here we hard-code two. *)
  let descriptions =
    [ "eio", "Effects-based, structured concurrency library";
      "core", "Industrial-strength alternative to stdlib from Jane Street" ]
  in

  (* 2. Build the index and persist it next to our docs. *)
  let _idx =
    Package_index.build_and_save
      ~net:env#net
      ~descriptions
      ~dir:(Eio.Path.cwd env)
  in
  ()
```

### Query from a long-running process

```ocaml
let handle_user_query env query =
  (* Make sure the index is loaded – build it if necessary. *)
  let dir = Eio.Path.getcwd env in
  let idx =
    match Package_index.load ~dir with
    | Some idx -> idx
    | None ->
      (* Fallback – never happens if the index is created offline. *)
      Package_index.build_and_save
        ~net:env#net
        ~descriptions:[ (* … *) ]
        ~dir
  in

  (* Embed the user query through OpenAI. *)
  let vec =
    let response = Openai.Embeddings.post_openai_embeddings env#net ~input:[ query ] in
    Array.of_list (List.hd response.data).embedding
  in

  (* Ask for the 5 most relevant packages. *)
  Package_index.query idx ~embedding:vec ~k:5
```

Expected output (with the toy index built above):

```text
["eio"]
```

---

## Implementation notes

* **Normalisation** – Both stored vectors and query vectors are L₂-normalised;
  hence the dot product used in `query` is equivalent to cosine similarity.
* **Concurrency** – All IO relies on the Eio runtime; the module itself is
  thread-safe provided you do not mutate the returned arrays.
* **Performance** – For a few thousand packages the query takes microseconds
  and allocates almost nothing.

---

## Limitations

* The quality of the ranking is bounded by the quality of the blurb provided at
  build time.  Garbage in, garbage out.
* Re-building the index requires an active network connection and consumes
  one OpenAI *embedding* request.
* Only one vector per package is stored.  The module is **not** suitable for
  fine-grained (document-level) search – use `Vector_db` instead.

---

## See also

* [`Vector_db`](./vector_db.doc.md) – full-text snippet index used *after*
  package selection.
* [`Bin_prot_utils_eio`](./bin_prot_utils_eio.doc.md) – helpers for fast and
  safe serialisation under Eio.

---

*Last updated: <!--DATE-->*

