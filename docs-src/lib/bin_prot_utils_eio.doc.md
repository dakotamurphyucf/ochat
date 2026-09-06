# Bin_prot_utils_eio

Read and write size-prefixed Bin_prot records using Eio filesystem capabilities.
Run inside an Eio fiber. These are generic serialization helpers, not the
agent server's checksummed journal or atomic snapshot publisher.

---

## Quick example

```ocaml
open! Core
module Int_file = Bin_prot_utils_eio.With_file_methods (Int)

let round_trip path =
  Int_file.File.write path 42;
  assert (Int_file.File.read path = 42)
```

---

## Function groups

| Group | Functions | Purpose |
| ----- | --------- | ------- |
| Low-level | `grow_buffer`, `append_bin_list_to_file`, `write_bin_prot'`,<br/>`read_bin_prot'`, `fold_bin_file_list`, `iter_bin_file_list`, `map_bin_file_list` | Work with explicit `writer` / `reader` values |
| Binable | `write_bin_prot`, `read_bin_prot`, `write_bin_prot_list`,<br/>`read_bin_prot_list`, `iter_bin_prot_list`, `fold_bin_prot_list`, `map_bin_prot_list` | Take a `Binable.S` module instead |
| Functor | `With_file_methods (M)` | Generates a `File` sub-module specialised to `M.t` |

Files use `Bin_prot.Utils.bin_dump ~header:true`. The reader type must match
the writer type; a length header is neither a schema identifier nor a checksum.
See the [complete interface](../../lib/bin_prot_utils_eio.mli).

| Operation | Semantics |
|---|---|
| `write_bin_prot'`, `write_bin_prot`, `File.write` | Truncate/create, then write one size-prefixed value. |
| `append_bin_list_to_file`, `write_bin_prot_list`, `File.write_all` | Append records in list order; create the file if absent. Despite its name, `write_all` does **not** truncate. |
| `read_bin_prot'`, `read_bin_prot`, `File.read` | Load the whole file and require exactly one complete record, with no trailing data. |
| `fold_bin_file_list`, `fold_bin_prot_list`, `File.fold` | Decode incrementally and fold in write order. |
| `iter_bin_file_list`, `iter_bin_prot_list`, `File.iter` | Decode incrementally and invoke the callback in write order. |
| `read_bin_file_list`, `read_bin_prot_list`, `File.read_all` | Decode incrementally, collecting all values in write order. |
| `map_bin_file_list`, `map_bin_prot_list`, `File.map` | Decode incrementally, collecting mapped values in write order. |

---

## Known limitations

- EOF is successful only between list records. Incomplete headers/bodies raise;
  callback exceptions, including `End_of_file`, and cancellation propagate.
- List reads/maps retain their complete results; folds/iteration do not. One
  encoded record can still require a large buffer; no record-size cap is set.
- Writes create files with mode 0600; existing permissions are not reset.
  There is no atomic replacement, fsync, transaction, or writer lock.
- A failed append can leave an incomplete tail. Preserve evidence and use a
  format-specific recovery procedure; these readers do not silently repair it.
- Callers relying on historically reversed lists or ignored truncated tails
  must adapt to corrected ordering and strict failure behavior.
- Append compatibility is retained. To replace a list, explicitly prepare a
  new file; do not repeatedly call `write_all` assuming overwrite semantics.
