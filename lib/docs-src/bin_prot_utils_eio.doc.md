# Bin_prot_utils_eio

Utilities for reading and writing size-prefixed Bin_prot values using
Eio’s non-blocking file-system API.  The functions mirror those in
`Bin_prot_utils` but avoid blocking I/O and integrate naturally with
Eio fibres.

---

## Quick example

```ocaml
module Int_file = Bin_prot_utils_eio.With_file_methods (Int)

Eio_main.run @@ fun env ->
  let path = Eio.Path.(Eio.Stdenv.cwd env / "counter.bin") in
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

All values are encoded with `Bin_prot.Utils.bin_dump ~header:true`.  The
resulting files are therefore fully compatible with the binaries
produced by Async’s `Writer.write_bin_prot`.

---

## Known limitations

1. Only regular files are supported (no arbitrary `Eio.Flow.source`).
2. No internal buffering beyond what `Bin_prot` already does.
3. Reading helpers load the whole file into memory except the `fold`
   variants, which stream.
