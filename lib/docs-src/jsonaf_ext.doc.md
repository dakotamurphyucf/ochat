# Jsonaf_ext

`Jsonaf_ext` is a very small helper module that bridges two widely-used
Jane-Street libraries:

* [`Jsonaf`](https://github.com/janestreet/jsonaf) – a JSON AST with
  convenient parsing and pretty-printing functions.
* [`bin_prot`](https://github.com/janestreet/bin_prot) – a fast binary
  serialisation protocol that underpins `[@@deriving bin_io]`.

The module simply **re-exports the full `Jsonaf` API** and, in addition,
implements the Bin\_prot type-class trio so that a `Jsonaf.t` value can be
embedded inside any data structure that relies on `bin_io` for
serialisation.

```ocaml
val bin_shape_t  : Bin_prot.Shape.t
val bin_size_t   : Jsonaf.t -> int
val bin_write_t  : (Bin_prot.Common.buf, Jsonaf.t) Bin_prot.Write.writer
val bin_reader_t : Jsonaf.t Bin_prot.Type_class.reader
val bin_writer_t : Jsonaf.t Bin_prot.Type_class.writer
val bin_t        : Jsonaf.t Bin_prot.Type_class.t
```

---

## How it works

1. `Jsonaf_ext` converts the JSON value to its textual form using
   `Jsonaf.to_string`.
2. The resulting UTF-8 string is serialised with the standard
   `bin_prot` string writer.
3. Deserialisation performs the inverse steps.

The process is loss-less with respect to the JSON semantics (although the
exact whitespace produced by `to_string` is not preserved).

### Round-trip guarantee

```ocaml
open Core

let () =
  let json  = `Array [ `String "hello" ; `Null ] in
  let size  = Jsonaf_ext.bin_size_t json in
  let buf   = Bigstring.create size in
  ignore (Jsonaf_ext.bin_write_t buf ~pos:0 json);
  let json' = Jsonaf_ext.bin_read_t buf ~pos_ref:(ref 0) in
  assert (Jsonaf.exactly_equal json json')
```

## Usage in user-defined types

Because `Jsonaf_ext` exposes `bin_writer_t`, `bin_reader_t` and `bin_t`, the
usual `[@@deriving bin_io]` works out-of-the-box:

```ocaml
type record =
  { id      : int
  ; payload : Jsonaf.t
  } [@@deriving bin_io]

(* serialise to a file *)
let write_record file r =
  let oc = Out_channel.create ~binary:true file in
  Bin_prot.Utils.bin_dump
    Jsonaf_ext.bin_t
    oc
    r;
  Out_channel.close oc

(* read it back *)
let read_record file =
  let ic = In_channel.create ~binary:true file in
  let r  = Bin_prot.Utils.bin_read
             Jsonaf_ext.bin_t
             ic in
  In_channel.close ic; r
```

## Function reference

• **`bin_shape_t`** – `Bin_prot.Shape.t` describing the on-wire format.

• **`bin_size_t t`** – size (bytes) of the serialised form of `t`.

• **`bin_write_t buf ~pos t`** – serialise `t` into a bigstring. Returns the
  position immediately _after_ the written data.

• **`bin_writer_t` / `bin_reader_t`** – standalone writer / reader values;
  handy when you need to pass them to generic functions such as
  `Bin_prot.Utils.bin_dump`.

• **`bin_t`** – the complete type-class bundle `{ writer; reader; shape }`.

## Limitations & Caveats

1. **Textual overhead** – the encoded form is the raw JSON text plus a small
   Bin\_prot header, so it is not the most space-efficient representation for
   very large documents.
2. **Formatting dependency** – any change to `Jsonaf.to_string`’s pretty-
   printing policy will alter the binary output, potentially breaking forward
   compatibility.  If you need long-term stability store a version tag next
   to the data.

---

