# Tikitoken — byte-pair encoding

[Interface](../../lib/tikitoken.mli) · [implementation](../../lib/tikitoken.ml).

## 1 High-level overview

Tikitoken builds a codec from a text vocabulary and encodes/decodes in memory.
File I/O belongs to the caller. The encoder uses Core, Base64 and PCRE; it is
not dependency-free or entirely free of native-library dependencies.

The implementation has one fixed pre-tokenization regex. Loading another
vocabulary does not select that vocabulary's reference regex or special-token
policy. The CLI uses the bundled o200k_base vocabulary; do not infer arbitrary
tiktoken/model compatibility merely from accepting a vocabulary file.

## 2 Vocabulary file format

Each nonblank line contains a Base64 byte sequence and integer rank, separated
by whitespace. Malformed Base64/ranks/rows raise; duplicate ranks can fail codec
construction. Include all single-byte tokens required by the input.
The codec contains encoder/decoder tables and a rolling-hash vocabulary index.

## 3 Public API walk-through

`create_codec contents` parses the vocabulary once. `encode ~codec ~text`
returns all token IDs. `decode ~codec ~encoded` concatenates token bytes and
silently ignores unknown IDs. Decoded bytes need not constitute valid UTF-8.

Encoding expects input accepted by the UTF-8 PCRE regex. There is no exposed
streaming interface, special-token policy, or model-specific chat-envelope
token accounting.

## 4 Usage examples

```ocaml
let count_file env vocabulary text =
  let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / vocabulary) in
  let codec = Tikitoken.create_codec contents in
  Core.List.length (Tikitoken.encode ~codec ~text)
```

Reuse the codec for repeated calls. For chunking, avoid blindly bisecting UTF-8
bytes; choose valid text boundaries and retokenize each chunk. Do not claim a
fixed token window by slicing arbitrary bytes or decoding arbitrary token
subsets without checking validity.

## 5 Internals & performance notes

The regex yields pieces; exact vocabulary matches take the fast path.
Otherwise a rolling-hash slice index finds vocabulary candidates, verifies bytes
to handle hash collisions, and a min-heap selects adjacent merges by rank.
Temporary node/adjacency arrays and heap entries are allocated for each piece;
the implementation is not recursive byte-string splitting.

The full match array and token lists are materialized. There is no
one-list-cell-per-token allocation bound, fixed small LOC size, or general
constant-memory guarantee.

## 6 Known limitations / future work

No automatic vocabulary/regex pairing, streaming input, or complete provider
billing accounting. Invalid inputs can raise. Token counts are useful estimates,
not a dollar spending cap.
