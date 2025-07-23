# Tikitoken – developer documentation

This document **complements** the inline `odoc` comments of
`tikitoken.{mli,ml}` with a more conversational overview, examples that
include I/O, and background material that does **not** belong in the API
reference.

## Table of contents

1. High-level overview  
2. Vocabulary file format  
3. Public API walk-through  
4. Usage examples  
5. Internals & performance notes  
6. Known limitations / future work

---

## 1  High-level overview

`Tikitoken` is a *pure* OCaml implementation of the byte-pair encoding
(BPE) used by the official Python **tiktoken** library.  It lets you
count tokens or round-trip prompts **without** shelling out to Python or
linking to C foreign code.  The module comes in at <200 LOC and has no
mutable global state, which makes it easy to embed in servers, CLI
tools or tests.

Typical workflow:

```ocaml
let bpe_contents = In_channel.read_all "./cl100k_base.tiktoken" in
let codec        = Tikitoken.create_codec bpe_contents in
let tokens       = Tikitoken.encode ~codec ~text:"Hello world!" in
printf "prompt consumes %d tokens\n" (List.length tokens);
```

## 2  Vocabulary file format

The reference library ships binary-compatible files such as
`cl100k_base.tiktoken`.  Each line contains:

```
<base64-bytes> <rank>\n
```

* *`<base64-bytes>`* – a Base64 representation of an **arbitrary** byte
  sequence (not necessarily valid UTF-8)
* *`<rank>`* – integer token id as used by the OpenAI HTTP APIs

The format is parsed by {!Tikitoken.create_codec} which builds two hash
maps for constant-time look-ups.

## 3  Public API walk-through

### Building a codec

```ocaml
val create_codec : string -> codec
```

Runs in `O(n)` where *n* is the number of vocabulary entries and returns
an in-memory bidirectional mapping.

### Encoding

```ocaml
val encode : codec:codec -> text:string -> int list
```

Splits the input string with the original **tiktoken** regex, performs
exact look-ups in the encoder table, and falls back to the recursive
byte-pair merge for out-of-vocabulary segments.  The function allocates
at most one small OCaml list cell per token.

### Decoding

```ocaml
val decode : codec:codec -> encoded:int list -> bytes
```

Simply concatenates the byte sequences referenced by the ids.  It is
meant for debugging; production code usually only needs the *length* of
the encoded list.

## 4  Usage examples

### 4.1  Counting tokens in a Markdown cell

```ocaml
let token_count md_cell =
  let bpe   = In_channel.read_all "cl100k_base.tiktoken" in
  let codec = Tikitoken.create_codec bpe in
  List.length (Tikitoken.encode ~codec ~text:md_cell)
```

### 4.2  Chunking a large document into 8 k-token windows

```ocaml
let rec split_into_chunks ~codec ~max_len text =
  let tokens = Tikitoken.encode ~codec ~text in
  if List.length tokens <= max_len then [ text ]
  else
    (* naïve strategy: cut the text in half and recurse *)
    let half     = String.length text / 2 in
    let left     = String.sub text 0 half in
    let right    = String.sub text half (String.length text - half) in
    split_into_chunks ~codec ~max_len left
    @ split_into_chunks ~codec ~max_len right
```

## 5  Internals & performance notes

* The heavy lifting happens in `byte_pair_merge` which implements the
  greedy merge loop exactly as described in the original paper.
* Both encoder and decoder use `Core.Hashtbl` with the default
  (~polymorphic) hash which is fast enough for vocabularies of O(100 k).
* Regular-expression splitting relies on the `pcre` library because the
  original pattern makes use of Unicode property escapes (`\p{L}`).

## 6  Known limitations / future work

* {p Lazy evaluation} – the current implementation allocates the *whole*
  encoded list even if the caller only needs its length.
* {p Streaming} – decoding works only on complete lists; streaming input
  would require a stateful interface.
* {p Error handling} – unknown token ids are silently ignored and out-of-
  vocabulary Unicode codepoints degrade to their byte representation.

Contributions welcome! 😉

