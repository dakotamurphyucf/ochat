(** Byte-pair encoder/decoder compatible with OpenAI's *tiktoken*.

    The module offers a minimal OCaml implementation of the byte-pair
    encoding (BPE) vocabulary used by OpenAI models such as *ochat-3.5-turbo*
    and *ochat-4*.  After initialising a {!type:codec} with the contents of a
    `*.tiktoken` vocabulary file you can:

    •  convert UTF-8 text to the list of integer token identifiers
       expected by the HTTP APIs ({!val:encode});
    •  perform the inverse mapping ({!val:decode}) – useful for debugging
       and for estimating request sizes.

    The implementation is

    •  {b pure}: all file I/O is left to the caller;
    •  {b dependency-light}: relies on `core`, `base64` and `pcre` only;
    •  {b faithful}: produces the same tokenisation as the reference
       Python library for the supported vocabularies.

    {1   Getting started}

    {[
      (* Load and initialise the vocabulary once *)
      let bpe   = In_channel.read_all "./cl100k_base.tiktoken" in
      let codec = Tikitoken.create_codec bpe in

      (* Count tokens of an arbitrary prompt *)
      let prompt = "Hello 🐫 world!" in
      let ids    = Tikitoken.encode ~codec ~text:prompt in
      printf "prompt length = %d tokens\n" (List.length ids);

      (* Round-trip to verify the implementation *)
      assert (Bytes.to_string (Tikitoken.decode ~codec ~encoded:ids) = prompt);
    ]}

    All public functions are allocation-friendly and run in the caller’s
    fibre – they do not block or perform hidden I/O.
*)

open Core

type vocab_index = (int64, (Bytes.t * int) list) Hashtbl.t

(** Bidirectional BPE vocabulary – a pair of hash tables. *)
type codec =
  { encoder : (bytes, int) Hashtbl.t (** maps byte sequences -> token id *)
  ; decoder : (int, bytes) Hashtbl.t (** inverse mapping *)
  ; vocab_idx : vocab_index
  }

(** [create_codec contents] parses a `*.tiktoken` vocabulary and returns
    a ready-to-use [codec].

    [contents] must follow the format used by the reference
    implementation: one entry per line, a Base64-encoded byte sequence
    followed by a single space and the integer rank, e.g.

    {v RGVm 12345 v}

    The function runs in O(n) where n is the number of entries and
    stores the mappings in two hash tables for O(1) look-ups. *)
val create_codec : string -> codec

(** [encode ~codec ~text] splits [text] with the *tiktoken* regular
    expression and looks up every segment in [codec.encoder].  Segments
    missing from the vocabulary are recursively broken down with the
    byte-pair merge algorithm.

    Returns the list of token identifiers encountered from left to
    right.  The length of the result therefore equals the number of
    produced tokens. *)
val encode : codec:codec -> text:string -> int list

(** [decode ~codec ~encoded] concatenates the byte sequences associated
    with the token identifiers in [encoded].

    Identifiers that are not present in the vocabulary are ignored and
    decoded as the empty string.  The resulting buffer is returned as a
    mutable [bytes] value – use [Bytes.to_string] if an immutable
    [string] is required. *)
val decode : codec:codec -> encoded:int list -> bytes
