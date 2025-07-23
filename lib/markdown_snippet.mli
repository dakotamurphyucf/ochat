
open! Core

(** Token-bounded slicing of arbitrary Markdown documents.

    This module provides a single high-level function {!slice} that breaks a
    Markdown buffer into overlapping windows that respect the token limits of
    common embedding models (OpenAI Ada, etc.).  It is primarily used by the
    markdown-indexing pipeline but can be reused in any context where large
    documents need to be chunked before vectorisation.

    {1 Metadata}

    Each slice comes with an accompanying {!module:Meta} record describing its
    provenance.  Values are stable under serialisation via
    {{!val:Core.Sexp.sexp_of_t}S-expressions} and {{!val:Bin_prot}Bin_prot}
    thanks to the [@@deriving] annotations in the implementation.

    The {!field:Meta.id} field is a deterministic MD5 hash of the *header*
    prepended to the slice plus the slice body itself.  It can therefore be
    used as a primary key for deduplication across indexing runs.

    {1 Chunking strategy}

    • Target size   : 64 ≤ tokens ≤ 320.  
    • Overlap       : 64 tokens (≈ 20 %).  
    • Split points  : ATX headings, thematic breaks ("---", "***", "___"),
      blank lines, fenced code fences, and GitHub-flavoured table rows.

    The heavy lifting is done in an internal [Chunker] module.  Entire code
    fences and tables are preserved – they will never be broken across slice
    boundaries.

    {1 Thread safety & performance}

    Token counting relies on {{!module:Tikitoken}Tikitoken}.  Calls are
    accelerated by a process-wide, [5000]-entry {!module:Lru_cache}, guarded
    by an {!module:Eio.Mutex}.  All exposed functions are therefore safe to
    invoke concurrently from multiple fibres.
*)

module Meta : sig
  type t =
    { id : string
    ; index : string
    ; doc_path : string
    ; title : string option
    ; line_start : int
    ; line_end : int
    }
  [@@deriving sexp, bin_io, compare, hash]

  (** Metadata attached to every slice.

      • [id]         – stable MD5 hash of the slice including header.  
      • [index]      – logical index name supplied by the caller
                        (e.g. "my-repo").  
      • [doc_path]   – relative path of the source Markdown file.  
      • [title]      – first H1/H2 heading in the file, if any (cached for
                        convenience).  
      • [line_start] – 1-based line number of the first line contained in the
                        slice.  
      • [line_end]   – 1-based line number of the last line contained in the
                        slice.
  *)
end

val slice
  :  index_name:string
  -> doc_path:string
  -> markdown:string
  -> tiki_token_bpe:string
  -> unit
  -> (Meta.t * string) list

(** [slice ~index_name ~doc_path ~markdown ~tiki_token_bpe ()] returns a list
    of token-bounded slices.

    Each element [(meta, text)] consists of the slice body [text] prefixed by
    a short OCaml comment header plus its corresponding {!Meta.t} meta-data.

    The function guarantees:

    • {!text} length in tokens lies within the inclusive range
      {b 64 – 320} unless the document itself is shorter.  
    • Consecutive slices overlap by 64 tokens to preserve context.  
    • No slice breaks inside fenced code blocks or GitHub-style tables.  

    The header injected at the top of every slice encodes provenance in a
    machine-parsable form:  
    {v
    (** Package:<index_name> Doc:<doc_path> Lines:<line_start>-<line_end> *)
    v}

    The function is deterministic: repeated invocations with identical inputs
    (including the [tiki_token_bpe] vocabulary) yield byte-identical output.

    @param index_name Logical name of the index (e.g. project slug).  Used in
                      the header and {!Meta.index}.
    @param doc_path   Relative file path of the source Markdown document.
    @param markdown   Full contents of the Markdown file (UTF-8).
    @param tiki_token_bpe Contents of a TikToken BPE vocabulary file.  The
                          codec is cached and reused across calls.
    @return Ordered list of slices, top-to-bottom.

    {2 Example}

    {[
      let tiki_bpe = In_channel.read_all "gpt4.tiktoken" in
      let markdown = In_channel.read_all "README.md" in
      let slices = Markdown_snippet.slice
        ~index_name:"my-project"
        ~doc_path:"README.md"
        ~markdown
        ~tiki_token_bpe:tiki_bpe
        ()
      in
      List.iter slices ~f:(fun (meta, txt) ->
        printf "%s\n---\n%!" (Sexp.to_string_hum (Meta.sexp_of_t meta));
        printf "%s\n" txt)
    ]}
*)
