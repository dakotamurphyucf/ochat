(** Token-bounded slicing of Markdown documentation.

    [`Odoc_snippet`] turns a single Markdown document – typically the
    result of running

    {v
      odoc html --output-dir=_doc/_html <pkg>.odoc
    v}

    followed by an HTML-to-Markdown converter – into an array of
    *snippets* optimised for retrieval-augmented generation systems.

    Each snippet is:

    • between **64** and **320** BPE tokens (as estimated by
      {!Tikitoken.encode});
    • overlapped by **64** tokens with its neighbour to give the LLM
      additional context;
    • prefixed with a small ocamldoc-style header so that the document
      viewer can later reconstruct source links.

    The core algorithm is exposed as {!val:slice}.  It is purely
    functional and CPU-bound – safe to call concurrently from multiple
    domains.

    {1 Example}

    ```ocaml
    let markdown = In_channel.read_all "Switch/index.md" in
    let snippets =
      Odoc_snippet.slice
        ~pkg:"eio"
        ~doc_path:"Eio/Switch/index.html"
        ~markdown
        ~tiki_token_bpe:"gpt-4"
        ()
    in
    List.iter snippets ~f:(fun (meta, body) ->
      printf "id=%s lines=%d-%d\n" meta.id meta.line_start meta.line_end)
    ```
*)

(** Metadata attached to every snippet. *)
type meta =
  { id : string
    (** Stable MD5 of the *full* snippet including header – can be
            used as a cache key. *)
  ; pkg : string (** Opam package this snippet belongs to (e.g. ["core"]). *)
  ; doc_path : string
    (** Relative path of the originating HTML page, for example
            ["Eio/Switch/index.html"].  Used to build jump-to-source
            links in UIs. *)
  ; title : string option
    (** First markdown heading ([# ] or [## ]) encountered in the
            document, if any. *)
  ; line_start : int
    (** 1-based inclusive starting line (after HTML->Markdown
            conversion). *)
  ; line_end : int
    (** Inclusive ending line.  Together with [line_start] this
            defines the coverage of the snippet within the source
            document. *)
  }
[@@deriving sexp, bin_io, hash, compare]

(** [slice ~pkg ~doc_path ~markdown ~tiki_token_bpe ()] partitions
    [markdown] into overlapping windows and returns them in document
    order.

    Each returned [text] begins with a header of the form:

    {v
    (** Package:<pkg> Module:<module_path> Lines:<start>-<end> *)

    <body>
    v}

    where [module_path] is inferred from [doc_path] ("README" for
    package top-level READMEs).

    Arguments:

    • [pkg] — opam package name;
    • [doc_path] — relative path of the HTML page the markdown was
      derived from;
    • [markdown] — UTF-8 markdown body to slice;
    • [tiki_token_bpe] — JSON BPE definition passed to
      {!Tikitoken.create_codec} (e.g. ["gpt-4"]).

    Returns: a list of [(meta, text)] pairs in the same order as the
    source document.  No guarantees are made about the exact number of
    snippets produced except that it is ≥ 1 for non-empty input.

    The function never raises; in case of internal tokenisation
    errors it falls back to a simple length-based heuristic. *)
val slice
  :  pkg:string
  -> doc_path:string
  -> markdown:string
  -> tiki_token_bpe:string
  -> unit
  -> (meta * string) list

(** {1 Low-level helpers}

    The {!module-Chunker} sub-module is exposed for advanced usage
    such as interactive viewers or unit tests that need to inspect
    the raw block segmentation performed by the algorithm.  Most
    applications should stick to {!slice}. *)

module Chunker : sig
  (** [chunk_by_heading_or_blank lines] groups [lines] into
      higher-level blocks:

      • headings starting with [#], [##] or [###];
      • fenced code blocks delimited by [` ``` ` fences];
      • markdown tables (consecutive lines beginning with [`|`]);
      • paragraphs separated by blank lines.

      The function is deterministic and preserves the original order
      of lines.  It uses heuristics fine-tuned for *odoc*-generated
      Markdown and is **not** a general-purpose parser. *)
  val chunk_by_heading_or_blank : string list -> string list
end
