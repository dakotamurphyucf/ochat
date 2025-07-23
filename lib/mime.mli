(** Helpers for dealing with MIME / "media" types.

    This module provides two simple utilities that cover the most common
    needs when serving files over HTTP or serialising data to JSON:

    {1 Overview}

    • {!guess_mime_type} – try to derive a MIME type from the filename
      extension.
    • {!is_text_mime}   – predicate that tells whether a MIME type is
      textual (i.e. starts with ["text/"]).  This is handy when deciding
      whether binary data must be base64-encoded in JSON payloads.

    The implementation recognises only a **small** whitelist of extensions
    (< 10).  For everything else the generic ["application/octet-stream"] is
    returned.  If the filename has {i no} extension the function yields
    [None].  Feel free to extend the mapping to suit your application.
*)

(** [guess_mime_type filename] inspects [filename]’s extension and returns a
    best-effort MIME type.

    Recognised mappings:
    {ul
      {- [".ml"], [".mli"], [".txt"], [".md"]   → ["text/plain"]}
      {- [".json"]                                → ["application/json"]}
      {- [".csv"]                                 → ["text/csv"]}
      {- [".png"]                                 → ["image/png"]}
      {- [".jpg"], [".jpeg"]                      → ["image/jpeg"]}
      {- [".gif"]                                 → ["image/gif"]}
      {- [".pdf"]                                 → ["application/pdf"]}
    }

    All comparisons are case-insensitive.  For unrecognised extensions the
    function returns [Some "application/octet-stream"].  It returns [None]
    only when [filename] contains no extension at all.

    Example guessing the type of a PNG file:
    {[
      Mime.guess_mime_type "logo.PNG" = Some "image/png"
    ]} *)
val guess_mime_type : string -> string option

(** [is_text_mime mime] is [true] iff [mime] starts with ["text/"].

    This is useful when embedding resources in JSON: textual data can be
    inserted verbatim whereas binary data should usually be base64-encoded.

    {[
      Mime.is_text_mime "text/html"        = true;
      Mime.is_text_mime "application/json" = false
    ]} *)
val is_text_mime : string -> bool
