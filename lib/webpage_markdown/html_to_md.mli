open! Core

(** Convert a parsed HTML [Soup.soup] to an [Omd.doc] AST.  A best-effort,
    loss-tolerant subset of HTML is mapped; unsupported constructs fall back
    to raw HTML blocks so that no information is silently lost. *)
val convert : Soup.soup Soup.node -> Omd.doc

(** [to_markdown_string soup] is the same conversion but immediately rendered
    back to Markdown.  Exposed so that other modules (e.g. [Md_render]) can
    reuse the renderer without re-implementing it. *)
val to_markdown_string : Soup.soup Soup.node -> string
