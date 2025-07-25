open! Core

(** HTML → Markdown conversion.

    This module transforms a parsed HTML {!Soup.soup} tree into OMD’s Markdown
    AST.  The conversion is conservative: every node that cannot be mapped to
    a Markdown construct is re-emitted as a raw HTML block so that no
    information is silently lost.

    {1 Pipeline}

    • Strip obvious “chrome” elements such as navigation bars, scripts or
      style sheets.{br}
    • If the document contains a GitHub/GitLab‐style
      [`<article class="markdown-body">`] element, keep only that subtree.{br}
    • Walk the remaining DOM depth-first and translate each tag to the
      corresponding OMD block or inline constructor.{br}
    • Return the fully populated {!Omd.doc} value.

    The public surface area is intentionally minimal – helpers live in the
    implementation file as they do not need to be reused.
*)

(** [convert soup] traverses [soup] and returns a Markdown {!Omd.doc}.

    The traversal is linear in the number of DOM nodes.  Unsupported or
    malformed fragments are preserved inside [`Html_block`] nodes.

    Example converting a small snippet:
    {[
      let html = "<p><em>Hi</em> <strong>there</strong></p>" in
      let soup = Soup.parse html in
      let md   = Html_to_md.convert soup in
      Md_render.to_string md = "*Hi* **there**"
    ]}
*)
val convert : Soup.soup Soup.node -> Omd.doc

(** [to_markdown_string soup] is [convert soup] followed by
    {!Md_render.to_string}.  It returns the Markdown as a raw [string].

    The function never raises.  If the input cannot be parsed the original
    HTML is wrapped in a fenced [```html] block.

    Example:
    {[
      let md = Html_to_md.to_markdown_string (Soup.parse "<h1>Title</h1>") in
      (* md = "# Title" *)
    ]}
*)
val to_markdown_string : Soup.soup Soup.node -> string
