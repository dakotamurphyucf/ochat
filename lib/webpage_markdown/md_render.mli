(** Minimal Markdown pretty-printer used by {!Html_to_md}.

    [`Md_render`] converts an [Omd.doc] – the abstract syntax tree used by the
    {!https://github.com/ocaml/omd Omd} Markdown library – into a textual
    document that renders correctly on GitHub, GitLab and any other
    CommonMark-compliant engine.

    The implementation deliberately supports only the node kinds emitted by
    {!Html_to_md.convert}.  Unknown or unsupported nodes are preserved inside
    fenced `html` blocks so that *no information is ever lost* during the
    HTML → Markdown round-trip.

    The conversion is pure, deterministic and never raises.
*)

(** [to_string doc] returns a UTF-8 encoded string containing the Markdown
    representation of [doc].

    Unknown nodes fall back to fenced `html` blocks.

    Example – render a small document with emphasis and a link:

    {[
      let doc : Omd.doc =
        [ Omd.Heading ([], 1, Omd.Text ([], "Example"));
          Omd.Paragraph
            ( [],
              Omd.Concat
                ( [],
                  [ Omd.Text ([], "Some ");
                    Omd.Emph ([], Omd.Text ([], "italics"));
                    Omd.Text ([], " and a ");
                    Omd.Link
                      ( [],
                        { Omd.label = Omd.Text ([], "link");
                          destination = "https://example.com";
                          title = None } ) ] ) ) ]
      in
      print_endline (Md_render.to_string doc)
      (* Output:
         # Example

         Some *italics* and a [link](https://example.com)
      *)
    ]}
*)
val to_string : Omd.doc -> string
