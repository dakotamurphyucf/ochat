(** ChatMarkdown pre-processing hook.

    The module offers a single public helper – {!val:preprocess} – that can
    be inserted in front of the main ChatMarkdown parser.  When it is
    *disabled* the function is a no-op and therefore has no impact on
    performance.  When it is *enabled* it applies the {e Recursive
    Meta-Prompting} refinement loop (see {!module:Recursive_mp}) to the raw
    prompt string and returns the refined version.

    Enablement is controlled by either of the following:

    • Setting the environment variable [OCHAT_META_REFINE] to a *truthy*
      value – one of ["1"], ["true"], ["yes"], ["on"] (case-insensitive).
    • Placing the HTML comment [<!-- META_REFINE -->] anywhere inside the
      prompt.

    When active, the refinement loop is executed synchronously.  The
    resulting prompt is guaranteed to remain valid ChatMarkdown: extra
    metadata is injected as HTML comments so that downstream parsers remain
    oblivious to the transformation.

    {1 Example}

    Invoking the pre-processor explicitly:

    {[
      let original = "<!-- META_REFINE -->\n<user>Translate to French</user>" in
      let refined  = Preprocessor.preprocess original in
      print_endline refined
    ]}

    In most situations callers do not need to interact with this module
    directly – {!Chatmd.Prompt.parse_chat_inputs} already pipes its input
    through [preprocess].
*)

(** [preprocess raw] returns [raw] unchanged if meta-refinement is disabled
    (see above).  Otherwise it runs {!Recursive_mp.refine} on [raw] and
    returns the improved prompt. *)
val preprocess : string -> string
